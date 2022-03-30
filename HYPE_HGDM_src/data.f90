!> \file data.f90
!> Contains module datamodule.

!> \brief Read and process model input from files, both static data
!> and forcing.
!>
!> Procedures for observations, for reading and preparing input data, 
!> for saving result files, and for loading and saving initial states
MODULE DATAMODULE
  !Copyright 2011-2020 SMHI
  !
  !This file is part of HYPE.
  !
  !HYPE is free software: you can redistribute it and/or modify it under
  !the terms of the Lesser GNU General Public License as published by
  !the Free Software Foundation, either version 3 of the License, or (at
  !your option) any later version. HYPE is distributed in the hope that
  !it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  !warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  !the Lesser GNU General Public License for more details. You should
  !have received a copy of the Lesser GNU General Public License along
  !with HYPE. If not, see <http://www.gnu.org/licenses/>.
  !
  !--------------------------------------------------------------------
  USE COMPOUT, ONLY : find_variable_index_type
  USE CONVERT, ONLY : lower_case, &
                      string_convert_to_DateType    
  USE LIBDATE
  USE READWRITE_ROUTINES, ONLY : count_data_cols, &
                                 count_data_rows, &
                                 read_basindata5, &
                                 read_basindata6, &
                                 read_column_headings, &
                                 read_column_headings2, &
                                 read_matrix,  &
                                 read_next_codestr_on_line, &
                                 add_number_to_filename, &
                                 read_parameterline
  USE TIMEROUTINES, ONLY : get_dayno_from_monthday, &
                           period_length, &
                           set_timestep_variables,  &
                           add_date_to_array, &
                           check_if_date_in_period
  !Subroutines also uses modvar, worldvar, modelmodule, optimization

  IMPLICIT NONE
  PRIVATE
  !----------------------------------------------
  ! Private procedures 
  !----------------------------------------------
  ! load_one_forcing_variabel
  ! get_forcing_index
  ! save_xoreginformation
  ! set_update_nutrientcorr
  ! get_one_current_forcing_data
  ! get_current_flow
  ! get_current_otherobs
  ! get_current_oregobs
  ! read_parameter_ensemble_task
  ! calculate_special_optpar_parameters
  ! find_connected_lakebasins
  ! load_lakedata
  ! load_damdata
  ! load_flooddata
  ! start_lakedata_table
!  ! finish_lakedata_table
  ! read_pointsourcedata
  ! read_monthly_leakage
  ! read_first_monthly_leakage
  ! read_one_year_monthly_leakage
  ! load_management_data
  ! load_forcing_information_data
  ! read_update_data
  ! get_subid_index
  ! set_irrigation_season_end
  ! set_yesno_variable_from_line
  ! count_number_of_output
  ! check_output_variables
  ! set_outvar_output
  ! set_outvar_crit
  ! open_subbasinfiles
  ! close_subbasinfiles
  ! check_outputfiles_for_illegal_input
  ! open_timefiles
  ! close_timefiles
  ! create_filename_for_basin
  ! create_filename_for_variable
  ! get_parametervalues
  ! get_unit_string
  ! write_ensemble_simulations_heading
  ! count_parfile_simulations
  ! read_simulation_perf_and_par
  PUBLIC ::  load_observations,&
             load_ascii_forcing,&
             load_ascii_qx_observations,&
             reset_observations,&
             prepare_for_update,&
             get_current_forcing,&
             close_observations,&
             load_basindata,&
             load_geoclass, &
             load_classdata, &
             set_model_base_configuration,&
             set_model_configuration,&
             initiate_model_parameters,&
             load_aquiferdata,&
             load_glacierdata,&
             load_branchdata,&
             finish_lakedata_table, &
             read_and_calc_basindata, &
             load_pointsourcedata,&
             get_current_pointsources, &
             load_permanent_soilleakage,&
             get_current_soilleakage,&
             calculate_path,&
             reform_inputdata_for_submodel,&
             load_cropdata,&
             get_hyss_arguments,&
             load_coded_info,&
             load_submodel_info,&
             set_outvar,&
             set_outvar_for_variable_new,&
             prepare_subbasin_output,&
             load_output_regions,&
             save_mapfiles,&
             write_subbasin_assessment,&
             write_simulation_assessment,&
             prepare_outputfiles, &
             close_outputfiles, &
             write_subbasinfiles, &
             write_subbasinfiles_in_parallel, &
             write_regionfiles, &
             write_regionfiles_in_parallel, &
             write_subbasinfiles_class, &
             write_subbasinfiles_class_in_parallel, &
             write_timefiles_class, &
             write_timefiles_class_in_parallel, &
             write_timefiles, &
             write_timefiles_in_parallel, &
             load_otest, &
             load_parameters,&
             load_optpar,&
             save_respar,&
             initiate_output_routines,&
             initiate_outvar,&
             set_outvar_test_information,&
             revise_outvar,&
             save_ensemble_simulations,&
             prepare_save_all_simulations,&
             write_simulation_results_perf_and_par,&
             count_ensemble_simulations,&
             load_ensemble_simulations,&
             save_loadfiles

CONTAINS
  
  !>Load forcing data files and other observations into the program
  !---------------------------------------------------------------------------------
  SUBROUTINE load_observations(dir,ns,bdate,edate,ndt,status) 
    
    USE WORLDVAR, ONLY : readformat
    
    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir     !<File directory (forcingdir)
    INTEGER, INTENT(IN)  :: ns              !<Number of subbasins, basemodel
    TYPE(datetype), INTENT(IN)  :: bdate    !<Begin time for simulation
    TYPE(datetype), INTENT(IN)  :: edate    !<End time for simulation
    INTEGER, INTENT(IN)  :: ndt             !<Number of timesteps
    INTEGER, INTENT(OUT) :: status          !<Error status of subroutine
    
    !> \b Algorithm \n
    !>Load observations from files
    IF(readformat==0)THEN   !ASCII-files
      CALL load_ascii_forcing(dir,ns,bdate,edate,ndt,status) 
      IF(status/=0) RETURN
      CALL load_ascii_qx_observations(dir,ns,ndt,bdate,edate,status) 
      IF(status/=0) RETURN
    ENDIF

  END SUBROUTINE load_observations
  
  !-----------------------------------------------------------------------------------------
  !>Reads forcing data from file. Also checks data.
  !!
  !>\b Consequences Module worldvar variables forcingdata and dates may change.
  !!Module modvar variables preci, tempi, snowfraci, shortwavei, windi, humidi,
  !!tmini, tmaxi, uwindi, vwindi may be allocated.
  !-----------------------------------------------------------------------------------------
  SUBROUTINE load_ascii_forcing(dir,ns,bdate,edate,ndt,status) 
    
    USE WORLDVAR, ONLY : dates, &       !OUT
                         readdaily, &
                         get_seq_filename, &
                         forcingdata, &
                         max_forcingdata, &
                         i_pobs,i_tobs, &
                         i_rhobs,i_sfobs, &
                         i_swobs,i_uobs, &
                         i_tminobs,i_tmaxobs, &
                         i_uwobs,i_vwobs
    USE MODVAR, ONLY : preci,tempi, &
                       humidi,snowfraci, &
                       shortwavei,windi, &
                       tmini,tmaxi, &
                       uwindi,vwindi
                       
    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir  !<File directory (forcingdir)
    INTEGER, INTENT(IN)  :: ns           !<Number of subbasins, basemodel
    TYPE(DateType), INTENT(IN)  :: bdate !<Begin simulation date=bdate
    TYPE(DateType), INTENT(IN)  :: edate !<End simulation date=sdate
    INTEGER, INTENT(IN)  :: ndt          !<Number of timesteps in simulation
    INTEGER, INTENT(OUT) :: status       !<Error number
    
    !Local variables
    INTEGER iforc
    CHARACTER(LEN=16) :: fname

    status = 0

    !Initial allocation of model variables
    IF(forcingdata(i_pobs)%readfile) ALLOCATE(preci(ns))
    IF(forcingdata(i_tobs)%readfile) ALLOCATE(tempi(ns))
    IF(forcingdata(i_sfobs)%readfile) ALLOCATE(snowfraci(ns))
    IF(forcingdata(i_swobs)%readfile) ALLOCATE(shortwavei(ns))
    IF(forcingdata(i_uobs)%readfile)  ALLOCATE(windi(ns))
    IF(forcingdata(i_rhobs)%readfile) ALLOCATE(humidi(ns))
    IF(forcingdata(i_tminobs)%readfile) ALLOCATE(tmini(ns))
    IF(forcingdata(i_tmaxobs)%readfile) ALLOCATE(tmaxi(ns))
    IF(forcingdata(i_uwobs)%readfile)  ALLOCATE(uwindi(ns))
    IF(forcingdata(i_vwobs)%readfile)  ALLOCATE(vwindi(ns))

    IF(.NOT. readdaily)THEN
      IF(.NOT.ALLOCATED(dates)) ALLOCATE(dates(ndt))
    ENDIF
    
    !Set path and name of files to be used
    DO iforc = 1,max_forcingdata
      IF(forcingdata(iforc)%readfile)THEN
        fname = forcingdata(iforc)%filename
        CALL get_seq_filename(fname)
        forcingdata(iforc)%filepath = TRIM(dir)//TRIM(fname)
      ENDIF
    ENDDO
    
    !Check data and prepare files to be read, possibly read data to memory
    DO iforc = 1,max_forcingdata
      CALL load_one_forcing_variabel(ns,ndt,bdate,edate,forcingdata(iforc),status)
      IF(status/=0) RETURN
    ENDDO

  END SUBROUTINE load_ascii_forcing

  !>Load one observation forcing file; check file, saves data or prepare to read continously
  !-----------------------------------------------------------------------------------------
  SUBROUTINE load_one_forcing_variabel(ns,ndt,bdate,edate,forcobs,status) 
  
    USE WORLDVAR, ONLY : fileunit_temp, &
                         forcingdatatype, &
                         set_timeformat, &
                         dates, &       !OUT (ev.)
                         i_t, &
                         readdaily, &
                         readmatlab, &
                         fileunit_get, &
                         fileunit_free
    USE MODVAR, ONLY : missing_value
    USE READWRITE_ROUTINES, ONLY : check_obs_timeperiod, &
                                   check_file_station_id_order, &
                                   prepare_read_matrix
                             
    !Argument declarations
    INTEGER, INTENT(IN)  :: ns           !<Number of subbasins, basemodel
    INTEGER, INTENT(IN)  :: ndt          !<Number of timesteps in simulation
    TYPE(DateType), INTENT(IN)  :: bdate !<Begin simulation date=bdate
    TYPE(DateType), INTENT(IN)  :: edate !<End simulation date=sdate
    TYPE(FORCINGDATATYPE),INTENT(INOUT) :: forcobs !<Forcing data information and values
    INTEGER, INTENT(OUT) :: status       !<Error number
    
    !Local variables
    LOGICAL fileexist
    LOGICAL notimefound
    INTEGER nt                         !Number of simulation time steps
    TYPE(DateType) fbdate,fedate       !Begin and end date of file
  
    status = 0
    IF(forcobs%readfile)THEN
      
      !Check that file exist
      INQUIRE(FILE=TRIM(forcobs%filepath),EXIST=fileexist)
      IF(.NOT.fileexist)THEN
        WRITE(6,*) 'ERROR: Forcing data file is missing'
        WRITE(6,*) 'ERROR: ', TRIM(forcobs%filepath)
        status = 1
        RETURN
      ENDIF
      WRITE(6,*) 'File opened: ', TRIM(forcobs%filepath)
      
      !Get and check stations
      IF(.NOT.ALLOCATED(forcobs%basinindex))ALLOCATE(forcobs%basinindex(ns))
      CALL check_file_station_id_order(fileunit_temp,forcobs%filepath,ns,forcobs%stationid,forcobs%basinindex,forcobs%ncols,status)
      IF(status.NE.0) RETURN
      IF(ALLOCATED(forcobs%stationid)) DEALLOCATE(forcobs%stationid)

      !Check time period
      CALL check_obs_timeperiod(fileunit_temp,forcobs%filepath,1,i_t,bdate,   &
             edate,fbdate,fedate,notimefound,status)
      IF(status.NE.0)THEN
        WRITE(6,*) 'ERROR: Forcing data times are missing'
        WRITE(6,*) 'ERROR: ', TRIM(forcobs%filepath)
        RETURN
      ENDIF  
      CALL set_timeformat(notimefound)
    
      !Load forcing data
      forcobs%fileunit = fileunit_get()
      CALL prepare_read_matrix(forcobs%fileunit,forcobs%filepath,1,bdate,status)
      IF(status.NE.0) RETURN
      IF(readdaily)THEN
        WRITE(6,*) 'File ready: ', TRIM(forcobs%filepath)
      ELSE
        ALLOCATE(forcobs%allvalues(ndt,forcobs%ncols))
        CALL READ_MATRIX(forcobs%fileunit,ndt,forcobs%ncols,edate,nt,   &
             dates,forcobs%allvalues,missing_value,readmatlab) 
        CLOSE(forcobs%fileunit)
        CALL fileunit_free(forcobs%fileunit)
        WRITE(6,*) 'File loaded: ', TRIM(forcobs%filepath)
      ENDIF
    ENDIF
    
  END SUBROUTINE load_one_forcing_variabel

  !>Find index of forcing based on filename
  !Added to be used for reading AssimInfo later.
  !--------------------------------------------------------------
  INTEGER FUNCTION get_forcing_index(name)
  
  USE CONVERT, ONLY : get_lower_case
  USE WORLDVAR, ONLY : max_forcingdata, &
                       forcingdata
  
    !Arguments
    CHARACTER(LEN=10), INTENT(IN) :: name
  
    !Local variables
    INTEGER i
    
    get_forcing_index = 0
    DO i = 1, max_forcingdata
      IF(get_lower_case(10,name)==get_lower_case(10,forcingdata(i)%filename))THEN
        get_forcing_index = i
        RETURN
      ENDIF
    ENDDO

  END FUNCTION get_forcing_index

  !>Reads Qobs and Xobs data values in file
  !!
  !>\b Consequences Module worldvar variables qobs, xobs, xoregobs, xcol, xorcol, readqobs,
  !!qobsindex, numqobsstn, bqdate, eqdate, bxdate, exdate, bxrdate and exrdate may change.
  !!Module modvar variables qobsi, xobsi and xoregobsi may be allocated, and 
  !!xobsindex and xoregindex may change.
  !-----------------------------------------------------------------------------------------
  SUBROUTINE load_ascii_qx_observations(dir,ns,ndt,bdate,edate,status) 

    USE WORLDVAR, ONLY : qobs,    & !OUT
         readqobs,     &  !OUT
         qobsindex,    &  !OUT
         numqobsstn,   &  !OUT
         xobs,    &   !OUT
         xoregobs,    &   !OUT
         readoutregion, &
         xcol,    &   !OUT
         xorcol,    &   !OUT
         readdaily,       &
         readmatlab,      &
         fileunit_qobs,   &
         fileunit_xobs,   &
         fileunit_xoreg,   &
         fileunit_temp,   &
         filename_Xobs,   &
         filename_Xoreg, &
         filename_Qobs,   &
                         i_t, &
         bqdate,  & !OUT
         eqdate,  & !OUT
         bxdate,  & !OUT
         exdate,  &  !OUT
         bxrdate,  & !OUT
         exrdate, &  !OUT
         noutreg
    USE MODVAR, ONLY : basin,     &
         qobsi,     & !OUT (allocated)
         xobsi,     & !OUT (allocated)
         xoregobsi,     & !OUT (allocated)
         xobsindex, & !OUT
         xoregindex, & !OUT
         missing_value,   &
         initiate_xobsinformation,  &
         save_xobsinformation
    USE READWRITE_ROUTINES, ONLY : check_obs_timeperiod, &
                                   check_station, &
                                   check_xobs, &
                                   prepare_read_matrix

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir       !<File directory (forcingdir)
    INTEGER, INTENT(IN)  :: ns                !<Number of subbasins, basemodel
    INTEGER, INTENT(IN)  :: ndt               !<Number of timesteps in simulation
    TYPE(DateType), INTENT(IN)   :: bdate     !<Begin simulation date
    TYPE(DateType), INTENT(IN)   :: edate     !<End simulation date
    INTEGER, INTENT(OUT) :: status            !<Error number
    
    !Local variables
    INTEGER nt,nskip
    TYPE(DateType) fbdate,fedate          !Begin and end date of file
    TYPE(DateType) tempdate
    LOGICAL qex
    LOGICAL xex,xorex
    LOGICAL notimefound
    REAL,ALLOCATABLE :: x2(:,:)           !Data, only used when reading all obs to memory
    REAL,ALLOCATABLE :: x3(:,:)           !Data, only used when reading all obs to memory
    INTEGER,ALLOCATABLE :: subnr(:)       !Help variable
    TYPE(DateType),ALLOCATABLE :: localdates(:) !Help variable
    INTEGER,ALLOCATABLE :: xvar(:,:),xorvar(:,:)      !Help variable (varid,subid or regid)

    status = 0
    !Initial allocation of model variables and local variables
    IF(.NOT. readdaily)THEN
      IF(.NOT.ALLOCATED(localdates)) ALLOCATE(localdates(ndt))
    ENDIF
    IF(.NOT.ALLOCATED(subnr)) ALLOCATE(subnr(ns))
    subnr = basin(:)%subid

    !Check if other observation stations are correct and present (only for Q), and get their index tables.
    INQUIRE(FILE=TRIM(dir)//filename_Qobs,EXIST=qex)
    IF(qex)THEN
      IF(.NOT.ALLOCATED(qobsindex)) ALLOCATE(qobsindex(ns))
      CALL check_station(fileunit_temp,TRIM(dir)//filename_Qobs,ns,0,subnr,numqobsstn,qobsindex,status)
      IF(status==3)THEN
        qex = .FALSE.
      ELSEIF(status.NE.0)THEN
        RETURN
      ENDIF
    ENDIF
    INQUIRE(FILE=TRIM(dir)//filename_Xobs,EXIST=xex)
    IF(xex)THEN
      CALL count_data_cols(fileunit_temp,TRIM(dir)//filename_Xobs,1, xcol,status)
      IF(status.NE.0) RETURN
      xcol = xcol-1
      IF(xcol>0)THEN
        IF(.NOT.ALLOCATED(xvar)) ALLOCATE(xvar(xcol,2))
        CALL check_xobs(fileunit_temp,TRIM(dir)//filename_Xobs,xcol,    &
             xvar,status) 
        IF(status.NE.0) RETURN
      ENDIF
    ENDIF
    IF(readoutregion)THEN
      INQUIRE(FILE=TRIM(dir)//filename_Xoreg,EXIST=xorex)
      IF(xorex)THEN
        CALL count_data_cols(fileunit_temp,TRIM(dir)//filename_Xoreg,1,xorcol,status)
        IF(status.NE.0) RETURN
        xorcol = xorcol-1
        IF(xorcol>0)THEN
          IF(.NOT.ALLOCATED(xorvar)) ALLOCATE(xorvar(xorcol,2))
          CALL check_xobs(fileunit_temp,TRIM(dir)//filename_Xoreg,xorcol,    &
                 xorvar,status) 
          IF(status.NE.0) RETURN
        ENDIF
      ENDIF      
    ENDIF

    !Check time periods of observations
    IF(qex)THEN
      CALL check_obs_timeperiod(fileunit_qobs,TRIM(dir)//filename_Qobs,1,i_t,   &
            bdate,edate,fbdate,fedate,notimefound,status)
      bqdate = bdate
      eqdate = edate
      IF(status==2)THEN  !Shorter time period
        status = 0
        IF(bdate.GT.fedate .OR. edate.LT.fbdate)THEN    !no overlap
          qex = .FALSE.
        ELSE                                      !some overlap
          tempdate = MaxDate(bdate,fbdate)
          bqdate = tempdate
          tempdate = MinDate(edate,fedate)
          eqdate = tempdate
        ENDIF
      ELSEIF(status.NE.0)THEN
        RETURN
      ENDIF
    ENDIF
    IF(xex .AND. xcol>0)THEN
      CALL check_obs_timeperiod(fileunit_xobs,TRIM(dir)//filename_Xobs,3,i_t,   &
            bdate,edate,fbdate,fedate,notimefound,status)
      bxdate = bdate
      exdate = edate
      IF(status==2)THEN  !Shorter time period
        status = 0
        IF(bdate.GT.fedate .OR. edate.LT.fbdate)THEN    !no overlap
          xex = .FALSE.
          xcol = 0
        ELSE                                      !some overlap
          bxdate = MaxDate(bdate,fbdate)
          exdate = MinDate(edate,fedate)
        ENDIF
      ELSEIF(status.NE.0)THEN
        RETURN
      ENDIF
    ENDIF
    IF(readoutregion)THEN
      IF(xorex .AND. xorcol>0)THEN
        CALL check_obs_timeperiod(fileunit_xoreg,TRIM(dir)//filename_Xoreg,3,i_t,   &
              bdate,edate,fbdate,fedate,notimefound,status)
        bxrdate = bdate
        exrdate = edate
        IF(status==2)THEN  !Shorter time period
          status = 0
          IF(bdate.GT.fedate .OR. edate.LT.fbdate)THEN    !no overlap
            xorex = .FALSE.
            xorcol = 0
          ELSE                                      !some overlap
            bxrdate = MaxDate(bdate,fbdate)
            exrdate = MinDate(edate,fedate)
          ENDIF
        ELSEIF(status.NE.0)THEN
          RETURN
        ENDIF
      ENDIF
    ENDIF

    !Load discharge data
    IF(qex)THEN
      CALL prepare_read_matrix(fileunit_qobs,TRIM(dir)//filename_Qobs,1,bqdate,status)
      IF(status.NE.0) RETURN
      IF(.NOT.ALLOCATED(qobsi)) ALLOCATE(qobsi(ns))
      IF(readdaily)THEN
        WRITE(6,*) 'File ready: ', TRIM(dir)//filename_Qobs
        readqobs = .TRUE.
      ELSE
        IF(.NOT.ALLOCATED(x2)) ALLOCATE(x2(ndt,numqobsstn))
        CALL READ_MATRIX(fileunit_qobs,ndt,numqobsstn,eqdate,nt,   &
             localdates,x2(:,:),missing_value,readmatlab) 
        CLOSE(fileunit_qobs)
        nskip = period_length(bdate,bqdate)
        ALLOCATE (qobs(nskip+1:nskip+nt,numqobsstn))
        qobs(:,:)=x2(1:nt,1:numqobsstn)
        DEALLOCATE(x2)
        WRITE(6,*) 'File loaded: ', TRIM(dir)//filename_Qobs
      ENDIF
    ENDIF

    !Load other observation data
    IF(xex .AND. xcol>0)THEN
      CALL prepare_read_matrix(fileunit_xobs,TRIM(dir)//filename_Xobs,3,bxdate,status)
      IF(status.NE.0) RETURN
      IF(.NOT.ALLOCATED(xobsi)) ALLOCATE(xobsi(xcol))
      xobsi = missing_value
      IF(readdaily)THEN
        WRITE(6,*) 'File ready: ', TRIM(dir)//filename_Xobs
      ELSE
        IF(.NOT.ALLOCATED(x3)) ALLOCATE(x3(ndt,xcol))
        CALL READ_MATRIX(fileunit_xobs,ndt,xcol,exdate,nt,localdates,    &
             x3(:,:),missing_value,readmatlab) 
        CLOSE(fileunit_xobs)
        nskip = period_length(bdate,bxdate)
        IF(.NOT.ALLOCATED(xobs)) ALLOCATE(xobs(nskip+1:nskip+nt,xcol))
        xobs(:,:) = x3(1:nt,:)
        DEALLOCATE(x3)
        WRITE(6,*) 'File loaded: ', TRIM(dir)//filename_Xobs
      ENDIF
    ENDIF
    CALL initiate_xobsinformation(xobsindex,ns)
    IF(xcol>0) CALL save_xobsinformation(xcol,xvar,ns)

    !Load outregion observation data
    IF(readoutregion)THEN
      IF(xorex .AND. xorcol>0)THEN
        CALL prepare_read_matrix(fileunit_xoreg,TRIM(dir)//filename_Xoreg,3,bxrdate,status)
        IF(status.NE.0) RETURN
        IF(.NOT.ALLOCATED(xoregobsi)) ALLOCATE(xoregobsi(xorcol))
        xoregobsi = missing_value
        IF(readdaily)THEN
          WRITE(6,*) 'File ready: ', TRIM(dir)//filename_Xoreg
        ELSE
          IF(.NOT.ALLOCATED(x3)) ALLOCATE(x3(ndt,xorcol))
          CALL READ_MATRIX(fileunit_xoreg,ndt,xorcol,exrdate,nt,localdates,    &
                 x3(:,:),missing_value,readmatlab) 
          CLOSE(fileunit_xoreg)
          nskip = period_length(bdate,bxrdate)
          IF(.NOT.ALLOCATED(xoregobs)) ALLOCATE(xoregobs(nskip+1:nskip+nt,xorcol))
          xoregobs(:,:) = x3(1:nt,:)
          DEALLOCATE(x3)
          WRITE(6,*) 'File loaded: ', TRIM(dir)//filename_Xoreg
        ENDIF
      ENDIF
      CALL initiate_xobsinformation(xoregindex,noutreg)
      IF(xorcol>0) CALL save_xoreginformation(xorcol,xorvar,noutreg)      
    ENDIF

    IF(ALLOCATED(xvar))       DEALLOCATE(xvar)
    IF(ALLOCATED(subnr))      DEALLOCATE(subnr)
    IF(ALLOCATED(xorvar))     DEALLOCATE(xorvar)
    IF(ALLOCATED(localdates)) DEALLOCATE(localdates)

  END SUBROUTINE load_ascii_qx_observations

  !>Save information about finding other observation series
  !>
  !> \b Consequences Module index array (xoregindex) is set.
  !-------------------------------------------------------
  SUBROUTINE save_xoreginformation(n,var2,nr)
    
    USE MODVAR, ONLY : xoregindex, &
                       max_outvar
    USE WORLDVAR, ONLY : outregion

    INTEGER, INTENT(IN) :: n          !<Number of columns in Xoregobs
    INTEGER, INTENT(IN) :: var2(n,2)  !<Variables and subid for Xoregobs columns
    INTEGER, INTENT(IN) :: nr         !<Number of outregions
    
    !Local variables
    INTEGER ivar,ireg,icol

    DO ivar = 1, max_outvar
      DO ireg = 1, nr
        DO icol = 1,n
          IF(var2(icol,1)==ivar .AND. var2(icol,2)==outregion(ireg)%outregid) THEN
            xoregindex(ivar,ireg) = icol
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE save_xoreginformation

  !>Prepare forcingdata and other observation for another simulation
  !>in case they are read each time step
  !-----------------------------------------------------------------
  SUBROUTINE reset_observations(dir,status) 

    USE WORLDVAR, ONLY : xcol,    &
         xorcol, &
         readdaily,       &
         readqobs,        &
         readformat,      &
         bdate,           &
         bpsdate, &
         bqdate,          &
         bxdate,bxrdate,  &
         fileunit_qobs,   &
         fileunit_xobs,   &
         fileunit_xoreg,  &
         filename_Xobs,   &
         filename_Xoreg,  &
         filename_Qobs,   &
         readoutregion,   &
         forcingdata, &
         max_forcingdata, &
         maxcharpath, &
         dailypsfile,monthlypsfile, &
         yearlypsfile, &
         fileunit_psts
    USE READWRITE_ROUTINES, ONLY : prepare_read_matrix
    USE MODELMODULE, ONLY : reload_modeldefined_observations

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir         !<File directory
    INTEGER, INTENT(OUT) :: status            !<Error number
    
    !Local variables
    LOGICAL xex
    INTEGER iforc
    CHARACTER(LEN=maxcharpath) :: pstsfilename

    IF(readdaily)THEN
      !Close files
      CALL close_observations(dir)
      IF(readformat==0)THEN
        DO iforc = 1, max_forcingdata   !Mandatory and optional forcing observations
          IF(forcingdata(iforc)%readfile)THEN
            CALL prepare_read_matrix(forcingdata(iforc)%fileunit,forcingdata(iforc)%filepath,1,bdate,status)
            IF(status.NE.0) RETURN
          ENDIF
        ENDDO
        IF(readqobs)THEN
          CALL prepare_read_matrix(fileunit_qobs,TRIM(dir)//filename_Qobs,1,bqdate,status)
          IF(status.NE.0) RETURN
        ENDIF
        INQUIRE(FILE=TRIM(dir)//filename_Xobs,EXIST=xex)    !Other observations
        IF(xex .and. xcol>0)THEN
          CALL prepare_read_matrix(fileunit_xobs,TRIM(dir)//filename_Xobs,3,bxdate,status)
          IF(status.NE.0) RETURN
        ENDIF
        IF(readoutregion .AND. xorcol>0)THEN    !Other observations
          CALL prepare_read_matrix(fileunit_xoreg,TRIM(dir)//filename_Xoreg,3,bxrdate,status)
          IF(status.NE.0) RETURN
        ENDIF
      ENDIF   !readformat
    ENDIF   !readdaily
    
    !Reset model defined and read observations
    CALL reload_modeldefined_observations(dir,status)
    IF(status.NE.0) RETURN

    !Reset point sources time files
    IF(dailypsfile)THEN
      CLOSE(fileunit_psts)
      pstsfilename = TRIM(dir)//'PSDailySeries.txt'
      CALL prepare_read_matrix(fileunit_psts,TRIM(pstsfilename),2,bpsdate,status)
      IF(status.NE.0) RETURN
    ELSEIF(monthlypsfile)THEN
      CLOSE(fileunit_psts)
      pstsfilename = TRIM(dir)//'PSMonthlySeries.txt'
      CALL prepare_read_matrix(fileunit_psts,TRIM(pstsfilename),2,bpsdate,status)
      IF(status.NE.0) RETURN
    ELSEIF(yearlypsfile)THEN
      CLOSE(fileunit_psts)
      pstsfilename = TRIM(dir)//'PSYearlySeries.txt'
      CALL prepare_read_matrix(fileunit_psts,TRIM(pstsfilename),2,bpsdate,status)
      IF(status.NE.0) RETURN
    ENDIF
    
  END SUBROUTINE reset_observations

  !>Read information for basins to update
  !!
  !>\b Consequences Module modvar variables conduct, doupdate, useinupdate 
  !>and wobsvarindex may change.
  !-----------------------------------------------------------------------------------------
  !SUBROUTINE prepare_for_update(dir,locupall,locupnone,locupnone_qar,locupnone_war,wupall,wupnone,ns,status)
  SUBROUTINE prepare_for_update(dir,locupall,locupnone,ns,status) 
  
    USE MODVAR, ONLY : conduct, & !OUT
                       doupdate,  & !OUT
                       dim_update,            &
                       i_quseobs,             &
                       i_cuseobs,             &
                       i_qar,                 &
                       i_war,                 &
                       i_tpcorr,              &
                       i_tncorr,              &
                       i_tploccorr,           &
                       i_tnloccorr,           &
                       i_wendupd,             &
                       !updatename,            &
                       useinupdate, & !OUT
                       basin,                 &
                       xobsindex,             &
                       wobsvarindex,          &
                       i_sp,i_pp,             &
                       i_in,i_on,             &
                       numsubstances, &
                       nsub_basemodel, &
                       set_update_method_names
    USE WORLDVAR, ONLY : fileunit_temp, &
                         filename_upd, &
                         qobsindex
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_prepare_update_ok, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir         !<File directory
    LOGICAL, INTENT(IN) :: locupall(dim_update)  !<Flag for update on all stations
    LOGICAL, INTENT(IN) :: locupnone(dim_update) !<Flag for no update on any stations
    INTEGER, INTENT(IN)  :: ns                  !<Number of subbasins
    INTEGER, INTENT(OUT) :: status              !<Error number
    
    !Local variables
    INTEGER i, j            !loop-index
    INTEGER qusenum
    INTEGER cusenum
    INTEGER qarnum
    INTEGER warnum
    INTEGER wendnum 
    INTEGER filenum 
    INTEGER qusetemp(nsub_basemodel)
    INTEGER cusetemp(nsub_basemodel)
    INTEGER qartemp(nsub_basemodel)
    INTEGER wartemp(nsub_basemodel)
    REAL    qarfac(nsub_basemodel)
    REAL    warfac(nsub_basemodel)
    INTEGER wendtemp(nsub_basemodel)
    REAL    tptemp(nsub_basemodel,2)
    REAL    tntemp(nsub_basemodel,2)
    REAL    tploctemp(nsub_basemodel,2)
    REAL    tnloctemp(nsub_basemodel,2)
    LOGICAL fileexist
    CHARACTER(LEN=100) :: propagate_str

    !Initiation
    CALL set_update_method_names()
    qusenum = 0
    cusenum = 0
    wendnum = 0
    filenum = 0
    qarnum  = 0
    warnum  = 0
    status  = 0
    !IF(.NOT.ALLOCATED(useinupdate)) ALLOCATE(useinupdate(dim_update))
    
    !> \b Algorithm \n
    !>Read update-file if available
    INQUIRE(FILE=TRIM(dir)//filename_upd,EXIST=fileexist) 
    IF(fileexist) CALL read_update_data(fileunit_temp,TRIM(dir)//filename_upd,    & 
         nsub_basemodel,qusenum,qusetemp,qarnum,warnum,qartemp,wartemp,qarfac,warfac,wendnum,  &
         wendtemp,filenum,tptemp,tntemp,tploctemp,tnloctemp,cusenum,cusetemp)    

    !>Prepare discharge data stations to use for replacing flow (quseobs)
    IF(doupdate(i_quseobs))THEN
      IF(locupnone(i_quseobs))THEN
        doupdate(i_quseobs) = .FALSE. 
      ELSEIF(locupall(i_quseobs) .OR. qusenum>0) THEN
        IF(.NOT.ALLOCATED(useinupdate(i_quseobs)%station)) ALLOCATE(useinupdate(i_quseobs)%station(ns))
        useinupdate(i_quseobs)%station = .FALSE.             
        IF(locupall(i_quseobs)) THEN
          WHERE(qobsindex(1:ns)>0)
            useinupdate(i_quseobs)%station(:) = .TRUE. 
          ENDWHERE
        ELSEIF(qusenum>0) THEN
          DO i = 1,ns
            DO j = 1, qusenum
              IF(basin(i)%subid == qusetemp(j)) THEN
                useinupdate(i_quseobs)%station(i)=.TRUE.
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ELSE
        WRITE(6,*) 'WARNING: missing stations in update.txt, no quseobs updating'
        doupdate(i_quseobs) = .FALSE.
      ENDIF
    ENDIF

    !>Prepare dicharge data stations and AR-coefficient to use for AR-updating flow  (qar)
    IF(doupdate(i_qar))THEN
      IF(locupnone(i_qar))THEN
        doupdate(i_qar) = .FALSE. 
      ELSEIF(qarnum>0) THEN
        IF(.NOT.ALLOCATED(useinupdate(i_qar)%station)) ALLOCATE(useinupdate(i_qar)%station(ns))
        IF(.NOT.ALLOCATED(useinupdate(i_qar)%factor)) ALLOCATE(useinupdate(i_qar)%factor(ns))
        useinupdate(i_qar)%station = .FALSE.             
        useinupdate(i_qar)%factor = 0
        DO i = 1,ns
          DO j = 1, qarnum
            IF(basin(i)%subid == qartemp(j)) THEN
              useinupdate(i_qar)%station(i) = .TRUE.
              useinupdate(i_qar)%factor(i) = qarfac(j)
              EXIT
            ENDIF
          ENDDO
        ENDDO
      ELSE
        WRITE(6,*) 'WARNING: missing stations in update.txt, no qar updating'
        doupdate(i_quseobs) = .FALSE.
      ENDIF
    ENDIF

    !>Prepare waterstage data stations and AR-coefficient to use for AR-updating flow  (war)
    IF(doupdate(i_war))THEN
      IF(locupnone(i_war))THEN
        doupdate(i_war) = .FALSE. 
      ELSEIF(warnum>0) THEN
        IF(.NOT.ALLOCATED(useinupdate(i_war)%station)) ALLOCATE(useinupdate(i_war)%station(ns))
        IF(.NOT.ALLOCATED(useinupdate(i_war)%factor)) ALLOCATE(useinupdate(i_war)%factor(ns))
        useinupdate(i_war)%factor = 0
        useinupdate(i_war)%station = .FALSE.             
        DO i = 1,ns
          DO j = 1, warnum
            IF(basin(i)%subid == wartemp(j)) THEN
              useinupdate(i_war)%station(i) = .TRUE.
              useinupdate(i_war)%factor(i) = warfac(j)
              EXIT
            ENDIF
          ENDDO
        ENDDO
      ELSE
        WRITE(6,*) 'WARNING: missing stations in update.txt, no war updating'
        doupdate(i_war) = .FALSE.
      ENDIF
    ENDIF
    
    !>Prepare for correction of phosphorus concentration by a factor (tpcorr,tploccorr)
    IF(doupdate(i_tpcorr).OR.doupdate(i_tploccorr))THEN
      IF(i_sp==0.OR.i_pp==0)THEN
        WRITE(6,*) 'ERROR: Updating of phosphorus not possible unless P is simulated'
        propagate_str = 'Updating of phosphorus not possible unless P is simulated'
        CALL propagate_external_msg(e_prepare_update_ok,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
      IF(filenum>0)THEN
        IF(doupdate(i_tpcorr))    CALL set_update_nutrientcorr(nsub_basemodel,ns,filenum,i_tpcorr,tptemp)
        IF(doupdate(i_tploccorr)) CALL set_update_nutrientcorr(nsub_basemodel,ns,filenum,i_tploccorr,tploctemp)
      ELSEIF(.NOT.fileexist)THEN
        WRITE(6,*) 'WARNING: missing update.txt file, necessary for update of phosphourus'
        doupdate(i_tpcorr) = .FALSE.
        doupdate(i_tploccorr) = .FALSE.
      ELSE
        WRITE(6,*) 'WARNING: missing data in update.txt file, no phosphourus updating'
        doupdate(i_tpcorr) = .FALSE.
        doupdate(i_tploccorr) = .FALSE.
      ENDIF
    ENDIF

    !>Prepare for correction of nitrogen concentration by a factor (tncorr,tnloccorr)
    IF(doupdate(i_tncorr).OR.doupdate(i_tnloccorr))THEN
      IF(i_in==0.OR.i_on==0)THEN
        WRITE(6,*) 'ERROR: Updating of nitrogen not possible unless N is simulated'
        propagate_str = 'Updating of phsosphorus not possible unless N is simulated'
        CALL propagate_external_msg(e_prepare_update_ok,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
      IF(filenum>0)THEN
        IF(doupdate(i_tncorr))    CALL set_update_nutrientcorr(nsub_basemodel,ns,filenum,i_tncorr,tntemp)
        IF(doupdate(i_tnloccorr)) CALL set_update_nutrientcorr(nsub_basemodel,ns,filenum,i_tnloccorr,tnloctemp)
      ELSEIF(.NOT.fileexist)THEN
        WRITE(6,*) 'WARNING: missing update.txt file, necessary for update nitrogen'
        doupdate(i_tncorr) = .FALSE.
        doupdate(i_tnloccorr) = .FALSE.
      ELSE
        WRITE(6,*) 'WARNING: missing data in update.txt file, no nitrogen updating'
        doupdate(i_tncorr) = .FALSE.
        doupdate(i_tnloccorr) = .FALSE.
      ENDIF
    ENDIF

    !>Prepare concentration data stations to use for replacing simulated concentrations (cuseobs)
    IF(doupdate(i_cuseobs))THEN
      IF(locupnone(i_cuseobs))THEN
        doupdate(i_cuseobs) = .FALSE. 
      ELSEIF(locupall(i_cuseobs) .OR. cusenum>0) THEN
        IF(.NOT.ALLOCATED(useinupdate(i_cuseobs)%station)) ALLOCATE(useinupdate(i_cuseobs)%station(ns))
        useinupdate(i_cuseobs)%station = .FALSE.             
        IF(locupall(i_cuseobs)) THEN
          useinupdate(i_cuseobs)%station = .TRUE.   !It is not certain data exist for each substance
        ELSEIF(cusenum>0) THEN
          DO i = 1,ns
            DO j = 1, cusenum
              IF(basin(i)%subid == cusetemp(j)) THEN
                useinupdate(i_cuseobs)%station(i)=.TRUE.
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ELSE
        WRITE(6,*) 'WARNING: missing stations in update.txt, no cuseobs updating'
        doupdate(i_cuseobs) = .FALSE.
      ENDIF
    ENDIF

    !>Prepare for updating lake waterstage at end of time step (wendupd)
    IF(doupdate(i_wendupd))THEN
      IF(locupnone(i_wendupd))THEN      
        doupdate(i_wendupd) = .FALSE. 
      ELSEIF(locupall(i_wendupd) .OR. wendnum>0) THEN
        IF(.NOT.ALLOCATED(useinupdate(i_wendupd)%station)) ALLOCATE(useinupdate(i_wendupd)%station(ns))
        useinupdate(i_wendupd)%station = .FALSE.             
        IF(locupall(i_wendupd)) THEN
          useinupdate(i_wendupd)%station = .TRUE. 
        ELSEIF(wendnum>0) THEN
          DO i = 1,ns
            DO j = 1, wendnum
              IF(basin(i)%subid == wendtemp(j)) THEN
                useinupdate(i_wendupd)%station(i)=.TRUE.
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        !Check for available observations
        DO i = 1,ns
          IF(useinupdate(i_wendupd)%station(i).AND. xobsindex(wobsvarindex,i)==0) useinupdate(i_wendupd)%station(i)=.FALSE.
        ENDDO
      ELSE
        WRITE(6,*) 'WARNING: missing stations in update.txt, no wendupd updating'
        doupdate(i_wendupd) = .FALSE. 
      ENDIF
    ENDIF

    !>Check for incompatible update methods (war not to be used with qar or wendupd)
    IF(doupdate(i_war).AND.doupdate(i_qar))THEN
      DO i = 1,ns
        IF(useinupdate(i_war)%station(i).AND.useinupdate(i_qar)%station(i))THEN
          useinupdate(i_war)%station(i)=.FALSE.
          WRITE(6,*)'WARNING: Update with q-AR and w-AR at the same subbasin is not allowed. w-AR turned off for subbasin',basin(i)%subid
          warnum = warnum - 1
        ENDIF
      ENDDO
      IF(warnum == 0) doupdate(i_war)=.FALSE.
    ENDIF
    IF(doupdate(i_war).AND.doupdate(i_wendupd))THEN
      DO i = 1,ns
        IF(useinupdate(i_war)%station(i).AND.useinupdate(i_wendupd)%station(i))THEN
          useinupdate(i_war)%station(i)=.FALSE.
          WRITE(6,*)'WARNING: Update with wendupd and w-AR at the same subbasin is not allowed. w-AR turned off for subbasin',basin(i)%subid
          warnum = warnum - 1
        ENDIF
      ENDDO
      IF(warnum == 0) doupdate(i_war)=.FALSE.
    ENDIF
    IF(doupdate(i_cuseobs))THEN
      IF(numsubstances==0)THEN
        doupdate(i_cuseobs) = .FALSE. 
        WRITE(6,*) 'WARNING: Not possible to update concentrations when no substances are simulated'
      ELSEIF(.NOT.ALLOCATED(xobsindex))THEN
        doupdate(i_cuseobs) = .FALSE. 
        WRITE(6,*) 'WARNING: Not possible to update concentrations when no observations are given'
      ENDIF
    ENDIF

    !Set flag for conducting AR-updating
    IF(doupdate(i_qar).OR.doupdate(i_war)) conduct%arupdating = .TRUE.

    !>Write information about used updating methods to logg-file
    IF(doupdate(i_quseobs).OR.doupdate(i_qar).OR.doupdate(i_war).OR.  &
       doupdate(i_wendupd).OR.doupdate(i_tpcorr).OR.doupdate(i_tncorr).OR. &
       doupdate(i_tploccorr).OR.doupdate(i_tnloccorr).OR.doupdate(i_cuseobs))THEN
      WRITE(6,*)
      WRITE(6,*)'-----Information about the simulation (cont.)----'
      IF(doupdate(i_quseobs))   WRITE(6,*)'Simulation with updated discharge, ',TRIM(useinupdate(i_quseobs)%name)
      IF(doupdate(i_qar))       WRITE(6,*)'Simulation with qAR-updated discharge, ',TRIM(useinupdate(i_qar)%name)
      IF(doupdate(i_war))       WRITE(6,*)'Simulation with wAR-updated discharge, ',TRIM(useinupdate(i_war)%name)
      IF(doupdate(i_wendupd))   WRITE(6,*)'Simulation with updated lake water stage, ',TRIM(useinupdate(i_wendupd)%name)
      IF(doupdate(i_tpcorr))    WRITE(6,*)'Simulation with updated TP-concentrations, ',TRIM(useinupdate(i_tpcorr)%name)
      IF(doupdate(i_tncorr))    WRITE(6,*)'Simulation with updated TN-concentrations, ',TRIM(useinupdate(i_tncorr)%name)
      IF(doupdate(i_tploccorr)) WRITE(6,*)'Simulation with updated TP-concentrations, ',TRIM(useinupdate(i_tploccorr)%name)
      IF(doupdate(i_tnloccorr)) WRITE(6,*)'Simulation with updated TN-concentrations, ',TRIM(useinupdate(i_tnloccorr)%name)
      IF(doupdate(i_cuseobs))   WRITE(6,*)'Simulation with updated concentrations, ',TRIM(useinupdate(i_cuseobs)%name)
      !IF(doupdate(i_quseobs))   WRITE(6,*)'Simulation with updated discharge, ',TRIM(updatename(i_quseobs))
      !IF(doupdate(i_qar))       WRITE(6,*)'Simulation with qAR-updated discharge, ',TRIM(updatename(i_qar))
      !IF(doupdate(i_war))       WRITE(6,*)'Simulation with wAR-updated discharge, ',TRIM(updatename(i_war))
      !IF(doupdate(i_wendupd))   WRITE(6,*)'Simulation with updated lake water stage, ',TRIM(updatename(i_wendupd))
      !IF(doupdate(i_tpcorr))    WRITE(6,*)'Simulation with updated TP-concentrations, ',TRIM(updatename(i_tpcorr))
      !IF(doupdate(i_tncorr))    WRITE(6,*)'Simulation with updated TN-concentrations, ',TRIM(updatename(i_tncorr))
      !IF(doupdate(i_tploccorr)) WRITE(6,*)'Simulation with updated TP-concentrations, ',TRIM(updatename(i_tploccorr))
      !IF(doupdate(i_tnloccorr)) WRITE(6,*)'Simulation with updated TN-concentrations, ',TRIM(updatename(i_tnloccorr))
      !IF(doupdate(i_cuseobs))   WRITE(6,*)'Simulation with updated concentrations, ',TRIM(updatename(i_cuseobs))
      WRITE(6,*)'-------------------------------------------------'
    ENDIF
    
  END SUBROUTINE prepare_for_update

  !>\brief Set variable for phosphorus and nitrogen correction/updating
  !>
  !>\b Consequences Module variables doupdate and useinupdate are allocated and set.
  !---------------------------------------------------------------
  SUBROUTINE set_update_nutrientcorr(dim,ns,nrow,icorr,dataarray)

    USE MODVAR, ONLY : basin, &
                       conductwarning, &
                       doupdate, &  !OUT
!                       updatename, &
                       useinupdate  !OUT

    !Argument declarations
    INTEGER, INTENT(IN)  :: dim         !<number of subbasins of base model
    INTEGER, INTENT(IN)  :: ns          !<number of subbasins
    INTEGER, INTENT(IN)  :: nrow        !<number of data
    INTEGER, INTENT(IN)  :: icorr       !<index for type of updating/correction
    REAL   , INTENT(IN)  :: dataarray(dim,2)        !<read data

    !Variable declaration
    INTEGER i,j

    !>\b Algorithm \n
    !>For each subbasin and data point: If equal set correction value for this subbasin
    IF(.NOT.ALLOCATED(useinupdate(icorr)%factor)) ALLOCATE(useinupdate(icorr)%factor(ns))
    useinupdate(icorr)%factor = 0.
    DO i = 1,ns
      DO j = 1, nrow
        IF(basin(i)%subid == NINT(dataarray(j,1))) THEN
          useinupdate(icorr)%factor(i)=dataarray(j,2)
          EXIT
        ENDIF
      ENDDO
    ENDDO
    !>If no correction value are found: turn off updating and deallocate array
    IF(.NOT.ANY(useinupdate(icorr)%factor/=0))THEN
      IF(conductwarning) WRITE(6,*) 'WARNING: missing data in update.txt file, no ',TRIM(useinupdate(icorr)%name),' updating simulated'
      doupdate(icorr) = .FALSE.
      DEALLOCATE(useinupdate(icorr)%factor)
    ENDIF

  END SUBROUTINE set_update_nutrientcorr

  !>Get current forcing from file or memory
  !--------------------------------------------------------------------
  SUBROUTINE get_current_forcing(idt,ns,current_date)

    USE WORLDVAR, ONLY : forcingdata, &
                         i_pobs,i_tobs, &
                         i_tminobs,i_tmaxobs, &
                         i_rhobs,i_sfobs, &
                         i_swobs,i_uobs, &
                         i_uwobs,i_vwobs, &
                         numqobsstn
    USE MODVAR, ONLY :   preci,tempi, & !OUT
                         snowfraci,shortwavei, & !OUT
                         windi,humidi, & !OUT
                         tmini,tmaxi, & !OUT
                         uwindi,vwindi, & !OUT
                         qobsi,xobsi,xoregobsi, &  !OUT
                         psload,absload !OUT

    !Argument declarations
    INTEGER, INTENT(IN)  :: idt  !<current timestep index
    INTEGER, INTENT(IN)  :: ns   !<number of subbasins
    TYPE(DateType), INTENT(OUT) :: current_date !<current date
    
      IF(forcingdata(i_pobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_pobs),current_date,preci(1:ns))
      IF(forcingdata(i_tobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_tobs),current_date,tempi(1:ns))
      IF(ALLOCATED(qobsi)) qobsi(1:ns) = get_current_flow(idt,current_date,ns,numqobsstn)
      IF(ALLOCATED(xobsi)) xobsi(:) = get_current_otherobs(idt,current_date)
      IF(ALLOCATED(xoregobsi)) xoregobsi(:) = get_current_oregobs(idt,current_date)
      IF(forcingdata(i_sfobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_sfobs),current_date,snowfraci(1:ns))
      IF(forcingdata(i_swobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_swobs),current_date,shortwavei(1:ns))
      IF(forcingdata(i_uobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_uobs),current_date,windi(1:ns))
      IF(forcingdata(i_rhobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_rhobs),current_date,humidi(1:ns))
      IF(forcingdata(i_tminobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_tminobs),current_date,tmini(1:ns))
      IF(forcingdata(i_tmaxobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_tmaxobs),current_date,tmaxi(1:ns))
      IF(forcingdata(i_uwobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_uwobs),current_date,uwindi(1:ns))
      IF(forcingdata(i_vwobs)%readfile) CALL get_one_current_forcing_data(idt,ns,forcingdata(i_vwobs),current_date,vwindi(1:ns))
      IF(ALLOCATED(psload).OR.ALLOCATED(absload)) CALL get_current_time_pointsource(current_date)

  END SUBROUTINE get_current_forcing

  !>Get current forcing data for one variable for selected subbasins from file or memory
  !---------------------------------------
  SUBROUTINE get_one_current_forcing_data(i,n,forcobs,current_date,current_value)
  
    USE WORLDVAR, ONLY : readdaily, &
                         readformat, &
                         readmatlab, &
                         forcingdatatype, &
                         get_current_date_memory, &
                         get_current_forcing_from_memory
    USE MODVAR, ONLY : missing_value
    USE READWRITE_ROUTINES, ONLY : read_matrix_line
    USE LIBDATE, ONLY : DateType
    
    !Argument declaration
    INTEGER, INTENT(IN) :: i  !<current time step
    INTEGER, INTENT(IN) :: n  !<number of subbasins
    TYPE(FORCINGDATATYPE), INTENT(IN) :: forcobs  !<data/information for one forcing variable
    TYPE(DateType), INTENT(OUT) :: current_date !<current date
    REAL, INTENT(OUT) :: current_value(n)     !<current value of the forcing variable
    
    !Variable declaration
    TYPE(DateType) d    !date
    REAL yt(forcobs%ncols)
    
    IF(readformat==0)THEN   !ASCII
      IF(readdaily)THEN
        CALL read_matrix_line(forcobs%fileunit,forcobs%ncols,d,yt,missing_value,readmatlab) 
        current_date = d
      ELSE
        current_date = get_current_date_memory(i)
        yt = get_current_forcing_from_memory(i,forcobs)
      ENDIF
      current_value = yt(forcobs%basinindex)
    ENDIF
    
  END SUBROUTINE get_one_current_forcing_data

  !>Get current discharge from file or memory
  !---------------------------------------------------------
  FUNCTION get_current_flow(i,cd,n,no) RESULT(current_flow)

    USE WORLDVAR, ONLY : readdaily,        &
                         readformat,       &
                         fileunit_qobs,    &
                         readmatlab,       &
                         qobsindex,        &
                         get_current_qobs, &
                         bqdate,           &
                         eqdate
    USE MODVAR, ONLY : missing_value
    USE READWRITE_ROUTINES, ONLY : read_matrix_line

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<current time step
    TYPE(DateType), INTENT(IN) :: cd  !<current date
    INTEGER, INTENT(IN) :: n          !<number of subbasins
    INTEGER, INTENT(IN) :: no         !<number of observation stations
    REAL current_flow(n)              !<current flow
    
    !Local variables
    TYPE(DateType) d
    REAL    y(n),x(no)

    IF(cd.LT.bqdate .OR. cd.GT.eqdate)THEN
      current_flow = missing_value
      RETURN
    ENDIF

    IF(readformat==0.OR.readformat==4)THEN   !ASCII
      IF(readdaily)THEN
        y = missing_value
        CALL READ_MATRIX_LINE(fileunit_qobs,no,d,x,missing_value,readmatlab) 
        WHERE(qobsindex(1:n)>0)
          y=x(qobsindex(1:n))
        ENDWHERE
        current_flow = y
        IF(d.NE.cd) WRITE(*,*) 'ERROR read Qobs, date not equal to Pobs date'   !should never happen
      ELSE
        current_flow = get_current_qobs(i,n)
      ENDIF
    ENDIF

  END FUNCTION get_current_flow

  !>Get current value of other observations from file or memory
  !------------------------------------------------------------
  FUNCTION get_current_otherobs(i,cd) RESULT(current_obs)

    USE WORLDVAR, ONLY : readdaily,        &
                         readformat,       &
                         fileunit_xobs,    &
                         get_current_xobs, &
                         readmatlab,       &
                         xcol,             &
                         bxdate,           &
                         exdate
    USE MODVAR, ONLY : missing_value
    USE READWRITE_ROUTINES, ONLY : read_matrix_line

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<current time step
    TYPE(DateType), INTENT(IN) :: cd  !<current date 
    REAL current_obs(xcol)            !<current observations
    
    ! Local variables
    TYPE(DateType) d   !date
    REAL y(xcol)

    IF(xcol==0) RETURN        !no observations

    IF(readformat==0.OR.readformat==4)THEN   !ASCII
      current_obs = missing_value
      IF(cd.LT.bxdate .OR. cd.GT.exdate) RETURN
      IF(readdaily)THEN
        CALL READ_MATRIX_LINE(fileunit_xobs,xcol,d,y,missing_value,readmatlab) 
        current_obs = y
        IF(d.NE.cd) WRITE(*,*) 'ERROR read Xobs, date not equal to Pobs date'   !should never happen
      ELSE
        current_obs = get_current_xobs(i)
      ENDIF
    ENDIF

  END FUNCTION get_current_otherobs

  !>Get current value of other observations from file or memory
  !------------------------------------------------------------
  FUNCTION get_current_oregobs(i,cd) RESULT(current_obs)

    USE WORLDVAR, ONLY : readdaily,        &
                         readformat,       &
                         fileunit_xoreg,    &
                         get_current_xoregobs, &
                         readmatlab,       &
                         xorcol,             &
                         bxrdate,           &
                         exrdate
    USE MODVAR, ONLY : missing_value
    USE READWRITE_ROUTINES, ONLY : read_matrix_line

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<current time step
    TYPE(DateType), INTENT(IN) :: cd  !<current date 
    REAL current_obs(xorcol)          !<current observations
    
    ! Local variables
    TYPE(DateType) d   !date
    REAL y(xorcol)

    IF(xorcol==0) RETURN        !no observations

    IF(readformat==0.OR.readformat==4)THEN   !ASCII
      current_obs = missing_value
      IF(cd.LT.bxrdate .OR. cd.GT.exrdate) RETURN
      IF(readdaily)THEN
        CALL READ_MATRIX_LINE(fileunit_xoreg,xorcol,d,y,missing_value,readmatlab) 
        current_obs = y
        IF(d.NE.cd) WRITE(*,*) 'ERROR read Xoregobs, date not equal to Pobs date'   !should never happen
      ELSE
        current_obs = get_current_xoregobs(i)
      ENDIF
    ENDIF

  END FUNCTION get_current_oregobs

  !>Get current value of time series pointsource from file and 
  !>set modvar variable psload.
  !------------------------------------------------------------
  SUBROUTINE get_current_time_pointsource(cd)

  
    USE WORLDVAR, ONLY : fileunit_psts, &
                         readmatlab, &
                         pstscol, &
                         dailypsfile,monthlypsfile,yearlypsfile, &
                         bpsdate, &
                         epsdate
    USE MODVAR, ONLY : missing_value, &
                       numsubstances, &
                       npsused,nabsused, &
                       psinfo, &
                       psload, & !OUT
                       absinfo, &
                       absload !OUT
    USE READWRITE_ROUTINES, ONLY : read_matrix_line

    !Argument declarations
    TYPE(DateType), INTENT(IN) :: cd  !<current date 
    
    ! Local variables
    INTEGER j,k
    TYPE(DateType) d   !date
    REAL y(pstscol)

    IF(dailypsfile)THEN
      !Initialize load to zero
      IF(npsused>0)THEN
        DO j = 1, npsused
          psload(j)%flow = 0.
          psload(j)%load = 0.
        ENDDO
      ENDIF
      IF(nabsused>0)THEN
        absload%flow = 0.
      ENDIF
      
      IF(cd.LT.bpsdate .OR. cd.GT.epsdate)RETURN
      
      !Read pointsource time serie and calculate and accumulate loads (m3/s)*(mg/L)=(g/s)
      CALL READ_MATRIX_LINE(fileunit_psts,pstscol,d,y,missing_value,readmatlab) 
      DO j = 1, npsused
        psload(j)%flow = psload(j)%flow + y(psinfo(j)%flowcol)
        IF(y(psinfo(j)%flowcol)>0.)THEN
          DO k = 1,numsubstances
            IF(psinfo(j)%conccol(k)>0) psload(j)%load(k) = psload(j)%load(k) + y(psinfo(j)%conccol(k))*y(psinfo(j)%flowcol)
          ENDDO
        ENDIF
      ENDDO
      DO j = 1, nabsused
        absload(j)%flow = absload(j)%flow + y(absinfo(j)%flowcol)
      ENDDO
      
      IF(d.NE.cd) WRITE(*,*) 'ERROR: read PSDailySeries, date not equal to Pobs date'   !should not happen?
    
    ELSEIF(monthlypsfile)THEN
      
      IF(cd.LT.bpsdate .OR. cd.GT.epsdate)THEN
        IF(npsused>0)THEN
          DO j = 1, npsused
            psload(j)%flow = 0.
            psload(j)%load = 0.
          ENDDO
        ENDIF
        IF(nabsused>0)THEN
          absload%flow = 0.
        ENDIF
        RETURN
      ENDIF
      
      IF(cd%Day==1)THEN
        IF(npsused>0)THEN
          DO j = 1, npsused
            psload(j)%flow = 0.
            psload(j)%load = 0.
          ENDDO
        ENDIF
        IF(nabsused>0)THEN
          absload%flow = 0.
        ENDIF
        !Read pointsource time serie and calculate and accumulate loads (m3/s)*(mg/L)=(g/s)
        CALL READ_MATRIX_LINE(fileunit_psts,pstscol,d,y,missing_value,readmatlab) 
        DO j = 1, npsused
          psload(j)%flow = psload(j)%flow + y(psinfo(j)%flowcol)
          IF(y(psinfo(j)%flowcol)>0.)THEN
            DO k = 1,numsubstances
              IF(psinfo(j)%conccol(k)>0) psload(j)%load(k) = psload(j)%load(k) + y(psinfo(j)%conccol(k))*y(psinfo(j)%flowcol)
            ENDDO
          ENDIF
        ENDDO
        DO j = 1, nabsused
          absload(j)%flow = absload(j)%flow + y(absinfo(j)%flowcol)
        ENDDO
      ENDIF        
      
    ELSEIF(yearlypsfile)THEN
      IF(cd.LT.bpsdate .OR. cd.GT.epsdate)THEN
        IF(npsused>0)THEN
          DO j = 1, npsused
            psload(j)%flow = 0.
            psload(j)%load = 0.
          ENDDO
        ENDIF
        IF(nabsused>0)THEN
          absload%flow = 0.
        ENDIF
        RETURN
      ENDIF
      
      IF(cd%Month==1.AND.cd%Day==1)THEN
        IF(npsused>0)THEN
          DO j = 1, npsused
            psload(j)%flow = 0.
            psload(j)%load = 0.
          ENDDO
        ENDIF
        IF(nabsused>0)THEN
          absload%flow = 0.
        ENDIF
        !Read pointsource time serie and calculate and accumulate loads (m3/s)*(mg/L)=(g/s)
        CALL READ_MATRIX_LINE(fileunit_psts,pstscol,d,y,missing_value,readmatlab) 
        DO j = 1, npsused
          psload(j)%flow = psload(j)%flow + y(psinfo(j)%flowcol)
          IF(y(psinfo(j)%flowcol)>0.)THEN
            DO k = 1,numsubstances
              IF(psinfo(j)%conccol(k)>0) psload(j)%load(k) = psload(j)%load(k) + y(psinfo(j)%conccol(k))*y(psinfo(j)%flowcol)
            ENDDO
          ENDIF
        ENDDO
        DO j = 1, nabsused
          absload(j)%flow = absload(j)%flow + y(absinfo(j)%flowcol)
        ENDDO
      ENDIF        
      
    ENDIF

  END SUBROUTINE get_current_time_pointsource

  !>Close files with observations used for forcing and calibration
  !--------------------------------------------------------------------
  SUBROUTINE close_observations(dir)

    USE WORLDVAR, ONLY : fileunit_qobs,  &
                         fileunit_xobs,  &
                         filename_xobs,  &
                         fileunit_xoreg, &
                         xcol,xorcol,    &
                         readqobs,       &
                         readdaily,      &
                         readoutregion,  &
                         forcingdata, &
                         max_forcingdata, &
                         dailypsfile,monthlypsfile,yearlypsfile, &
                         fileunit_psts

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir  !<File directory

    !Local variables
    INTEGER iforc
    LOGICAL xex

    IF(readdaily)THEN
      DO iforc = 1,max_forcingdata
        IF(forcingdata(iforc)%readfile)CLOSE(forcingdata(iforc)%fileunit)
      ENDDO
      IF(readqobs) CLOSE(fileunit_qobs)
      INQUIRE(FILE=TRIM(dir)//filename_Xobs,EXIST=xex)
      IF(xex .AND. xcol>0) CLOSE(fileunit_xobs)
      IF(readoutregion .AND. xorcol>0) CLOSE(fileunit_xoreg)
    ENDIF
    IF(dailypsfile)THEN
      CLOSE(fileunit_psts)
    ELSEIF(monthlypsfile)THEN
      CLOSE(fileunit_psts)
    ELSEIF(yearlypsfile)THEN
      CLOSE(fileunit_psts)
    ENDIF

  END SUBROUTINE close_observations

  !>Gets the basic information about soil-landuse-classes and subbasins from 
  !>GeoClass and GeoData. Additional geographical data for lakes, dams and 
  !>floodplains are also collected as well as data on water management (e.g.
  !>irrigation) and forcing data location.
  !!
  !!\b Consequences Module worldvar variables atmdepvegreset may be set.
  !!Module modvar variables basin, classdata, wetland, nclass, nluse, nsoil, 
  !!modeloption, conduct%riverwetland and numrotations are set. 
  !----------------------------------------------------------------
  SUBROUTINE load_basindata(dir,fdir,n,status) 

    USE WORLDVAR, ONLY : fileunit_temp,   &
                         atmdepvegreset       !OUT
    USE MODVAR, ONLY : basin,             & !OUT
                       classdata,         & !OUT 
                       wetland,           & !OUT
                       nluse,nsoil,       & !OUT
                       nclass,            & !OUT
                       slc_iwet,slc_owet, &
                       numsubstances,     &
                       allocate_basinvariables, &
                       conductbasinpetmodel, &
                       petmodel, &
                       modeloption, &  !OUT
                       p_connectivity, &
                       p_petmodel,p_wetland, &
                       conduct, & !OUT
                       numrotations, &    !OUT
                       lakesectiondata

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir      !<File directory (model files)
    CHARACTER(LEN=*), INTENT(IN) :: fdir     !<File directory (forcing data)
    INTEGER, INTENT(OUT) :: n                !<Number of subbasins
    INTEGER, INTENT(OUT) :: status           !<Error status
    
    !Local parameters
    CHARACTER(LEN=12),PARAMETER :: lfile='GeoClass.txt'   !Name of GeoClass file 
    CHARACTER(LEN=13),PARAMETER :: lfile2 = 'ClassData.txt'    !Name of column heading ClassData file to be read instead of GeoClass
    CHARACTER(LEN=11),PARAMETER :: infile='GeoData.txt'   !Name of GeoData file 
    INTEGER,PARAMETER :: rcols = 50         !Number of data columns
    INTEGER,PARAMETER :: maxgccol = 13      !Maximum number of data columns in GeoClass
    INTEGER :: ncols                        !Total number of columns (rcols+number of s-lu-combinations (slc,dhslc,cr2) 
    
    !Local variables
    INTEGER mcols                   !Actual number of land-soil combinations in GeoData.txt
    INTEGER,ALLOCATABLE :: lakedataid(:)    !lakedataid from GeoData

    !> \b Algorithm \n
    status = 0

    !>Read the class characteristics; either from ClassData or GeoClass
    CALL load_classdata(fileunit_temp,TRIM(dir)//lfile2,status)
    IF(status/=0)THEN
      status = 0
      CALL load_geoclass(TRIM(dir)//lfile,classdata,nclass,nluse,nsoil,numrotations,atmdepvegreset)
    ENDIF
    
    IF(slc_iwet>0.OR.slc_owet>0)THEN
      conduct%wetland = .TRUE.
      IF(modeloption(p_wetland)/=2) WRITE(6,*) 'WARNING: Wetlands will be simulated although modeloption wetlandmodel 2 not set'
    ENDIF

    !>Count number of columns in GeoData
    CALL count_data_cols(fileunit_temp,TRIM(dir)//infile,0,ncols,status)
    IF(status/=0)RETURN

    !>Count number of subbasins in GeoData
    CALL count_data_rows(fileunit_temp,TRIM(dir)//infile,1,n,status)
    IF(status/=0)RETURN

    !>Initiate subbasin data
    CALL allocate_basinvariables(n,numsubstances)
    basin(1:n)%ilakecatch = -1. !Flag for not read i GeoData
    wetland(1:n,1:2)%area = 0.
    petmodel = 0
    IF(.NOT.ALLOCATED(lakedataid)) ALLOCATE(lakedataid(n))
    lakedataid = 0

    !>Read the description of the basins (GeoData.txt)
    CALL read_and_calc_basindata(fileunit_temp,TRIM(dir)//infile,n,lakedataid,ncols,mcols)

    !Check if lakesection/depth_hgdm is present when connectivity has been chosen
    IF(conduct%connectivity)THEN
      IF(.NOT.ALLOCATED(lakesectiondata) .AND. modeloption(p_connectivity)==1)THEN
        WRITE(6,*) 'Warning: No input data for connectivity found (lakesectiondata), calculations will be turned off.'
        modeloption(p_connectivity) = 0
        conduct%connectivity = .FALSE.
      ENDIF
      !IF(SUM(basin(:)%depth_hgdm))==0 .AND. modeloption(p_connectivity)==2)THEN
      !  WRITE(6,*) 'Warning: No input data for connectivity found (depth_hgdm), calculations will be turned off.'
      !  modeloption(p_connectivity) = 0
      !  conduct%connectivity = .FALSE.
      !ENDIF
      !IF(modeloption(p_connectivity)==3 .AND. .NOT.ALLOCATED(lakesectiondata) .AND. SUM(basin(:)%depth_hgdm))==0)THEN
      !  WRITE(6,*) 'Warning: No input data for connectivity found (lakesectiondata or depth_hgdm), calculations will be turned off.'
      !  modeloption(p_connectivity) = 0
      !  conduct%connectivity = .FALSE.
      !ENDIF
    ELSE
      IF(ALLOCATED(lakesectiondata))THEN
        WRITE(6,*) 'Warning: Input data for connectivity found (lakesectiondata), but modeloption for connectivity is not set.'      
        WRITE(6,*) 'Warning: Hydrological connectivity within sub-basins will not be simulated.'
        DEALLOCATE(lakesectiondata)
      ENDIF
      !IF(SUM(basin(:)%depth_hgdm))==0)THEN
      !  WRITE(6,*) 'Warning: Input data for connectivity found (depth_hgdm), but modeloption for connectivity is not set.'      
      !  WRITE(6,*) 'Warning: Hydrological connectivity within sub-basins will not be simulated.'
      !ENDIF
    ENDIF
    !IF(.NOT.ALLOCATED(lakesectiondata) .AND. modeloption(p_connectivity)>0)THEN
    !    WRITE(6,*) 'Warning: No input data for connectivity found (lakesectiondata), calculations will be turned off.'
    !    modeloption(p_connectivity) = 0
    !    conduct%connectivity = .FALSE.
    !ELSE
    !  conduct%connectivity = .TRUE.
    !  IF(ALLOCATED(lakesectiondata) .AND. modeloption(p_connectivity)==0)THEN
    !    WRITE(6,*) 'Warning: Input data for connectivity found (lakesectiondata), but modeloption for connectivity is not set.'      
    !    WRITE(6,*) 'Warning: Hydrological connectivity within sub-basins will not be simulated.'
    !    DEALLOCATE(lakesectiondata)
    !    conduct%connectivity = .FALSE.
    !  ENDIF
    !ENDIF

    !Check if river wetland is present
    IF(conduct%riverwetland)THEN
      IF(SUM(wetland(1:n,1:2)%area)==0)THEN
        WRITE(6,*) 'Warning: No input data for river wetlands found, calculations will be turned off.'
        conduct%riverwetland = .FALSE.
        IF(ALLOCATED(wetland)) DEALLOCATE(wetland)
      ENDIF
    ELSE
      IF(SUM(wetland(1:n,1:2)%area)>0)THEN
        WRITE(6,*) 'Warning: Input data for river wetlands found, but modeloption is not set.'
        WRITE(6,*) 'Warning: River wetland will not be simulated.'
      ENDIF
      IF(ALLOCATED(wetland)) DEALLOCATE(wetland)
    ENDIF

    !Check if petmodel is present
    IF(SUM(petmodel(1:n))==0)THEN
      conductbasinpetmodel = .FALSE.
      DEALLOCATE(petmodel)
    ELSE
      conductbasinpetmodel = .TRUE.
      IF(modeloption(p_petmodel)>0)THEN
        WRITE(6,*) 'WARNING: Modeloption for petmodel given in info.txt will be ignored. petmodel from GeoData will be used.'
        modeloption(p_petmodel)=0
      ELSE
        WRITE(6,*)'Model option: Different PET model for each subbasin will be used'
      ENDIF
    ENDIF
    
    !!Check that regions matching with CropData. TODO: elaborate
    !IF(ALLOCATED(cropindex))THEN
    !  IF(MAXVAL(basin%region)>SIZE(cropindex,2))THEN
    !    WRITE(6,*) 'ERROR: Maximum number of regions differ between GeoData and CropData'
    !    status = 1
    !    return
    !  ENDIF
    !ENDIF

    !>Read irrigation information of the basins (MgmtData.txt)
    CALL load_management_data(fileunit_temp,TRIM(dir),n,status)
    IF(status/=0)RETURN

    !>Read observation coupling and elevation from separate file (ForcKey.txt)
    CALL load_forcing_information_data(fileunit_temp,TRIM(fdir),n,status)
    IF(status/=0)RETURN

    !>Read additional description of important lakes (LakeData.txt) (possibly overwrite GeoData information)
    CALL load_lakedata(fileunit_temp,dir,n,lakedataid,status)
    IF(status/=0)RETURN
    IF(ALLOCATED(lakedataid)) DEALLOCATE(lakedataid)

    !>Read additional description of important dams (DamData.txt)
    CALL load_damdata(fileunit_temp,dir,n,status)
    IF(status/=0)RETURN

    !>Read additional description of flooding areas (FloodData.txt)
    CALL load_flooddata(fileunit_temp,dir,n,status)
    IF(status/=0)RETURN

  END SUBROUTINE load_basindata

  !>\brief Gets the information about classes from ClassData.txt
  !!Reads the file with column headings
  !!
  !>\b Consequences Module variables will be allocated and set
  !------------------------------------------------------------------------------
  SUBROUTINE load_classdata(funit,infile,status)

    USE WORLDVAR, ONLY : atmdepvegreset       !OUT
    USE MODVAR, ONLY : classdata,         & !OUT 
                       nluse,nsoil,       & !OUT
                       nclass,            & !OUT
                       set_soildepth_classdata, &
                       set_soildepth,           &
                       set_coded_classes,       &
                       slc_iwet,slc_owet, &
                       numrotations    !OUT

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit           !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of characteristics file to be read
    INTEGER, INTENT(OUT) :: status          !<File found and read
    
    !Local parameters
    INTEGER, PARAMETER :: nstr = 11   !Maximum length of headings
    INTEGER, PARAMETER :: nline = 200  !Maximum length of file line
    INTEGER,PARAMETER :: maxcol = 15  !Maximum number of data columns in ClassData
    
    !Local variables
    INTEGER i,islc,j,nskip
    INTEGER d,temp
    INTEGER nrows,mcols
    LOGICAL fileexist
    REAL,ALLOCATABLE :: xr(:,:),slcdata(:,:) !Real data read from file and sorted
    REAL    y(maxcol)                  !Data on row
    CHARACTER(LEN=nstr) str(maxcol)      !Content string
    CHARACTER(LEN=nline) line

    !>\b Algoritm \n
    !>Check for file.
    status = 0
    INQUIRE(FILE=infile,EXIST=fileexist)
    IF(.NOT.fileexist)THEN
      status = 1
      RETURN
    ENDIF
    
    !>Count number of classes (actually nslc include all rows)
    WRITE(6,*) 'File opened: ', TRIM(infile)
    CALL count_data_rows(funit,infile,0,nrows,status)
    IF(status/=0) STOP 1
    
    !>Allocate variable for holding class information
    IF(.NOT.ALLOCATED(slcdata)) ALLOCATE(slcdata(maxcol,nrows))  !big enough
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(maxcol,nrows))  !big enough
    slcdata = 0.
    xr = 0.

    !>Read information of GeoClass_col.txt
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read',ERR=202) 
    
    !>\li Skip comment lines in the beginning of the file
    nskip = 0
    DO 
      READ(funit,'(a)',END=200,ERR=201) line    
      IF(line(1:1)=='!')THEN 
        nskip = nskip + 1
      ELSE
        EXIT
      ENDIF
    ENDDO
    
    !>\li Read column headings
    CALL read_column_headings2(nline,line,maxcol,nstr,str,mcols,status)
    IF(status/=0) STOP 1
    islc = 0
    DO i=1,mcols
      IF(str(i)(1:nstr)=='class     ')THEN
        islc = i
        EXIT
      ENDIF
    ENDDO
    IF(islc==0)THEN
      WRITE(6,*) 'ERROR: No column with classes found in ',TRIM(infile)
      STOP 1
    ENDIF
    
    !>\li Read class information (requires slc in order in file NOT)
    DO
      y = 0
      READ(funit,*,END=204,ERR=201) y(1:mcols)
      d = NINT(y(islc))
      xr(:,d)=y(:)
    ENDDO
204 CONTINUE
    CLOSE(funit)
    
    nclass = NINT(MAXVAL(xr(islc,:)))
    IF(nclass==0)THEN
      WRITE(6,*) 'ERROR: No classes found in ',TRIM(infile)
      STOP 1
    ENDIF

    !>Sort class information
    DO i = 1,mcols
      IF(str(i)(1:nstr)=='class      ') CYCLE
      IF(str(i)(1:nstr)=='landuse    ') slcdata(1,:) = xr(i,:)
      IF(str(i)(1:nstr)=='soiltype   ') slcdata(2,:) = xr(i,:)
      IF(str(i)(1:nstr)=='cropid     ') slcdata(3,:) = xr(i,:)
      IF(str(i)(1:nstr)=='2ndcropid  ') slcdata(4,:) = xr(i,:)
      IF(str(i)(1:nstr)=='rotgroup   ') slcdata(5,:) = xr(i,:)
      IF(str(i)(1:nstr)=='atmdepveg  ') slcdata(6,:) = xr(i,:)
      IF(str(i)(1:nstr)=='classmodel ') slcdata(7,:) = xr(i,:)
      IF(str(i)(1:nstr)=='tiledepth  ') slcdata(8,:) = xr(i,:)
      IF(str(i)(1:nstr)=='streamdepth') slcdata(9,:) = xr(i,:)
      IF(str(i)(1:nstr)=='numlayers  ') slcdata(10,:) = xr(i,:)  !onödig?
      IF(str(i)(1:nstr)=='depthsl1   ') slcdata(11,:) = xr(i,:)
      IF(str(i)(1:nstr)=='depthsl2   ') slcdata(12,:) = xr(i,:)
      IF(str(i)(1:nstr)=='depthsl3   ') slcdata(13,:) = xr(i,:)
      IF(str(i)(1:nstr)=='traveltime ') slcdata(14,:) = xr(i,:)
    ENDDO

    IF(.NOT.ALLOCATED(classdata)) ALLOCATE(classdata(nclass))
    nluse = NINT(MAXVAL(slcdata(1,1:nclass)))
    nsoil = NINT(MAXVAL(slcdata(2,1:nclass)))
    classdata(1:nclass)%luse  = NINT(slcdata(1,1:nclass))
    classdata(1:nclass)%soil  = NINT(slcdata(2,1:nclass))
    classdata(1:nclass)%crop  = NINT(slcdata(3,1:nclass))
    classdata(1:nclass)%crop2 = NINT(slcdata(4,1:nclass))
    classdata(1:nclass)%rotation = NINT(slcdata(5,1:nclass))
    numrotations = MAXVAL(classdata(:)%rotation)
    classdata(1:nclass)%vegtype = NINT(slcdata(6,1:nclass))
    classdata(1:nclass)%tiledepth = slcdata(8,1:nclass)
    classdata(1:nclass)%streamdepth = slcdata(9,1:nclass)
    classdata(1:nclass)%traveltime = slcdata(14,1:nclass)
    IF(MAXVAL(slcdata(10,:))==0)THEN
      !Count number of soillayers
      DO j = 1,nclass
        slcdata(10,j)=3
        IF(slcdata(13,j)==0.OR.slcdata(13,j)==slcdata(12,j)) slcdata(10,j)=2
        IF(slcdata(12,j)==0.OR.slcdata(12,j)==slcdata(11,j)) slcdata(10,j)=1
      ENDDO
    ENDIF
    DO j = 1, nclass
      CALL set_soildepth_classdata(NINT(slcdata(10,j)),slcdata(11:10+NINT(slcdata(10,j)),j),classdata(j)) !saved values from GeoClass.txt
      temp = set_soildepth(NINT(slcdata(10,j)),j,slcdata(11:10+NINT(slcdata(10,j)),j)) !initial values from GeoClass.txt
    ENDDO

    !>Check and set vegtype and atmdepvegreset.
    atmdepvegreset = .FALSE.
    DO j = 1,nclass
      IF(classdata(j)%vegtype==0)THEN
        classdata(j)%vegtype=1
        atmdepvegreset = .TRUE.
        WRITE(6,*) 'Warning: vegetation type not given in GeoClass.txt for slc-class',j,' vegtype=1 (open) is used'
      ENDIF
    ENDDO
    
    !Find coded classes
    CALL set_coded_classes(nclass,slcdata(7,1:nclass))

    !Check no tiles in wetland
    IF(slc_iwet>0)THEN
      IF(classdata(slc_iwet)%tiledepth>0.)THEN
        WRITE(6,*) 'ERROR: Tiles not allowed together with wetland. SLC: ',slc_iwet
        STOP 1
      ENDIF
    ENDIF
    IF(slc_owet>0)THEN
      IF(classdata(slc_owet)%tiledepth>0.)THEN
        WRITE(6,*) 'ERROR: Tiles not allowed together with wetland. SLC: ',slc_owet
        STOP 1
      ENDIF
    ENDIF

    !Clean up subroutine
    IF(ALLOCATED(slcdata)) DEALLOCATE(slcdata)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    WRITE(6,*) 'Class information loaded (ClassData)'
    RETURN

200 CONTINUE
    CLOSE(funit)
    WRITE(6,*) 'Error: No classes found in ',TRIM(infile)
    status = 1
    RETURN
    
201 CONTINUE
    WRITE(6,*) 'Error reading ',TRIM(infile)
    CLOSE(funit)
    status = 1
    RETURN
202 CONTINUE
    WRITE(6,*) 'Error opening ',TRIM(infile)
    status = 1
    RETURN

  END SUBROUTINE load_classdata

  !>Read class characteristics file GeoClass.txt
  !
  !>\b Consequences Module modvar variables classdata, classgroup, nclass, nluse,
  !>nsoil, numrotations and vegtypereset will be allocated and changed by the 
  !>argument output of this subroutine. Module modvar variables for special classes
  !>will be set by a subroutine call.
  !------------------------------------------------------------------
  SUBROUTINE  load_geoclass(fname,geoclassdata,numslc,maxluse,maxsoil,maxrotation,usevegtypedefault)
    
    USE WORLDVAR, ONLY : fileunit_temp
    USE MODVAR, ONLY : classtype, &
                       set_soildepth_classdata, &
                       set_soildepth,           &
                       set_coded_classes, &
                       slc_iwet,slc_owet
    USE READWRITE_ROUTINES, ONLY : read_geoclass

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: fname    !<File path/name
    TYPE(CLASSTYPE),ALLOCATABLE, INTENT(INOUT) :: geoclassdata(:)  !<class characteristics
    INTEGER, INTENT(OUT) :: numslc            !<Number of land-soil use combination in GeoClass.txt
    INTEGER, INTENT(OUT) :: maxluse          !<Number of landuses
    INTEGER, INTENT(OUT) :: maxsoil          !<Number of soiltypes
    INTEGER, INTENT(OUT) :: maxrotation      !<Number of groups of classes that exchange crops between years
    LOGICAL, INTENT(OUT) :: usevegtypedefault  !<Flag for vegetation type being set to open (default)
    
    !Local parameters
    INTEGER,PARAMETER :: maxgccol = 13      !Maximum number of data columns in GeoClass

    !Local variables
    INTEGER j                  !class index (slc)
    INTEGER temp               !help variable
    REAL,ALLOCATABLE :: slcdata(:,:)   !data from Geoclass-file
    
    !>\b Algorithm \n
    !>Read the file with key to class characteristics (GeoClass.txt)
    CALL read_geoclass(fileunit_temp,TRIM(fname),maxgccol,slcdata,numslc)

    !>Set read data to class characteristic structure
    IF(.NOT.ALLOCATED(geoclassdata)) ALLOCATE(geoclassdata(numslc))
    maxluse = NINT(MAXVAL(slcdata(1,1:numslc)))
    maxsoil = NINT(MAXVAL(slcdata(2,1:numslc)))
    geoclassdata(1:numslc)%luse  = NINT(slcdata(1,1:numslc))
    geoclassdata(1:numslc)%soil  = NINT(slcdata(2,1:numslc))
    geoclassdata(1:numslc)%crop  = NINT(slcdata(3,1:numslc))
    geoclassdata(1:numslc)%crop2 = NINT(slcdata(4,1:numslc))
    geoclassdata(1:numslc)%rotation = NINT(slcdata(5,1:numslc))
    maxrotation = MAXVAL(geoclassdata(:)%rotation)
    geoclassdata(1:numslc)%vegtype = NINT(slcdata(6,1:numslc))
    geoclassdata(1:numslc)%tiledepth = slcdata(8,1:numslc)
    geoclassdata(1:numslc)%streamdepth = slcdata(9,1:numslc)
    DO j = 1, numslc
      CALL set_soildepth_classdata(NINT(slcdata(10,j)),slcdata(11:10+NINT(slcdata(10,j)),j),geoclassdata(j)) !saved values from GeoClass.txt
      temp = set_soildepth(NINT(slcdata(10,j)),j,slcdata(11:10+NINT(slcdata(10,j)),j)) !initial values from GeoClass.txt
    ENDDO
     
    !>Check and set vegtype and atmdepvegreset.
    usevegtypedefault = .FALSE.
    DO j = 1,numslc
      IF(geoclassdata(j)%vegtype==0)THEN
        geoclassdata(j)%vegtype=1
        usevegtypedefault = .TRUE.
        WRITE(6,*) 'Warning: vegetation type not given in GeoClass.txt for slc-class',j,' vegtype=1 (open) is used'
      ENDIF
    ENDDO

    !>Find coded classes
    CALL set_coded_classes(numslc,slcdata(7,1:numslc))

    !Check no tiles in wetland
    IF(slc_iwet>0)THEN
      IF(geoclassdata(slc_iwet)%tiledepth>0.)THEN
        WRITE(6,*) 'ERROR: Tiles not allowed together with wetland. SLC: ',slc_iwet
        STOP 1
      ENDIF
    ENDIF
    IF(slc_owet>0)THEN
      IF(geoclassdata(slc_owet)%tiledepth>0.)THEN
        WRITE(6,*) 'ERROR: Tiles not allowed together with wetland. SLC: ',slc_owet
        STOP 1
      ENDIF
    ENDIF

    !Clean up subroutine
    IF(ALLOCATED(slcdata)) DEALLOCATE(slcdata)
    WRITE(6,*) 'Class information loaded (GeoClass)'

  END SUBROUTINE load_geoclass

  !>\brief Set model base configuration
  !>
  !>Handle dimension of river queue state; the queue length may be different 
  !>for current model, initial state file, submodel and for optimization. 
  !>Set structure holding state dimensions.
  !>Transform crop information for southern hemisphere.
  !>
  !>\b Consequences Module modvar variables cropdata and cropirrdata 
  !>may be changed.
  !------------------------------------------------------------------
  SUBROUTINE set_model_base_configuration(nsbase,stateinput,idir,idir2,dim,status)
  
  USE WORLDVAR, ONLY : doens,doopt
  USE MODVAR, ONLY : conductregest, &
                     STATEDIMENSIONTYPE, &
                     nclass, &
                     numsubstances,naquifers, &
                     maxsoillayers,nrivertypes, &
                     nlaketypes,timesteps_per_day, &
                     maxlakesections,p_connectivity, &
                     modeloption,lakesectiondata,basin
  USE MODELMODULE, ONLY : set_parameters_region_division, &
                          load_modeldefined_input, &
                          get_special_model_parameters, &
                          calculate_special_model_parameters
  USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_model_base_conf_ok, &
                                    e_error,e_warning,e_info
  
  !Argument declaration
  INTEGER, INTENT(IN) :: nsbase  !<number of subbasin
  LOGICAL, INTENT(IN) :: stateinput  !<code for reading state
  CHARACTER(LEN=*), INTENT(IN) :: idir  !<file directory (modeldir)
  CHARACTER(LEN=*), INTENT(IN) :: idir2  !<file directory (forcingdir)
  TYPE(STATEDIMENSIONTYPE), INTENT(INOUT) :: dim !<simulation dimensions
  INTEGER, INTENT(OUT) :: status  !<error status of subroutine
  
  !Local variables
  REAL optrivvel,optdamp
  INTEGER i
  INTEGER rvindex,dpindex   !index of rivvel and damp in modparid (not used here)
  INTEGER helparray(nsbase)
  INTEGER dimrivlag
  INTEGER nparens   !number of par_nnn.txt files
  LOGICAL taskAS,taskBS
  TYPE(DateType):: date  !not used
  CHARACTER(LEN=100) :: propagate_str
  
  !Initialisations
  status = 0
  helparray = 0
  date = DateType(2000,1,1,0,0)

  !Find maximum dimension of river queue variables
  CALL load_modeldefined_input(idir,idir2,nsbase,nsbase,helparray,date,date,.FALSE.,conductregest,status) !read reg_par.txt
  IF(status/=0) RETURN
  optrivvel = 9999.;optdamp = 9999.
  CALL get_special_model_parameters(rvindex,dpindex)
  IF(doopt) CALL calculate_special_optpar_parameters(idir,rvindex,dpindex,optrivvel,optdamp)  !optpar.txt
  IF(doens)THEN
    CALL read_parameter_ensemble_task(idir,taskAS,taskBS) 
    IF(taskAS.OR.taskBS)THEN
      CALL calculate_special_optpar_parameters(idir,rvindex,dpindex,optrivvel,optdamp)  !optpar.txt
    ELSE
      CALL count_parfile_simulations(nparens)
      CALL calculate_special_partxt_parameters(idir,nparens,rvindex,dpindex,optrivvel,optdamp)  !par_nnn.txt
    ENDIF
  ENDIF
  CALL calculate_special_model_parameters(nsbase,optrivvel,optdamp,dimrivlag)  !par.txt,reg_par.txt
  
  !Set state dimension variables
  dim%substance = numsubstances
  dim%slcclass = nclass
  dim%aquifer = naquifers
  dim%river = nrivertypes
  dim%lake = nlaketypes
  dim%soillayer = maxsoillayers
  dim%riverqueue = dimrivlag
  dim%timestep = timesteps_per_day
  
  !Check maximum number of lakesections set in basindata for fill-and-spill ilake OR HGDM model
  dim%lakesection = 0
  IF(modeloption(p_connectivity)==1)THEN !ilake section model
    DO i = 1, nsbase
      IF(basin(i)%lakesection > dim%lakesection) dim%lakesection = basin(i)%lakesection
    ENDDO
  ELSEIF(modeloption(p_connectivity)==2)THEN  !HGDM model
    dim%lakesection = 1
  ELSEIF(modeloption(p_connectivity)==3)THEN  !combination of ilake section model and HGDM model
    dim%lakesection = 1
    DO i = 1, nsbase
      IF(basin(i)%lakesection > dim%lakesection) dim%lakesection = basin(i)%lakesection
    ENDDO
  ENDIF

  !Check if river queue variables is compatible with initial state and calibration
  IF(doopt.AND.stateinput.AND.(optrivvel<9999. .OR. optdamp<9999.))THEN
    WRITE(6,*) 'ERROR: Starting state is not compatible with autocalibration of'
    WRITE(6,*) 'ERROR: parameters rivvel and damp.'
    propagate_str = 'Starting state is not compatible with autocalibration of parameter rivvel and damp'
    CALL propagate_external_msg(e_model_base_conf_ok,e_error,propagate_str)
    status = 1
    RETURN
  ENDIF
  
  END SUBROUTINE set_model_base_configuration

  !>Set model choice configuration and state dimensions
  !------------------------------------------------------------------
  SUBROUTINE set_model_configuration(config,simconfig)
  
  USE MODVAR, ONLY : nclass,classmodel, &
                     modeloption,p_growthstart, &
                     basin,nsub, &
                     deallocate_glacier, &
                     STATECONFIGURATIONTYPE, &
                     SIMULATIONCONFIGURATIONTYPE
  USE MODELMODULE, ONLY : set_special_models
  
  !Argument declarations
  TYPE(STATECONFIGURATIONTYPE), INTENT(INOUT) :: config !<simulation configuration
  TYPE(SIMULATIONCONFIGURATIONTYPE), INTENT(INOUT) :: simconfig !<simulation configuration

  !Local variables
  INTEGER i,itype
  LOGICAL :: glacexist   !status of glacier model
  LOGICAL :: irrexist    !status of irrigation model
 
  CALL set_special_models(nclass,classmodel,glacexist,irrexist)
  config%irrigation = irrexist

  !Deallocate and set glacier-variables
  IF(.NOT.glacexist) CALL deallocate_glacier()
  config%glacier = glacexist
  
  IF(modeloption(p_growthstart)==1) config%growthdegreeday = .TRUE.
  
  itype = 2
  IF(.NOT.ALLOCATED(simconfig%olakedeppar)) ALLOCATE(simconfig%olakedeppar(nsub))
  DO i = 1,nsub
    simconfig%olakedeppar(i) = (basin(i)%lakedepth(itype)<=0.)
  ENDDO
  
  IF(config%riverwetland) config%wetland = .TRUE.

  END SUBROUTINE set_model_configuration

  !>\brief Allocate model parameters and set their region division coupling.
  !>
  !>\b Consequences Module modvar arrays related to parameters are 
  !>allocated and set.
  !------------------------------------------------------------------
  SUBROUTINE initiate_model_parameters(ns_base,status)
  
  USE MODVAR, ONLY : allocate_modelparameters
  USE MODELMODULE, ONLY : set_parameters_region_division
                          
  
  !Argument declaration
  INTEGER, INTENT(IN) :: ns_base  !<number of subbasin
  INTEGER, INTENT(OUT) :: status  !<error status of subroutine
  
  !Local variable
  INTEGER nregpar  !number of regional parameters
  
  !Initialisations
  status = 0

  !Allocate model parameters and calculate number of regional parameters
  CALL allocate_modelparameters(ns_base,nregpar)

  !Allocate and set region division to the one used for each parameter.
  CALL set_parameters_region_division(nregpar)
  
  END SUBROUTINE initiate_model_parameters

  !>Get the information about branches from BranchData.txt
  !
  !>\b Consequences Module modvar variable branchsubid will be set
  !------------------------------------------------------------------
  SUBROUTINE load_branchdata(dir,status) 

    USE WORLDVAR, ONLY : i_str,       &
                         i_intg,      &
                         i_real,      &
                         fileunit_temp, &
                         maxcharpath
    USE MODVAR, ONLY : branchsubid         !OUT
    USE CONVERT, ONLY : real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_load_branch_data, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir  !<File directory
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local parameters
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading

    !Local variables
    LOGICAL fex
    INTEGER i,j
    INTEGER ncols                   !Total number of columns
    INTEGER mcols                   !Help variable
    INTEGER nrows                   !Number of branches in BranchData.txt
    CHARACTER(LEN=maxcharpath) filename
    INTEGER,ALLOCATABLE :: code(:)        !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)      !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)      !Index for column integer variables
    INTEGER,ALLOCATABLE :: xi(:,:)           !Integer data read from file
    REAL,ALLOCATABLE    :: xr(:,:)           !Real data read from file
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)  !Content string
    CHARACTER(LEN=100) :: propagate_str

    status = 0
    filename = TRIM(dir)//'BranchData.txt'

    !> \b Algorithm \n
    !>Check if file exist
    INQUIRE(FILE=filename,EXIST=fex)
    IF(.NOT.fex) RETURN   
    WRITE(6,*) 'File opened: ', TRIM(filename)

    !Count number of columns in BranchData.txt
    CALL count_data_cols(fileunit_temp,TRIM(filename),0,ncols,status)
    IF(status/=0)RETURN

    !Count number of rows in BranchData.txt
    CALL count_data_rows(fileunit_temp,TRIM(filename),1,nrows,status)
    IF(status/=0)RETURN

    !>Initiate branch data
    IF(.NOT.ALLOCATED(branchsubid)) ALLOCATE(branchsubid(nrows))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(ncols))
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,ncols))
    branchsubid%source = 0
    branchsubid%branch = 0
    branchsubid%mainpart  = 1.
    branchsubid%maxQ = 0.
    branchsubid%minQ = 0.
    branchsubid%maxQbranch = 0.
    branchsubid%recQbranch = .FALSE.
    branchsubid%lb2outlet = .FALSE.
    branchsubid%uplakebasin = .FALSE.

    !>Read BranchData-file
    OPEN(UNIT = fileunit_temp,FILE = TRIM(filename), STATUS = 'old', ACTION='read')     

    !Read the column headings from file
    CALL read_column_headings(fileunit_temp,ncols,letters,str,mcols,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading heading in file: ',TRIM(filename)
      propagate_str = 'reading heading in file: '//TRIM(filename)
      CALL propagate_external_msg(e_load_branch_data,e_error,propagate_str)
      RETURN
    ENDIF

    !Code variables for easy finding of variable type for columns
    code=i_str    !string, ignore
    DO i = 1,ncols
      IF(str(i)(1:letters)=='sourceid  ') code(i) = i_intg
      IF(str(i)(1:letters)=='branchid  ') code(i) = i_intg
      IF(str(i)(1:letters)=='mainpart  ') code(i) = i_real
      IF(str(i)(1:letters)=='maxqmain  ') code(i) = i_real
      IF(str(i)(1:letters)=='maxqbranch') code(i) = i_real
      IF(str(i)(1:letters)=='qbranch   ') code(i) = i_intg
      IF(str(i)(1:letters)=='minqmain  ') code(i) = i_real
    ENDDO

    CALL read_basindata5(fileunit_temp,filename,ncols,nrows,ncols,code,rindex,iindex,xi,xr)  !Read all data
    CLOSE(UNIT=fileunit_temp)

    !>Save loaded branch data temporary
    DO i = 1,mcols
      IF(str(i)(1:letters)=='sourceid  ')   branchsubid(1:nrows)%source     = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='branchid  ')   branchsubid(1:nrows)%branch     = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='mainpart  ')   branchsubid(1:nrows)%mainpart   = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='maxqmain  ')   branchsubid(1:nrows)%maxQ       = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='minqmain  ')   branchsubid(1:nrows)%minQ       = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='maxqbranch')   branchsubid(1:nrows)%maxQbranch = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='qbranch   ')THEN
        DO j = 1,nrows
          branchsubid(j)%recQbranch = (xi(j,iindex(i))==1)
          branchsubid(j)%lb2outlet = (xi(j,iindex(i))==2)
        ENDDO
      ENDIF
    ENDDO
    
    !>Check for illegal combinations of branch data and other illegal values
    DO i = 1,nrows
      IF(branchsubid(i)%maxQbranch>0)THEN
        IF(branchsubid(i)%maxQ>0)THEN
          branchsubid(i)%maxQbranch = 0.
          WRITE(6,*) 'WARNING: Maximum flow in both main channel and branch is given for source: ',branchsubid(i)%source
          WRITE(6,*) 'WARNING: This is not allowed. Maximum flow of branch will be removed.'
          WRITE(6,*) 'WARNING: You are suggested to correct your BranchData.txt file.'
        ENDIF
      ENDIF
      IF(branchsubid(i)%maxQbranch<0)THEN
        branchsubid(i)%maxQbranch = 0.
        WRITE(6,*) 'WARNING: Maximum flow in branch is negative for source: ',branchsubid(i)%source
        WRITE(6,*) 'WARNING: This is not allowed. Maximum flow of branch will be removed.'
        WRITE(6,*) 'WARNING: You are suggested to correct your BranchData.txt file.'
      ENDIF
      IF(branchsubid(i)%maxQ<0)THEN
        branchsubid(i)%maxQ = 0.
        WRITE(6,*) 'WARNING: Maximum flow in main channel is negative for source: ',branchsubid(i)%source
        WRITE(6,*) 'WARNING: This is not allowed. Maximum flow of main channel will be removed.'
        WRITE(6,*) 'WARNING: You are suggested to correct your BranchData.txt file.'
      ENDIF
      IF(branchsubid(i)%minQ<0)THEN
        branchsubid(i)%minQ = 0.
        WRITE(6,*) 'WARNING: Minimum flow in main channel is negative for source: ',branchsubid(i)%source
        WRITE(6,*) 'WARNING: This is not allowed. Minimum flow of main channel will be removed.'
        WRITE(6,*) 'WARNING: You are suggested to correct your BranchData.txt file.'
      ENDIF
      IF(branchsubid(i)%mainpart<0. .OR. branchsubid(i)%mainpart>1.)THEN
        WRITE(6,*) 'ERROR: Main channel flow fraction is not between 0 and 1 for source: ',branchsubid(i)%source
        WRITE(6,*) 'ERROR: This is not allowed.'
        WRITE(6,*) 'ERROR: Fraction is needed for upstream area calculations.'
        WRITE(6,*) 'ERROR: You need to change your BranchData.txt file.'
        propagate_str = 'Main channel flow fraction is not between 0 and 1 for source: '//real_to_str(REAL(branchsubid(i)%source))
        CALL propagate_external_msg(e_load_branch_data,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
      IF(branchsubid(i)%recQbranch.OR.branchsubid(i)%lb2outlet)THEN
        IF(branchsubid(i)%maxQ>0. .OR. branchsubid(i)%minQ>0. .OR. branchsubid(i)%maxQbranch>0.)THEN
          branchsubid(i)%maxQ = 0.
          branchsubid(i)%minQ = 0.
          branchsubid(i)%maxQbranch = 0.
          WRITE(6,*) 'WARNING: Wanted flow in branch cannot be combined with maximum or minimum flow in main channel or branch.' 
          WRITE(6,*) 'WARNING: This is not allowed, and maximum/minimum flows will be removed for source: ',branchsubid(i)%source
          WRITE(6,*) 'WARNING: You are suggested to correct your BranchData.txt file.'
        ENDIF
      ENDIF
    ENDDO

    !Deallocate local arrays (for gfortran)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)

    WRITE(6,*) 'Bifurcation information loaded (BranchData.txt)'

  END SUBROUTINE load_branchdata

  !>Check for connected lakebasins
  !------------------------------------------------------------------
  SUBROUTINE find_connected_lakebasins(ndim,basins,downbasins,upbasin) 

    USE MODVAR, ONLY : lakebasinindex, &
                       lakebasin, &
                       find_lbtype, &
                       is_upstream_lakebasin

    !Argument declarations
    INTEGER, INTENT(IN) :: ndim               !<Number of subbasins to be checked
    INTEGER, INTENT(IN) :: basins(ndim)       !<Subid of all subbasins to be checked; basin(i)%subid, branchsubid(i)%source
    INTEGER, INTENT(IN) :: downbasins(ndim)   !<Subid of their downstream subbasins; pathsubid(i)%main or branchsubid(i)%branch
    LOGICAL, INTENT(OUT) :: upbasin(ndim)     !<Flag for lakebasins upstream of other lakebasins
    
    !Local variables
    INTEGER i
    INTEGER iup,idown,ilakeup,ilakedown
    INTEGER upldtype
    
    !>Check for connected lakebasins
    upbasin = .FALSE.
    IF(.NOT.ALLOCATED(lakebasinindex)) RETURN  !no lakebasins
    
    DO i = 1,ndim
      iup = basins(i)
      CALL find_lbtype(iup,upldtype)
      IF(is_upstream_lakebasin(upldtype))THEN
        idown = downbasins(i)
        IF(idown<=0) CYCLE   !no downstream basin
        ilakeup = lakebasin(lakebasinindex(iup))%ilk
        IF(lakebasinindex(idown)<=0) CYCLE   ! no downstream lakebasin
        ilakedown = lakebasin(lakebasinindex(idown))%ilk
        IF(ilakeup==ilakedown)THEN  !same lake
          upbasin(i) = .TRUE.
        ENDIF
      ENDIF
    ENDDO
    
  END SUBROUTINE find_connected_lakebasins

  !>Get the information about aquifers from AquiferData.txt
  !!
  !>\b Consequences Module modvar variable aquifer and pathsubid will be set. 
  !!Module modvar varible nregions may be increased.
  !------------------------------------------------------------------
  SUBROUTINE load_aquiferdata(dir,nsub,numaqu,status) 

    USE WORLDVAR, ONLY : i_str,       &
                         i_intg,      &
                         i_real,      &
                         fileunit_temp, &
                         maxcharpath
    USE MODVAR, ONLY : basin,     &
                       pathsubid, &  !OUT
                       aquifer,   &  !OUT
                       nregions,  &  !OUT
                       max_subid, &
                       modeloption, &
                       p_deepgroundwater
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_load_aquiferdata, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir    !<File directory
    INTEGER, INTENT(IN)  :: nsub            !<Number of subbasins (basemodel)
    INTEGER, INTENT(OUT) :: numaqu          !<Number of aquifers
    INTEGER, INTENT(OUT) :: status          !<Error status
    
    !Local parameters
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading

    !Local variables
    LOGICAL fex
    INTEGER i,j
    INTEGER ncols                   !Total number of columns
    INTEGER mcols                   !Help variable
    INTEGER nrows                   !Number of rows in AquiferData.txt
    INTEGER subid_i,aquid_i,recha_i,outfr_i   !data column index for these columns
    INTEGER area_i,depth_i,basdp_i,topdp_i,passdep_i   !data column index for these columns
    INTEGER rate_i,delay_i,por_i,reg_i  !data column index for these columns
    INTEGER temp_i,coIN_i,coSP_i  !data column index for these columns
    INTEGER aqindex(max_subid)  !aquid limited to max_subid
    CHARACTER(LEN=maxcharpath) filename
    INTEGER,ALLOCATABLE :: code(:)        !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)      !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)      !Index for column integer variables
    INTEGER,ALLOCATABLE :: xi(:,:)           !Integer data read from file
    REAL,ALLOCATABLE    :: xr(:,:)           !Real data read from file
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)  !Content string
    CHARACTER(LEN=250) :: propagate_str

    status = 0
    numaqu = 0
    filename = TRIM(dir)//'AquiferData.txt'

    !Check if file exist
    INQUIRE(FILE=filename,EXIST=fex)
    IF(fex)THEN
      IF(modeloption(p_deepgroundwater)/=2)THEN
        WRITE(6,*) 'Warning: Existing AquiferData.txt will not be used, since modeloption not set.'
        RETURN
      ENDIF
    ELSE
      IF(modeloption(p_deepgroundwater)==2)THEN
        WRITE(6,*) 'Warning: Use modeloption with aquifer without AquiferData.txt not possible.'
        WRITE(6,*) 'Warning: Deep groundwater modeloption turned off.'
        modeloption(p_deepgroundwater)=0
        RETURN
      ENDIF
      RETURN   
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(filename)

    !Count number of columns in AquiferData.txt
    CALL count_data_cols(fileunit_temp,TRIM(filename),0,ncols,status)
    IF(status/=0)RETURN

    !Count number of rows in AquiferData.txt
    CALL count_data_rows(fileunit_temp,TRIM(filename),1,nrows,status)
    IF(status/=0)RETURN

    !Initiate branch data
    IF(.NOT.ALLOCATED(pathsubid)) ALLOCATE(pathsubid(nsub))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(ncols))
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,ncols))
    pathsubid%aquid = 0
    pathsubid%rechargebasin = .FALSE.
    pathsubid%recievefraction = 0.

    !Read AquiferData-file
    OPEN(UNIT = fileunit_temp,FILE = TRIM(filename), STATUS = 'old', ACTION='read')     

    !Read the column headings from file
    CALL read_column_headings(fileunit_temp,ncols,letters,str,mcols,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading heading in file: ',TRIM(filename)
      propagate_str = 'reading heading in file: '//TRIM(filename)
      CALL propagate_external_msg(e_load_aquiferdata,e_error,propagate_str)
      RETURN
    ENDIF

    propagate_str = 'columns ignored: -'

    !Code variables for easy finding of variable type for columns
    code=i_str    !string, ignore
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ') code(i) = i_intg
      IF(str(i)(1:letters)=='aquid     ') code(i) = i_intg
      IF(str(i)(1:letters)=='parreg    ') code(i) = i_intg
      IF(str(i)(1:letters)=='recharge  ') code(i) = i_intg
      IF(str(i)(1:letters)=='area      ') code(i) = i_real
      IF(str(i)(1:letters)=='inidepth  ') code(i) = i_real
      IF(str(i)(1:letters)=='topdepth  ') code(i) = i_real
      IF(str(i)(1:letters)=='basedepth ') code(i) = i_real
      IF(str(i)(1:letters)=='passivedep') code(i) = i_real
      IF(str(i)(1:letters)=='porosity  ') code(i) = i_real
      IF(str(i)(1:letters)=='retfrac   ') code(i) = i_real
      IF(str(i)(1:letters)=='retrate   ') code(i) = i_real
      IF(str(i)(1:letters)=='delay     ') code(i) = i_real
      IF(str(i)(1:letters)=='temp      ') code(i) = i_real
      IF(str(i)(1:letters)=='conc_in   ') code(i) = i_real
      IF(str(i)(1:letters)=='conc_sp   ') code(i) = i_real
    ENDDO

    CALL read_basindata5(fileunit_temp,filename,ncols,nrows,ncols,code,rindex,iindex,xi,xr)  !Read all data
    CLOSE(UNIT=fileunit_temp)

    !Find and check data columns
    subid_i=0;aquid_i=0;recha_i=0;area_i=0;depth_i=0;basdp_i=0;reg_i=0;passdep_i=0
    topdp_i=0;por_i=0;outfr_i=0;rate_i=0;delay_i=0;temp_i=0;coIN_i=0;coSP_i=0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ')   subid_i = iindex(i)
      IF(str(i)(1:letters)=='aquid     ')   aquid_i = iindex(i)
      IF(str(i)(1:letters)=='parreg    ')   reg_i   = iindex(i)
      IF(str(i)(1:letters)=='recharge  ')   recha_i = iindex(i)
      IF(str(i)(1:letters)=='area      ')   area_i  = rindex(i)
      IF(str(i)(1:letters)=='inidepth  ')   depth_i = rindex(i)
      IF(str(i)(1:letters)=='basedepth ')   basdp_i = rindex(i)
      IF(str(i)(1:letters)=='topdepth  ')   topdp_i = rindex(i)
      IF(str(i)(1:letters)=='passivedep')   passdep_i = rindex(i)
      IF(str(i)(1:letters)=='porosity  ')   por_i   = rindex(i)
      IF(str(i)(1:letters)=='retfrac   ')   outfr_i = rindex(i)
      IF(str(i)(1:letters)=='retrate   ')   rate_i  = rindex(i)
      IF(str(i)(1:letters)=='delay     ')   delay_i = rindex(i)
      IF(str(i)(1:letters)=='temp      ')   temp_i  = rindex(i)
      IF(str(i)(1:letters)=='conc_in   ')   coIN_i  = rindex(i)
      IF(str(i)(1:letters)=='conc_sp   ')   coSP_i  = rindex(i)
      IF(code(i) == i_str) THEN
        propagate_str=TRIM(propagate_str)//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO
    CALL propagate_external_msg(e_load_aquiferdata,e_info,TRIM(propagate_str),REAL(code))
    IF(subid_i==0 .OR. aquid_i==0 .OR. recha_i==0 .OR. area_i==0 .OR. &
       depth_i==0 .OR. basdp_i==0 .OR. por_i==0  .OR. reg_i==0 .OR. &
       outfr_i==0 .OR. rate_i==0  .OR. delay_i==0)THEN
      WRITE(6,*) 'Some columns are missing in AquiferData.txt'
      propagate_str = 'either of subid,aquid,recha,area,depth,basdp,por,reg,outfr,rate or delay columns is missing'
      CALL propagate_external_msg(e_load_aquiferdata,e_error,propagate_str)
      status=1
      RETURN
    ENDIF
    !IF(passdep_i==0) passdep_i = basdp_i    !No passive volume; basedepth = passive depth
    
    !Count number of aquifers
    numaqu = 0
    DO i = 1,nrows
      IF(xi(i,subid_i)==0)THEN  !Row with aquifer information
        numaqu = numaqu + 1
        aqindex(xi(i,aquid_i))=numaqu
      ENDIF
    ENDDO

    ALLOCATE(aquifer(numaqu))
    !Default values
    aquifer%temperature = 0.
    aquifer%conc_IN = 0.
    aquifer%conc_SP = 0.
    aquifer%passivevol = 0.
    aquifer%passivedep = 0.

    !Save loaded data
    DO i = 1,nrows
      IF(xi(i,subid_i)>0)THEN   !Row with aquifer subbasin coupling information
        DO j = 1,nsub
          IF(basin(j)%subid==xi(i,subid_i)) EXIT
        ENDDO
        IF(j<=nsub)THEN
          pathsubid(j)%aquid = aqindex(xi(i,aquid_i))
          pathsubid(j)%rechargebasin = (xi(i,recha_i)==1)
          pathsubid(j)%recievefraction = xr(i,outfr_i)
        ELSE
          WRITE(6,*) 'Warning: subid in AquiferData not in GeoData', xi(i,subid_i)
        ENDIF
      ELSEIF(xi(i,subid_i)==0)THEN  !Row with aquifer information
        aquifer(aqindex(xi(i,aquid_i)))%area = xr(i,area_i)
        aquifer(aqindex(xi(i,aquid_i)))%parregion(2) = xi(i,reg_i)
        aquifer(aqindex(xi(i,aquid_i)))%retrate = xr(i,rate_i)
        aquifer(aqindex(xi(i,aquid_i)))%percdelay = xr(i,delay_i)
        aquifer(aqindex(xi(i,aquid_i)))%basedepth = xr(i,basdp_i)
!        IF(passdep_i>0) aquifer(aqindex(xi(i,aquid_i)))%passivedep = xr(i,passdep_i)
        aquifer(aqindex(xi(i,aquid_i)))%reference = xr(i,basdp_i)   !m default
        IF(passdep_i>0)THEN
          IF(xr(i,passdep_i)<0)THEN
            aquifer(aqindex(xi(i,aquid_i)))%reference = xr(i,passdep_i)
            aquifer(aqindex(xi(i,aquid_i)))%passivevol = (xr(i,basdp_i)-xr(i,passdep_i))*xr(i,area_i)*xr(i,por_i)  !m3
            aquifer(aqindex(xi(i,aquid_i)))%passivedep = xr(i,passdep_i)
          ENDIF
        ENDIF
        aquifer(aqindex(xi(i,aquid_i)))%inivol = (xr(i,depth_i)-aquifer(aqindex(xi(i,aquid_i)))%reference)*xr(i,area_i)*xr(i,por_i)  !m3
!        IF(topdp_i>0) aquifer(aqindex(xi(i,aquid_i)))%maxvol = (xr(i,topdp_i)-xr(i,basdp_i))*xr(i,area_i)*xr(i,por_i)  !m3
!        IF(topdp_i>0) aquifer(aqindex(xi(i,aquid_i)))%maxvol = (xr(i,topdp_i)-xr(i,passdep_i))*xr(i,area_i)*xr(i,por_i)  !m3 (only needed for N sim)
        IF(topdp_i>0) aquifer(aqindex(xi(i,aquid_i)))%maxvol = (xr(i,topdp_i)-aquifer(aqindex(xi(i,aquid_i)))%reference)*xr(i,area_i)*xr(i,por_i)  !m3 (only needed for N sim)
!        aquifer(aqindex(xi(i,aquid_i)))%basedepth = xr(i,basdp_i)
!        aquifer(aqindex(xi(i,aquid_i)))%passivedep = xr(i,passdep_i)
        aquifer(aqindex(xi(i,aquid_i)))%porosity = xr(i,por_i)
        IF(temp_i>0) aquifer(aqindex(xi(i,aquid_i)))%temperature = xr(i,temp_i)
        IF(coIN_i>0) aquifer(aqindex(xi(i,aquid_i)))%conc_IN = xr(i,coIN_i)*1.E-3
        IF(coSP_i>0) aquifer(aqindex(xi(i,aquid_i)))%conc_SP = xr(i,coSP_i)*1.E-3
      ENDIF
    ENDDO
    
    !Check/Increase number of parameter regions
    !nregions(1) = MAX(nregions(1),MAXVAL(aquifer(1:numaqu)%parregion))
    nregions(2) = MAXVAL(aquifer(1:numaqu)%parregion(2))

    !Deallocate local arrays (for gfortran)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)

    WRITE(6,*) 'Aquifer information loaded'

  END SUBROUTINE load_aquiferdata

  !>Get the information about glaciers from GlacierData.txt
  !!
  !>\b Consequences Module modvar variables glacier and glacierindex will be set. 
  !------------------------------------------------------------------
  SUBROUTINE load_glacierdata(dir,nsb,status) 

    USE WORLDVAR, ONLY : i_str, &
                         i_intg, &
                         i_real, &
                         bdate, &
                         fileunit_temp, &
                         maxcharpath, &
                         meandaysofyear
    USE MODVAR, ONLY : basin, &
                       glacier, &       !OUT
                       nglaciers, &     !OUT
                       glacierindex, &  !OUT
                       missing_value
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_load_glacierdata, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir  !<File directory
    INTEGER, INTENT(IN)  :: nsb           !<Number of subbasins (basemodel)
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local parameters
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading

    !Local variables
    LOGICAL fex
    INTEGER i,j
    INTEGER ncols                   !Total number of columns
    INTEGER mcols                   !Help variable
    INTEGER nrows                   !Number of rows in AquiferData.txt
    INTEGER subid_i         !data column index for these columns
    INTEGER gyear,gmon,gday
    CHARACTER(LEN=maxcharpath) filename
    INTEGER,ALLOCATABLE :: glaciersubid(:)  !subid from GlacierData
    INTEGER,ALLOCATABLE :: glacdate(:)  !date of glacier slc information from GlacierData
    INTEGER,ALLOCATABLE :: code(:)        !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)      !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)      !Index for column integer variables
    INTEGER,ALLOCATABLE :: xi(:,:)           !Integer data read from file
    REAL,ALLOCATABLE    :: xr(:,:)           !Real data read from file
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)  !Content string
    CHARACTER(LEN=250) :: propagate_str

    status = 0
    filename = TRIM(dir)//'GlacierData.txt'

    !Check if file exist
    INQUIRE(FILE=filename,EXIST=fex)
    IF(.NOT.fex) RETURN   
    WRITE(6,*) 'File opened: ', TRIM(filename)

    !Count number of columns in GlacierData.txt
    CALL count_data_cols(fileunit_temp,TRIM(filename),0,ncols,status)
    IF(status/=0)RETURN

    !Count number of rows in GlacierData.txt
    CALL count_data_rows(fileunit_temp,TRIM(filename),1,nrows,status)
    IF(status/=0)RETURN

    !Initiate
    IF(.NOT.ALLOCATED(glaciersubid)) ALLOCATE(glaciersubid(nrows))
    IF(.NOT.ALLOCATED(glacdate)) ALLOCATE(glacdate(nrows))
    IF(.NOT.ALLOCATED(glacier))      ALLOCATE(glacier(nrows))
    IF(.NOT.ALLOCATED(glacierindex)) ALLOCATE(glacierindex(nsb))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(ncols))
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,ncols))
    glacier(1:nrows)%gtype = INT(missing_value)
    glacier(1:nrows)%volcorr = 0.
    glacier(1:nrows)%yeardiff = 0.
    glacdate = INT(missing_value)  !date of SLC area (used to rescale initial volume)
    glacier(1:nrows)%glacinimb = 0.                 !annual mass balance for initial glacier volume correction
    glacier(1:nrows)%inivol = missing_value
    glacierindex = 0

    !Read GlacierData-file
    OPEN(UNIT = fileunit_temp,FILE = TRIM(filename), STATUS = 'old', ACTION='read')     

    !Read the column headings from file
    CALL read_column_headings(fileunit_temp,ncols,letters,str,mcols,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading heading in file: ',TRIM(filename)
      propagate_str = 'reading heading in file: '//TRIM(filename)
      CALL propagate_external_msg(e_load_glacierdata,e_error,TRIM(propagate_str))
      RETURN
    ENDIF

    propagate_str = 'columns ignored: -'

    !Code variables for easy finding of variable type for columns
    code=i_str    !string, ignore
    subid_i=0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ')THEN
        code(i) = i_intg
        subid_i = i
      ENDIF
      IF(str(i)(1:letters)=='glactype  ') code(i) = i_intg
      IF(str(i)(1:letters)=='logvolcor ') code(i) = i_real
      IF(str(i)(1:letters)=='slcdate   ') code(i) = i_intg
      IF(str(i)(1:letters)=='annualmb  ') code(i) = i_real
      IF(str(i)(1:letters)=='slcvolume ') code(i) = i_real
      IF(code(i) == i_str) THEN
        propagate_str=TRIM(propagate_str)//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_load_glacierdata,e_info,TRIM(propagate_str),REAL(code))

    !Check data columns
    IF(subid_i==0)THEN
      WRITE(6,*) 'SUBID columns are missing in GlacierData.txt'
      propagate_str = 'SUBID columns are missing in GlacierData.txt'
      CALL propagate_external_msg(e_load_glacierdata,e_error,TRIM(propagate_str))
      status=1
      RETURN
    ENDIF

    !Read data
    CALL read_basindata5(fileunit_temp,filename,ncols,nrows,ncols,code,rindex,iindex,xi,xr)  !Read all data
    CLOSE(UNIT=fileunit_temp)

    !Set glacier characheristics array
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ') glaciersubid(1:nrows) = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='glactype  ') glacier(1:nrows)%gtype = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='logvolcor ') glacier(1:nrows)%volcorr = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='slcdate   ') glacdate(1:nrows) = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='annualmb  ') glacier(1:nrows)%glacinimb = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='slcvolume ') glacier(1:nrows)%inivol = xr(1:nrows,rindex(i))
    ENDDO

    !Calculate year difference between date of slc area information and simulation begin date
    DO j=1,nrows
      IF(glacdate(j)>0)THEN !years between SLC date and simulation start date
        gyear = INT(DBLE(glacdate(j)*1.d-4 + 0.1d0))
        gmon = INT((DBLE(glacdate(j)-DBLE(gyear)*1.d4)*1.d-2 + 0.1d0))
        gday = INT(DBLE(glacdate(j)-DBLE(gyear)*1.d4-DBLE(gmon)*1.d2 + 0.1d0))
        glacier(j)%yeardiff = TimeLag(bdate,DateType(gyear,gmon,gday,0,0))/meandaysofyear
      ENDIF
    ENDDO

    !Calculate glacier finding index array
    DO j = 1,nrows
      DO i = 1,nsb
        IF(basin(i)%subid==glaciersubid(j))THEN
          glacierindex(i) = j
          EXIT
        ENDIF
      ENDDO
    ENDDO
    
    nglaciers = nrows
        
    !Deallocate local arrays (for gfortran)
    IF(ALLOCATED(glaciersubid)) DEALLOCATE(glaciersubid)
    IF(ALLOCATED(glacdate)) DEALLOCATE(glacdate)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)

    WRITE(6,*) 'Glacier information loaded'

  END SUBROUTINE load_glacierdata

  !>Collects the information about special lakes from LakeData.txt
  !>
  !>\b Consequences Module modvar variables elake, lake, lakebasin, 
  !>lakeindex, lakebasinindex and lakeout2index may be allocated and set. 
  !>Module modvar variables basin may be changed.
  !------------------------------------------------------------------
  SUBROUTINE load_lakedata(funit,dir,n,lakedataid,status) 

    USE WORLDVAR, ONLY : maxcharpath,  &
                         i_str,        &
                         i_intg,       &
                         i_real
    USE MODVAR,   ONLY : basin,        &    !OUT
                         classbasin,   &
                         slc_olake,    &
                         missing_value,  &
                         elake,        &    !OUT
                         lake,         &    !OUT
                         lakebasin,    &    !OUT
                         lakeindex,    &    !OUT
                         lakeout2index,  &  !OUT
                         lakebasinindex, &  !OUT
                         max_par,      &    
                         modparid,     &    
                         m_ldpar
    USE CONVERT,  ONLY : integer_convert_to_logical,&
                         int_to_str,real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_lakedata,e_lakedata_geoid, &
                                    e_lakedata_ldtype,e_lakedata_not,e_lakedata_elakeoutlet, &
                                    e_error,e_warning,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit         !<File unit, temporary file open
    CHARACTER (LEN=*), INTENT(IN) :: dir  !<File directory
    INTEGER, INTENT(IN)  :: n             !<Number of subbasins (base model)
    INTEGER, INTENT(IN)  :: lakedataid(n) !<lakedataid from GeoData
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local parameters
    INTEGER,PARAMETER :: i_simple = 1            !Lakedata type; simple olake
!    INTEGER,PARAMETER :: i_combil = 2            !Lakedata type; lake composed of several lake basins
!    INTEGER,PARAMETER :: i_lbasin = 3            !Lakedata type; lake basin
!    INTEGER,PARAMETER :: i_llbasin = 4           !Lakedata type; last lake basin
    INTEGER,PARAMETER :: i_ldout1 = 5            !Lakedata type; outlet 1
    INTEGER,PARAMETER :: i_ldout2 = 6            !Lakedata type; outlet 2
    INTEGER,PARAMETER :: i_ebasin = 7            !Lakedata type; lake basin of ldtype 7, equal water level
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    LOGICAL fex
    INTEGER maxcol
    INTEGER i,j,k,ilake,ilakee
    INTEGER ictype                    !Column with lakedata type ('ldtype')
    INTEGER iclakeid                  !Column with lakeid
    INTEGER iclakedataid              !Column with id (lakedataid in GeoData)
    INTEGER ica                       !Column with area
    INTEGER icdepth                   !Column with depth
    INTEGER icrate,icqprod
    INTEGER nrows                     !Number of lakes in file
    INTEGER ncols                     !Number of columns in file
    INTEGER nlake
    INTEGER nebasin,nlakeeb           !Number of lakebasins in LakeData and number of lakes with lakebasins of the equal water level type
    INTEGER MonthDayDate              !Non-formated date from lakeData.txt
    INTEGER dayNumber                 !Corresponding date in day number format
    INTEGER isblast,ieblast,iebasin,ioutlet
    REAL    lakearea                  !lakearea calculated from GeoData
    CHARACTER(LEN=maxcharpath) filename
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)   !Content string
    INTEGER,ALLOCATABLE :: lakeindex2(:)      !row->isub
    INTEGER,ALLOCATABLE :: lakeindex3(:)      !row->ilake
    INTEGER,ALLOCATABLE :: lakeindex6(:)      !row->iebasin
    INTEGER,ALLOCATABLE :: lakeindex7(:)      !iebasin->row
    INTEGER,ALLOCATABLE :: qprod1date(:),qprod1dateelake(:,:)
    INTEGER,ALLOCATABLE :: qprod2date(:),qprod2dateelake(:,:)
    INTEGER,ALLOCATABLE :: lakeidofnlakeeb(:) !lakeid of ilake of ebasins, ilakee->lakeid
    INTEGER,ALLOCATABLE :: lakeebofebasin(:) !ilake of ebasins, iebasin->ilakee
    INTEGER,ALLOCATABLE :: noutlet(:) !number of outlets
    INTEGER,ALLOCATABLE :: outletsofelake(:,:)  !ielake,ioutlet->j=row
    INTEGER,ALLOCATABLE :: code(:)            !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)          !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)          !Index for column integer variables
    INTEGER,ALLOCATABLE :: xi(:,:)            !Integer data read from file
    REAL,ALLOCATABLE :: xr(:,:)               !Real data read from file
    REAL,ALLOCATABLE :: regvolume(:)          !Regulation volume (M m3)
    REAL,ALLOCATABLE :: wamp(:)               !Regulation amplitude (real) (m)
    REAL,ALLOCATABLE :: regvolumeelake(:)          !Regulation volume (M m3)
    REAL,ALLOCATABLE :: wampelake(:)               !Regulation amplitude (real) (m)
    LOGICAL,ALLOCATABLE :: outlet(:)  !Flag for ebasins with outlet
    CHARACTER(LEN=150) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    status = 0
    filename = TRIM(dir)//'LakeData.txt'

    !Check if file exist and if lake-class exist
    INQUIRE(FILE=filename,EXIST=fex)
    IF(.NOT.fex)THEN
      IF(ANY(lakedataid(:)>0))THEN
        propagate_str = 'No LakeData.txt found, but lakedataid used in GeoData.txt'
        CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      ELSE
        propagate_str = 'LakeData.txt not found, but not required'
        CALL propagate_external_msg(e_lakedata_not,e_info,propagate_str)
      ENDIF
      RETURN
    ENDIF
    IF(slc_olake==0)THEN
      WRITE(6,*) 'No class for olake to be used with LakeData.txt'
      propagate_str = 'No class for olake to be used with LakeData.txt'
      CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(filename)

    !Count number of columns and rows, and allocate variables for reading
    CALL count_data_cols(funit,filename,0,maxcol,status)
    IF(status/=0)THEN
      WRITE(6,*) 'ERROR: counting columns in LakeData'
      propagate_str = 'counting columns in LakeData'
      CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      RETURN
    ENDIF
    CALL count_data_rows(funit,filename,1,nrows,status)
    IF(status/=0)RETURN
    IF(nrows==0)THEN
      WRITE(6,*) 'WARNING: No lakes in LakeData.txt ',TRIM(filename)
      propagate_str = 'No lakes in LakeData.txt '//TRIM(filename)
      CALL propagate_external_msg(e_lakedata_not,e_warning,propagate_str)
      RETURN
    ENDIF
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(maxcol))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(maxcol))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(maxcol))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(maxcol))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,maxcol))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,maxcol))

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*maxcol)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !Open LakeData-file and read headings
    OPEN(UNIT = funit,FILE = filename, STATUS = 'old', ACTION='read')
    CALL read_column_headings(funit,maxcol,letters,str,ncols,status)
    IF(status.NE.0)THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(filename)
      propagate_str = 'reading file: '//TRIM(filename)
      CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      RETURN
    ENDIF

    !Find variable columns and set variable type
    code = i_str      !string, ignore
    ica = 0
    ictype = 0
    iclakedataid = 0
    iclakeid = 0
    icdepth = 0
    icrate = 0
    icqprod = 0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='area      ')THEN
        code(i) = i_real
        ica = i
      ENDIF
      IF(str(i)(1:letters)=='ldtype    ')THEN
        code(i) = i_intg   !type of info: lake, basin or whole lake (that is divided in basins), outlet 1 or 2
        ictype = i
      ENDIF
      IF(str(i)(1:letters)=='lakedataid')THEN
        code(i) = i_intg   !lakedataid in GeoData
        iclakedataid = i
      ENDIF
      IF(str(i)(1:letters)=='lake_depth')THEN
        code(i) = i_real   !lake_depth
        icdepth = i
      ENDIF
      IF(str(i)(1:letters)=='w0ref     ') code(i) = i_real   !w0
      IF(str(i)(1:letters)=='deltaw0   ') code(i) = i_real   !deltaw0
      IF(str(i)(1:letters)=='rate      ')THEN
        code(i) = i_real   !lake_rate
        icrate = i
      ENDIF
      IF(str(i)(1:letters)=='exp       ') code(i) = i_real   !lake_exp
      IF(str(i)(1:letters)=='regvol    ') code(i) = i_real   !regvol
      IF(str(i)(1:letters)=='wamp      ') code(i) = i_real   !reg. amplitude
      IF(str(i)(1:letters)=='qprod1    ')THEN
        code(i) = i_real   !two date-based production rates, qprod1 and qprod2, now replacing the unique rate qprod
        icqprod = i
      ENDIF
      IF(str(i)(1:letters)=='qprod2    ') code(i) = i_real
      IF(str(i)(1:letters)=='maxqprod  ') code(i) = i_real   !max qprod
      IF(str(i)(1:letters)=='minflow   ') code(i) = i_intg   !flag for minimum flow
      IF(str(i)(1:letters)=='obsflow   ') code(i) = i_intg   !flag for wanted bifurcation flow
      IF(str(i)(1:letters)=='datum1    ') code(i) = i_intg   !two dates for the corresponding two dam production flows (date of start)               
      IF(str(i)(1:letters)=='datum2    ') code(i) = i_intg
      IF(str(i)(1:letters)=='qamp      ') code(i) = i_real   !lake_qamp
      IF(str(i)(1:letters)=='qpha      ') code(i) = i_real   !lake_qpha
      IF(str(i)(1:letters)=='limqprod  ') code(i) = i_real   !limprod
      IF(str(i)(1:letters)=='lakeid    ')THEN
        code(i) = i_intg
        iclakeid = i
      ENDIF
      !All model parameters that can be given in LakeData is real
      DO j=1,max_par
        IF(str(i)(1:letters)==modparid(j)%shortname .AND. modparid(j)%deptype==m_ldpar) code(i) = i_real
      ENDDO
      IF(code(i) == i_str) THEN
        propagate_str_extra=propagate_str_extra//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO
    CALL propagate_external_msg(e_lakedata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Check for necessary input
    IF(ictype==0)THEN
      WRITE(6,*) 'ERROR: ldtype not found in LakeData. ',TRIM(filename)
      propagate_str = 'ldtype not found in LakeData '//TRIM(filename)
      CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      status = 1
    ENDIF
    IF(iclakedataid==0)THEN
      WRITE(6,*) 'ERROR: lakedataid not found in LakeData. ',TRIM(filename)
      propagate_str = 'lakedataid not found in LakeData '//TRIM(filename)
      CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      status = 1
    ENDIF
    IF(ica==0)THEN             !Check for area, necessary for now
      WRITE(6,*) 'ERROR: area not found in LakeData. ',TRIM(filename)
      propagate_str = 'area not found in LakeData '//TRIM(filename)
      CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
      status = 1
    ENDIF
    IF(status==1) RETURN
    
    !Read all data
    CALL read_basindata5(funit,filename,maxcol,nrows,ncols,code,rindex,iindex,xi,xr)
    CLOSE(UNIT=funit)

    !Count number of laketypes: lakes, lakes formed by basins and lakebasins.
    nlake = 0
    nebasin = 0    !lakebasins of ldtype=7, i.e. equal water level
    nlakeeb = 0    !lakes of ldtype=7, i.e. equal water level
    IF(.NOT.ALLOCATED(lakeidofnlakeeb)) ALLOCATE(lakeidofnlakeeb(nrows))
    IF(.NOT.ALLOCATED(lakeindex6)) ALLOCATE(lakeindex6(nrows))
    IF(.NOT.ALLOCATED(lakeindex7)) ALLOCATE(lakeindex7(nrows))
    IF(.NOT.ALLOCATED(lakeebofebasin)) ALLOCATE(lakeebofebasin(nrows))
    lakeidofnlakeeb = 0
    lakeindex6 = 0; lakeindex7 = 0
    lakeebofebasin = 0
    DO j = 1,nrows
      IF(xi(j,iindex(ictype))==i_simple) nlake = nlake + 1
      IF(xi(j,iindex(ictype))==i_ldout1 .OR. xi(j,iindex(ictype))==i_ldout2) nlake = nlake + 1  !both are counted
      IF(xi(j,iindex(ictype))==i_ebasin)THEN
        nebasin = nebasin + 1
        lakeindex6(j) = nebasin
        lakeindex7(nebasin) = j
        DO k = 1,nlakeeb
          IF(xi(j,iindex(iclakeid))==lakeidofnlakeeb(k)) EXIT
        ENDDO
        IF(k>nlakeeb)THEN !first basin of this lake
          nlakeeb = nlakeeb + 1
          lakeidofnlakeeb(nlakeeb) = xi(j,iindex(iclakeid))
        ENDIF
        lakeebofebasin(nebasin) = k
      ENDIF
    ENDDO

    !Check needed for ebasins
    IF(nlakeeb>0)THEN
      IF(iclakeid==0)THEN             !Check for necessary data
        WRITE(6,*) 'ERROR: lakeid not found in LakeData. ',TRIM(filename)
        WRITE(6,*) 'ERROR: It is necessary for lakes formed by basins (ldtype 7).'
        propagate_str = 'lakeid not found, necessary for lakes formed by basins (ldtype 7): '//TRIM(filename)
        CALL propagate_external_msg(e_lakedata_not,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
    ENDIF
    
    !Allocate variables for lake information
    IF(.NOT.ALLOCATED(lakebasin)) ALLOCATE(lakebasin(nebasin))
    IF(.NOT.ALLOCATED(lake)) ALLOCATE(lake(nlake))
    IF(.NOT.ALLOCATED(elake)) ALLOCATE(elake(nlakeeb))
    IF(.NOT.ALLOCATED(regvolume)) ALLOCATE(regvolume(nlake))
    IF(.NOT.ALLOCATED(wamp)) ALLOCATE(wamp(nlake))
    IF(.NOT.ALLOCATED(qprod1date)) ALLOCATE(qprod1date(nlake)) 
    IF(.NOT.ALLOCATED(qprod2date)) ALLOCATE(qprod2date(nlake))
    regvolume = 0.; wamp = missing_value
    qprod1date = 0; qprod2date = 0  

    !Find subbasin-lake coupling: lakeindex(isb),lakebasinindex(isb), lakeindex2(ldrow) and lakeindex3(ldrow)
    !Actually only lakeindex2 and lakebasinindex (for ebasin) is set in this section; lakeindex2(irow)=isub
    IF(.NOT.ALLOCATED(lakeindex)) ALLOCATE(lakeindex(n))
    IF(.NOT.ALLOCATED(lakeout2index)) ALLOCATE(lakeout2index(n))
    IF(nebasin>0 .AND.(.NOT.ALLOCATED(lakebasinindex))) ALLOCATE(lakebasinindex(n))
    IF(.NOT.ALLOCATED(lakeindex2)) ALLOCATE(lakeindex2(nrows))
    IF(.NOT.ALLOCATED(lakeindex3)) ALLOCATE(lakeindex3(nrows))
    lakeindex = 0
    lakeout2index = 0
    IF(nebasin>0) lakebasinindex = 0
    lakeindex2 = 0
    lakeindex3 = 0
    DO i = 1,n    !loop over all subbasins
      IF(lakedataid(i)==0)CYCLE
      DO j = 1,nrows
        IF(lakedataid(i)==xi(j,iindex(iclakedataid)))THEN
          lakeindex2(j)=i
          IF(lakeindex6(j)>0)THEN
            lakebasinindex(i) = lakeindex6(j) !+ nbasin
          ENDIF
          !IF(xi(j,iindex(ictype)) < i_simple .OR. xi(j,iindex(ictype)) > i_ebasin .OR. xi(j,iindex(ictype))==i_combil .OR. xi(j,iindex(ictype))==i_lbasin .OR. xi(j,iindex(ictype))==i_llbasin) THEN
          IF(.NOT.(xi(j,iindex(ictype)) == i_simple .OR. (xi(j,iindex(ictype)) >= i_ldout1 .AND. xi(j,iindex(ictype)) <= i_ebasin))) THEN
            propagate_str = 'ldtype '//TRIM(int_to_str(xi(j,iindex(ictype))))//' is not valid for lakedataid '//TRIM(int_to_str(xi(j,iindex(iclakedataid))))//' and lakeid '//TRIM(int_to_str(xi(j,iindex(iclakeid))))
            CALL propagate_external_msg(e_lakedata_ldtype,e_error,TRIM(propagate_str))
          ENDIF
          IF(xi(j,iindex(ictype))==i_ldout1)THEN
            !Check for second outlet
            DO k=1,nrows
              IF(xi(j,iindex(iclakeid))==xi(k,iindex(iclakeid))) lakeindex2(k)=i
            ENDDO
          ELSEIF(xi(j,iindex(ictype))==i_ldout2)THEN
            !lakedataid should only be given for main outlet
            WRITE(6,*) 'WARNING: lakedataid is given for second outlet of lake in LakeData.txt'
            WRITE(6,*) 'Lake in subbasin ',basin(i)%subid, '.'
            WRITE(6,*) 'This may lead to lakedata-parameters are not read correctly.'
            WRITE(6,*) 'Please change LakeData.txt to have lakedataid=0 for ldtype=6.'
            propagate_str = 'lakedataid is given for second outlet of lake in subbasin '//real_to_str(REAL(basin(i)%subid))//' Please change LakeData.txt to have lakedataid=0 for ldtype=6'
            CALL propagate_external_msg(e_lakedata_geoid,e_warning,propagate_str)
            DO k=1,nrows
              IF(xi(j,iindex(iclakeid))==xi(k,iindex(iclakeid))) lakeindex2(k)=i  !for safety find first outlet
            ENDDO
          ENDIF
          EXIT
        ENDIF
      ENDDO
      IF(j==nrows+1)THEN
        WRITE(6,*) 'ERROR: A lake in GeoData is missing in LakeData. lakedataid: ',lakedataid(i)
        propagate_str='A lake in GeoData is missing in LakeData. lakedataid: '//real_to_str(REAL(lakedataid(i)))
        CALL propagate_external_msg(e_lakedata_geoid,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF  
    ENDDO

    !Lakes composed of lakebasins (equal water level)
    !------------------------------------------------
    IF(nlakeeb>0)THEN
      ALLOCATE(noutlet(nlakeeb))
      ALLOCATE(outletsofelake(nlakeeb,5))
      ALLOCATE(outlet(nebasin))
      ALLOCATE(regvolumeelake(nlakeeb))
      ALLOCATE(wampelake(nlakeeb))
      ALLOCATE(qprod1dateelake(nlakeeb,5))
      ALLOCATE(qprod2dateelake(nlakeeb,5))
      noutlet = 0
      outletsofelake = 0
      regvolumeelake = missing_value
      wampelake = missing_value
      qprod1dateelake = 0
      qprod2dateelake = 0
      outlet = .FALSE.
      
      !Count outlets, and allocate elake-structure
      DO iebasin = 1,nebasin
        IF(icrate>0 .AND. icqprod>0)THEN
          IF(xr(lakeindex7(iebasin),rindex(icrate))>0. .OR. xr(lakeindex7(iebasin),rindex(icqprod))>0. .OR. xr(lakeindex7(iebasin),rindex(icrate))==missing_value)THEN !hanterar anv av gratk
            noutlet(lakeebofebasin(iebasin)) = noutlet(lakeebofebasin(iebasin)) + 1
            outlet(iebasin) = .TRUE.
          ENDIF
        ELSEIF(icrate>0)THEN
          IF(xr(lakeindex7(iebasin),rindex(icrate))>0. .OR. xr(lakeindex7(iebasin),rindex(icrate))==missing_value)THEN !hanterar anv av gratk
            noutlet(lakeebofebasin(iebasin)) = noutlet(lakeebofebasin(iebasin)) + 1
            outlet(iebasin) = .TRUE.
          ENDIF
        ELSEIF(icqprod>0)THEN
          IF(xr(lakeindex7(iebasin),rindex(icqprod))>0.)THEN 
            noutlet(lakeebofebasin(iebasin)) = noutlet(lakeebofebasin(iebasin)) + 1
            outlet(iebasin) = .TRUE.
          ENDIF
        ENDIF
      ENDDO
      DO ilakee =  1,nlakeeb
        elake(ilakee)%noutlet = noutlet(ilakee)
        ALLOCATE(elake(ilakee)%outlet(noutlet(ilakee)))
        IF(noutlet(ilakee)==0)THEN
          WRITE(6,*) 'ERROR: A multi-basin lake has no outlet defined (rate or qprod). lakeid: ',lakeidofnlakeeb(ilakee)
          WRITE(propagate_str,*) 'A multi-basin lake has no outlet defined (rate or qprod). lakeid: ',lakeidofnlakeeb(ilakee)
          CALL propagate_external_msg(e_lakedata_elakeoutlet,e_error,propagate_str)
          status = 1
          RETURN
        ENDIF
      ENDDO
      
      !Find main outlet (last lakebasin)
      ioutlet = 1
      DO ilakee =  1,nlakeeb
        isblast = 0
        ieblast = 0
        DO iebasin = 1,nebasin
          IF(lakeebofebasin(iebasin) == ilakee)THEN
            IF(isblast<lakeindex2(lakeindex7(iebasin)))THEN
              isblast = lakeindex2(lakeindex7(iebasin))
              ieblast = iebasin
            ENDIF
          ENDIF
        ENDDO
        lakebasin(ieblast)%last = .TRUE.
        lakebasin(ieblast)%ilk = ilakee     !only the last is set here
        outletsofelake(ilakee,ioutlet) = lakeindex7(ieblast)  !=j=row
        elake(ilakee)%outlet(ioutlet)%isb = lakeindex2(lakeindex7(ieblast))
      ENDDO
      
      !Find other outlets
      DO ilakee = 1,nlakeeb
        IF(elake(ilakee)%noutlet>1)THEN
          noutlet(ilakee) = 1
          DO iebasin = 1, nebasin
            IF(outlet(iebasin))THEN
              IF(lakebasin(iebasin)%last) CYCLE
              IF(lakeebofebasin(iebasin)==ilakee)THEN   !same lake
                noutlet(ilakee) = noutlet(ilakee) + 1
                lakebasin(iebasin)%ilk = ilakee 
                outletsofelake(ilakee,noutlet(ilakee)) = lakeindex7(iebasin)  !=j=row
                elake(ilakee)%outlet(noutlet(ilakee))%isb = lakeindex2(lakeindex7(iebasin))
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      
      DO ilakee =  1,nlakeeb                      !Save information permanent or temporary
        DO ioutlet = 1,elake(ilakee)%noutlet
          DO i = 1,ncols
            IF(ioutlet==1)THEN
              IF(str(i)=='w0ref     ')   elake(ilakee)%w0ref    = xr(outletsofelake(ilakee,ioutlet),rindex(i))
              IF(str(i)=='wamp      ')   wampelake(ilakee)           = xr(outletsofelake(ilakee,ioutlet),rindex(i))
              IF(str(i)=='regvol    ')   regvolumeelake(ilakee)      = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            ENDIF
            IF(str(i)=='deltaw0   ')   elake(ilakee)%outlet(ioutlet)%deltaw0   = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='qprod1    ')   elake(ilakee)%outlet(ioutlet)%qprod(1)  = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='qprod2    ')   elake(ilakee)%outlet(ioutlet)%qprod(2)  = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='maxqprod  ')   elake(ilakee)%outlet(ioutlet)%mqprod    = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='datum1    ')   qprod1dateelake(ilakee,ioutlet)     = xi(outletsofelake(ilakee,ioutlet),iindex(i))
            IF(str(i)=='datum2    ')   qprod2dateelake(ilakee,ioutlet)     = xi(outletsofelake(ilakee,ioutlet),iindex(i))
            IF(str(i)=='rate      ')   elake(ilakee)%outlet(ioutlet)%rate      = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='exp       ')   elake(ilakee)%outlet(ioutlet)%exp       = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='qamp      ')   elake(ilakee)%outlet(ioutlet)%qamp      = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='qpha      ')   elake(ilakee)%outlet(ioutlet)%qpha      = xr(outletsofelake(ilakee,ioutlet),rindex(i))
            IF(str(i)=='limqprod  ')   elake(ilakee)%outlet(ioutlet)%limprod   = xr(outletsofelake(ilakee,ioutlet),rindex(i))
          ENDDO
        ENDDO
      ENDDO

      !Continue with their lakebasins, set lake depth
      DO iebasin = 1,nebasin
        j=lakeindex7(iebasin)
        IF(icdepth>0)THEN
          IF(xr(j,rindex(icdepth)).NE.missing_value) basin(lakeindex2(j))%lakedepth(2) = xr(j,rindex(icdepth))
        ENDIF
        lakebasin(iebasin)%ilk = lakeebofebasin(iebasin) !+ nlakefb  !set above only for outlets
      ENDDO
      
      !Calculate and set whole lake area, wmin and wamp-coefficient
      elake(:)%wmin = missing_value
      elake(:)%wampcoeff = missing_value
      DO ilakee = 1,nlakeeb
        lakearea = 0
        DO iebasin = 1,nebasin
          IF(ilakee == lakeebofebasin(iebasin))THEN
            lakearea = lakearea + basin(lakeindex2(lakeindex7(iebasin)))%area * classbasin(lakeindex2(lakeindex7(iebasin)),slc_olake)%part
          ENDIF
        ENDDO
        elake(ilakee)%area = lakearea
        IF(lakearea>0. .AND. regvolumeelake(ilakee)>0.)THEN
          elake(ilakee)%wmin = 0. - regvolumeelake(ilakee) * 1000000. / lakearea
          IF(wampelake(ilakee)/=missing_value) &
            elake(ilakee)%wampcoeff = wampelake(ilakee)/(-1.*elake(ilakee)%wmin)
        ENDIF
        DO ioutlet = 1,elake(ilakee)%noutlet
          MonthDayDate = qprod1dateelake(ilakee,ioutlet)
          CALL get_dayno_from_monthday(MonthDayDate, dayNumber)
          elake(ilakee)%outlet(ioutlet)%qdate(1) = dayNumber
          MonthDayDate = qprod2dateelake(ilakee,ioutlet)
          CALL get_dayno_from_monthday(MonthDayDate, dayNumber)
          elake(ilakee)%outlet(ioutlet)%qdate(2) = dayNumber
        ENDDO
      ENDDO
     
    ENDIF

    !Simple lakes, and lakes with two outlets defined
    !------------------------------------------------
    ilake = 0
    DO j = 1,nrows
      IF(lakeindex2(j)==0) CYCLE    !skip rows with lakes formed by basins and lakes not included in simulation
      IF(xi(j,iindex(ictype))==i_simple)THEN     !single lake
        ilake = ilake + 1
        lake(ilake)%area = xr(j,rindex(ica))    !is this used? remove?
        IF(icdepth>0)THEN
          IF(xr(j,rindex(icdepth)).NE.missing_value) basin(lakeindex2(j))%lakedepth(2) = xr(j,rindex(icdepth))
        ENDIF
        DO i = 1,ncols
          IF(str(i)=='w0ref     ')   lake(ilake)%w0ref    = xr(j,rindex(i))
          IF(str(i)=='deltaw0   ')   lake(ilake)%deltaw0  = xr(j,rindex(i))
          IF(str(i)=='qprod1    ')   lake(ilake)%qprod1   = xr(j,rindex(i))
          IF(str(i)=='qprod2    ')   lake(ilake)%qprod2   = xr(j,rindex(i))
          IF(str(i)=='maxqprod  ')   lake(ilake)%mqprod   = xr(j,rindex(i))
          IF(str(i)=='minflow   ')   lake(ilake)%minflow  = integer_convert_to_logical(-xi(j,iindex(i)))
          IF(str(i)=='datum1    ')   qprod1date(ilake)    = xi(j,iindex(i))
          IF(str(i)=='datum2    ')   qprod2date(ilake)    = xi(j,iindex(i))
          IF(str(i)=='rate      ')   lake(ilake)%rate     = xr(j,rindex(i))
          IF(str(i)=='exp       ')   lake(ilake)%exp      = xr(j,rindex(i))
          IF(str(i)=='qamp      ')   lake(ilake)%qamp     = xr(j,rindex(i))
          IF(str(i)=='qpha      ')   lake(ilake)%qpha     = xr(j,rindex(i))
          IF(str(i)=='limqprod  ')   lake(ilake)%limprod  = xr(j,rindex(i))
          IF(str(i)=='regvol    ')   regvolume(ilake)     = xr(j,rindex(i))
          IF(str(i)=='wamp      ')   wamp(ilake)          = xr(j,rindex(i))
        ENDDO
        lakeindex(lakeindex2(j))=ilake
        lakeindex3(j)=ilake
      ELSEIF(xi(j,iindex(ictype))==i_ldout1)THEN     !lake with 2 outlet, outlet 1
        ilake = ilake + 1
        IF(icdepth>0)THEN
          IF(xr(j,rindex(icdepth)).NE.missing_value) basin(lakeindex2(j))%lakedepth(2) = xr(j,rindex(icdepth))
        ENDIF
        DO i = 1,ncols
          IF(str(i)=='w0ref     ')   lake(ilake)%w0ref    = xr(j,rindex(i))
          IF(str(i)=='deltaw0   ')   lake(ilake)%deltaw0  = xr(j,rindex(i))
          IF(str(i)=='qprod1    ')   lake(ilake)%qprod1   = xr(j,rindex(i))
          IF(str(i)=='qprod2    ')   lake(ilake)%qprod2   = xr(j,rindex(i))
          IF(str(i)=='maxqprod  ')   lake(ilake)%mqprod   = xr(j,rindex(i))
          IF(str(i)=='minflow   ')   lake(ilake)%minflow  = integer_convert_to_logical(-xi(j,iindex(i)))
          IF(str(i)=='datum1    ')   qprod1date(ilake)    = xi(j,iindex(i))
          IF(str(i)=='datum2    ')   qprod2date(ilake)    = xi(j,iindex(i))
          IF(str(i)=='rate      ')   lake(ilake)%rate     = xr(j,rindex(i))
          IF(str(i)=='exp       ')   lake(ilake)%exp      = xr(j,rindex(i))
          IF(str(i)=='qamp      ')   lake(ilake)%qamp     = xr(j,rindex(i))
          IF(str(i)=='qpha      ')   lake(ilake)%qpha     = xr(j,rindex(i))
          IF(str(i)=='limqprod  ')   lake(ilake)%limprod  = xr(j,rindex(i))
          IF(str(i)=='regvol    ')   regvolume(ilake)     = xr(j,rindex(i))
          IF(str(i)=='wamp      ')   wamp(ilake)          = xr(j,rindex(i))
        ENDDO
        lakeindex(lakeindex2(j))=ilake
        lakeindex3(j)=ilake
      ELSEIF(xi(j,iindex(ictype))==i_ldout2)THEN     !lake with 2 outlet, outlet 2
        ilake = ilake + 1
        DO i = 1,ncols
          !no w0ref or lake_depth
          IF(str(i)=='deltaw0   ')   lake(ilake)%deltaw0  = xr(j,rindex(i))
          IF(str(i)=='qprod1    ')   lake(ilake)%qprod1   = xr(j,rindex(i))
          IF(str(i)=='qprod2    ')   lake(ilake)%qprod2   = xr(j,rindex(i))
          IF(str(i)=='maxqprod  ')   lake(ilake)%mqprod   = xr(j,rindex(i))
          IF(str(i)=='minflow   ')   lake(ilake)%minflow  = integer_convert_to_logical(-xi(j,iindex(i)))
          IF(str(i)=='obsflow   ')   lake(ilake)%obsflow  = integer_convert_to_logical(-xi(j,iindex(i)))
          IF(str(i)=='datum1    ')   qprod1date(ilake)    = xi(j,iindex(i))
          IF(str(i)=='datum2    ')   qprod2date(ilake)    = xi(j,iindex(i))
          IF(str(i)=='rate      ')   lake(ilake)%rate     = xr(j,rindex(i))
          IF(str(i)=='exp       ')   lake(ilake)%exp      = xr(j,rindex(i))
          IF(str(i)=='qamp      ')   lake(ilake)%qamp     = xr(j,rindex(i))
          IF(str(i)=='qpha      ')   lake(ilake)%qpha     = xr(j,rindex(i))
          IF(str(i)=='regvol    ')   regvolume(ilake)     = xr(j,rindex(i))
          IF(str(i)=='wamp      ')   wamp(ilake)          = xr(j,rindex(i))
          IF(str(i)=='w0ref     ')   lake(ilake)%w0ref    = xr(j,rindex(i))  !w0 relativt w0ref i out1
          IF(str(i)=='limqprod  ')   lake(ilake)%limprod  = xr(j,rindex(i))
        ENDDO
        lakeout2index(lakeindex2(j))=ilake
        lakeindex3(j)=ilake
      ENDIF
    ENDDO
    IF(ilake<nlake)THEN
      WRITE(6,*) 'Warning: Some lakes in LakeData is not coupled to subbasins in GeoData.'
      WRITE(6,*) 'Warning: LakeData has',nlake,' lake(s), while GeoData has',ilake,' lake(s).'
    ENDIF

    !Calculate other lake variables; wmin, datum, wampcoeff
    lake(:)%wmin = missing_value
    lake(:)%wampcoeff = missing_value
    DO j = 1,nrows
      IF(lakeindex3(j)>0)THEN   !this is zero for new lakebasins
        IF(regvolume(lakeindex3(j))>0.)THEN
          IF(xi(j,iindex(ictype))==i_simple)THEN
            lakearea = basin(lakeindex2(j))%area * classbasin(lakeindex2(j),slc_olake)%part
          ELSEIF(xi(j,iindex(ictype))==i_ldout1)THEN
            lakearea = basin(lakeindex2(j))%area * classbasin(lakeindex2(j),slc_olake)%part
          ELSEIF(xi(j,iindex(ictype))==i_ldout2)THEN
            lakearea = basin(lakeindex2(j))%area * classbasin(lakeindex2(j),slc_olake)%part
          ENDIF
          IF(lakearea>0)THEN
            lake(lakeindex3(j))%wmin = 0. - regvolume(lakeindex3(j)) * 1000000. / lakearea
            IF(wamp(lakeindex3(j))/=missing_value) &
              lake(lakeindex3(j))%wampcoeff = wamp(lakeindex3(j))/(-1.*lake(lakeindex3(j))%wmin)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    DO ilake = 1,nlake 
      MonthDayDate = qprod1date(ilake)
      CALL get_dayno_from_monthday(MonthDayDate, dayNumber)
      lake(ilake)%datum1 = dayNumber
      MonthDayDate = qprod2date(ilake)
      CALL get_dayno_from_monthday(MonthDayDate, dayNumber)
      lake(ilake)%datum2 = dayNumber
    ENDDO

    !Build table with lakedata parameter values
    CALL start_lakedata_table(maxcol,ncols,nrows,str,xi,xr,iindex,rindex,n,lakedataid)

    !Deallocate local variables
    IF(ALLOCATED(lakeindex2)) DEALLOCATE(lakeindex2)
    IF(ALLOCATED(lakeindex3)) DEALLOCATE(lakeindex3)
    IF(ALLOCATED(lakeindex6)) DEALLOCATE(lakeindex6)
    IF(ALLOCATED(lakeindex7)) DEALLOCATE(lakeindex7)
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    IF(ALLOCATED(regvolume)) DEALLOCATE(regvolume)
    IF(ALLOCATED(wamp)) DEALLOCATE(wamp)
    IF(ALLOCATED(qprod1date)) DEALLOCATE(qprod1date) 
    IF(ALLOCATED(qprod2date)) DEALLOCATE(qprod2date)
    IF(ALLOCATED(lakeidofnlakeeb)) DEALLOCATE(lakeidofnlakeeb)
    IF(ALLOCATED(lakeebofebasin)) DEALLOCATE(lakeebofebasin)
    IF(ALLOCATED(noutlet)) DEALLOCATE(noutlet)
    IF(ALLOCATED(outlet)) DEALLOCATE(outlet)
    IF(ALLOCATED(outletsofelake)) DEALLOCATE(outletsofelake)
    IF(ALLOCATED(regvolumeelake)) DEALLOCATE(regvolumeelake)
    IF(ALLOCATED(wampelake)) DEALLOCATE(wampelake)
    IF(ALLOCATED(qprod1dateelake)) DEALLOCATE(qprod1dateelake)
    IF(ALLOCATED(qprod2dateelake)) DEALLOCATE(qprod2dateelake)

    WRITE(6,*) 'Lake/dam information loaded from LakeData.txt'

  END SUBROUTINE load_lakedata

  !>Collects the information about dams from DamData.txt
  !>
  !>\b Consequences Module modvar variables dam, damindex may be allocated and set. 
  !>Module modvar variable basin may be changed (lake_depth).
  !------------------------------------------------------------------
  SUBROUTINE load_damdata(funit,dir,n,status)

    USE WORLDVAR, ONLY : maxcharpath,  &
                         i_str,        &
                         i_intg,       &
                         i_real
    USE MODVAR,   ONLY : basin,        &    !OUT
                         classbasin,   &
                         slc_olake,    &
                         missing_value,  &
                         dam,         &    !OUT
                         damindex        !INOUT
    USE CONVERT, ONLY : real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_damdata,e_error,e_info,e_warning

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit         !<File unit, temporary file open
    CHARACTER (LEN=*), INTENT(IN) :: dir  !<File directory
    INTEGER, INTENT(IN)  :: n             !<Number of subbasins (base model)
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local parameters
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading

    !Local variables
    LOGICAL fex
    INTEGER i,idam
    INTEGER idtype                    !Column with subid
    INTEGER nrows                     !Number of dams in file
    INTEGER ncols                     !Number of columns in file
    INTEGER maxcol
    INTEGER MonthDayDate              !Non-formated date from DamData.txt
    INTEGER dayNumber                 !Corresponding date in day number format
    CHARACTER(LEN=maxcharpath) filename
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)   !Content string
    INTEGER,ALLOCATABLE :: code(:)            !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)          !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)          !Index for column integer variables
    INTEGER,ALLOCATABLE :: xi(:,:)            !Integer data read from file
    REAL,ALLOCATABLE :: xr(:,:)               !Real data read from file
    REAL damarea                              ! area of dam reservoir (same as lakearea)
    INTEGER,ALLOCATABLE :: qprod1date(:)
    INTEGER,ALLOCATABLE :: qprod2date(:)
    INTEGER, ALLOCATABLE :: damindex2(:)      ! row where
    REAL,ALLOCATABLE :: wamp(:)               !Regulation amplitude (real) (m)
    REAL,ALLOCATABLE :: qinflow(:,:)          !Monthly natural flow (m3/s)
    CHARACTER(LEN=150) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    status = 0
    ALLOCATE(damindex(n))
    damindex = 0
    filename = TRIM(dir)//'DamData.txt'

    !Check if file exist and if outlet lake-class exist
    INQUIRE(FILE=filename,EXIST=fex)
    IF(.NOT.fex) RETURN
    IF(slc_olake==0)THEN
      WRITE(6,*) 'No class for olake to be used with DamData.txt'
      propagate_str = 'No class for olake to be used with DamData.txt'
      CALL propagate_external_msg(e_damdata,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(filename)

    !Count number of columns and rows, and allocate variables for reading
    CALL count_data_cols(funit,filename,0,maxcol,status)
    IF(status/=0)THEN
      WRITE(6,*) 'ERROR: counting columns in DamData'
      propagate_str = 'counting columns in DamData'
      CALL propagate_external_msg(e_damdata,e_error,propagate_str)
      RETURN
    ENDIF
    CALL count_data_rows(funit,filename,1,nrows,status)
    IF(status/=0)RETURN
    IF(nrows==0)THEN
      WRITE(6,*) 'WARNING: No dam in DamData.txt ',TRIM(filename)
      propagate_str = 'No dam in DamData.txt '//TRIM(filename)
      CALL propagate_external_msg(e_damdata,e_warning,propagate_str)
      RETURN
    ENDIF
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(maxcol))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(maxcol))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(maxcol))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(maxcol))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,maxcol))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,maxcol))
    IF(.NOT.ALLOCATED(qprod1date)) ALLOCATE(qprod1date(nrows)) 
    IF(.NOT.ALLOCATED(qprod2date)) ALLOCATE(qprod2date(nrows))
    IF(.NOT.ALLOCATED(wamp)) ALLOCATE(wamp(nrows))
    IF(.NOT.ALLOCATED(qinflow)) ALLOCATE(qinflow(12,nrows))
    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*maxcol)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'
    qprod1date = 0; qprod2date = 0  
    wamp = missing_value
    qinflow = missing_value

    !Open DamData-file and read headings
    OPEN(UNIT = funit,FILE = filename, STATUS = 'old', ACTION='read')
    CALL read_column_headings(funit,maxcol,letters,str,ncols,status)
    IF(status.NE.0)THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(filename)
      propagate_str = 'reading file: '//TRIM(filename)
      CALL propagate_external_msg(e_damdata,e_error,propagate_str)
      RETURN
    ENDIF

    !Find variable columns and set variable type
    code = i_str      !string, ignore
    idtype = 0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ')THEN
        code(i) = i_intg
        idtype = i                                        !! idtype is column where SUBID is
      ENDIF
      
      IF(str(i)(1:letters)=='lake_depth') code(i) = i_real
      IF(str(i)(1:letters)=='purpose   ') code(i) = i_intg   !Purpose of dam (1=irrigation,2=water supply, 3=Flood control, 4 =hydroelectricity)
      IF(str(i)(1:letters)=='regvol    ') code(i) = i_real   !dam_regvol
      IF(str(i)(1:letters)=='datum1    ') code(i) = i_intg   !date for two date based prod rate
      IF(str(i)(1:letters)=='datum2    ') code(i) = i_intg   !date for two date based prod rate
      IF(str(i)(1:letters)=='rate      ') code(i) = i_real   !dam_rate
      IF(str(i)(1:letters)=='exp       ') code(i) = i_real   !dam_exp
      IF(str(i)(1:letters)=='w0ref     ') code(i) = i_real   !dam_w0
      IF(str(i)(1:letters)=='wamp      ') code(i) = i_real   !wamp (m)
      IF(str(i)(1:letters)=='limqprod  ') code(i) = i_real   !dam_limprod
      IF(str(i)(1:letters)=='qprod1    ') code(i) = i_real   !two date-based production rates, qprod1 and qprod2, now replacing the unique rate qprod
      IF(str(i)(1:letters)=='qprod2    ') code(i) = i_real
      IF(str(i)(1:letters)=='qamp      ') code(i) = i_real   !dam_qamp
      IF(str(i)(1:letters)=='qpha      ') code(i) = i_real   !dam_qpha
      IF(str(i)(1:letters)=='qinfjan   ') code(i) = i_real   !dam_qinfjan !Mean monthly inflow to dam (simulated using natural run)
      IF(str(i)(1:letters)=='qinffeb   ') code(i) = i_real   !dam_qinffeb
      IF(str(i)(1:letters)=='qinfmar   ') code(i) = i_real   !dam_qinfmar
      IF(str(i)(1:letters)=='qinfapr   ') code(i) = i_real   !dam_qinfapr
      IF(str(i)(1:letters)=='qinfmay   ') code(i) = i_real   !dam_qinfmay
      IF(str(i)(1:letters)=='qinfjun   ') code(i) = i_real   !dam_qinfjun
      IF(str(i)(1:letters)=='qinfjul   ') code(i) = i_real   !dam_qinfjul
      IF(str(i)(1:letters)=='qinfaug   ') code(i) = i_real   !dam_qinfaug
      IF(str(i)(1:letters)=='qinfsep   ') code(i) = i_real   !dam_qinfsep
      IF(str(i)(1:letters)=='qinfoct   ') code(i) = i_real   !dam_qinfoct
      IF(str(i)(1:letters)=='qinfnov   ') code(i) = i_real   !dam_qinfnov
      IF(str(i)(1:letters)=='qinfdec   ') code(i) = i_real   !dam_qinfdec
      IF(str(i)(1:letters)=='snowfrac  ') code(i) = i_real   !dam_snowfrac
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_damdata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Read all data
    CALL read_basindata5(funit,filename,maxcol,nrows,ncols,code,rindex,iindex,xi,xr)
    CLOSE(UNIT=funit)

    !Allocate variables for dam information
    IF(.NOT.ALLOCATED(dam)) ALLOCATE(dam(nrows))
    ALLOCATE(damindex2(nrows))

    !Find subbasin-dam coupling: damindex(isb)
    IF(idtype==0)THEN
      WRITE(6,*) 'ERROR: subid not found in DamData. ',TRIM(filename)
      propagate_str = 'subid not found in DamData. '//TRIM(filename)
      CALL propagate_external_msg(e_damdata,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    DO idam = 1,nrows
      DO i = 1,n
        IF(basin(i)%subid==xi(idam,iindex(idtype)))THEN
          damindex(i)=idam
          damindex2(idam)=i    !! find row in geodata for dam row
          EXIT
        ENDIF
      ENDDO
      IF(i>n)THEN
        WRITE(6,*) 'ERROR: A dam in DamData is missing in GeoData. subid: ',xi(idam,iindex(idtype))
        propagate_str = 'A dam in DamData is missing in GeoData. subid: '//real_to_str(REAL(xi(idam,iindex(idtype))))
        CALL propagate_external_msg(e_damdata,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF  
    ENDDO

    !Save dam information
    DO idam = 1,nrows
      DO i = 1,ncols
        IF(str(i)=='regvol    ')   dam(idam)%regvol    = xr(idam,rindex(i))
        IF(str(i)=='rate      ')   dam(idam)%rate      = xr(idam,rindex(i))
        IF(str(i)=='exp       ')   dam(idam)%exp       = xr(idam,rindex(i))
        IF(str(i)=='w0ref     ')   dam(idam)%w0ref     = xr(idam,rindex(i))
        IF(str(i)=='wamp      ')   wamp(idam)          = xr(idam,rindex(i))
        IF(str(i)=='qprod1    ')   dam(idam)%qprod1    = xr(idam,rindex(i))
        IF(str(i)=='qprod2    ')   dam(idam)%qprod2    = xr(idam,rindex(i))
        IF(str(i)=='qamp      ')   dam(idam)%qamp      = xr(idam,rindex(i))
        IF(str(i)=='qpha      ')   dam(idam)%qpha      = xr(idam,rindex(i))
        IF(str(i)=='limqprod  ')   dam(idam)%limprod   = xr(idam,rindex(i))
        IF(str(i)=='qinfjan   ')   qinflow(1,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinffeb   ')   qinflow(2,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfmar   ')   qinflow(3,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfapr   ')   qinflow(4,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfmay   ')   qinflow(5,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfjun   ')   qinflow(6,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfjul   ')   qinflow(7,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfaug   ')   qinflow(8,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfsep   ')   qinflow(9,idam)     = xr(idam,rindex(i))
        IF(str(i)=='qinfoct   ')   qinflow(10,idam)    = xr(idam,rindex(i))
        IF(str(i)=='qinfnov   ')   qinflow(11,idam)    = xr(idam,rindex(i))
        IF(str(i)=='qinfdec   ')   qinflow(12,idam)    = xr(idam,rindex(i))
        IF(str(i)=='snowfrac  ')   dam(idam)%snowfrac  = xr(idam,rindex(i))
        IF(str(i)=='lake_depth')THEN
          IF(xr(idam,rindex(i)).NE.missing_value)   basin(damindex2(idam))%lakedepth(2) = xr(idam,rindex(i))
        ENDIF
        !Integer input
        IF(str(i)=='datum1    ')   qprod1date(idam)    = xi(idam,iindex(i))
        IF(str(i)=='datum2    ')   qprod2date(idam)    = xi(idam,iindex(i))
        IF(str(i)=='purpose   ')   dam(idam)%purpose   = xi(idam,iindex(i))
      ENDDO
    ENDDO
   
    !Calculate other lake/dam variables; w0ref, wmin, datum, wampcoeff, qinflow
    dam(:)%wmin = missing_value
    dam(:)%wampcoeff = missing_value
    DO i = 1,n
      IF(damindex(i)>0)THEN
        damarea = basin(i)%area * classbasin(i,slc_olake)%part
        IF(damarea>0)THEN
          dam(damindex(i))%wmin = 0. - dam(damindex(i))%regvol * 1000000. / damarea
          IF(wamp(damindex(i))/=missing_value) &
              dam(damindex(i))%wampcoeff = wamp(damindex(i))/(-1.*dam(damindex(i))%wmin)
        ENDIF
      ENDIF
    ENDDO
    DO idam = 1,nrows 
      MonthDayDate = qprod1date(idam)
      CALL get_dayno_from_monthday(MonthDayDate, dayNumber)
      dam(idam)%datum1 = dayNumber
      MonthDayDate = qprod2date(idam)
      CALL get_dayno_from_monthday(MonthDayDate, dayNumber)
      dam(idam)%datum2 = dayNumber
      dam(idam)%qinfmed = SUM(qinflow(:,idam))/12.
      dam(idam)%qinfmin = MINVAL(qinflow(:,idam))
      dam(idam)%qinfmax = MAXVAL(qinflow(:,idam))
    ENDDO
 
    !Deallocate local variables
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    IF(ALLOCATED(qprod1date)) DEALLOCATE(qprod1date) 
    IF(ALLOCATED(qprod2date)) DEALLOCATE(qprod2date)
    IF(ALLOCATED(wamp)) DEALLOCATE(wamp)
    IF(ALLOCATED(damindex2)) DEALLOCATE(damindex2)

    WRITE(6,*) 'Dam information loaded from DamData.txt'

  END SUBROUTINE load_damdata

  !>Collects the information about flooding areas from FloodData.txt
  !>
  !>\b Consequences Module modvar variables conduct,flooding,floodindex may be allocated and set. 
  !------------------------------------------------------------------
  SUBROUTINE load_flooddata(funit,dir,n,status)

    USE WORLDVAR, ONLY : maxcharpath,  &
                         i_str,        &
                         i_intg,       &
                         i_real
    USE MODVAR,   ONLY : basin,        &
                         missing_value, &
                         conduct, &   !OUT
                         flooding,      &   !OUT
                         floodindex,    &   !OUT
                         modeloption,   &
                         p_floodplain
    USE CONVERT, ONLY : real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_flooddata,e_error,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit         !<File unit, temporary file open
    CHARACTER (LEN=*), INTENT(IN) :: dir  !<File directory
    INTEGER, INTENT(IN)  :: n             !<Number of subbasins (base model)
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local parameters
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading

    !Local variables
    LOGICAL fex
    LOGICAL hrefstatus  !Flag for href set in FloodData
    INTEGER i,idam
    INTEGER maxcol
    INTEGER idtype                    !Column with subid
    INTEGER nrows                     !Number of dams in file
    INTEGER ncols                     !Number of columns in file
    INTEGER,ALLOCATABLE :: code(:)    !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)  !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)  !Index for column integer variables
    CHARACTER(LEN=maxcharpath) filename
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)   !Content string
    INTEGER,ALLOCATABLE :: xi(:,:)            !Integer data read from file
    REAL,ALLOCATABLE :: xr(:,:)               !Real data read from file
    CHARACTER(LEN=150) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    status = 0
    filename = TRIM(dir)//'FloodData.txt'
    !conductflood = .FALSE.
    hrefstatus = .FALSE.

    !Check if file exist and if floodmodel is set
    INQUIRE(FILE=filename,EXIST=fex)
    IF(.NOT.fex) RETURN
    IF(modeloption(p_floodplain)==0)THEN
      WRITE(6,*) 'WARNING: floodmodel not set in info.txt. Simulation will not simulate floodplains or use FloodData.txt'
      RETURN
    ENDIF

    WRITE(6,*) 'File opened: ', TRIM(filename)
    IF(.NOT.ALLOCATED(floodindex)) ALLOCATE(floodindex(n))
    floodindex = 0

    !Count number of data rows and columns, and allocate variables for reading
    CALL count_data_cols(funit,filename,0,maxcol,status)
    IF(status/=0)THEN
      WRITE(6,*) 'ERROR: counting columns in FloodData.txt'
      RETURN
    ENDIF
    CALL count_data_rows(funit,filename,1,nrows,status)
    IF(status/=0)RETURN
    IF(nrows==0)THEN
      WRITE(6,*) 'WARNING: No flood areas in FloodData.txt ',TRIM(filename)
      RETURN
    ENDIF
    IF(.NOT.ALLOCATED(flooding)) ALLOCATE(flooding(nrows))
    flooding%fpfmr = 0.
    flooding%fpfol = 0.
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(maxcol))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(maxcol))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(maxcol))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(maxcol))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,maxcol))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,maxcol))
    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*maxcol)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !Open FloodData-file and read headings
    OPEN(UNIT = funit,FILE = filename, STATUS = 'old', ACTION='read')
    CALL read_column_headings(funit,maxcol,letters,str,ncols,status)
    IF(status.NE.0)THEN
      WRITE(6,*) 'ERROR: reading file: ',TRIM(filename)
      propagate_str = 'reading file: '//TRIM(filename)
      CALL propagate_external_msg(e_flooddata,e_error,propagate_str)
      RETURN
    ENDIF

    !Find variable columns and set variable type
    code = i_str      !string, ignore
    idtype = 0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ')THEN
        code(i) = i_intg
        idtype = i                                        !! idtype is column where SUBID is
      ENDIF
      IF(str(i)(1:letters)=='fpfol     ') code(i) = i_real
      IF(str(i)(1:letters)=='fpfmr     ') code(i) = i_real
      IF(str(i)(1:letters)=='floll     ') code(i) = i_real
      IF(str(i)(1:letters)=='flolp     ') code(i) = i_real
      IF(str(i)(1:letters)=='flmrr     ') code(i) = i_real
      IF(str(i)(1:letters)=='flmrp     ') code(i) = i_real
      IF(str(i)(1:letters)=='rclfp     ') code(i) = i_real
      IF(str(i)(1:letters)=='rcrfp     ') code(i) = i_real
      IF(str(i)(1:letters)=='rcfpl     ') code(i) = i_real
      IF(str(i)(1:letters)=='rcfpr     ') code(i) = i_real
      IF(str(i)(1:letters)=='fymol     ') code(i) = i_real
      IF(str(i)(1:letters)=='fymmr     ') code(i) = i_real
      IF(str(i)(1:letters)=='hrefr     ')THEN
        code(i) = i_real
        hrefstatus = .TRUE.
      ENDIF
      IF(str(i)(1:letters)=='hrefl     ')THEN
        code(i) = i_real
        hrefstatus = .TRUE.
      ENDIF
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_flooddata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Read all data
    CALL read_basindata5(funit,filename,maxcol,nrows,ncols,code,rindex,iindex,xi,xr)
    CLOSE(UNIT=funit)

    !Find subbasin-floodplain coupling: floodindex(isb)
    IF(idtype==0)THEN
      WRITE(6,*) 'ERROR: subid not found in FloodData. ',TRIM(filename)
      propagate_str = 'subid not found in FloodData. '//TRIM(filename)
      CALL propagate_external_msg(e_flooddata,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    DO idam = 1,nrows
      DO i = 1,n
        IF(basin(i)%subid==xi(idam,iindex(idtype)))THEN
          floodindex(i)=idam
          EXIT
        ENDIF
      ENDDO
      IF(i>n)THEN
        WRITE(6,*) 'Error: A floodplain in FloodData in a subbasin missing in GeoData. subid: ',xi(idam,iindex(idtype))
        propagate_str = 'A floodplain in FloodData in a subbasin missing in GeoData. subid: '//real_to_str(REAL(xi(idam,iindex(idtype))))
        CALL propagate_external_msg(e_flooddata,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF  
    ENDDO

    !Save floodplain information
    DO idam = 1,nrows
      DO i = 1,ncols
        IF(str(i)(1:letters)=='fpfol     ') flooding(idam)%fpfol  = xr(idam,rindex(i))  !floodplain
        IF(str(i)(1:letters)=='fpfmr     ') flooding(idam)%fpfmr  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='floll     ') flooding(idam)%floll  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='flolp     ') flooding(idam)%flolp  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='flmrr     ') flooding(idam)%flmrr  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='flmrp     ') flooding(idam)%flmrp  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='rclfp     ') flooding(idam)%rcl2fp = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='rcrfp     ') flooding(idam)%rcr2fp = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='rcfpl     ') flooding(idam)%rcfp2l = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='rcfpr     ') flooding(idam)%rcfp2r = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='fymol     ') flooding(idam)%fymol  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='fymmr     ') flooding(idam)%fymmr  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='hrefr     ') flooding(idam)%hrefr  = xr(idam,rindex(i))
        IF(str(i)(1:letters)=='hrefl     ') flooding(idam)%hrefl  = xr(idam,rindex(i))
      ENDDO
    ENDDO
   
    !Calculate floodplain reference level (deepest bottom of floodplain), file holds threshold
    IF(.NOT.hrefstatus)THEN
      flooding%hrefl = missing_value
      flooding%hrefr = missing_value
    ELSE
      WHERE(flooding%hrefl/=missing_value)
        flooding%hrefl = flooding%hrefl - flooding%flolp
      ENDWHERE
      WHERE(flooding%hrefr/=missing_value)
        flooding%hrefr = flooding%hrefr - flooding%flmrp
      ENDWHERE
    ENDIF
    
    
    !Check if flooded area is simulated
    IF(SUM(flooding%fpfmr) + SUM(flooding%fpfol)>0.) conduct%floodplain = .TRUE.

    !Deallocate local variables
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)

    WRITE(6,*) 'Floodplain information loaded'

  END SUBROUTINE load_flooddata

  !------------------------------------------------------------------
  !>\brief Set lakedatapar values to those model variable values specified
  !>in LakeData.txt. 
  !!
  !>\b Consequences Module modvar variables lakedatapar_ini,lakedatapar,
  !> lakedataparindex may be allocated and set. 
  !------------------------------------------------------------------
  SUBROUTINE start_lakedata_table(maxcol,ncols,nrows,str,xi,xr,iindex,rindex,ns,lakedataid)

    USE MODVAR, ONLY : modparid,          &
                       lakedatapar_ini,   &   !OUT
                       lakedatapar,       &   !OUT
                       lakedataparindex,  &   !OUT
                       max_par,           &
                       m_ldpar,           &
                       nregions,      &
                       missing_value,     &
                       basin

    !Argument declaration
    INTEGER, INTENT(IN)  :: maxcol                    !<Maximum possible amount of columns on LakeData.txt
    INTEGER, INTENT(IN)  :: ncols                     !<Number of columns effectively used in LakeData.txt
    INTEGER, INTENT(IN)  :: nrows                     !<Number of rows effectively used in LakeData.txt
    CHARACTER(LEN=10), INTENT(IN) :: str(maxcol)      !<Character array with labels of ALL columns given in LakeData.txt (column headers)
    INTEGER, INTENT(IN)  :: xi(nrows,maxcol)          !<Integer values aquired by reading LakeData.txt
    REAL, INTENT(IN)     :: xr(nrows,maxcol)          !<Float values aquired by reading LakeData.txt
    INTEGER, INTENT(IN)  :: iindex(maxcol)            !<Index for column real variables
    INTEGER, INTENT(IN)  :: rindex(maxcol)            !<Index for column integer variables
    INTEGER, INTENT(IN)  :: ns                        !<Number of subbasins (base model)
    INTEGER, INTENT(IN)  :: lakedataid(ns)            !<lakedataid from GeoData.txt
    
    !Local variables
    INTEGER rowCounter, colCounter, modparCounter, basinCounter
    INTEGER nlakedatapar, tempVal1, nlakeregions

    nlakedatapar = 0          !Amount of lakedatapar model variables; set to 0, then find the largest one
    nlakeregions = nregions(6)
    DO modparCounter = 1,max_par
      IF(modparid(modparCounter)%deptype==m_ldpar)THEN
        nlakedatapar = MAX(nlakedatapar,modparid(modparCounter)%parno)
      ENDIF
    ENDDO

    IF(.NOT.ALLOCATED(lakedatapar)) ALLOCATE(lakedatapar(nlakeregions+nrows, nlakedatapar)) !This is for model simulation
    IF(.NOT.ALLOCATED(lakedatapar_ini)) ALLOCATE(lakedatapar_ini(nlakeregions+nrows, nlakedatapar)) !This is for LakeData values (more rows than needed)
    lakedatapar_ini = missing_value       !Initialize the table of parameter values with the missing value (-9999)
    IF(.NOT.ALLOCATED(lakedataparindex)) ALLOCATE(lakedataparindex(ns,2))
    lakedataparindex = 0              !Initialize the line index table with 0 (useful for 2nd colum, with the row number)

    !Parameter by parameter (= column by column), copy those parameter values given in LakeData.txt in the table "lakedatapar_ini" (it's a modvar)
    !Values are put at the bottom of the table. Eventual parameter values coming from par.txt are appended later in the table top rows, in subroutine finish_lakedata_table.
    !The order in which lakes are given in LakeData.txt is respected linewise
    DO colCounter = 1,ncols

      !Loop through all model variables to look for label match with the currently considered parameter, labelled str(colCounter) in lakeData
      DO modparCounter = 1,max_par
        !When a matching variable shortname is found, determine if the model variable has the parameter value read from lakedata.txt (deptype = m_ldpar), or that from par.txt (deptype = others)
        IF(str(colCounter)==modparid(modparCounter)%shortname .AND. modparid(modparCounter)%deptype==m_ldpar)THEN
          !Parameter values coming from lakedata are stored in xr for float values; take them from there
          IF(rindex(colCounter).NE.0)THEN
            DO rowCounter = 1,nrows
              lakedatapar_ini(nlakeregions+rowCounter, modparid(modparCounter)%parno) = xr(rowCounter, rindex(colCounter))
            ENDDO
          ENDIF
          EXIT
        ENDIF   !End of the check on label match and on the origin of the parameter value (LakeData.txt or par.txt)
      ENDDO   !End of model variable name loop

      !Find corresponding subbasin for every lake, and set pointer to that lake
      IF(str(colCounter)=='lakedataid')THEN
        DO rowCounter = 1,nrows
          tempVal1 = xi(rowCounter, iindex(colCounter))
          IF(tempVal1>0)THEN
            DO basinCounter = 1,ns
              IF(lakedataid(basinCounter) == tempVal1)THEN    
                lakedataparindex(basinCounter, 2) = nlakeregions+rowCounter
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF

    ENDDO   !End of lakeData column labels loop
    
    !Point ilake and non-LakeData lakes to lakeregion-row in lakedatapar
    DO basinCounter = 1,ns
      lakedataparindex(basinCounter,1) = basin(basinCounter)%parregion(6)
      IF(lakedataparindex(basinCounter,2) == 0)THEN
        lakedataparindex(basinCounter,2) = basin(basinCounter)%parregion(6)
      ENDIF
    ENDDO

  END SUBROUTINE start_lakedata_table

  !>Set lakedatapar values to those specified in HYPE parameter variables.
  !------------------------------------------------------------------
  SUBROUTINE finish_lakedata_table(ns)
!  SUBROUTINE finish_lakedata_table(lakedatafile,ns)

    USE MODVAR, ONLY : modparid, &
                       lakedatapar, & !OUT
                       lakedatapar_ini, &
                       lakedataparindex, &  !OUT
                       basin, &
                       genpar, &
                       regpar, &    
                       max_par, &
                       m_gpar, &
                       m_rpar, &
                       m_ldpar, &
                       nregions, &  
                       missing_value

    !Argument declaration
!    LOGICAL, INTENT(IN) :: lakedatafile   !<status of LakeData.txt
    INTEGER, INTENT(IN) :: ns             !<number of subbasins (nsub_basemodel)
    
    !Local variables
    LOGICAL found         !corresponding parameter found
    LOGICAL lakedatafile  !status of LakeData file
    INTEGER parCounter    !index for loop to find corresponding par
    INTEGER ldparCounter  !index for loop to find ldpar
    INTEGER lakeCounter   !lake/row in lakedata
    INTEGER isb           !subbasin index/number
    INTEGER dimlakepar    !size of lakedatapar
    INTEGER dimlakepar2   !second dimension of lakedatapar
    INTEGER nlakeregions
    INTEGER,ALLOCATABLE :: subbasinindex(:) !subbasin index for lakedata (rows)

    !>\b Algorithm \n
    lakedatafile=.FALSE.
    IF(ALLOCATED(lakedatapar_ini)) lakedatafile=.TRUE.

    !Start of subroutine
    IF(lakedatafile)THEN

      !Set lakedatapar to initial values from LakeData.txt
      lakedatapar = lakedatapar_ini

      !Set index table for finding subbasin index for lakedata (row)
      dimlakepar = SIZE(lakedatapar,1)
      nlakeregions = nregions(6)
      IF(.NOT.ALLOCATED(subbasinindex)) ALLOCATE(subbasinindex(dimlakepar))
      subbasinindex = 0
      DO lakeCounter = nlakeregions+1,dimlakepar
        DO isb = 1,ns
          IF(lakedataparindex(isb,2)==lakeCounter)THEN
            EXIT
          ENDIF
        ENDDO
        IF(isb<=ns) subbasinindex(lakeCounter) = isb
      ENDDO

      DO ldparCounter = 1,max_par
        !For every lakedata-parameter find the corresponding parameter in par.txt
        IF(modparid(ldparCounter)%deptype==m_ldpar)THEN

          !Loop through all model parameters to look for label match with the 
          !currently considered parameter, labelled str(colCounter) in LakeData.txt
          found = .FALSE.
          DO parCounter = 1,max_par
            IF(modparid(parCounter)%deptype==m_gpar .OR. modparid(parCounter)%deptype==m_rpar)THEN
              IF(modparid(parCounter)%shortname==modparid(ldparCounter)%shortname)THEN  !Check label match with model parameter of ldpar-type
                !Corresponding parameter found!
                found = .TRUE.
                !Set general/lakeregion parameter values in lakedatapar
                IF(modparid(parCounter)%deptype==m_gpar)THEN 
                  lakedatapar(1:nlakeregions, modparid(ldparCounter)%parno) = genpar(modparid(parCounter)%parno)
                ELSEIF(modparid(parCounter)%deptype==m_rpar)THEN
                  lakedatapar(1:nlakeregions, modparid(ldparCounter)%parno) = regpar(modparid(parCounter)%parno,1:nlakeregions)
                ENDIF

                !Set missing parameters from LakeData file to general/lakeregion parameter value
                DO lakeCounter = nlakeregions+1,dimlakepar
                  IF(lakedatapar(lakeCounter, modparid(ldparCounter)%parno)==missing_value)THEN
                    IF(modparid(parCounter)%deptype==m_gpar)THEN
                      lakedatapar(lakeCounter, modparid(ldparCounter)%parno) = genpar(modparid(parCounter)%parno)
                    ENDIF
                    IF(modparid(parCounter)%deptype==m_rpar)THEN
                      IF(subbasinindex(lakeCounter)>0)THEN
                        lakedatapar(lakeCounter, modparid(ldparCounter)%parno) = regpar(modparid(parCounter)%parno,basin(subbasinindex(lakeCounter))%parregion(6))
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO

                !Next ldpar
                EXIT
              ENDIF !shortname is equal
            ENDIF !gpar or lrpar
          ENDDO !parCounter

          IF(.NOT.found)THEN  !This variable does not have a corresponing par.txt parameter
            !Set non-LakeData set lake to zero
            lakedatapar(1:nlakeregions, modparid(ldparCounter)%parno) = 0.
            DO lakeCounter = nlakeregions+1,dimlakepar
              IF(lakedatapar(lakeCounter, modparid(ldparCounter)%parno)==missing_value)THEN
                lakedatapar(lakeCounter, modparid(ldparCounter)%parno) = 0.
              ENDIF
            ENDDO
          ENDIF

        ENDIF !ldpar
      ENDDO !ldparCounter

      IF(ALLOCATED(subbasinindex)) DEALLOCATE(subbasinindex)

    ELSE
      !No LakeData.txt: set lakedatapar with only the general/lakeregion parameters 
      nlakeregions = nregions(6)
      dimlakepar = nlakeregions
      dimlakepar2 = 0          !Amount of lakedatapar model variables; set to 0, then find the largest one
      DO parCounter = 1,max_par
        IF(modparid(parCounter)%deptype==m_ldpar)THEN
          dimlakepar2 = MAX(dimlakepar2,modparid(parCounter)%parno)
        ENDIF
      ENDDO
      IF(.NOT.ALLOCATED(lakedatapar)) ALLOCATE(lakedatapar(nlakeregions,dimlakepar2))
      IF(.NOT.ALLOCATED(lakedataparindex)) ALLOCATE(lakedataparindex(ns,2))

      !Point ilake and non-LakeData olakes to lakeregion-row in lakedatapar
      lakedataparindex(:,1) = basin(:)%parregion(6)   !parregion 6 is hardcoded for lakedatapar lakeregion
      lakedataparindex(:,2) = lakedataparindex(:,1)

      DO ldparCounter = 1,max_par
          !For every lakedata-parameter find the corresponding parameter in par.txt
        IF(modparid(ldparCounter)%deptype==m_ldpar)THEN

            !Loop through all model parameters to look for label match with the currently considered parameter, labelled str(colCounter) in LakeData.txt
          DO parCounter = 1,max_par
            IF(modparid(parCounter)%deptype==m_gpar .OR. modparid(parCounter)%deptype==m_rpar)THEN
              IF(modparid(parCounter)%shortname==modparid(ldparCounter)%shortname)THEN  !Check label match with model parameter of ldpar-type
                !Corresponding parameter found!

                !Set general/lakeregion parameter values in lakedatapar
                IF(modparid(parCounter)%deptype==m_gpar)THEN 
                  lakedatapar(1:nlakeregions, modparid(ldparCounter)%parno) = genpar(modparid(parCounter)%parno)
                ELSEIF(modparid(parCounter)%deptype==m_rpar)THEN
                  lakedatapar(1:nlakeregions, modparid(ldparCounter)%parno) = regpar(modparid(parCounter)%parno,1:nlakeregions)
                ENDIF

                !Next ldpar
                EXIT
              ENDIF !shortname is equal
            ENDIF !gpar or lrpar
          ENDDO !parCounter
        ENDIF !ldpar
      ENDDO !ldparCounter
    ENDIF

  END SUBROUTINE finish_lakedata_table

  !>\brief Gets the information about subbasins from GeoData.
  !!Reads the matrix of basin data values from the file vith mcols columns
  !!
  !>\b Consequences Module variables will be allocated and set
  !------------------------------------------------------------------------------
  SUBROUTINE read_and_calc_basindata(funit,infile,n,lakedataid,maxcol,mcols) 

    USE WORLDVAR, ONLY : i_str, &
                         i_intg, &
                         i_real, &
                         subweightcrit  !OUT
    USE MODVAR, ONLY : timesteps_per_day, &
                       conductbasinlocsoil, & !OUT
                       simulate, &  !OUT (%ilakecatch)
                       deposition, &
                       i_in,i_on,i_sp,i_pp,i_t1,i_t2,i_ss,i_ae,  &
                       max_pstype, &
                       basin,        &    !OUT
                       classbasin,   &    !OUT
                       pathsubid,    &    !OUT
                       load,         &    !OUT
                       wetland,      &    !OUT
                       petmodel,     &    !OUT
                       nregiondivisions, &
                       nregions, &        !OUT  !Number of parameter regions
                       lakesectiondata    !OUT
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_geodata,e_error,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit                  !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile        !<Name of characteristics file to be read
    INTEGER, INTENT(IN)  :: n                      !<Number of subbasins
    INTEGER, INTENT(OUT) :: lakedataid(n)          !<lakedataid from GeoData
    INTEGER, INTENT(IN)  :: maxcol                 !<Maximum number of data columns
    INTEGER, INTENT(OUT) :: mcols                  !<Actual number of columns
    
    !Local parameters
    INTEGER, PARAMETER :: letters = 11  !Max number of letters (read) in heading

    !Local variables
    INTEGER i
    INTEGER idslc,idlks
    INTEGER xi(n,maxcol)               !Integer data read from file
    INTEGER code(maxcol)               !Code for column variable
    INTEGER rindex(maxcol)             !Index for column real variables
    INTEGER iindex(maxcol)             !Index for column integer variables
    INTEGER status                     !Error status
    INTEGER wsfdir                     !direction for Winstral coefficient
    LOGICAL deploadnfound,drydep_nfound,wetdep_nfound !Help variable for deallocating deposition-variables if not used
    LOGICAL locsoil_found              !Help variable for loc_soil column in GeoData
    LOGICAL lks_nfound,lks_dfound,lks_ifound,lks_afound !Help variables for deallocating lakesectiondata if not used
    REAL    onetspday                  !Reciprocal of timesteps per day
    REAL    xr(n,maxcol)               !Real data read from file
    REAL    localsource(n,8)           !Read rural source info (subbasins,column)
    REAL    rivratknoice(n),rivratpnoice(n)    !Rating curve parameters for river (no ice): q=k(w-w0)^p
    REAL    rivratkice(n),rivratpice(n)    !Rating curve parameters for river during ice: q=k(w-w0)^p
    CHARACTER(LEN=letters) str(maxcol)      !Content string
    CHARACTER(LEN=150) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    !Start of subroutine
    status = 0
    localsource = 0.
    rivratknoice = 0.; rivratpnoice = 0.
    rivratkice = 0.; rivratpice = 0.
    deploadnfound = .FALSE.
    drydep_nfound = .FALSE.
    wetdep_nfound = .FALSE.
    locsoil_found = .FALSE.
    lks_nfound = .FALSE.
    lks_dfound = .FALSE.
    lks_ifound = .FALSE.
    lks_afound = .FALSE.
    onetspday = 1./REAL(timesteps_per_day)
    
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old', ACTION='read')     !Open GeoData-file
    WRITE(6,*) 'File opened: ', TRIM(infile)

    !Reads the column headings from file
    CALL read_column_headings(funit,maxcol,letters,str,mcols,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(infile)
      propagate_str = 'reading file: '//TRIM(infile)
      CALL propagate_external_msg(e_geodata,e_error,propagate_str)
      RETURN
    ENDIF

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*maxcol)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !Code variables for easy finding of variable type
    code=i_str    !string, ignore
    DO i = 1,mcols
      IF(str(i)(1:letters)=='area       ') code(i) = i_real
      IF(str(i)(1:letters)=='subid      ') code(i) = i_intg
      IF(str(i)(1:letters)=='xcoord     ') code(i) = i_real
      IF(str(i)(1:letters)=='ycoord     ') code(i) = i_real
      IF(str(i)(1:letters)=='longitude  ') code(i) = i_real
      IF(str(i)(1:letters)=='latitude   ') code(i) = i_real
      IF(str(i)(1:letters)=='elev_mean  ') code(i) = i_real
      IF(str(i)(1:letters)=='elev_std   ') code(i) = i_real
      IF(str(i)(1:letters)=='slope_mean ') code(i) = i_real
      IF(str(i)(1:letters)=='slope_std  ') code(i) = i_real
      IF(str(i)(1:letters)=='ps1_vol    '.OR.str(i)(1:letters)=='ps2_vol    '.OR.str(i)(1:letters)=='ps3_vol    ') &
        WRITE(6,*) 'WARNING: point sources no longer read from GeoData'
      IF(str(i)(1:letters)=='loc_tp     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_sp     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_tn     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_in     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_t1     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_t2     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_ts     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_ss     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_vol    ') code(i) = i_real
      IF(str(i)(1:10)=='locsoil   ')THEN
        code(i) = i_real
        locsoil_found = .TRUE.
      ENDIF
      IF(str(i)(1:letters)=='region     ') code(i) = i_intg
      IF(str(i)(1:letters)=='petmodel   ') code(i) = i_intg
      IF(str(i)(1:letters)=='lakeregion ') code(i) = i_intg
      IF(str(i)(1:letters)=='parreg_4   ') code(i) = i_intg
      IF(str(i)(1:letters)=='parreg_5   ') code(i) = i_intg
      IF(str(i)(1:letters)=='ilregion   ') code(i) = i_intg
      IF(str(i)(1:letters)=='olregion   ') code(i) = i_intg
      IF(str(i)(1:letters)=='lake_depth ') code(i) = i_real
      IF(str(i)(1:letters)=='lake_whigh '.OR.str(i)(1:letters)=='lake_qavg  '.OR.   &
         str(i)(1:letters)=='lake_qamp  '.OR.str(i)(1:letters)=='lake_rate  '.OR.   &
         str(i)(1:letters)=='lake_qhigh '.OR.str(i)(1:letters)=='lake_exp   '.OR.   &
         str(i)(1:letters)=='mq         '.OR.str(i)(1:letters)=='regvol     '.OR.   &
         str(i)(1:letters)=='lake_qpha  '.OR.str(i)(1:letters)=='lake_wref  ')THEN
        WRITE(6,*) 'WARNING: specific lake parameter no longer read from GeoData',str
      ENDIF
      IF(str(i)(1:letters)=='icatch     ') code(i) = i_real
      IF(str(i)(1:letters)=='iwetcatch  ') code(i) = i_real
      IF(str(i)(1:letters)=='mainfl     ') WRITE(6,*) 'WARNING: mainfl in GeoData is an column name no longer used' 
      IF(str(i)(1:letters)=='branch     ') WRITE(6,*) 'WARNING: branch in GeoData is an column name no longer used'
      IF(str(i)(1:letters)=='grwflow1   ') WRITE(6,*) 'WARNING: grwflow1 in GeoData is an column name no longer used'
      IF(str(i)(1:letters)=='branchdown ') WRITE(6,*) 'WARNING: branchdown in GeoData is an column name no longer used'
      IF(str(i)(1:letters)=='maindown   ') code(i) = i_intg
      IF(str(i)(1:letters)=='grwdown    ') code(i) = i_intg
      IF(str(i)(1:letters)=='grwolake   ') code(i) = i_real
      IF(str(i)(1:4)=='slc_')        code(i) = i_real
      IF(str(i)(1:6)=='dhslc_')      code(i) = i_real
      IF(str(i)(1:4)=='scr_')        code(i) = i_real
      IF(str(i)(1:7)=='sloslc_')     code(i) = i_real
      IF(str(i)(1:7)=='aspslc_')     code(i) = i_real 
      IF(str(i)(1:3)=='ws_')         code(i) = i_real
      IF(str(i)(1:letters)=='parreg     ') code(i) = i_intg
      IF(str(i)(1:letters)=='wqparreg   ') code(i) = i_intg
      IF(str(i)(1:letters)=='rivlen     ') code(i) = i_real
      IF(str(i)(1:letters)=='loc_rivlen ') code(i) = i_real
      IF(str(i)(1:letters)=='wetdep_n   ')THEN
        code(i) = i_real
        wetdep_nfound = .TRUE.  ! For deallocating deposition%inwetconc if not used
      ENDIF
      IF(str(i)(1:letters)=='drydep_n1  ')THEN
        code(i) = i_real
        drydep_nfound = .TRUE.  ! For deallocating deposition%indryload if not used
      ENDIF
      IF(str(i)(1:letters)=='drydep_n2  ')THEN
        code(i) = i_real
        drydep_nfound = .TRUE.  ! For deallocating deposition%indryload if not used
      ENDIF
      IF(str(i)(1:letters)=='drydep_n3  ')THEN
        code(i) = i_real
        drydep_nfound = .TRUE.  ! For deallocating deposition%indryload if not used
      ENDIF
      IF(str(i)(1:letters)=='deploadn1  ') THEN
        code(i) = i_real
        deploadnfound = .TRUE.  ! For deallocating deposition% if total IN load on water is not used
      ENDIF
      IF(str(i)(1:letters)=='deploadn2  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn3  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn4  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn5  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn6  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn7  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn8  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn9  ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn10 ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn11 ') code(i) = i_real
      IF(str(i)(1:letters)=='deploadn12 ') code(i) = i_real
      IF(str(i)(1:letters)=='lrwet_area ') code(i) = i_real
      IF(str(i)(1:letters)=='mrwet_area ') code(i) = i_real
      IF(str(i)(1:letters)=='lrwet_dep  ') code(i) = i_real
      IF(str(i)(1:letters)=='mrwet_dep  ') code(i) = i_real
      IF(str(i)(1:letters)=='lrwet_part ') code(i) = i_real
      IF(str(i)(1:letters)=='mrwet_part ') code(i) = i_real
      IF(str(i)(1:letters)=='buffer     ') code(i) = i_real
      IF(str(i)(1:letters)=='eroindex   ') code(i) = i_real
      IF(str(i)(1:letters)=='close_w    ') code(i) = i_real
      IF(str(i)(1:letters)=='lakedataid ') code(i) = i_intg   !id i LakeData
      IF(str(i)(1:letters)=='pobsid     ') WRITE(6,*) 'WARNING: pobsid etc in GeoData in no longer used, remove confusing columns'
      IF(str(i)(1:letters)=='tobsid     ') WRITE(6,*) 'WARNING: tobsid etc in GeoData in no longer used, remove confusing columns'
      IF(str(i)(1:letters)=='mrratck_noi') code(i) = i_real
      IF(str(i)(1:letters)=='mrratcp_noi') code(i) = i_real
      IF(str(i)(1:letters)=='mrratck_ice') code(i) = i_real
      IF(str(i)(1:letters)=='mrratcp_ice') code(i) = i_real
      IF(str(i)(1:letters)=='mrratcw0   ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_jan  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_feb  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_mar  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_apr  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_may  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_jun  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_jul  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_aug  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_sep  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_oct  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_nov  ') code(i) = i_real
      IF(str(i)(1:letters)=='cloud_dec  ') code(i) = i_real
      IF(str(i)(1:letters)=='weight_sub ') code(i) = i_real
      IF(str(i)(1:letters)=='lks_num    ') THEN
        code(i) = i_intg  !number of lake type 1 sections in this subbasin
        lks_nfound = .TRUE.
      ENDIF
      IF(str(i)(1:7)=='lks_dp_')THEN
        code(i) = i_real  !lake section depth above lakedepth at outlet
        lks_dfound = .TRUE.
      ENDIF
      IF(str(i)(1:7)=='lks_fi_')THEN
        code(i) = i_real  !lake section fraction of icatch
        lks_ifound = .TRUE.
      ENDIF
      IF(str(i)(1:7)=='lks_fa_')THEN
        code(i) = i_real  !lake section fraction of lakearea
        lks_afound = .TRUE.
      ENDIF
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_geodata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    IF(.NOT.deploadnfound) DEALLOCATE(deposition%inloadwater)
    IF(.NOT.wetdep_nfound) DEALLOCATE(deposition%inwetconc)
    IF(.NOT.drydep_nfound)THEN
      DEALLOCATE(deposition%indryload)
    ELSE
      deposition%indryload = 0.
    ENDIF
    IF(.NOT.lks_nfound .OR. .NOT.lks_ifound .OR. .NOT.lks_afound .OR. .NOT.lks_dfound)THEN
      IF(ALLOCATED(lakesectiondata)) DEALLOCATE(lakesectiondata)
    ENDIF
    
    !Read all data
    CALL read_basindata5(funit,infile,maxcol,n,mcols,code,rindex,iindex,xi,xr)
    CLOSE(UNIT=funit)

    DO i = 1,mcols
      IF(str(i)(1:letters)=='area       ')   basin(1:n)%area      = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='subid      ')   basin(1:n)%subid     = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='xcoord     ')   basin(1:n)%xcoord    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='ycoord     ')   basin(1:n)%ycoord    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='longitude  ')   basin(1:n)%longitude = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='latitude   ')   basin(1:n)%latitude  = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='elev_mean  ')   basin(1:n)%elev      = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='elev_std   ')   basin(1:n)%selev     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='slope_mean ')   basin(1:n)%slope     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='slope_std  ')   basin(1:n)%sslope    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_tp     ')   localsource(1:n,1) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_sp     ')   localsource(1:n,2) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_tn     ')   localsource(1:n,3) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_in     ')   localsource(1:n,4) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_t1     ')   localsource(1:n,5) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_t2     ')   localsource(1:n,6) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_ts     ')   localsource(1:n,7) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_ss     ')   localsource(1:n,8) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='loc_vol    ')  load(1:n)%volloc    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='locsoil    ')  load(1:n)%locsoil   = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='region     ')  basin(1:n)%region   = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='petmodel   ')  petmodel(1:n) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='lakeregion ')  basin(1:n)%parregion(6) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='ilregion   ')  basin(1:n)%parregion(4) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='olregion   ')  basin(1:n)%parregion(5) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='parreg_4   ')  basin(1:n)%parregion(4) = xi(1:n,iindex(i)) !Alternative
      IF(str(i)(1:letters)=='parreg_5   ')  basin(1:n)%parregion(5) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='lake_depth ')  basin(1:n)%lakedepth(2) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='icatch     ')  basin(1:n)%ilakecatch = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='iwetcatch  ')  basin(1:n)%iwetcatch  = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_jan  ')  basin(1:n)%cloudiness(1) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_feb  ')  basin(1:n)%cloudiness(2) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_mar  ')  basin(1:n)%cloudiness(3) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_apr  ')  basin(1:n)%cloudiness(4) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_may  ')  basin(1:n)%cloudiness(5) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_jun  ')  basin(1:n)%cloudiness(6) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_jul  ')  basin(1:n)%cloudiness(7) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_aug  ')  basin(1:n)%cloudiness(8) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_sep  ')  basin(1:n)%cloudiness(9) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_oct  ')  basin(1:n)%cloudiness(10) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_nov  ')  basin(1:n)%cloudiness(11) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='cloud_dec  ')  basin(1:n)%cloudiness(12) = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='maindown   ')  pathsubid(1:n)%main   = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='grwdown    ')  pathsubid(1:n)%grw1   = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='grwolake   ')  pathsubid(1:n)%grwtolake = xr(1:n,rindex(i))
      IF(str(i)(1:4)=='slc_') THEN
        idslc = 0
        IF(ICHAR(str(i)(5:5))>=49 .AND. ICHAR(str(i)(5:5))<=57)THEN
          idslc = ICHAR(str(i)(5:5)) - 48
        ENDIF
        IF(ICHAR(str(i)(6:6))>=48 .AND. ICHAR(str(i)(6:6))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(6:6)) - 48
        ENDIF
        IF(ICHAR(str(i)(7:7))>=48 .AND. ICHAR(str(i)(7:7))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(7:7)) - 48
        ENDIF
        IF(ICHAR(str(i)(8:8))>=48 .AND. ICHAR(str(i)(8:8))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(8:8)) - 48
        ENDIF
        classbasin(1:n,idslc)%part=xr(1:n,rindex(i))
      ENDIF
      IF(str(i)(1:6)=='dhslc_') THEN
        idslc = 0
        IF(ICHAR(str(i)(7:7))>=49 .AND. ICHAR(str(i)(7:7))<=57)THEN
          idslc = ICHAR(str(i)(7:7)) - 48
        ENDIF
        IF(ICHAR(str(i)(8:8))>=48 .AND. ICHAR(str(i)(8:8))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(8:8)) - 48
        ENDIF
        IF(ICHAR(str(i)(9:9))>=48 .AND. ICHAR(str(i)(9:9))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(9:9)) - 48
        ENDIF
        IF(ICHAR(str(i)(10:10))>=48 .AND. ICHAR(str(i)(10:10))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(10:10)) - 48
        ENDIF
        classbasin(1:n,idslc)%deltah = xr(1:n,rindex(i))
      ENDIF
      IF(str(i)(1:4)=='scr_') THEN    !area part with secondary crop
        idslc = 0
        IF(ICHAR(str(i)(5:5))>=49 .AND. ICHAR(str(i)(5:5))<=57)THEN
          idslc = ICHAR(str(i)(5:5)) - 48
        ENDIF
        IF(ICHAR(str(i)(6:6))>=48 .AND. ICHAR(str(i)(6:6))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(6:6)) - 48
        ENDIF
        IF(ICHAR(str(i)(7:7))>=48 .AND. ICHAR(str(i)(7:7))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(7:7)) - 48
        ENDIF
        IF(ICHAR(str(i)(8:8))>=48 .AND. ICHAR(str(i)(8:8))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(8:8)) - 48
        ENDIF
        classbasin(1:n,idslc)%part2cr = xr(1:n,rindex(i))
      ENDIF
      IF(str(i)(1:7)=='sloslc_') THEN      !slope
        idslc = 0
        IF(ICHAR(str(i)(8:8))>=49 .AND. ICHAR(str(i)(8:8))<=57)THEN
          idslc = ICHAR(str(i)(8:8)) - 48
        ENDIF
        IF(ICHAR(str(i)(9:9))>=48 .AND. ICHAR(str(i)(9:9))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(9:9)) - 48
          IF(ICHAR(str(i)(10:10))>=48 .AND. ICHAR(str(i)(10:10))<=57)THEN
            IF(idslc>0) idslc = idslc * 10
            idslc = idslc + ICHAR(str(i)(10:10)) - 48
            IF(ICHAR(str(i)(11:11))>=48 .AND. ICHAR(str(i)(11:11))<=57)THEN
              IF(idslc>0) idslc = idslc * 10
              idslc = idslc + ICHAR(str(i)(11:11)) - 48
            ENDIF
          ENDIF
        ENDIF
        classbasin(1:n,idslc)%slope = xr(1:n,rindex(i))
      ENDIF
      IF(str(i)(1:7)=='aspslc_') THEN    !aspect
        idslc = 0
        IF(ICHAR(str(i)(8:8))>=49 .AND. ICHAR(str(i)(8:8))<=57)THEN
          idslc = ICHAR(str(i)(8:8)) - 48
        ENDIF
        IF(ICHAR(str(i)(9:9))>=48 .AND. ICHAR(str(i)(9:9))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(9:9)) - 48
          IF(ICHAR(str(i)(10:10))>=48 .AND. ICHAR(str(i)(10:10))<=57)THEN
            IF(idslc>0) idslc = idslc * 10
            idslc = idslc + ICHAR(str(i)(10:10)) - 48
            IF(ICHAR(str(i)(11:11))>=48 .AND. ICHAR(str(i)(11:11))<=57)THEN
              IF(idslc>0) idslc = idslc * 10
              idslc = idslc + ICHAR(str(i)(11:11)) - 48
            ENDIF
          ENDIF
        ENDIF
        classbasin(1:n,idslc)%aspect = xr(1:n,rindex(i))
      ENDIF
      IF(str(i)(1:3)=='ws_') THEN   !wsf
        idslc = 0
        IF(ICHAR(str(i)(4:4))>=49 .AND. ICHAR(str(i)(4:4))<=57)THEN 
          idslc = ICHAR(str(i)(4:4)) - 48
          wsfdir = ICHAR(str(i)(6:6)) - 48 
        ENDIF
        IF(ICHAR(str(i)(5:5))>=48 .AND. ICHAR(str(i)(5:5))<=57)THEN
          IF(idslc>0) idslc = idslc * 10
          idslc = idslc + ICHAR(str(i)(5:5)) - 48
          wsfdir = ICHAR(str(i)(7:7)) - 48 
          IF(ICHAR(str(i)(6:6))>=48 .AND. ICHAR(str(i)(6:6))<=57)THEN
            IF(idslc>0) idslc = idslc * 10
            idslc = idslc + ICHAR(str(i)(6:6)) - 48
            wsfdir = ICHAR(str(i)(8:8)) - 48 
            IF(ICHAR(str(i)(7:7))>=48 .AND. ICHAR(str(i)(7:7))<=57)THEN
              IF(idslc>0) idslc = idslc * 10
              idslc = idslc + ICHAR(str(i)(7:7)) - 48
              wsfdir = ICHAR(str(i)(9:9)) - 48 
            ENDIF        
          ENDIF        
        ENDIF
        classbasin(1:n,idslc)%wsf(wsfdir) = xr(1:n,rindex(i))
      ENDIF
      IF(str(i)(1:4)=='lks_' .AND. ALLOCATED(lakesectiondata))THEN
        IF(str(i)(1:letters)=='lks_num    ') basin(1:n)%lakesection = xi(1:n,iindex(i))
        IF(str(i)(1:7)=='lks_dp_')THEN
          idlks=0
          IF(ICHAR(str(i)(8:8))>=49 .AND. ICHAR(str(i)(8:8))<=57)THEN
            idlks = ICHAR(str(i)(8:8)) - 48
          ENDIF
          IF(ICHAR(str(i)(9:9))>=48 .AND. ICHAR(str(i)(9:9))<=57)THEN
            IF(idlks>0) idlks = idlks * 10
            idlks = idlks + ICHAR(str(i)(8:8)) - 48
          ENDIF
          lakesectiondata(1:n,idlks)%depth = xr(1:n,rindex(i))
        ENDIF
        IF(str(i)(1:7)=='lks_fi_')THEN
          idlks=0
          IF(ICHAR(str(i)(8:8))>=49 .AND. ICHAR(str(i)(8:8))<=57)THEN
            idlks = ICHAR(str(i)(8:8)) - 48
          ENDIF
          IF(ICHAR(str(i)(9:9))>=48 .AND. ICHAR(str(i)(9:9))<=57)THEN
            IF(idlks>0) idlks = idlks * 10
            idlks = idlks + ICHAR(str(i)(8:8)) - 48
          ENDIF
          lakesectiondata(1:n,idlks)%ficatch = xr(1:n,rindex(i))
        ENDIF
        IF(str(i)(1:7)=='lks_fa_')THEN
          idlks=0
          IF(ICHAR(str(i)(8:8))>=49 .AND. ICHAR(str(i)(8:8))<=57)THEN
            idlks = ICHAR(str(i)(8:8)) - 48
          ENDIF
          IF(ICHAR(str(i)(9:9))>=48 .AND. ICHAR(str(i)(9:9))<=57)THEN
            IF(idlks>0) idlks = idlks * 10
            idlks = idlks + ICHAR(str(i)(8:8)) - 48
          ENDIF
          lakesectiondata(1:n,idlks)%farea = xr(1:n,rindex(i))
        ENDIF
      ENDIF
      IF(str(i)(1:letters)=='parreg     ')  basin(1:n)%parregion(1) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='wqparreg   ')  basin(1:n)%parregion(3) = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='loc_rivlen ')  basin(1:n)%rivlen(1)    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='rivlen     ')  basin(1:n)%rivlen(2)    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='wetdep_n   ')  deposition%inwetconc(1:n) = xr(1:n,rindex(i))*1.E-3    !ug/L -> mg/L
      IF(str(i)(1:letters)=='drydep_n1  ')  deposition%indryload(1:n,1) = xr(1:n,rindex(i))*onetspday  !kg/km2/d->kg/km2/ts
      IF(str(i)(1:letters)=='drydep_n2  ')  deposition%indryload(1:n,2) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='drydep_n3  ')  deposition%indryload(1:n,3) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn1  ')  deposition%inloadwater(1:n,1) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn2  ')  deposition%inloadwater(1:n,2) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn3  ')  deposition%inloadwater(1:n,3) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn4  ')  deposition%inloadwater(1:n,4) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn5  ')  deposition%inloadwater(1:n,5) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn6  ')  deposition%inloadwater(1:n,6) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn7  ')  deposition%inloadwater(1:n,7) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn8  ')  deposition%inloadwater(1:n,8) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn9  ')  deposition%inloadwater(1:n,9) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn10 ')  deposition%inloadwater(1:n,10) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn11 ')  deposition%inloadwater(1:n,11) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='deploadn12 ')  deposition%inloadwater(1:n,12) = xr(1:n,rindex(i))*onetspday
      IF(str(i)(1:letters)=='lrwet_area ')  wetland(1:n,1)%area     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrwet_area ')  wetland(1:n,2)%area     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='lrwet_dep  ')  wetland(1:n,1)%depth    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrwet_dep  ')  wetland(1:n,2)%depth    = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='lrwet_part ')  wetland(1:n,1)%part     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrwet_part ')  wetland(1:n,2)%part     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='buffer     ')  basin(1:n)%buffer       = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='eroindex   ')  basin(1:n)%eroindex     = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='close_w    ')  basin(1:n)%closewater   = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='lakedataid ')  lakedataid(1:n)         = xi(1:n,iindex(i))
      IF(str(i)(1:letters)=='mrratck_noi')  rivratknoice(1:n)      = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrratcp_noi')  rivratpnoice(1:n)      = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrratck_ice')  rivratkice(1:n)        = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrratcp_ice')  rivratpice(1:n)        = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='mrratcw0   ')  basin(1:n)%mrivc(1)   = xr(1:n,rindex(i))
      IF(str(i)(1:letters)=='weight_sub ')THEN
        IF(.NOT.ALLOCATED(subweightcrit)) ALLOCATE(subweightcrit(n))
        subweightcrit(1:n) = xr(1:n,rindex(i))
      ENDIF
    ENDDO

    !Check groundwater flow direction      
    IF(SUM(pathsubid(1:n)%grw1)==0) pathsubid(1:n)%grw1 = pathsubid(1:n)%main
    !Calculate number of parameter regions
    DO i=1,nregiondivisions
      nregions(i) = MAXVAL(basin(1:n)%parregion(i))
    ENDDO
    !Set which subbasins will use parameter for ilakecatch
    IF(.NOT.ALLOCATED(simulate%ilakecatpar)) ALLOCATE(simulate%ilakecatpar(n))
    simulate%ilakecatpar(1:n) = (basin(1:n)%ilakecatch<0.)
    !Set 
    DO i = 1,n
      !Calculate rural household loads (loc load in GeoData) or zeroing the loads.
      load(i)%locconc = 0.  !default/initialise
      IF(i_in>0)THEN
        load(i)%locconc(i_in) = localsource(i,3)*localsource(i,4)
        load(i)%locconc(i_on) = localsource(i,3)*(1. - localsource(i,4))
      ENDIF
      IF(i_sp>0)THEN
        load(i)%locconc(i_sp) = localsource(i,1)*localsource(i,2)
        load(i)%locconc(i_pp) = localsource(i,1)*(1. - localsource(i,2))
      ENDIF
      IF(i_t1>0) load(i)%locconc(i_t1) = localsource(i,5)
      IF(i_t2>0) load(i)%locconc(i_t2) = localsource(i,6)
      IF(i_ss>0)THEN
        load(i)%locconc(i_ss) = localsource(i,7)*localsource(i,8)
        load(i)%locconc(i_ae) = localsource(i,7)*(1. - localsource(i,8))
      ENDIF
      !Recalculate river rating curve parameters for water level calculations
      !q=k*(w-w0)**p -> w=a*q**b+w0,a=(1/k)**b,b=1/p
      IF(rivratknoice(i)>0.)THEN
        IF(rivratpnoice(i)>0.) basin(i)%mrivc(3) = 1./rivratpnoice(i)
        basin(i)%mrivc(2) = (1./rivratknoice(i))**basin(i)%mrivc(3)
        basin(i)%mrivc(5) = basin(i)%mrivc(3)
        basin(i)%mrivc(4) = basin(i)%mrivc(2)
        IF(rivratkice(i)>0.)THEN
          IF(rivratpice(i)>0.) basin(i)%mrivc(5) = 1./rivratpice(i)
          basin(i)%mrivc(4) = (1./rivratkice(i))**basin(i)%mrivc(5)
        ENDIF
      ENDIF
    ENDDO
    conductbasinlocsoil = locsoil_found
    !Check subbasin criteria weight allocated and set
    IF(.NOT.ALLOCATED(subweightcrit))THEN
      ALLOCATE(subweightcrit(n))
      subweightcrit(1:n) = 1.   !default
    ENDIF
    WRITE(6,*) 'Geographical information loaded'


  END SUBROUTINE read_and_calc_basindata

  !>\brief Gets the information about nutrient point sources from PointSourceData.txt
  !>Recalculate into form suitable for model. Set permanent loads.
  !!
  !>\b Consequences Module modvar variable psinfo or load is set. Module worldvar variable 
  !>psdates may be allocated and set.
  !>
  !> \b Reference ModelDescription Chapter 5 (Point sources)
  !------------------------------------------------------------------------------
  SUBROUTINE load_pointsourcedata(dir,infile,n,status) 

    USE WORLDVAR, ONLY : steplen, &
                         readpstime, &
                         bdate, &
                         sdate, &
                         i_t,i_m,i_y, &
                         fileunit_psts, &
                         maxcharpath, &
                         dailypsfile,monthlypsfile,yearlypsfile, & !OUT
                         psdates, &    !OUT
                         bpsdate, &    !OUT
                         epsdate, &    !OUT
                         pstscol, & !OUT
                         psdates    !OUT
    USE MODVAR, ONLY : basin,       &
                       i_in,i_on,i_sp,i_pp, &
                       i_t1,i_t2,i_ss,i_ae, &
                       max_pstype, &
                       numsubstances, &
                       simulate, &
                       substance_name, &
                       timesteps_per_day, &
                       absinfo, & !OUT
                       absload, & !OUT
                       npsused,nabsused, & !OUT
                       psinfo, &  !OUT
                       psload, &  !OUT
                       load             !OUT
    USE READWRITE_ROUTINES, ONLY : find_reorder_index, &
                                   prepare_read_matrix, &
                                   check_obs_timeperiod, &
                                   read_headings_pointsource_timeseries
    USE CONVERT, ONLY : get_lower_case
 
    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir         !<File directory
    CHARACTER (LEN=*), INTENT(IN) :: infile      !<Name of pointsource file to be read (PointSourceData.txt)
    INTEGER, INTENT(IN)  :: n                    !<Number of subbasins
    INTEGER, INTENT(OUT) :: status               !<Error status
    
    !Local constants
    INTEGER, PARAMETER :: dim = 50
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    LOGICAL pexist
    LOGICAL notimefound
    INTEGER i,j,k,kps,kab
    INTEGER nrows
    INTEGER maxdates
    INTEGER,ALLOCATABLE :: aindex(:)                  !Index connectingPS-row to basinindex
    INTEGER,ALLOCATABLE :: readsubid(:)
    INTEGER,ALLOCATABLE :: readpsid(:)
    INTEGER,ALLOCATABLE :: colpsid(:)
    INTEGER,ALLOCATABLE :: readpstype(:)
    INTEGER,ALLOCATABLE :: readlocation(:)
    REAL    help                       !Total load
    REAL    onetspday                  !Reciprocal of timesteps per day
    REAL,ALLOCATABLE :: pointsource(:,:) !Read pointsource info (subbasins,column)
    CHARACTER(LEN=letters),ALLOCATABLE :: colname(:)  !Heading 
    CHARACTER(LEN=50),ALLOCATABLE :: readdate1(:)  !String date read from file
    CHARACTER(LEN=50),ALLOCATABLE :: readdate2(:)  !String date read from file
    CHARACTER(LEN=maxcharpath) :: pstsfilename  !Filename for PointSourceTimeSeries.txt
    TYPE(DateType) :: onedate                 !One date
    TYPE(DateType) :: fbdate, fedate          !File begin and end dates
    TYPE(DateType),ALLOCATABLE :: psdateslocal(:)  !Array of dates from file

    !Start of subroutine
    status = 0

    !Read PointsourceData.txt, subroutine will allocate readsubid etc.
    CALL read_pointsourcedata(dir,infile,nrows,status,pexist,readsubid,readpsid,readpstype,readlocation,pointsource,readdate1,readdate2) 
    IF(.NOT.pexist)THEN
      !DO i=1,n    !Deallocate variables for loads
      !  DEALLOCATE(load(i)%psvol)
      !  DEALLOCATE(load(i)%psload)
      !ENDDO
      RETURN
    ENDIF
    IF(status/=0)RETURN
    
    !>If periodically constant pointsources are used
    IF(.NOT.readpstime)THEN
      !>\li Allocate and initiate variables 
      IF(.NOT.ALLOCATED(aindex)) ALLOCATE(aindex(nrows))
      DO i = 1,n
        ALLOCATE(load(i)%psvol(max_pstype))
        ALLOCATE(load(i)%psload(max_pstype,numsubstances))
        load(i)%psvol(:) = 0.   !Zeroing the point source loads for accumulation
        load(i)%psload(:,:) = 0.
      ENDDO
 
      !>\li Collect dates where point source load changes; fromdata and todate+1
      maxdates = 0
      IF(.NOT.ALLOCATED(psdateslocal)) ALLOCATE(psdateslocal(2*nrows))
      DO i = 1,nrows
        IF(readdate1(i)(1:1)=='0')THEN
        ELSE
          CALL string_convert_to_datetype(readdate1(i),onedate)
          CALL add_date_to_array(onedate,2*nrows,maxdates,psdateslocal)
        ENDIF
        IF(readdate2(i)(1:1)=='0')THEN
        ELSE
          CALL string_convert_to_datetype(readdate2(i),onedate)
          onedate = AddDates(onedate,steplen)
          CALL add_date_to_array(onedate,2*nrows,maxdates,psdateslocal)
        ENDIF
      ENDDO
      IF(maxdates>0)THEN
        IF(.NOT.ALLOCATED(psdates)) ALLOCATE(psdates(maxdates))
        psdates=psdateslocal(1:maxdates)
      ENDIF
      DEALLOCATE(psdateslocal)
    
      CALL find_reorder_index(infile,nrows,readsubid,n,basin%subid,.FALSE.,aindex,status) !status=1 is not an error
      IF(.NOT.ALLOCATED(psdates))THEN
        !>\li Calculate permanent point source loads
        onetspday = 1./REAL(timesteps_per_day)
        DO j=1,nrows
          IF(aindex(j)>0)THEN !Point sources not in model are ignored
            IF(readdate1(j)=='0'.AND.readdate2(j)=='0')THEN
              i=aindex(j)
              IF(pointsource(j,5)<0.)THEN !Negative point source, abstraction
                load(i)%abstrvol = - pointsource(j,5)/86400.  !m3/d->m3/s
                load(i)%abstrind = readlocation(j)
              ELSEIF(pointsource(j,5)>0.)THEN
                k=readpstype(j)
!                IF(k>0)THEN
                IF(k>0 .AND. (readlocation(j)>4.OR.readlocation(j)<=0))THEN   !Try to avoid T1 ps !This means old PS-files may not work automatically.
                  load(i)%psvol(k) = load(i)%psvol(k) + pointsource(j,5)/86400.  !m3/d->m3/s
                  IF(i_in>0)THEN
                    help = pointsource(j,3)*pointsource(j,5)*onetspday*1.E-3    !kg TN/timestep
                    load(i)%psload(k,i_in) = load(i)%psload(k,i_in) + help*pointsource(j,4)
                    load(i)%psload(k,i_on) = load(i)%psload(k,i_on) + help*(1.-pointsource(j,4))
                  ENDIF
                  IF(i_sp>0)THEN
                    help = pointsource(j,1)*pointsource(j,5)*onetspday*1.E-3    !kg TP/timestep
                    load(i)%psload(k,i_sp) = load(i)%psload(k,i_sp) + help*pointsource(j,2)
                    load(i)%psload(k,i_pp) = load(i)%psload(k,i_pp) + help*(1.-pointsource(j,2))
                  ENDIF
                  IF(simulate%substance(i_t1))THEN
                    help = pointsource(j,6)*pointsource(j,5)*onetspday*1.E-3    !amount T1/timestep
                    load(i)%psload(k,i_t1) = load(i)%psload(k,i_t1) + help
                  ENDIF
                  IF(simulate%substance(i_t2))THEN
                    help = pointsource(j,7)*pointsource(j,5)*onetspday*1.E-3    !amount T2/timestep
                    load(i)%psload(k,i_t2) = load(i)%psload(k,i_t2) + help
                  ENDIF
                  IF(simulate%substance(i_ss))THEN
                    help = pointsource(j,8)*pointsource(j,5)*onetspday*1.E-3    !amount TS/timestep
                    load(i)%psload(k,i_ss) = load(i)%psload(k,i_ss) + help*pointsource(j,9)
                    load(i)%psload(k,i_ae) = load(i)%psload(k,i_ae) + help*(1.-pointsource(j,9))
                  ENDIF
                ELSEIF(k==-1)THEN !Negative point source, abstraction
                  load(i)%abstrvol = ABS(pointsource(j,5)/86400.)  !m3/d->m3/s
                  load(i)%abstrind = readlocation(j)
                ELSE
                  WRITE(6,*) 'Warning: An permanent point source without correct ps_type/ps_source found'
                  WRITE(6,*) 'Warning: This is not a recommended use of PointSourceData.txt'
                  WRITE(6,*) 'Warning: Point source may be ignored!'
                ENDIF
              ENDIF
            ENDIF
          ELSE
            WRITE(6,*) 'Point source in subid',readsubid(j),'ignored, not in model domain.'
          ENDIF
        ENDDO
      ENDIF
    
      !>\li Deallocate load variable for subbasins without point sources
      DO i = 1,n
        pexist = .FALSE.
        DO j= 1,nrows
          IF(aindex(j)==i.AND.pointsource(j,5)>0.)THEN
            pexist = .TRUE.
            EXIT
          ENDIF
        ENDDO
        IF(.NOT.pexist)THEN
          DEALLOCATE(load(i)%psvol)
          DEALLOCATE(load(i)%psload)
        ENDIF
      ENDDO
    
    !>If timeseries of pointsources is used:
    ELSEIF(readpstime)THEN
    
      !!Deallocate variables for periodical and constant loads
      !DO i=1,n
      !  DEALLOCATE(load(i)%psvol)
      !  DEALLOCATE(load(i)%psload)
      !ENDDO
      
      !>\li Check if PointSourceTimeSerie.txt exist as assumed from read_pointsourcedata.
      pstsfilename = TRIM(dir)//'PSDailySeries.txt'
      INQUIRE(FILE=TRIM(pstsfilename),EXIST=pexist)
      IF(pexist)THEN
        dailypsfile = .TRUE.
        monthlypsfile = .FALSE.
        yearlypsfile = .FALSE.
      ELSE
        dailypsfile = .FALSE.
        pstsfilename = TRIM(dir)//'PSMonthlySeries.txt'
        INQUIRE(FILE=TRIM(pstsfilename),EXIST=pexist)
        IF(pexist)THEN
          monthlypsfile = .TRUE.
          yearlypsfile = .FALSE.
        ELSE
          monthlypsfile = .FALSE.
          pstsfilename = TRIM(dir)//'PSYearlySeries.txt'
          INQUIRE(FILE=TRIM(pstsfilename),EXIST=pexist)
          IF(pexist)THEN
            yearlypsfile = .TRUE.
          ELSE
            WRITE(6,*) 'ERROR: File '//TRIM(pstsfilename)//' not found, neither is PSDailySeries.txt'
            WRITE(6,*) 'ERROR: It was expected based on information in PointSourceData.txt.'
            STOP 1
          ENDIF
        ENDIF
      ENDIF

      !>\li Allocate find corresponding index for subid
      IF(.NOT.ALLOCATED(aindex)) ALLOCATE(aindex(nrows))
      CALL find_reorder_index(infile,nrows,readsubid,n,basin%subid,.FALSE.,aindex,status) !status=1 is not an error
      
      !>\li Calculate the number of used positive and negative pointsources, and allocate variable to hold pointsource information
      npsused = 0
      nabsused = 0
      DO j=1,nrows
        IF(aindex(j)>0)THEN !Point sources not in model are ignored
          IF(readpstype(j)>0.AND.readpstype(j)<=max_pstype)THEN !Point sources without type is abstraction
            npsused = npsused + 1
          ELSEIF(readpstype(j)==-1)THEN
            nabsused = nabsused + 1
          ENDIF
        ENDIF
      ENDDO
      
      IF(npsused>0.OR.nabsused>0)THEN
        IF(npsused>0 .AND. .NOT.ALLOCATED(psinfo)) ALLOCATE(psinfo(npsused))
        IF(nabsused>0 .AND. .NOT.ALLOCATED(absinfo)) ALLOCATE(absinfo(nabsused))
      
        !>\li Set pointsource information variable
        kps = 0; kab = 0
        DO j=1,nrows
          IF(aindex(j)>0)THEN
            IF(readpstype(j)>0.AND.readpstype(j)<=max_pstype)THEN
              kps = kps + 1
              psinfo(kps)%subindex = aindex(j)
              psinfo(kps)%subid = readsubid(j)
              psinfo(kps)%psid = readpsid(j)
              psinfo(kps)%pstype = readpstype(j)
              psinfo(kps)%sw_code = readlocation(j)
              ALLOCATE(psinfo(kps)%conccol(numsubstances))
              psinfo(kps)%conccol = 0
            ELSEIF(readpstype(j)==-1)THEN
              kab = kab + 1
              absinfo(kab)%subindex = aindex(j)
              absinfo(kab)%subid = readsubid(j)
              absinfo(kab)%psid = readpsid(j)
              absinfo(kab)%sw_code = readlocation(j)
            ELSE
              WRITE(6,*) 'WARNING: Pointsource with psid:',readpsid(j)
              WRITE(6,*) 'WARNING: will not be used, ps_type is not valid.'
            ENDIF
          ENDIF
        ENDDO
      
        !>Check if timeseries are correct and present, and calculate their index coupling
        CALL count_data_cols(fileunit_psts,TRIM(pstsfilename),0,pstscol,status)
        IF(status.NE.0) RETURN
        pstscol = pstscol-1 !number of columns with data in file
        IF(pstscol<=0)THEN
          WRITE(6,*) 'ERROR: no data found in',TRIM(pstsfilename)
          status = 1
          RETURN
        ENDIF
      
        ALLOCATE(colname(pstscol))
        ALLOCATE(colpsid(pstscol))
        CALL read_headings_pointsource_timeseries(fileunit_psts,TRIM(pstsfilename),pstscol,    &
              colname,colpsid,status) 
        IF(status.NE.0) RETURN
        DO j = 1,pstscol
          DO k = 1,npsused
            IF(psinfo(k)%psid==colpsid(j))THEN
              IF(get_lower_case(4,colname(j)(1:4))=='flow') psinfo(k)%flowcol = j
              DO i = 1,numsubstances
                IF(get_lower_case(6,colname(j)(1:6))==get_lower_case(2,substance_name(i))//'conc') psinfo(k)%conccol(i) = j
              ENDDO
              EXIT
            ENDIF
          ENDDO
          DO k = 1,nabsused
            IF(absinfo(k)%psid==colpsid(j))THEN
              IF(get_lower_case(4,colname(j)(1:4))=='flow') absinfo(k)%flowcol = j
              EXIT
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(colname)
        DEALLOCATE(colpsid)
        
        !>\li Check time periods of pointsource time series
        IF(dailypsfile)THEN
          CALL check_obs_timeperiod(fileunit_psts,TRIM(pstsfilename),2,i_t,   &
                bdate,sdate,fbdate,fedate,notimefound,status)
          bpsdate = bdate
          epsdate = sdate
          IF(status==2)THEN  !Shorter time period
            status = 0
            IF(bdate.GT.fedate .OR. sdate.LT.fbdate)THEN    !no overlap
              dailypsfile = .FALSE.
              npsused = 0
              nabsused = 0
              IF(ALLOCATED(psinfo)) DEALLOCATE(psinfo)
              RETURN
            ELSE                                      !some overlap
              bpsdate = MaxDate(bdate,fbdate)
              epsdate = MinDate(sdate,fedate)
            ENDIF
          ELSEIF(status.NE.0)THEN
            RETURN
          ENDIF

          !>\li Prepare PointSourceTimeSeries.txt to be read.
          CALL prepare_read_matrix(fileunit_psts,TRIM(pstsfilename),2,bpsdate,status)
          IF(status.NE.0) RETURN
          
        ELSEIF(monthlypsfile)THEN
          CALL check_obs_timeperiod(fileunit_psts,TRIM(pstsfilename),2,i_m,   &
                bdate,sdate,fbdate,fedate,notimefound,status)
          bpsdate = bdate
          epsdate = sdate
          IF(status==2)THEN  !Shorter time period
            status = 0
            IF(bdate.GT.fedate .OR. sdate.LT.fbdate)THEN    !no overlap
              monthlypsfile = .FALSE.
              npsused = 0
              nabsused = 0
              IF(ALLOCATED(psinfo)) DEALLOCATE(psinfo)
              RETURN
            ELSE                                      !some overlap
              bpsdate = MaxDate(bdate,fbdate)
              epsdate = MinDate(sdate,fedate)
            ENDIF
          ELSEIF(status.NE.0)THEN
            RETURN
          ENDIF

          !>\li Prepare PSMonthlySeries.txt to be read.
          CALL prepare_read_matrix(fileunit_psts,TRIM(pstsfilename),2,bpsdate,status)
          IF(status.NE.0) RETURN
          
        ELSEIF(yearlypsfile)THEN
          CALL check_obs_timeperiod(fileunit_psts,TRIM(pstsfilename),2,i_y,   &
                bdate,sdate,fbdate,fedate,notimefound,status)
          bpsdate = bdate
          epsdate = sdate
          IF(status==2)THEN  !Shorter time period
            status = 0
            IF(bdate.GT.fedate .OR. sdate.LT.fbdate)THEN    !no overlap
              yearlypsfile = .FALSE.
              npsused = 0
              nabsused = 0
              IF(ALLOCATED(psinfo)) DEALLOCATE(psinfo)
              RETURN
            ELSE                                      !some overlap
              bpsdate = MaxDate(bdate,fbdate)
              epsdate = MinDate(sdate,fedate)
            ENDIF
          ELSEIF(status.NE.0)THEN
            RETURN
          ENDIF
        
          !>\li Prepare PSYearlySeries.txt to be read.
          CALL prepare_read_matrix(fileunit_psts,TRIM(pstsfilename),2,bpsdate,status)
          IF(status.NE.0) RETURN
          
        ENDIF

        !Allocate variable to hold current pointsource data
        IF(npsused>0)THEN
          IF(.NOT.ALLOCATED(psload)) ALLOCATE(psload(npsused))
          DO i = 1, npsused
            ALLOCATE(psload(i)%load(numsubstances))
          ENDDO
        ENDIF
        IF(nabsused>0)THEN
          IF(.NOT.ALLOCATED(absload)) ALLOCATE(absload(nabsused))
        ENDIF
        WRITE(6,*) 'File ready: ', TRIM(pstsfilename)

      ELSE
        WRITE(6,*) 'WARNING: No point sources or abstractions found in PointSourceData.txt.'
      ENDIF
    ENDIF

    !Deallocate local arrays
    IF(ALLOCATED(aindex)) DEALLOCATE(aindex)
    IF(ALLOCATED(readsubid)) DEALLOCATE(readsubid)
    IF(ALLOCATED(readpsid)) DEALLOCATE(readpsid)
    IF(ALLOCATED(readpstype)) DEALLOCATE(readpstype)
    IF(ALLOCATED(readlocation)) DEALLOCATE(readlocation)
    IF(ALLOCATED(readdate1)) DEALLOCATE(readdate1)
    IF(ALLOCATED(readdate2)) DEALLOCATE(readdate2)
    IF(ALLOCATED(pointsource)) DEALLOCATE(pointsource)
    WRITE(6,*) 'PointSourceData information loaded'

  END SUBROUTINE load_pointsourcedata

  !>\brief Reads the matrix of point source data from the file. 
  !!
  !> \b Reference ModelDescription Chapter 5 (Point sources)
  !------------------------------------------------------------------------------
  SUBROUTINE read_pointsourcedata(dir,infile,nrows,status,fexist,readsubid,readpsid,readpstype,readlocation,pointsource,readdate1,readdate2) 

    USE WORLDVAR, ONLY : i_str,       &
                         i_intg,      &
                         i_real,      &
                         fileunit_temp
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_pointsourcedata, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir         !<File directory
    CHARACTER (LEN=*), INTENT(IN) :: infile      !<Name of pointsource file to be read (PointSourceData.txt)
    INTEGER, INTENT(OUT) :: nrows                !<Number of rows in file
    INTEGER, INTENT(OUT) :: status               !<Error status
    LOGICAL, INTENT(OUT) :: fexist               !<Status of file exist
    INTEGER,ALLOCATABLE, INTENT(INOUT) :: readsubid(:)    !<subid if point sources
    INTEGER,ALLOCATABLE, INTENT(INOUT) :: readpsid(:)     !<pointsource id
    INTEGER,ALLOCATABLE, INTENT(INOUT) :: readpstype(:)   !<type of point source
    INTEGER,ALLOCATABLE, INTENT(INOUT) :: readlocation(:)   !<location of abstraction/tracer outlet
    REAL,ALLOCATABLE, INTENT(INOUT) :: pointsource(:,:)   !<Read pointsource info (subbasins,column)
    CHARACTER(LEN=50),ALLOCATABLE, INTENT(INOUT) :: readdate1(:)  !<From date of point source
    CHARACTER(LEN=50),ALLOCATABLE, INTENT(INOUT) :: readdate2(:)  !<To date of point source
    
    !Local constants
    INTEGER, PARAMETER :: dim = 50      !Max number of letters in skipped column data
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    INTEGER i
    INTEGER ncols,mcols
    INTEGER,ALLOCATABLE :: xi(:,:)               !Integer data read from file
    INTEGER,ALLOCATABLE :: code(:)               !Code for column variable
    INTEGER,ALLOCATABLE :: rindex(:)             !Index for column real variables
    INTEGER,ALLOCATABLE :: iindex(:)             !Index for column integer variables
    INTEGER,ALLOCATABLE :: sindex(:)             !Index for column string variables
    REAL,ALLOCATABLE :: xr(:,:)               !Real data read from file
    CHARACTER(LEN=dim),ALLOCATABLE :: xs(:,:)  !String data read from file
    CHARACTER(LEN=letters),ALLOCATABLE :: str(:)   !Content string
    CHARACTER(LEN=100) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    !Start of subroutine
    status = 0
    
    !Check existence of file
    INQUIRE(FILE=TRIM(dir)//infile,EXIST=fexist)
    IF(.NOT.fexist) RETURN
    WRITE(6,*) 'File opened: ', TRIM(dir)//infile
    
    !Count number of columns and rows in PointSourceData
    CALL count_data_cols(fileunit_temp,TRIM(dir)//infile,0,ncols,status)
    IF(status/=0)RETURN
    CALL count_data_rows(fileunit_temp,TRIM(dir)//infile,1,nrows,status)
    IF(status/=0)RETURN

    !Allocate variables for holding file data
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(ncols))
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(sindex)) ALLOCATE(sindex(ncols))
    IF(.NOT.ALLOCATED(readsubid)) ALLOCATE(readsubid(nrows))
    IF(.NOT.ALLOCATED(readpsid)) ALLOCATE(readpsid(nrows))
    IF(.NOT.ALLOCATED(readpstype)) ALLOCATE(readpstype(nrows))
    IF(.NOT.ALLOCATED(readlocation)) ALLOCATE(readlocation(nrows))
    IF(.NOT.ALLOCATED(readdate1)) ALLOCATE(readdate1(nrows))
    IF(.NOT.ALLOCATED(readdate2)) ALLOCATE(readdate2(nrows))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,ncols))
    IF(.NOT.ALLOCATED(xs)) ALLOCATE(xs(nrows,ncols))
    IF(.NOT.ALLOCATED(pointsource)) ALLOCATE(pointsource(nrows,9))
    readpstype = 0  !default point source is first
    readlocation = 0
    pointsource = 0.
    readdate1 = '0'
    readdate2 = '0'
    
    !Read PointSourceData.txt file
    OPEN(UNIT = fileunit_temp,FILE = TRIM(dir)//infile, STATUS = 'old', ACTION='read')     !Open PointSourceData-file

    !Reads the column headings from file
    CALL read_column_headings(fileunit_temp,ncols,letters,str,mcols,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(TRIM(dir)//infile)
      propagate_str = 'reading file: '//TRIM(TRIM(dir)//infile)
      CALL propagate_external_msg(e_pointsourcedata,e_error,propagate_str)
      RETURN
    ENDIF

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*ncols)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !Code column for variable type
    code=i_str    !string, ignore
    DO i = 1,ncols
      IF(str(i)(1:letters)=='subid     ') code(i) = i_intg
      IF(str(i)(1:letters)=='psid      ') code(i) = i_intg
      IF(str(i)(1:letters)=='ps_source ') code(i) = i_intg
      IF(str(i)(1:letters)=='ps_type   ') code(i) = i_intg
      IF(str(i)(1:letters)=='ps_tpconc ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_spfrac ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_tnconc ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_infrac ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_vol    ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_t1     ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_t2     ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_tsconc ') code(i) = i_real
      IF(str(i)(1:letters)=='ps_ssfrac ') code(i) = i_real
      IF(code(i) == i_str.AND.(str(i)(1:letters)/='fromdate  '.AND.str(i)(1:letters)/='todate    ')) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_pointsourcedata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Read all data values
    CALL read_basindata6(fileunit_temp,TRIM(dir)//infile,ncols,nrows,ncols,dim,code,rindex,iindex,sindex,xi,xr,xs)
    CLOSE(UNIT=fileunit_temp)

    !Move data from read matrix to pointsource variables
    DO i = 1,ncols
      IF(str(i)(1:letters)=='fromdate  ') readdate1(1:nrows)     = xs(1:nrows,sindex(i)) !String date
      IF(str(i)(1:letters)=='todate    ') readdate2(1:nrows)     = xs(1:nrows,sindex(i))
      IF(str(i)(1:letters)=='subid     ') readsubid(1:nrows)     = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='psid      ') readpsid(1:nrows)      = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='ps_type   ') readpstype(1:nrows)    = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='ps_source ') readlocation(1:nrows)  = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='ps_tpconc ') pointsource(1:nrows,1) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_spfrac ') pointsource(1:nrows,2) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_tnconc ') pointsource(1:nrows,3) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_infrac ') pointsource(1:nrows,4) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_vol    ') pointsource(1:nrows,5) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_t1     ') pointsource(1:nrows,6) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_t2     ') pointsource(1:nrows,7) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_tsconc ') pointsource(1:nrows,8) = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ps_ssfrac ') pointsource(1:nrows,9) = xr(1:nrows,rindex(i))
    ENDDO
    
    !Deallocate local arrays
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(sindex)) DEALLOCATE(sindex)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    IF(ALLOCATED(xs)) DEALLOCATE(xs)

  END SUBROUTINE read_pointsourcedata

  !>\brief Gets the information about temporary point sources from PointSourceData.txt.
  !>Recalculate into form suitable for model.
  !!
  !>\b Consequences Module modvar variable load and t1load are changed
  !>
  !> \b Reference ModelDescription Chapter 5 (Point sources)
  !------------------------------------------------------------------------------
  SUBROUTINE get_current_pointsources(dir,infile,ns,time,status) 

    USE WORLDVAR, ONLY : i_str,       &
                         i_intg,      &
                         i_real,      &
                         bdate,  &
                         psdates,     &
                         fileunit_temp
    USE MODVAR, ONLY : timesteps_per_day, &
                       simulate, &
                       i_in,i_on,i_sp,i_pp,i_ae, &
                       i_t1,i_t2,i_ss,   &
                       max_pstype,  &
                       basin,       &
                       load,        & !OUT
                       tload,       & !OUT
                       tloadexist     !OUT
    USE READWRITE_ROUTINES, ONLY : find_reorder_index
 
    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir         !<File directory
    CHARACTER (LEN=*), INTENT(IN) :: infile      !<Name of pointsource file to be read (PointSourceData.txt)
    INTEGER, INTENT(IN)  :: ns                   !<Number of subbasins (submodel)
     TYPE(DateType), INTENT(IN) :: time           !<Current time (e.g. 2005-01-26 18:00)
    INTEGER, INTENT(OUT) :: status               !<Error status
    
    !Local constants
    INTEGER, PARAMETER :: dim = 50
    
    !Local variables
    LOGICAL fexist                     !Status of file exist
    INTEGER i,j,k
    INTEGER nrows
    INTEGER maxdates
    INTEGER numt1
    INTEGER,ALLOCATABLE :: aindex(:)                  !Index connectingPS-row to basinindex
    INTEGER,ALLOCATABLE :: readsubid(:)
    INTEGER,ALLOCATABLE :: readpsid(:)
    INTEGER,ALLOCATABLE :: readpstype(:)
    INTEGER,ALLOCATABLE :: readlocation(:)
    INTEGER,ALLOCATABLE :: tloadindexlocal(:,:)
    REAL    help                       !Total load
    REAL    onetspday                  !Reciprocal of timesteps per day
    REAL,ALLOCATABLE :: pointsource(:,:) !Read pointsource info (subbasins,column)
    REAL,ALLOCATABLE :: tloadlocal(:,:)  !Read pointsource info on T1 sources
    CHARACTER(LEN=50),ALLOCATABLE :: readdate1(:)  !String date read from file
    CHARACTER(LEN=50),ALLOCATABLE :: readdate2(:)  !String date read from file
    TYPE(DateType),ALLOCATABLE :: psdateslocal(:,:)  !Array of dates from file

    !Start of subroutine
    status = 0
    
    !Check if beginning of simulation or current time in change array
    IF(time>bdate)THEN
      maxdates = SIZE(psdates)
      DO i = 1,maxdates
        IF(time==psdates(i)) EXIT
      ENDDO
      IF(i>maxdates) RETURN
    ENDIF
    
    !Read PointsourceData.txt, subroutine will allocate readsubid etc.
    CALL read_pointsourcedata(dir,infile,nrows,status,fexist,readsubid,readpsid,readpstype,readlocation,pointsource,readdate1,readdate2) 
    IF(status/=0)RETURN

    !Allocate and initiate variables 
    IF(.NOT.ALLOCATED(aindex)) ALLOCATE(aindex(nrows))
    IF(.NOT.ALLOCATED(psdateslocal)) ALLOCATE(psdateslocal(nrows,2))
    IF(.NOT.ALLOCATED(tloadindexlocal)) ALLOCATE(tloadindexlocal(2,nrows))
    IF(.NOT.ALLOCATED(tloadlocal)) ALLOCATE(tloadlocal(3,nrows))
    DO i=1,ns    !Zeroing the point source loads for renewed accumulation
      IF(ALLOCATED(load(i)%psvol)) load(i)%psvol(:) = 0.
      IF(ALLOCATED(load(i)%psload)) load(i)%psload(:,:) = 0.
    ENDDO

    !Transform dates to DateType
    DO j=1,nrows
      IF(readdate1(j)=='0')THEN
        psdateslocal(j,1)=datetype(3000,0,0,0,0)
      ELSE
        CALL string_convert_to_datetype(readdate1(j),psdateslocal(j,1))
      ENDIF
      IF(readdate2(j)=='0')THEN
        psdateslocal(j,2)=datetype(0,0,0,0,0)
      ELSE
        CALL string_convert_to_datetype(readdate2(j),psdateslocal(j,2))
      ENDIF
    ENDDO

    !Calculate and add permanent point source loads and 
    !temporary point source load discharging today
    numt1 = 0
    onetspday = 1./REAL(timesteps_per_day)
    CALL find_reorder_index(infile,nrows,readsubid,ns,basin%subid,.FALSE.,aindex,status) !status=1 is not an error
    DO j=1,nrows
      IF(aindex(j)>0)THEN !Point sources not in (sub)model are ignored
        i=aindex(j)
        IF(readdate1(j)=='0'.AND.readdate2(j)=='0'.OR. &              !permanent point source
          (readdate1(j)=='0'.AND.psdateslocal(j,2)>=time) .OR. &      !ending point source
          (psdateslocal(j,1)<=time.AND.readdate2(j)=='0') .OR. &      !starting point source
          (psdateslocal(j,1)<=time.AND.psdateslocal(j,2)>=time))THEN  !temporary point source
          IF(pointsource(j,5)<0.)THEN   !Negative point source, abstraction
            load(i)%abstrvol = - pointsource(j,5)/86400.  !m3/d->m3/s
            load(i)%abstrind = readlocation(j)
          ELSEIF(pointsource(j,5)>0.)THEN
            k=readpstype(j)
            IF(k>0)THEN
              load(i)%psvol(k) = load(i)%psvol(k) + pointsource(j,5)/86400.  !m3/d->m3/s
              IF(i_in>0)THEN
                help = pointsource(j,3)*pointsource(j,5)*onetspday*1.E-3    !kg TN/timestep
                load(i)%psload(k,i_in) = load(i)%psload(k,i_in) + help*pointsource(j,4)
                load(i)%psload(k,i_on) = load(i)%psload(k,i_on) + help*(1.-pointsource(j,4))
              ENDIF
              IF(i_sp>0)THEN
                help = pointsource(j,1)*pointsource(j,5)*onetspday*1.E-3    !kg TP/timestep
                load(i)%psload(k,i_sp) = load(i)%psload(k,i_sp) + help*pointsource(j,2)
                load(i)%psload(k,i_pp) = load(i)%psload(k,i_pp) + help*(1.-pointsource(j,2))
              ENDIF
              IF(simulate%substance(i_t1))THEN
                help = pointsource(j,6)*pointsource(j,5)*onetspday*1.E-3    !amount T1/timestep
                load(i)%psload(k,i_t1) = load(i)%psload(k,i_t1) + help
              ENDIF
              IF(simulate%substance(i_t2))THEN
                help = pointsource(j,7)*pointsource(j,5)*onetspday*1.E-3    !amount T1/timestep
                load(i)%psload(k,i_t2) = load(i)%psload(k,i_t2) + help
              ENDIF
              IF(simulate%substance(i_ss))THEN
                help = pointsource(j,8)*pointsource(j,5)*onetspday*1.E-3    !amount TS/timestep
                load(i)%psload(k,i_ss) = load(i)%psload(k,i_ss) + help*pointsource(j,9)
                load(i)%psload(k,i_ae) = load(i)%psload(k,i_ae) + help*(1.-pointsource(j,9))
              ENDIF
            ELSEIF(readlocation(j)>0)THEN
              IF(simulate%substance(i_t1) .OR. simulate%substance(i_t2))THEN
                numt1 = numt1 + 1
                tloadlocal(1,numt1) = pointsource(j,5)/86400.  !m3/d->m3/s
                tloadlocal(2,numt1) = pointsource(j,6)
                tloadlocal(3,numt1) = pointsource(j,7)
                tloadindexlocal(1,numt1) = readlocation(j)
                tloadindexlocal(2,numt1) = i
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        WRITE(6,*) 'Point source in subid',readsubid(j),'ignored, not in model domain.'
      ENDIF
    ENDDO

    !Set T1 loads if present
    IF(numt1>0)THEN
      IF(.NOT.ALLOCATED(tload))THEN
        ALLOCATE(tload(numt1))
        ALLOCATE(tloadexist(ns))
        tloadexist = .FALSE.
      ENDIF
      tload%psvol = tloadlocal(1,1:numt1)
      tload%psconc = tloadlocal(2,1:numt1)
      tload%pstemp = tloadlocal(3,1:numt1)
      tload%sw_code = tloadindexlocal(1,1:numt1)
      tload%subindex = tloadindexlocal(2,1:numt1)
      tloadexist(tloadindexlocal(2,1:numt1)) = .TRUE.
    ELSE
      IF(ALLOCATED(tload))THEN
        DEALLOCATE(tload)
        DEALLOCATE(tloadexist)
      ENDIF
    ENDIF
    
    !Deallocate local variables
    IF(ALLOCATED(readsubid)) DEALLOCATE(readsubid)
    IF(ALLOCATED(readpsid)) DEALLOCATE(readpsid)
    IF(ALLOCATED(readpstype)) DEALLOCATE(readpstype)
    IF(ALLOCATED(readlocation)) DEALLOCATE(readlocation)
    IF(ALLOCATED(readdate1)) DEALLOCATE(readdate1)
    IF(ALLOCATED(readdate2)) DEALLOCATE(readdate2)
    IF(ALLOCATED(pointsource)) DEALLOCATE(pointsource)
    IF(ALLOCATED(aindex)) DEALLOCATE(aindex)
    IF(ALLOCATED(psdateslocal)) DEALLOCATE(psdateslocal)
    IF(ALLOCATED(tloadindexlocal)) DEALLOCATE(tloadindexlocal)
    IF(ALLOCATED(tloadlocal)) DEALLOCATE(tloadlocal)
    WRITE(6,*) 'PointSourceData information loaded'
    
  END SUBROUTINE get_current_pointsources

  !>\brief Gets the information about nutrient soil leakage from file.
  !>
  !>Alternative 1: Soil nutrient leakage concentration is read from file and 
  !>will replace the concentration calculated by HYPE soil routines. The runoff
  !>from classes gets the new concentration.
  !!
  !>Alternative 2: Soil nutrient leakage load is read from file. A simlified soil
  !>routine is used.
  !!
  !>\b Consequences Module modvar variable soilleak is allocated and set. 
  !>
  !>\b Reference ModelDescription Chapter 4 (Nutrient soil leakage from outer source)
  !------------------------------------------------------------------------------
  SUBROUTINE load_permanent_soilleakage(dir,n,bd,status) 

    USE WORLDVAR, ONLY : maxcharpath
    USE MODVAR, ONLY : modeloption, &
                       p_soilleakage, &
                       numsubstances,i_t2, &
                       nclass, &
                       slc_ilake,slc_olake, &
                       slc_mriver,slc_lriver, &
                       substance_name, &
                       soilleak, &
                       basin
 
    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir  !<file directory
    INTEGER, INTENT(IN)  :: n             !<number of subbasins
    TYPE(datetype), INTENT(IN) :: bd      !<simulation begin date
    INTEGER, INTENT(OUT) :: status        !<error status
    
    !Local variables
    INTEGER isubst,icl
    INTEGER,ALLOCATABLE :: geoid(:)    !Model subid
    CHARACTER(LEN=maxcharpath) infile  !Name of file

    !>\b Algorithm \n
    !Start of subroutine
    status = 0
    IF(modeloption(p_soilleakage)==0)RETURN
    
    !>Load monthly soilleakage concentrations
    IF(modeloption(p_soilleakage)==1)THEN
      IF(numsubstances==0)THEN
        WRITE(6,*) 'Warning: No substances are simulated. Turning off soil leakage concentration model.'
        modeloption(p_soilleakage)=0
        RETURN
      ENDIF
    
      !>\li Allocate and initiate variables 
      IF(.NOT.ALLOCATED(soilleak%concentration)) ALLOCATE(soilleak%concentration(numsubstances,12,n))
      soilleak%concentration = 0.
      IF(.NOT.ALLOCATED(geoid)) ALLOCATE(geoid(n))
      geoid = basin%subid
      
      !>\li Read data files
      DO isubst=1,numsubstances
        WRITE(infile,'(A12,A2,A4)') 'LeakageData_',substance_name(isubst),'.txt'
        CALL read_monthly_leakage(dir,infile,n,geoid,status,soilleak%concentration(isubst,:,:)) 
        IF(status/=0)RETURN
      ENDDO
      
    !>Load monthly soilleakage loads
    ELSEIF(modeloption(p_soilleakage)==2)THEN
      IF(numsubstances==0)THEN
        WRITE(6,*) 'Warning: No substances are simulated. Turning off soil leakage load model.'
        modeloption(p_soilleakage)=0
        RETURN
      ENDIF
    
      !>\li Allocate and initiate variables 
      IF(.NOT.ALLOCATED(soilleak%load)) ALLOCATE(soilleak%load(numsubstances,nclass,12,n))
      soilleak%load = 0.
      IF(.NOT.ALLOCATED(geoid)) ALLOCATE(geoid(n))
      geoid = basin%subid
      
      !>\li Read data files
      DO isubst=1,numsubstances
        IF(isubst == i_t2) CYCLE  !T2 has no soil load
        DO icl = 1,nclass
          IF(icl==slc_ilake .OR. icl==slc_olake .OR. icl==slc_mriver .OR. icl==slc_lriver) CYCLE
          WRITE(infile,'(A4,A2,A4,I3.3,A4)') 'Load',substance_name(isubst),'_SLC',icl,'.txt'
          CALL read_monthly_leakage(dir,infile,n,geoid,status,soilleak%load(isubst,icl,:,:)) 
          IF(status/=0)RETURN
        ENDDO
      ENDDO
      
    !>Load first year of monthly soilleakage loads
    ELSEIF(modeloption(p_soilleakage)==3)THEN
      IF(numsubstances==0)THEN
        WRITE(6,*) 'Warning: No substances are simulated. Turning off soil leakage load model.'
        modeloption(p_soilleakage)=0
        RETURN
      ENDIF
    
      !>\li Allocate and initiate variables 
      IF(.NOT.ALLOCATED(soilleak%load)) ALLOCATE(soilleak%load(numsubstances,nclass,12,n))
      soilleak%load = 0.
      IF(.NOT.ALLOCATED(geoid)) ALLOCATE(geoid(n))
      geoid = basin%subid
      
      !>\li Read data files
      DO isubst=1,numsubstances
        IF(isubst == i_t2) CYCLE  !T2 has no soil load
        DO icl = 1,nclass
          IF(icl==slc_ilake .OR. icl==slc_olake .OR. icl==slc_mriver .OR. icl==slc_lriver) CYCLE
          WRITE(infile,'(A4,A2,A4,I3.3,A4)') 'Load',substance_name(isubst),'_SLC',icl,'.txt'
          CALL read_first_monthly_leakage(dir,infile,bd,n,geoid,status,soilleak%load(isubst,icl,:,:)) 
          IF(status/=0)RETURN
        ENDDO
      ENDDO
      
    ENDIF
    
    !Finish the subroutine
    IF(ALLOCATED(geoid)) DEALLOCATE(geoid)
    WRITE(6,*) 'Soil leakage data loaded'

  END SUBROUTINE load_permanent_soilleakage

  !>\brief Gets the information about nutrient soil leakage from file.
  !>
  !>Alternative 3: Soil nutrient leakage load is read from file. A simlified soil
  !>routine is used.
  !!
  !>\b Consequences Module modvar variable soilleak is allocated and set. 
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus in land routines
  !>(Nutrient soil leakage from outer source)
  !------------------------------------------------------------------------------
  SUBROUTINE get_current_soilleakage(dir,n,time,status) 

    USE WORLDVAR, ONLY : maxcharpath
    USE MODVAR, ONLY : modeloption, &
                       p_soilleakage, &
                       numsubstances,i_t2, &
                       nclass, &
                       slc_ilake,slc_olake, &
                       slc_mriver,slc_lriver, &
                       substance_name, &
                       soilleak, &
                       basin
 
    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir         !<File directory
    INTEGER, INTENT(IN)  :: n                    !<Number of subbasins, submodel
    TYPE(DateType), INTENT(IN) :: time           !<Current time (e.g. 2005-01-26 18:00)
    INTEGER, INTENT(OUT) :: status               !<Error status
    
    !Local variables
    INTEGER isubst,icl
    INTEGER,ALLOCATABLE :: geoid(:)    !Model subid
    CHARACTER(LEN=maxcharpath) infile  !Name of file

    !>\b Algorithm \n
    !Start of subroutine
    status = 0
    IF(modeloption(p_soilleakage)==0)RETURN !Not use soil laekage
    IF(modeloption(p_soilleakage)==1)RETURN !Use permanent soil leakage
    IF(modeloption(p_soilleakage)==2)RETURN !-"-
    
    !>Load current soilleakage load from files
    IF(modeloption(p_soilleakage)==3)THEN
      IF(numsubstances==0)THEN
        WRITE(6,*) 'Warning: No substances are simulated. Turning off soil leakage model.'
        modeloption(p_soilleakage)=0
        RETURN
      ENDIF
      
      !Read files only for a new year.
      IF(time%day/=1)RETURN
      IF(time%month/=1)RETURN
    
      !>\li Allocate and initiate variables 
      soilleak%load = 0.
      IF(.NOT.ALLOCATED(geoid)) ALLOCATE(geoid(n))
      geoid = basin%subid
      
      !>\li Read data files
      DO isubst=1,numsubstances
        IF(isubst == i_t2) CYCLE  !T2 has no soil load
        DO icl = 1,nclass
          IF(icl==slc_ilake .OR. icl==slc_olake .OR. icl==slc_mriver .OR. icl==slc_lriver) CYCLE !Water classes has no soil load
          WRITE(infile,'(A4,A2,A4,I3.3,A4)') 'Load',substance_name(isubst),'_SLC',icl,'.txt'
          CALL read_one_year_monthly_leakage(dir,infile,time,n,geoid,status,soilleak%load(isubst,icl,:,:)) 
          IF(status/=0)RETURN
        ENDDO
      ENDDO
      
    ENDIF
    
    !Finish the subroutine
    IF(ALLOCATED(geoid)) DEALLOCATE(geoid)  !Ska jag skippa avallokering av dessa?
    WRITE(6,*) 'Current soil leakage data loaded'

  END SUBROUTINE get_current_soilleakage

  !>\brief Reads the matrix of monthly soil leakage concentrations or loads
  !>
  !>Reads the typical monthly soil leakage and checks them.
  !>
  !> \b Reference ModelDescription Chapter X (XXX)
  !------------------------------------------------------------------------------
  SUBROUTINE read_monthly_leakage(dir,infile,n,geoid,status,leakage) 

    USE WORLDVAR, ONLY : fileunit_temp, &
                         maxcharpath
    USE READWRITE_ROUTINES, ONLY : find_reorder_index
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_leakagedata, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir    !<File directory
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of leakage file
    INTEGER, INTENT(IN) :: n                !<Number of subbasins
    INTEGER, INTENT(IN) :: geoid(n)         !<Subid of subbasins
    INTEGER, INTENT(OUT) :: status          !<Error status
    REAL, INTENT(INOUT) :: leakage(12,n)    !<Soil leakage concentrations
    
    
    !Local variables
    INTEGER isub
    INTEGER ncols,nrows
    INTEGER,ALLOCATABLE :: leakid(:)           !Subid read from file
    INTEGER,ALLOCATABLE :: mixindex(:)         !Index for matching columns
    REAL,ALLOCATABLE :: readleakage(:,:)       !Temporary read leakage concentrations
    LOGICAL fileexist
    CHARACTER(LEN=maxcharpath) filename        !Name and path of file 
    CHARACTER(LEN=150) :: propagate_str

    !Start of subroutine
    status = 0
    leakage = 0
    filename = TRIM(dir)//infile
    
    !Check existence of file
    INQUIRE(FILE=filename,EXIST=fileexist)
    IF(.NOT.fileexist)THEN
      WRITE(6,*) 'ERROR: File not found: ',TRIM(filename)
      propagate_str = 'File not found: '//TRIM(filename)
      CALL propagate_external_msg(e_leakagedata,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(filename)
    
    !Count and check columns and rows in LeakageData
    CALL count_data_rows(fileunit_temp,TRIM(filename),1,nrows,status)
    IF(status/=0)RETURN
    IF(n/=nrows)THEN
      WRITE(6,*) 'ERROR: Number of rows in leakage file not equal to number of subbasins.'
      propagate_str = 'Number of rows in leakage file not equal to number of subbasins.'
      CALL propagate_external_msg(e_leakagedata,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    CALL count_data_cols(fileunit_temp,TRIM(filename),0,ncols,status)
    IF(status/=0)RETURN
    IF(ncols<13)THEN
      WRITE(6,*) 'ERROR: Number of columns does not match 12 months.'
      propagate_str = 'Number of columns does not match 12 months.'
      CALL propagate_external_msg(e_leakagedata,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    
    !Read data from file
    IF(.NOT.ALLOCATED(leakid)) ALLOCATE(leakid(n))
    IF(.NOT.ALLOCATED(readleakage)) ALLOCATE(readleakage(12,n))
    OPEN(UNIT = fileunit_temp,FILE = TRIM(filename), STATUS = 'old', ACTION='read')
    READ(fileunit_temp,*) !skip heading
    DO isub = 1,n
      READ(fileunit_temp,*) leakid(isub),readleakage(:,isub)
    ENDDO
    CLOSE(UNIT=fileunit_temp)
    
    !Check concentration and subbasin order and transform the read leakage to correct format
    IF(MINVAL(readleakage)<0.) WRITE(6,*) 'WARNING: Soil leakage less than zero found in file.'
    IF(.NOT.ALLOCATED(mixindex)) ALLOCATE(mixindex(n))
    CALL find_reorder_index(TRIM(filename),n,geoid,n,leakid,.TRUE.,mixindex,status)
    IF(status.NE.0) RETURN
    DO isub = 1,n
      leakage(:,isub) = readleakage(:,mixindex(isub))
    ENDDO
    
    !Finish the subroutine
    IF(ALLOCATED(mixindex)) DEALLOCATE(mixindex)
    IF(ALLOCATED(leakid)) DEALLOCATE(leakid)
    IF(ALLOCATED(readleakage)) DEALLOCATE(readleakage)
    !WRITE(6,*) 'Soil leakage data information loaded'

  END SUBROUTINE read_monthly_leakage

  !>\brief Reads monthly time series of soil leakage, first year
  !>
  !> \b Reference ModelDescription Chapter X (XXX)
  !------------------------------------------------------------------------------
  SUBROUTINE read_first_monthly_leakage(dir,infile,bd,n,geoid,status,leakage) 

    USE WORLDVAR, ONLY : fileunit_temp, &
                         maxcharpath
    USE READWRITE_ROUTINES, ONLY : find_reorder_index

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir    !<File directory
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of leakage file
    TYPE(datetype), INTENT(IN) :: bd        !<Simulation begin date
    INTEGER, INTENT(IN) :: n                !<Number of subbasins (base)
    INTEGER, INTENT(IN) :: geoid(n)         !<Subid of subbasins (base)
    INTEGER, INTENT(OUT) :: status          !<Error status
    REAL, INTENT(INOUT) :: leakage(12,n)    !<Soil leakage concentrations
    
    
    !Local variables
    INTEGER isub,month
    INTEGER ncols,ndata
    INTEGER,ALLOCATABLE :: leakid(:)           !Subid read from file
    INTEGER,ALLOCATABLE :: mixindex(:)         !Index for matching columns
    TYPE(datetype) :: date    !datetype object corresponding to read str
    REAL,ALLOCATABLE :: readrow(:),readleakage(:,:)       !Temporary read leakage concentrations
    LOGICAL fileexist
    CHARACTER(LEN=maxcharpath) filename        !Name and path of file 
    CHARACTER(LEN=100) str

    !Start of subroutine
    status = 0
    leakage = 0
    filename = TRIM(dir)//infile
    
    !Check existence of file
    INQUIRE(FILE=filename,EXIST=fileexist)
    IF(.NOT.fileexist)THEN
      WRITE(6,*) 'ERROR: File not found: ',TRIM(filename)
      status = 1
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(filename)
    
    !Count and check columns and rows in leakage file
    CALL count_data_cols(fileunit_temp,TRIM(filename),0,ncols,status)
    IF(status/=0)RETURN
    ndata = ncols - 1
    !IF(n/=ndata)THEN
      !Is this a problem. Set missing to zero?
      !WRITE(6,*) 'WARNING: Number of columns in file',TRIM(infile),'is not equal to number of subbasins.'
      !status = 1
      !RETURN
    !ENDIF
    
    !Read data from file for the first year of the file
    IF(.NOT.ALLOCATED(leakid)) ALLOCATE(leakid(ndata))
    IF(.NOT.ALLOCATED(readleakage)) ALLOCATE(readleakage(ndata,12)) !Max twelve month will be read for first year
    IF(.NOT.ALLOCATED(readrow)) ALLOCATE(readrow(ndata))
    readleakage = 0.
    OPEN(UNIT = fileunit_temp,FILE = TRIM(filename), STATUS = 'old', ACTION='read')
    READ(fileunit_temp,*,ERR=102) str,leakid(1:ndata) !read heading
    DO 
      READ(fileunit_temp,*,END=100,ERR=102) str,readrow(1:ndata)
      CALL string_convert_to_datetype(str,date)
      IF(date%Year<bd%Year) CYCLE
      IF(date%Year>bd%Year)THEN
        IF(date%Month==1.AND.date%day==1) WRITE(6,*) 'WARNING: Load for later year from ',TRIM(infile),' used for', bd%Year
      ENDIF
      month = date%Month
      readleakage(1:ndata,month)=readrow(1:ndata)
      IF(month==12)EXIT
    ENDDO
100 CLOSE(UNIT=fileunit_temp)
    
    !Check data and subbasin order and transform the read leakage to correct format
    IF(MINVAL(readleakage)<0.) WRITE(6,*) 'WARNING: Values less than zero found in file.'
    IF(.NOT.ALLOCATED(mixindex)) ALLOCATE(mixindex(n))
    CALL find_reorder_index(TRIM(filename),n,geoid,ndata,leakid,.TRUE.,mixindex,status)
    IF(status.NE.0) RETURN    !Missing subid/columns are not handled!
    DO isub = 1,n
      leakage(:,isub) = readleakage(mixindex(isub),:)
    ENDDO
    
    !Finish the subroutine
    IF(ALLOCATED(mixindex)) DEALLOCATE(mixindex)
    IF(ALLOCATED(leakid)) DEALLOCATE(leakid)
    IF(ALLOCATED(readleakage)) DEALLOCATE(readleakage)
    IF(ALLOCATED(readrow)) DEALLOCATE(readrow)
!    WRITE(6,*) 'Soil leakage data information loaded'
    RETURN
    
102 WRITE(6,*) 'Error: reading file',TRIM(infile)
    status = 1
    RETURN
  END SUBROUTINE read_first_monthly_leakage

  !>\brief Reads monthly time series of soil leakage, one year
  !>
  !>Reduced the data for submodel in needed.
  !>
  !> \b Reference ModelDescription Chapter X (XXX)
  !------------------------------------------------------------------------------
  SUBROUTINE read_one_year_monthly_leakage(dir,infile,cd,n,geoid,status,leakage) 

    USE WORLDVAR, ONLY : fileunit_temp, &
                         maxcharpath
    USE READWRITE_ROUTINES, ONLY : find_reorder_index

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir    !<file directory
    CHARACTER (LEN=*), INTENT(IN) :: infile !<name of leakage file
    TYPE(datetype), INTENT(IN) :: cd        !<current date
    INTEGER, INTENT(IN) :: n                !<number of subbasins
    INTEGER, INTENT(IN) :: geoid(n)         !<subid of subbasins
    INTEGER, INTENT(OUT) :: status          !<error status
    REAL, INTENT(INOUT) :: leakage(12,n)    !<soil leakage concentrations
    
    !Local variables
    INTEGER isub,month
    INTEGER ncols,ndata
    INTEGER,ALLOCATABLE :: leakid(:)           !Subid read from file
    INTEGER,ALLOCATABLE :: mixindex(:)         !Index for matching columns
    TYPE(datetype) :: date    !datetype object corresponding to read str
    REAL,ALLOCATABLE :: readrow(:),readleakage(:,:)       !Temporary read leakage concentrations
    CHARACTER(LEN=maxcharpath) filename        !Name and path of file 
    CHARACTER(LEN=100) str

    !Start of subroutine
    status = 0
    leakage = 0
    filename = TRIM(dir)//infile
    
    !Count and check columns and rows in leakage file
    CALL count_data_cols(fileunit_temp,TRIM(filename),0,ncols,status)
    IF(status/=0)RETURN
    ndata = ncols - 1
    
    !Read data from file for the current year
    IF(.NOT.ALLOCATED(leakid)) ALLOCATE(leakid(ndata))
    IF(.NOT.ALLOCATED(readleakage)) ALLOCATE(readleakage(ndata,12))
    IF(.NOT.ALLOCATED(readrow)) ALLOCATE(readrow(ndata))
    readleakage = 0.
    OPEN(UNIT = fileunit_temp,FILE = TRIM(filename), STATUS = 'old', ACTION='read')
    READ(fileunit_temp,*,ERR=102) str,leakid(1:ndata) !read heading
    DO 
      READ(fileunit_temp,*,END=100,ERR=102) str,readrow(1:ndata)
      CALL string_convert_to_datetype(str,date)
      IF(date%Year<cd%Year) CYCLE
      IF(date%Year>cd%Year)THEN
        IF(date%Month==1.AND.date%day==1) WRITE(6,*) 'WARNING: Load for later year from ',TRIM(infile),' used for', cd%Year
      ENDIF
      month = date%Month
      readleakage(:,month)=readrow(:)
      IF(month==12)EXIT
    ENDDO
100 CLOSE(UNIT=fileunit_temp)
    
    !Check data and subbasin order and transform the read leakage to correct format
    IF(MINVAL(readleakage)<0.) WRITE(6,*) 'WARNING: Values less than zero found in file.'
    IF(.NOT.ALLOCATED(mixindex)) ALLOCATE(mixindex(n))
    CALL find_reorder_index(TRIM(filename),n,geoid,ndata,leakid,.FALSE.,mixindex,status)
    !??? Is missing columns handled? No
    IF(status.NE.0) RETURN
    DO isub = 1,n
      leakage(:,isub) = readleakage(mixindex(isub),:)
    ENDDO
    
    !Finish the subroutine
    IF(ALLOCATED(mixindex)) DEALLOCATE(mixindex)
    IF(ALLOCATED(leakid)) DEALLOCATE(leakid)
    IF(ALLOCATED(readleakage)) DEALLOCATE(readleakage)
    IF(ALLOCATED(readrow)) DEALLOCATE(readrow)
    !WRITE(6,*) 'Soil leakage data information loaded'
    RETURN
    
102 WRITE(6,*) 'Error: reading file',TRIM(infile)
    status = 1
    RETURN
  END SUBROUTINE read_one_year_monthly_leakage

  !>\brief Gets the information about irrigation and water transfer from MgmtData.
  !--------------------------------------------------------------------
  SUBROUTINE load_management_data(funit,dir,n,status) 

    USE WORLDVAR, ONLY : i_str,       &
                         i_intg,      &
                         i_real,      &
                         maxcharpath
    USE MODVAR, ONLY : basin, &
                       irrigationsystem, &  !OUT
                       watertransfer, &  !OUT
                       nwtransfer, & !OUT
                       conduct  !OUT
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_mgmtdata,e_error,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit          !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: dir   !<File directory
    INTEGER, INTENT(IN)  :: n              !<Number of subbasins
    INTEGER, INTENT(OUT) :: status         !<Error status
    
    !Local constants
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    INTEGER ncols,nrows                !size of data in file
    INTEGER mcol,nirrows
    INTEGER i,j,isub,irrow,itrow
    INTEGER typecolumn
    LOGICAL fileex                     !Status of file
    LOGICAL swcol,gwcol                !Status of sw/gw_part columns
    LOGICAL receiverfound
    CHARACTER(LEN=maxcharpath) infile             !Name of irrigation characteristics file 
    INTEGER, ALLOCATABLE :: xi(:,:)               !Integer data read from file
    INTEGER, ALLOCATABLE :: code(:)               !Code for column variable
    INTEGER, ALLOCATABLE :: rindex(:)             !Index for column real variables
    INTEGER, ALLOCATABLE :: iindex(:)             !Index for column integer variables
    REAL, ALLOCATABLE    :: xr(:,:)               !Real data read from file
    CHARACTER(LEN=letters), ALLOCATABLE :: str(:)      !File column content string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra !Propagate string extra

    status = 0
    nwtransfer = 0
    infile = TRIM(dir)//'MgmtData.txt'

    !Check if file exist
    INQUIRE(FILE=TRIM(infile),EXIST=fileex)
    IF(.NOT.fileex)THEN
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(infile)

    !Count number of columns in MgmtData
    CALL count_data_cols(funit,TRIM(infile),0,ncols,status)
    IF(status/=0)RETURN

    !Count number of subbasins in MgmtData
    CALL count_data_rows(funit,TRIM(infile),1,nrows,status)
    IF(status/=0)RETURN
    IF(nrows==0)THEN
      WRITE(6,*) 'No data in MgmtData.txt file found.'
      RETURN
    ENDIF

    !Allocate and initiate local variables
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,ncols))  
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))  
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,ncols))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(ncols))
    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*ncols)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'
    swcol = .FALSE.; gwcol = .FALSE.

    !Open file
    OPEN(UNIT = funit,FILE = TRIM(infile), STATUS = 'old', ACTION='read')     

    !Read the column headings from file
    CALL read_column_headings(funit,ncols,letters,str,mcol,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(infile)
      RETURN
    ENDIF

    !Code variables for easy finding of variable type
    code=i_str    !string, ignore
    typecolumn = 0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='mgmttype  ')THEN
        code(i) = i_intg
        typecolumn = i
      ENDIF
      IF(str(i)(1:letters)=='subid     ') code(i) = i_intg
      IF(str(i)(1:letters)=='regsrcid  ') code(i) = i_intg
      IF(str(i)(1:letters)=='local_eff ') code(i) = i_real
      IF(str(i)(1:letters)=='region_eff') code(i) = i_real
      IF(str(i)(1:letters)=='gw_part   ') code(i) = i_real
      IF(str(i)(1:letters)=='demandtype') code(i) = i_intg
      IF(str(i)(1:letters)=='irrdam    ') code(i) = i_intg
      IF(str(i)(1:letters)=='waterbody ') code(i) = i_intg
      IF(str(i)(1:letters)=='receiver  ') code(i) = i_intg
      IF(str(i)(1:letters)=='flow      ') code(i) = i_real
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:10))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_mgmtdata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Read all data
    CALL read_basindata5(funit,infile,ncols,nrows,ncols,code,rindex,iindex,xi,xr)

    CLOSE(UNIT=funit)

    !Count irrigation rows and water transfer rows
    IF(typecolumn>0)THEN
      nirrows = 0
      DO i = 1,nrows
        IF(xi(i,iindex(typecolumn))==1) nirrows = nirrows+1
        IF(xi(i,iindex(typecolumn))==2) nwtransfer = nwtransfer+1
      ENDDO
    ELSE
      nirrows = nrows
      nwtransfer=0
    ENDIF
    IF(nwtransfer>0) conduct%watertransfer = .TRUE.

    !Allocate irrigation and water transfer variables
    IF(.NOT.ALLOCATED(irrigationsystem)) ALLOCATE(irrigationsystem(nirrows))
    IF(conduct%watertransfer .AND. .NOT.ALLOCATED(watertransfer)) ALLOCATE(watertransfer(nwtransfer))

    IF(nirrows==nrows)THEN
      !Old setting, only irrigation in file
      DO i = 1,ncols
        IF(str(i)(1:letters)=='subid     ') irrigationsystem(1:nrows)%subid       = xi(1:nrows,iindex(i))
        IF(str(i)(1:letters)=='regsrcid  ') irrigationsystem(1:nrows)%regsourceid = xi(1:nrows,iindex(i))
        IF(str(i)(1:letters)=='local_eff ') irrigationsystem(1:nrows)%local_eff   = xr(1:nrows,rindex(i))
        IF(str(i)(1:letters)=='region_eff') irrigationsystem(1:nrows)%reg_eff     = xr(1:nrows,rindex(i))
        IF(str(i)(1:letters)=='demandtype') irrigationsystem(1:nrows)%demandtype  = xi(1:nrows,iindex(i))
        IF(str(i)(1:letters)=='gw_part   ')THEN
          irrigationsystem(1:nrows)%gw_part = xr(1:nrows,rindex(i))
          gwcol = .TRUE.
        ENDIF
        IF(str(i)(1:letters)=='irrdam    ')THEN
          DO j = 1,nrows
            irrigationsystem(j)%dam = xi(j,iindex(i))==1
          ENDDO
        ENDIF
      ENDDO
    ELSE
      !Water transfer data in file
      DO i = 1,ncols
        irrow = 0
        itrow = 0
        DO j = 1,nrows
          IF(xi(j,iindex(typecolumn))==1)THEN !Irrigation
            irrow = irrow + 1
            IF(str(i)(1:letters)=='subid     ') irrigationsystem(irrow)%subid       = xi(j,iindex(i))
            IF(str(i)(1:letters)=='regsrcid  ') irrigationsystem(irrow)%regsourceid = xi(j,iindex(i))
            IF(str(i)(1:letters)=='local_eff ') irrigationsystem(irrow)%local_eff   = xr(j,rindex(i))
            IF(str(i)(1:letters)=='region_eff') irrigationsystem(irrow)%reg_eff     = xr(j,rindex(i))
            IF(str(i)(1:letters)=='demandtype') irrigationsystem(irrow)%demandtype  = xi(j,iindex(i))
            IF(str(i)(1:letters)=='gw_part   ')THEN
              irrigationsystem(irrow)%gw_part = xr(j,rindex(i))
              gwcol = .TRUE.
            ENDIF
            IF(str(i)(1:letters)=='irrdam    ')THEN
              irrigationsystem(irrow)%dam = xi(j,iindex(i))==1
            ENDIF
          ELSEIF(xi(j,iindex(typecolumn))==2)THEN !Water transfer
            itrow = itrow + 1
            IF(str(i)(1:letters)=='subid     ') watertransfer(itrow)%srcsubid = xi(j,iindex(i))
            IF(str(i)(1:letters)=='flow      ') watertransfer(itrow)%flow     = xr(j,rindex(i))
            IF(str(i)(1:letters)=='receiver  ')THEN
              receiverfound = .FALSE.
              DO isub = 1,n
                IF(basin(isub)%subid==xi(j,iindex(i)))THEN
                  watertransfer(itrow)%rcvindex = isub
                  receiverfound = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT.receiverfound)THEN
                WRITE(6,*) 'WARNING: No reciever of the water transfer is found in MgmtData.txt: row: ',j
                watertransfer(itrow)%rcvindex = 0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDIF

    !Check if irrigation used
    IF(gwcol)THEN
      irrigationsystem(1:nirrows)%sw_part = 1. - irrigationsystem(1:nirrows)%gw_part
      WRITE(6,*) 'INFO: Irrigation present in model.'
    ELSE
      IF(ALLOCATED(irrigationsystem)) DEALLOCATE(irrigationsystem)
    ENDIF

    !Deallocate local variables
    IF(ALLOCATED(xi)) DEALLOCATE(xi)  
    IF(ALLOCATED(code)) DEALLOCATE(code)  
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    IF(ALLOCATED(str)) DEALLOCATE(str) 

    WRITE(6,*) 'Management data information loaded'

  END SUBROUTINE load_management_data

!--------------------------------------------------------------------
  !>\brief Gets information about forcing data coupling to subbasin 
  !!and other information.
  !!Reads the matrix of basin data values from the file with mcols
  !!columns. Output: data variables, heading variable and actual
  !!number of data columns
  !!
  !>\b Consequences Module worldvar variables tobsid, pobsid
  !> are set. 
  !>Module modvar variable tobselevation is allocated and set.
  !--------------------------------------------------------------------
  SUBROUTINE load_forcing_information_data(funit,dir,ns,status) 

    USE WORLDVAR, ONLY : get_seq_filename, &
                         forcingdata, &
                         max_forcingdata, &
                         i_pobs, &
                         i_tobs, &
                         i_tminobs, &
                         i_tmaxobs, &
                         i_rhobs, &
                         i_sfobs, &
                         i_swobs, &
                         i_uobs, &
                         i_uwobs, &
                         i_vwobs, &
                         readobsid,   &
                         i_str,       &
                         i_intg,      &
                         i_real,      &
                         maxcharpath
    USE MODVAR, ONLY : basin,   &
                       tobselevation  !OUT
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_forckey,e_error,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit                  !<Unit number for file
    CHARACTER (LEN=*), INTENT(IN) :: dir           !<File directory
    INTEGER, INTENT(IN) :: ns                      !<Number of subbasins (base model)
    INTEGER, INTENT(OUT) :: status                 !<Error status
    
    !Local constants
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    INTEGER ncols                      !size of data in file
    INTEGER mcols
    INTEGER i,j,iforc
    INTEGER isubid,itobselev  !Columns in file
    INTEGER ifobsid(max_forcingdata)   !Columns in file
    LOGICAL fileex                     !Existence of file
    CHARACTER(LEN=maxcharpath) infile  !Name and path of file 
    CHARACTER(LEN=16) filename         !Name of file 
    INTEGER, ALLOCATABLE :: xi(:,:)               !Integer data read from file
    INTEGER, ALLOCATABLE :: code(:)               !Code for column variable
    INTEGER, ALLOCATABLE :: rindex(:)             !Index for column real variables
    INTEGER, ALLOCATABLE :: iindex(:)             !Index for column integer variables
    REAL, ALLOCATABLE    :: xr(:,:)               !Real data read from file
    CHARACTER(LEN=letters), ALLOCATABLE :: colstr(:)   !File column content string
    CHARACTER(LEN=150) :: propagate_str                 !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra !Propagate string extra

    status = 0
    !Set default values for obsid
    DO iforc=1,max_forcingdata
      IF(forcingdata(iforc)%readfile)THEN
        IF(.NOT.ALLOCATED(forcingdata(iforc)%stationid))ALLOCATE(forcingdata(iforc)%stationid(ns))
        forcingdata(iforc)%stationid(1:ns) = basin(1:ns)%subid
      ENDIF
    ENDDO

    !Check if file exist and need to be read
    filename = 'ForcKey.txt'
    CALL get_seq_filename(filename)
    infile = TRIM(dir)//TRIM(filename)
    INQUIRE(FILE=TRIM(infile),EXIST=fileex)
    IF(.NOT.fileex)THEN
      IF(readobsid)THEN
        WRITE(6,*) 'ERROR: No ForcKey-file found, even though readobsid turned on in info.txt.'
        STOP 1
      ENDIF
      RETURN
    ELSEIF(fileex.AND..NOT.readobsid)THEN
      WRITE(6,*) 'WARNING: Existing ForcKey-file will not be read, because readobsid turned off in info.txt'
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(infile)

    !Count number of columns in file
    CALL count_data_cols(funit,TRIM(infile),0,ncols,status)
    IF(status/=0)RETURN

    !Allocation of local variables
    ALLOCATE(xi(ns,ncols))  
    ALLOCATE(code(ncols))  
    ALLOCATE(rindex(ncols))
    ALLOCATE(iindex(ncols))
    ALLOCATE(xr(ns,ncols))
    ALLOCATE(colstr(ncols))

    !Read file
    OPEN(UNIT=funit,FILE=TRIM(infile),STATUS='old',ACTION='read',ERR=900)
    CALL read_column_headings(funit,ncols,letters,colstr,mcols,status)

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*ncols)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    IF(status/=0) RETURN
    code = i_str
    isubid = 0
    itobselev = 0
    ifobsid = 0
    DO i = 1,mcols
      IF(colstr(i)(1:letters)=='subid     ')THEN
        code(i) = i_intg
        isubid = i
      ENDIF
      DO iforc=1,max_forcingdata
        IF(colstr(i)(1:letters)==forcingdata(iforc)%idcode)THEN
          code(i) = i_intg
          IF(forcingdata(iforc)%readfile) ifobsid(iforc) = i
        ENDIF  
      ENDDO
      IF(colstr(i)(1:letters)=='tobselev  ')THEN
        code(i) = i_real
        itobselev = i
      ENDIF
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(colstr(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_forckey,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    IF(isubid==0)THEN
      WRITE(6,*) 'ERROR: subid missing in file:', TRIM(infile)
      propagate_str = 'subid missing in file: '//TRIM(infile)
      CALL propagate_external_msg(e_forckey,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    CALL read_basindata5(funit,infile,ncols,ns,mcols,code,rindex,iindex,xi,xr) 
    CLOSE(funit) 
    IF(itobselev==0) WRITE(6,*) 'WARNING: no elevation for tobs found in file:', TRIM(infile)

    !Set forcing data information to subbasin array
    IF(itobselev>0)THEN
      IF(.NOT.ALLOCATED(tobselevation)) ALLOCATE(tobselevation(ns))
      DO i = 1,ns
        IF(basin(i)%subid==xi(i,iindex(isubid)))THEN
          tobselevation(i) = xr(i,rindex(itobselev))
        ELSE
          DO j = 1,ns
            IF(basin(i)%subid==xi(j,iindex(isubid)))THEN
              tobselevation(i) = xr(j,rindex(itobselev))
              EXIT
            ENDIF  
          ENDDO
        ENDIF
      ENDDO
    ENDIF

    !Set subbasin - observation coupling variables
    DO i = 1,ns
      IF(basin(i)%subid==xi(i,iindex(isubid)))THEN
        DO iforc=1,max_forcingdata
          IF(ifobsid(iforc)>0) forcingdata(iforc)%stationid(i) = xi(i,iindex(ifobsid(iforc)))
        ENDDO
      ELSE
        DO j = 1,ns
          IF(basin(i)%subid==xi(j,iindex(isubid)))THEN
            DO iforc=1,max_forcingdata
              IF(ifobsid(iforc)>0) forcingdata(iforc)%stationid(i) = xi(j,iindex(ifobsid(iforc)))
            ENDDO
            EXIT
          ENDIF  
        ENDDO
      ENDIF
    ENDDO

    !Deallocate local variables
    IF(ALLOCATED(xi)) DEALLOCATE(xi)  
    IF(ALLOCATED(code)) DEALLOCATE(code)  
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    IF(ALLOCATED(colstr)) DEALLOCATE(colstr) 

    WRITE(6,*) 'Forcing key information loaded'

    RETURN
    
900 WRITE(6,*) 'Error open file: ', TRIM(infile)
    propagate_str = 'Error open file: '//TRIM(infile)
    CALL propagate_external_msg(e_forckey,e_error,propagate_str)
    status = 1
    RETURN

  END SUBROUTINE load_forcing_information_data

  !>\brief Gets the information about update from update.txt.
  !-----------------------------------------------------------------
  SUBROUTINE read_update_data(funit,infile,ns,nqsub,qupdsub,nqarsub,nwarsub, &
       qarupdsub,warupdsub,qarupdar,warupdar,nwsub,wendsub,nrow,tpcorrarr,&
       tncorrarr,tploccorrarr,tnloccorrarr,ncsub,cupdsub)

    USE WORLDVAR, ONLY : i_str,i_intg,i_real
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_update_file, &
                                    e_error,e_warning,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit                !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile      !<Name of characteristics file to be read
    INTEGER, INTENT(IN)  :: ns                   !<Number of subbasins (basemodel)
    INTEGER, INTENT(OUT) :: nqsub                !<Number of subbasins with quseobs update
    INTEGER, INTENT(OUT) :: qupdsub(ns)          !<Subid of subbasins with quseobs update
    INTEGER, INTENT(OUT) :: nqarsub              !<Number of subbasins with qAR date
    INTEGER, INTENT(OUT) :: nwarsub              !<Number of subbasins with wAR date
    INTEGER, INTENT(OUT) :: qarupdsub(ns)        !<Subid of subbasins with qAR update
    INTEGER, INTENT(OUT) :: warupdsub(ns)        !<Subid of subbasins with wAR update
    REAL, INTENT(OUT)    :: qarupdar(ns)         !<AR-factor for qAR-update
    REAL, INTENT(OUT)    :: warupdar(ns)         !<AR-factor for wAR-update
    INTEGER, INTENT(OUT) :: nwsub                !<Number of subbasins with wendupd update
    INTEGER, INTENT(OUT) :: wendsub(ns)          !<Subid of subbasins with wendupd update
    INTEGER, INTENT(OUT) :: nrow                 !<Number of data rows in file
    REAL,    INTENT(OUT) :: tpcorrarr(ns,2)      !<Subid,value of subbasins with tpcorr update
    REAL,    INTENT(OUT) :: tncorrarr(ns,2)      !<Subid,value of subbasins with tncorr update
    REAL,    INTENT(OUT) :: tploccorrarr(ns,2)   !<Subid,value of subbasins with tploccorr update
    REAL,    INTENT(OUT) :: tnloccorrarr(ns,2)   !<Subid,value of subbasins with tnloccorr update
    INTEGER, INTENT(OUT) :: ncsub                !<Number of subbasins with cuseobs update
    INTEGER, INTENT(OUT) :: cupdsub(ns)          !<Subid of subbasins with cuseobs update

    !Local parameters
    INTEGER, PARAMETER :: nskip = 1   !heading on row 1
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables declarations
    INTEGER i,irow                     !Loop counters
    INTEGER maxcol
    INTEGER isub                       !Data column with subid
    INTEGER iquseobs                   !Data column with status for quseobs
    INTEGER icuseobs                   !Data column with status for cuseobs
    INTEGER iqar,iwar                  !Data column with status for q-AR and w-AR
    INTEGER iqarar                     !Data column with AR-factor for q-AR
    INTEGER iwendupd                   !Data column with status for wendupd
    INTEGER itpcorr                    !Data column with value for tp correction
    INTEGER itncorr                    !Data column with value for tn correction
    INTEGER itploccorr                 !Data column with value for local tp correction
    INTEGER itnloccorr                 !Data column with value for local tn correction
    INTEGER mcols                      !Actual number of columns
    INTEGER status                     !Error status
    INTEGER, ALLOCATABLE ::  code(:)   !Code for column variable
    INTEGER, ALLOCATABLE ::  rindex(:) !Index for column real variables
    INTEGER, ALLOCATABLE ::  iindex(:) !Index for column integer variables
    INTEGER, ALLOCATABLE :: xi(:,:)    !Integer data read from file
    INTEGER, ALLOCATABLE :: temparray(:) !Temporary data array
    REAL, ALLOCATABLE :: temparray2(:) !Temporary data array2
    REAL, ALLOCATABLE    :: xr(:,:)    !Real data read from file
    CHARACTER(LEN=letters), ALLOCATABLE :: str(:)      !Content string
    CHARACTER(LEN=150) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    !Initiations
    status = 0
    nqsub  = 0
    qupdsub = 0
    ncsub  = 0
    cupdsub = 0
    nqarsub  = 0 
    qarupdsub = 0
    nwarsub  = 0 
    warupdsub = 0
    qarupdar = 0 
    warupdar = 0 
    nrow = 0
    tpcorrarr = 0.
    tncorrarr = 0.
    tploccorrarr = 0.
    tnloccorrarr = 0.

    !Count the number of columns and rows and allocate variables accordingly
    WRITE(6,*) 'File opened: ', TRIM(infile)
    CALL count_data_cols(funit,infile,0,maxcol,status)
    IF(status/=0)THEN
      WRITE(6,*) 'ERROR: counting columns in update.txt'
      propagate_str = 'counting columns in update.txt'
      CALL propagate_external_msg(e_update_file,e_error,propagate_str)
      RETURN
    ENDIF
    CALL count_data_rows(funit,infile,nskip,nrow,status)
    IF(status/=0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(infile)
      propagate_str = 'reading file: '//TRIM(infile)
      CALL propagate_external_msg(e_update_file,e_error,propagate_str)
      RETURN
    ENDIF
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(maxcol))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(maxcol))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(maxcol))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(maxcol))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrow,maxcol))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrow,maxcol))

    !Open file for reading data
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old', ACTION='read')     

    !Reads the column headings
    CALL read_column_headings(funit,maxcol,letters,str,mcols,status)
    IF(status/=0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(infile)
      propagate_str = 'reading file: '//TRIM(infile)
      CALL propagate_external_msg(e_update_file,e_error,propagate_str)
      RETURN
    ENDIF

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*maxcol)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !Code variables for easy finding of variable type
    code=i_str    !string, ignore
    DO i = 1,mcols
      IF(str(i)(1:letters)=='subid     ') code(i) = i_intg
      IF(str(i)(1:letters)=='cuseobs   ') code(i) = i_intg
      IF(str(i)(1:letters)=='quseobs   ') code(i) = i_intg
      IF(str(i)(1:letters)=='wendupd   ') code(i) = i_intg
      IF(str(i)(1:letters)=='tpcorr    ') code(i) = i_real
      IF(str(i)(1:letters)=='tncorr    ') code(i) = i_real
      IF(str(i)(1:letters)=='tploccorr ') code(i) = i_real
      IF(str(i)(1:letters)=='tnloccorr ') code(i) = i_real
      IF(str(i)(1:letters)=='qarupd    ') code(i) = i_intg !Q-AR on(1), off(0)
      IF(str(i)(1:letters)=='warupd    ') code(i) = i_intg !W-AR on(1), off(0)
      IF(str(i)(1:letters)=='arfact    ') code(i) = i_real !factor for AR
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_update_file,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Read all data
    CALL read_basindata5(funit,infile,maxcol,nrow,mcols,code,rindex,iindex,xi,xr)

    CLOSE(UNIT=funit)

    !Find variables for update function
    isub = 0; iquseobs = 0; icuseobs = 0; iwendupd=0; itpcorr=0; itncorr=0; itploccorr=0; itnloccorr=0; iqar=0; iqarar=0; iwar=0
    DO i = 1,mcols
      IF(str(i)(1:letters)=='subid     ')   isub = i
      IF(str(i)(1:letters)=='quseobs   ')   iquseobs = i
      IF(str(i)(1:letters)=='cuseobs   ')   icuseobs = i
      IF(str(i)(1:letters)=='wendupd   ')   iwendupd = i
      IF(str(i)(1:letters)=='tpcorr    ')   itpcorr = i
      IF(str(i)(1:letters)=='tncorr    ')   itncorr = i
      IF(str(i)(1:letters)=='tploccorr ')   itploccorr = i
      IF(str(i)(1:letters)=='tnloccorr ')   itnloccorr = i
      IF(str(i)(1:letters)=='qarupd    ')   iqar = i  
      IF(str(i)(1:letters)=='warupd    ')   iwar = i  
      IF(str(i)(1:letters)=='arfact    ')   iqarar = i
    ENDDO
    IF(isub==0)THEN
      WRITE(6,*) 'No subid information in update.txt'
      RETURN
    ENDIF

    !Set variables for update function quseobs      
    IF(iquseobs>0)THEN      !Save subid for stations to be updated
      IF(.NOT.ALLOCATED(temparray)) ALLOCATE(temparray(nrow))
      temparray = 0
      DO irow = 1,nrow
        IF(xi(irow,iindex(iquseobs))==1)THEN
          nqsub = nqsub + 1
          temparray(nqsub) = xi(irow,iindex(isub))
        ENDIF
      ENDDO
      IF(nqsub>0)THEN
        qupdsub(1:nqsub)=temparray(1:nqsub)
      ENDIF
      IF(ALLOCATED(temparray)) DEALLOCATE(temparray)
    ENDIF

    !Set variables for update function cuseobs      
    IF(icuseobs>0)THEN      !Save subid for stations to be updated
      IF(.NOT.ALLOCATED(temparray)) ALLOCATE(temparray(nrow))
      temparray = 0
      DO irow = 1,nrow
        IF(xi(irow,iindex(icuseobs))==1)THEN
          ncsub = ncsub + 1
          temparray(ncsub) = xi(irow,iindex(isub))
        ENDIF
      ENDDO
      IF(ncsub>0)THEN
        cupdsub(1:ncsub)=temparray(1:ncsub)
      ENDIF
      IF(ALLOCATED(temparray)) DEALLOCATE(temparray)
    ENDIF

    !Set variables for update function qAR  
    IF(iqar>0)THEN
      IF(.NOT.ALLOCATED(temparray)) ALLOCATE(temparray(nrow))
      IF(.NOT.ALLOCATED(temparray2)) ALLOCATE(temparray2(nrow))
      temparray = 0
      temparray2 = 0
      DO irow = 1,nrow
        IF(xi(irow,iindex(iqar))==1)THEN
          nqarsub = nqarsub + 1
          temparray(nqarsub) = xi(irow,iindex(isub))
          temparray2(nqarsub) = xr(irow,rindex(iqarar))
        ENDIF
      ENDDO
      IF(nqarsub>0)THEN
        qarupdsub(1:nqarsub) = temparray(1:nqarsub)
        qarupdar(1:nqarsub) = temparray2(1:nqarsub)
      ENDIF
      IF(ALLOCATED(temparray)) DEALLOCATE(temparray)
      IF(ALLOCATED(temparray2)) DEALLOCATE(temparray2)
    ENDIF

    !Set variables for update function wAR  
    IF(iwar>0)THEN
      IF(.NOT.ALLOCATED(temparray)) ALLOCATE(temparray(nrow))
      IF(.NOT.ALLOCATED(temparray2)) ALLOCATE(temparray2(nrow))
      temparray = 0
      temparray2 = 0
      DO irow = 1,nrow
        IF(xi(irow,iindex(iwar))==1)THEN
          nwarsub = nwarsub + 1
          temparray(nwarsub) = xi(irow,iindex(isub))
          temparray2(nwarsub) = xr(irow,rindex(iqarar))
        ENDIF
      ENDDO
      IF(nwarsub>0)THEN
        warupdsub(1:nwarsub) = temparray(1:nwarsub)
        warupdar(1:nwarsub) = temparray2(1:nwarsub) !Same as for qAR
      ENDIF
      IF(ALLOCATED(temparray)) DEALLOCATE(temparray)
      IF(ALLOCATED(temparray2)) DEALLOCATE(temparray2)
    ENDIF

    !Set variables for update function tpcorr
    IF(itpcorr>0)THEN      !Save subid/value for all stations to be updated
      tpcorrarr(1:nrow,1) = REAL(xi(1:nrow,iindex(isub)))
      tpcorrarr(1:nrow,2) = xr(1:nrow,rindex(itpcorr))
    ENDIF
    !Set variables for update function tncorr
    IF(itncorr>0)THEN      !Save subid/value for all stations to be updated
      tncorrarr(1:nrow,1) = REAL(xi(1:nrow,iindex(isub)))
      tncorrarr(1:nrow,2) = xr(1:nrow,rindex(itncorr))
    ENDIF
    !Set variables for update function tploccorr
    IF(itploccorr>0)THEN      !Save subid/value for all stations to be updated
      tploccorrarr(1:nrow,1) = REAL(xi(1:nrow,iindex(isub)))
      tploccorrarr(1:nrow,2) = xr(1:nrow,rindex(itploccorr))
    ENDIF
    !Set variables for update function tnloccorr
    IF(itnloccorr>0)THEN      !Save subid/value for all stations to be updated
      tnloccorrarr(1:nrow,1) = REAL(xi(1:nrow,iindex(isub)))
      tnloccorrarr(1:nrow,2) = xr(1:nrow,rindex(itnloccorr))
    ENDIF

    !Set variables for update function wendupd      
    IF(iwendupd>0)THEN      !Save subid for stations to be updated
      IF(.NOT.ALLOCATED(temparray)) ALLOCATE(temparray(nrow))
      temparray = 0
      DO irow = 1,nrow
        IF(xi(irow,iindex(iwendupd))==1)THEN
          nwsub = nwsub + 1
          temparray(nwsub) = xi(irow,iindex(isub))
        ENDIF
      ENDDO
      IF(nwsub>0)THEN
        wendsub(1:nwsub)=temparray(1:nwsub)
      ENDIF
      IF(ALLOCATED(temparray)) DEALLOCATE(temparray)
    ENDIF

    IF(ALLOCATED(code)) DEALLOCATE(code)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(str)) DEALLOCATE(str)
    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)

    WRITE(6,*) 'Updating information loaded'

  END SUBROUTINE read_update_data

  !>Calculate the coupling between subbasins by index from the existing
  !>coupling by subid. In addition set the flag for upstream lakebasins 
  !>flowing to downstream lakebasin and branches that are secondary 
  !>outlets of lakebasin lake.
  !>
  !>\b Consequences Module modvar variables path, branchdata and branchindex 
  !>may be allocated and set. Module modvar variables pathsubid and branchsubid 
  !>is deallocated. 
  !--------------------------------------------------------------------
  SUBROUTINE calculate_path(ns) 

    USE MODVAR, ONLY : path,        &   !OUT
                       pathsubid,   &
                       branchdata,  &   !OUT
                       branchsubid, &
                       branchindex, &   !OUT
                       lakebasinindex, &
                       lakebasin, &
                       elake

    !Argument declarations
    INTEGER, INTENT(IN) :: ns                   !<Number of subbasins (of submodel)
    
    !Local variables
    INTEGER i,i2,ibd,ioutlet,isb
    INTEGER dim   !size of branchdata table
    INTEGER mainflow(ns)
    INTEGER grwflow(ns)
    INTEGER,ALLOCATABLE :: source(:),branch(:)
    INTEGER,ALLOCATABLE :: basins(:),downbasins(:)
    LOGICAL,ALLOCATABLE :: upbasin(:)

    !>\b Algorithm \n
    !>Set main path and groundwater flow path
    IF(.NOT.ALLOCATED(path)) ALLOCATE(path(ns))
    mainflow = pathsubid%main
    grwflow = pathsubid%grw1
    path%grwtolake = pathsubid%grwtolake
    path%aquid     = pathsubid%aquid
    path%rechargebasin = pathsubid%rechargebasin
    path%recievefraction = pathsubid%recievefraction
    DO i = 1,ns
      path(i)%main   = get_subid_index(mainflow(i),ns)      !uses basin%subid!
      path(i)%grw1   = get_subid_index(grwflow(i),ns)
    ENDDO
    IF(ALLOCATED(pathsubid)) DEALLOCATE(pathsubid)

    !>Check for connected lakebasins in main flow path
    ALLOCATE(basins(ns))
    ALLOCATE(downbasins(ns))
    ALLOCATE(upbasin(ns))
    DO i = 1,ns
      basins(i) = i
      downbasins(i) = path(i)%main
    ENDDO
    CALL find_connected_lakebasins(ns,basins,downbasins,upbasin) 
    DO i = 1,ns
      path(i)%uplakebasin = upbasin(i)
    ENDDO
    DEALLOCATE(basins)
    DEALLOCATE(downbasins)
    DEALLOCATE(upbasin)
    
    !>Set branch path and data, if present
    dim = 0
    IF(ALLOCATED(branchsubid))THEN
      dim = SIZE(branchsubid)
      IF(.NOT.ALLOCATED(branchdata)) ALLOCATE(branchdata(dim))
      IF(.NOT.ALLOCATED(branchindex)) ALLOCATE(branchindex(ns))
      IF(.NOT.ALLOCATED(source)) ALLOCATE(source(dim))
      IF(.NOT.ALLOCATED(branch)) ALLOCATE(branch(dim))
      branchindex = 0
      source = branchsubid%source
      branch = branchsubid%branch
      
      !Copy branch flow data
      branchdata%mainpart   = branchsubid%mainpart
      branchdata%maxQ       = branchsubid%maxQ
      branchdata%minQ       = branchsubid%minQ
      branchdata%maxQbranch = branchsubid%maxQbranch
      branchdata%recQbranch = branchsubid%recQbranch
      branchdata%lb2outlet = branchsubid%lb2outlet  !set false here, and true below
      !branchdata%uplakebasin = branchsubid%uplakebasin !set below
      
      !Calculate corresponding index
      DO i = 1,dim
        branchdata(i)%source = get_subid_index(source(i),ns)
        IF(branchdata(i)%source>0) branchindex(branchdata(i)%source) = i
        branchdata(i)%branch = get_subid_index(branch(i),ns)
      ENDDO
      IF(ALLOCATED(branchsubid)) DEALLOCATE(branchsubid)
      IF(ALLOCATED(source)) DEALLOCATE(source)
      IF(ALLOCATED(branch)) DEALLOCATE(branch)
      IF(SUM(branchindex)==0)THEN   !No branches in model
        IF(ALLOCATED(branchindex)) DEALLOCATE(branchindex)
        IF(ALLOCATED(branchdata)) DEALLOCATE(branchdata)
        WRITE(6,*) 'No branches in (sub-)model found'
      ENDIF
    ENDIF

    !>Check for connected lakebasins in branched flow path
    IF(ALLOCATED(branchdata))THEN
      dim = SIZE(branchdata)
      ALLOCATE(basins(dim))
      ALLOCATE(downbasins(dim))
      ALLOCATE(upbasin(dim))
      DO i = 1,dim
        basins(i) = branchdata(i)%source
        downbasins(i) = branchdata(i)%branch
      ENDDO
      CALL find_connected_lakebasins(dim,basins,downbasins,upbasin) 
      DO i = 1,dim
        branchdata(i)%uplakebasin = upbasin(i)
      ENDDO
      DEALLOCATE(basins)
      DEALLOCATE(downbasins)
      DEALLOCATE(upbasin)
    ENDIF

    !>Check for secondary outlet of lakebasins in branched flow path
    IF(ALLOCATED(branchdata).AND.ALLOCATED(lakebasinindex))THEN
      dim = SIZE(branchdata)
      DO i = 1,ns
        IF(lakebasinindex(i)>0)THEN
          DO ioutlet=2,elake(lakebasin(lakebasinindex(i))%ilk)%noutlet
            !>Find subbasin index that outlet originated from
            i2 = elake(lakebasin(lakebasinindex(i))%ilk)%outlet(ioutlet)%isb
            DO ibd = 1,dim
              isb = branchdata(ibd)%source
              IF(isb==i2)  branchdata(ibd)%lb2outlet = .TRUE.
            ENDDO
          ENDDO
        ENDIF
      ENDDO
    ENDIF
    
  END SUBROUTINE calculate_path

  !>Reform the information about subbasins to arrays for submodel
  !-------------------------------------------------------------------
  SUBROUTINE reform_inputdata_for_submodel(nsfile,ns,indexarray)

    USE MODVAR, ONLY : basin, &
                       classbasin, &
                       pathsubid, &
                       load, &
                       absinfo,psinfo, &
                       deposition, &
                       wetland, &
                       conduct, &
                       basintype,        &
                       classbasintype,   &
                       pathtype,         &
                       npcloadtype,      &
                       depositiontype,      &
                       wetlandtype,      &
                       soilleak, &
                       tobselevation, &
                       basinpar,         &
                       damindex,         & 
                       floodindex,       & 
                       lakeindex,        & 
                       lakeout2index,    & 
                       lakebasinindex,   & 
                       lakedataparindex, & 
                       glacierindex,     &
                       watertransfer, &
                       nwtransfer, &
                       nclass,           &
                       numsubstances,    &
                       modparid,         &
                       m_bpar,           &
                       max_par,          &
                       allocate_basinvariables, &
                       xobsindex,    &
                       max_outvar,   &
                       max_pstype, &
                       npsused,nabsused
    USE WORLDVAR, ONLY : qobsindex,    &
                         forcingdata, &
                         max_forcingdata, &
                         noutreg, &
                         outregion, &
                         subweightcrit

    !Argument declarations
    INTEGER, INTENT(IN) :: nsfile           !<number of subbasins in file
    INTEGER, INTENT(IN) :: ns               !<number of subbasins to be simulated
    INTEGER, INTENT(IN) :: indexarray(ns)   !<index for basemodel
    
    !Local variables 
    INTEGER i,isub,iforc,k,s,nbasinpar,temp_isb
    TYPE(BASINTYPE),ALLOCATABLE :: temp_basin(:)  !help variables for reindexing
    TYPE(PATHTYPE),ALLOCATABLE  :: temp_pathsubid(:)
    TYPE(NPCLOADTYPE),ALLOCATABLE  :: temp_load(:)
    TYPE(DEPOSITIONTYPE)  :: temp_deposition
    TYPE(CLASSBASINTYPE),ALLOCATABLE :: temp_classbasin(:,:)
    TYPE(WETLANDTYPE), ALLOCATABLE :: temp_wetland(:,:)
    REAL,ALLOCATABLE    :: temp_tobselev(:)
    REAL,ALLOCATABLE    :: temp_basinpar(:,:)
    REAL,ALLOCATABLE    :: temp_helpconc(:,:,:)
    REAL,ALLOCATABLE    :: temp_helpload(:,:,:,:)
    REAL,ALLOCATABLE    :: temp_subweightcrit(:)
    INTEGER,ALLOCATABLE :: temp_lakeindex(:)
    INTEGER,ALLOCATABLE :: temp_glacierindex(:)
    INTEGER,ALLOCATABLE :: temp_lakebasinindex(:)
    INTEGER,ALLOCATABLE :: temp_lakedataparindex(:,:)
    INTEGER,ALLOCATABLE :: tmp(:),xtmp(:,:)

    !Copy subbasin information from GeoData to temporary arrays
    IF(.NOT.ALLOCATED(temp_basin)) ALLOCATE(temp_basin(nsfile))
    temp_basin      = basin
    DEALLOCATE(basin)
    IF(.NOT.ALLOCATED(temp_pathsubid)) ALLOCATE(temp_pathsubid(nsfile))
    temp_pathsubid  = pathsubid
    DEALLOCATE(pathsubid)
    IF(.NOT.ALLOCATED(temp_classbasin)) ALLOCATE(temp_classbasin(nsfile,nclass))
    temp_classbasin = classbasin
    DEALLOCATE(classbasin)
    ALLOCATE(temp_load(nsfile))
    DO i = 1,nsfile
      IF(ALLOCATED(load(i)%psvol))THEN
        ALLOCATE(temp_load(i)%psvol(max_pstype))
        ALLOCATE(temp_load(i)%psload(max_pstype,numsubstances))
        DO k=1,max_pstype
          temp_load(i)%psvol(k) = load(i)%psvol(k)
          DO s=1,numsubstances
            temp_load(i)%psload(k,s) = load(i)%psload(k,s)
          ENDDO
        ENDDO
      ENDIF
      ALLOCATE(temp_load(i)%locconc(numsubstances))
      DO s=1,numsubstances
        temp_load(i)%locconc(s) = load(i)%locconc(s)
      ENDDO
    ENDDO
    temp_load%volloc   = load%volloc
    temp_load%abstrvol = load%abstrvol
    temp_load%abstrind = load%abstrind
    DO i = 1,nsfile
      IF(ALLOCATED(load(i)%psvol)) DEALLOCATE(load(i)%psvol)
      IF(ALLOCATED(load(i)%psload)) DEALLOCATE(load(i)%psload)
      IF(ALLOCATED(load(i)%locconc)) DEALLOCATE(load(i)%locconc)
    ENDDO
    DEALLOCATE(load)
    IF(ALLOCATED(absinfo))THEN
      DO i = 1,nabsused
        temp_isb = absinfo(i)%subindex
        absinfo(i)%subindex = 0
        DO isub = 1,ns
          IF(temp_isb == indexarray(isub))THEN
            absinfo(i)%subindex = isub
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    IF(ALLOCATED(psinfo))THEN
      DO i = 1,npsused
        temp_isb = psinfo(i)%subindex
        psinfo(i)%subindex = 0
        DO isub = 1,ns
          IF(temp_isb == indexarray(isub))THEN
            psinfo(i)%subindex = isub
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    IF(ALLOCATED(deposition%inloadwater)) THEN
      IF(.NOT.ALLOCATED(temp_deposition%inloadwater)) ALLOCATE(temp_deposition%inloadwater(nsfile,12))
      temp_deposition%inloadwater = deposition%inloadwater
      DEALLOCATE(deposition%inloadwater)
    ENDIF
    IF(ALLOCATED(deposition%indryload)) THEN
      IF(.NOT.ALLOCATED(temp_deposition%indryload)) ALLOCATE(temp_deposition%indryload(nsfile,3))
      temp_deposition%indryload = deposition%indryload
      DEALLOCATE(deposition%indryload)
    ENDIF
    IF(ALLOCATED(deposition%inwetconc)) THEN
      IF(.NOT.ALLOCATED(temp_deposition%inwetconc)) ALLOCATE(temp_deposition%inwetconc(nsfile))
      temp_deposition%inwetconc = deposition%inwetconc
      DEALLOCATE(deposition%inwetconc)
    ENDIF
    IF(.NOT.ALLOCATED(temp_wetland)) ALLOCATE(temp_wetland(nsfile,2))
    IF(conduct%riverwetland)THEN  
      temp_wetland = wetland
      DEALLOCATE(wetland)
    ENDIF
    IF(.NOT.ALLOCATED(temp_subweightcrit)) ALLOCATE(temp_subweightcrit(nsfile)) 
    temp_subweightcrit = subweightcrit
    DEALLOCATE(subweightcrit)

    !Allocate for and copy reform subbasin information
    CALL allocate_basinvariables(ns,numsubstances)
    basin(:)        = temp_basin(indexarray(:))
    DEALLOCATE(temp_basin)
    pathsubid(:)    = temp_pathsubid(indexarray(:))
    DEALLOCATE(temp_pathsubid)
    classbasin(:,:) = temp_classbasin(indexarray(:),:)
    DEALLOCATE(temp_classbasin)
    load(:)%volloc   = temp_load(indexarray(:))%volloc
    load(:)%abstrvol = temp_load(indexarray(:))%abstrvol
    load(:)%abstrind = temp_load(indexarray(:))%abstrind
    DO i = 1,ns
      IF(ALLOCATED(temp_load(indexarray(i))%psvol))THEN
        ALLOCATE(load(i)%psvol(max_pstype))
        ALLOCATE(load(i)%psload(max_pstype,numsubstances))
        DO k=1,max_pstype
          load(i)%psvol(k) = temp_load(indexarray(i))%psvol(k)
          DO s=1,numsubstances
            load(i)%psload(k,s) = temp_load(indexarray(i))%psload(k,s)
          ENDDO
        ENDDO
      ENDIF
      DO s=1,numsubstances
        load(i)%locconc(s) = temp_load(indexarray(i))%locconc(s)
      ENDDO
    ENDDO
    DO i = 1,nsfile
      IF(ALLOCATED(temp_load(i)%psvol)) DEALLOCATE(temp_load(i)%psvol)
      IF(ALLOCATED(temp_load(i)%psload)) DEALLOCATE(temp_load(i)%psload)
      IF(ALLOCATED(temp_load(i)%locconc)) DEALLOCATE(temp_load(i)%locconc)
    ENDDO
    DEALLOCATE(temp_load)
    IF(ALLOCATED(temp_deposition%inloadwater)) THEN
      deposition%inloadwater = temp_deposition%inloadwater(indexarray(:),:)
      DEALLOCATE(temp_deposition%inloadwater)
    ELSE
      IF(ALLOCATED(deposition%inloadwater)) DEALLOCATE(deposition%inloadwater)
    ENDIF
    IF(ALLOCATED(temp_deposition%indryload)) THEN
      deposition%indryload = temp_deposition%indryload(indexarray(:),:)
      DEALLOCATE(temp_deposition%indryload)
    ELSE
      IF(ALLOCATED(deposition%indryload)) DEALLOCATE(deposition%indryload)
    ENDIF
    IF(ALLOCATED(temp_deposition%inwetconc)) THEN
      deposition%inwetconc = temp_deposition%inwetconc(indexarray(:))
      DEALLOCATE(temp_deposition%inwetconc)
    ELSE
      IF(ALLOCATED(deposition%inwetconc)) DEALLOCATE(deposition%inwetconc)
    ENDIF
    IF(conduct%riverwetland)THEN  
      wetland(:,:)    = temp_wetland(indexarray(:),:)
      IF(SUM(wetland%area)==0)THEN
        conduct%riverwetland = .FALSE.
        DEALLOCATE(wetland)
      ENDIF
    ELSE
      IF(ALLOCATED(wetland)) DEALLOCATE(wetland)
    ENDIF
    DEALLOCATE(temp_wetland)
    ALLOCATE(subweightcrit(ns))
    subweightcrit = temp_subweightcrit(indexarray(:))
    DEALLOCATE(temp_subweightcrit)
    
    !Reform soilleak structure
    IF(ALLOCATED(soilleak%load))THEN
      ALLOCATE(temp_helpload(numsubstances,nclass,12,nsfile))
      temp_helpload = soilleak%load
      DEALLOCATE(soilleak%load)
      ALLOCATE(soilleak%load(numsubstances,nclass,12,ns))
      soilleak%load = temp_helpload(:,:,:,indexarray(:))
    ENDIF
    IF(ALLOCATED(soilleak%concentration))THEN
      ALLOCATE(temp_helpconc(numsubstances,12,nsfile))
      temp_helpconc = soilleak%concentration
      DEALLOCATE(soilleak%concentration)
      ALLOCATE(soilleak%concentration(numsubstances,12,ns))
      soilleak%concentration = temp_helpconc(:,:,indexarray(:))
    ENDIF
    
    ! nregions shall not be changed!

    DO i=1,noutreg
      DO isub=1,outregion(i)%nsubbasin
        DO k=1,ns
          IF(indexarray(k)==outregion(i)%subindex(isub))THEN
            outregion(i)%subindex(isub) = k
            EXIT
          ENDIF
        ENDDO
        IF(k>ns)THEN
          WRITE(6,*) 'Warning: outregion',i,'is not (fully) included in submodel.'
          outregion(i)%nsubbasin = 0  !To check against calculation
          outregion(i)%subindex = 0 !To find error
          EXIT
        ENDIF
      ENDDO
    ENDDO

    !Reform observation indexarray for submodel
    DO iforc = 1, max_forcingdata
      IF(ALLOCATED(forcingdata(iforc)%basinindex))THEN
        IF(.NOT.ALLOCATED(tmp)) ALLOCATE(tmp(ns))
        tmp = forcingdata(iforc)%basinindex(indexarray(:))
        DEALLOCATE(forcingdata(iforc)%basinindex)
        CALL MOVE_ALLOC(tmp,forcingdata(iforc)%basinindex)
      ENDIF
    ENDDO
    IF(ALLOCATED(qobsindex))THEN
      IF(.NOT.ALLOCATED(tmp)) ALLOCATE(tmp(ns))
      tmp = qobsindex(indexarray(:))
      DEALLOCATE(qobsindex)
      CALL MOVE_ALLOC(tmp,qobsindex)
    ENDIF
    IF(ALLOCATED(xobsindex))THEN 
      IF(.NOT.ALLOCATED(xtmp)) ALLOCATE(xtmp(max_outvar,ns))
      xtmp = xobsindex(:,indexarray(:))
      DEALLOCATE(xobsindex)
      CALL MOVE_ALLOC(xtmp,xobsindex)
    ENDIF

    !Reform subbasin information about forcing
    IF(ALLOCATED(tobselevation))THEN
      ALLOCATE(temp_tobselev(nsfile))
      temp_tobselev = tobselevation
      DEALLOCATE(tobselevation)
      ALLOCATE(tobselevation(ns))
      tobselevation(:)   = temp_tobselev(indexarray(:))
      DEALLOCATE(temp_tobselev)
    ENDIF
    
    !Reform subbasin information from LakeData and DamData
    !Only index for lakes handled the lakedata is kept as is
    IF(ALLOCATED(lakeindex))THEN
      IF(.NOT.ALLOCATED(temp_lakeindex)) ALLOCATE(temp_lakeindex(nsfile))
      temp_lakeindex    = lakeindex
      DEALLOCATE(lakeindex)
      ALLOCATE(lakeindex(ns))
      lakeindex(:)      = temp_lakeindex(indexarray(:))
      DEALLOCATE(temp_lakeindex)
    ENDIF
    IF(ALLOCATED(lakeout2index))THEN
      IF(.NOT.ALLOCATED(temp_lakeindex)) ALLOCATE(temp_lakeindex(nsfile))
      temp_lakeindex    = lakeout2index
      DEALLOCATE(lakeout2index)
      ALLOCATE(lakeout2index(ns))
      lakeout2index(:)      = temp_lakeindex(indexarray(:))
      DEALLOCATE(temp_lakeindex)
    ENDIF

    IF(ALLOCATED(lakebasinindex))THEN
      IF(.NOT.ALLOCATED(temp_lakebasinindex)) ALLOCATE(temp_lakebasinindex(nsfile))
      temp_lakebasinindex = lakebasinindex
      DEALLOCATE(lakebasinindex)
      ALLOCATE(lakebasinindex(ns))
      lakebasinindex(:) = temp_lakebasinindex(indexarray(:))
      DEALLOCATE(temp_lakebasinindex)
    ENDIF

    IF(ALLOCATED(lakedataparindex))THEN
      IF(.NOT.ALLOCATED(temp_lakedataparindex)) ALLOCATE(temp_lakedataparindex(nsfile,2))
      temp_lakedataparindex = lakedataparindex
      DEALLOCATE(lakedataparindex)
      ALLOCATE(lakedataparindex(ns,2))
      lakedataparindex(:,:) = temp_lakedataparindex(indexarray(:),:)
      DEALLOCATE(temp_lakedataparindex)
    ENDIF

    IF(ALLOCATED(damindex))THEN
      IF(.NOT.ALLOCATED(temp_lakeindex)) ALLOCATE(temp_lakeindex(nsfile))
      temp_lakeindex    = damindex
      DEALLOCATE(damindex)
      ALLOCATE(damindex(ns))
      damindex(:)      = temp_lakeindex(indexarray(:))
      DEALLOCATE(temp_lakeindex)
    ENDIF

      IF(ALLOCATED(glacierindex))THEN
        IF(.NOT.ALLOCATED(temp_glacierindex)) ALLOCATE(temp_glacierindex(nsfile))
        temp_glacierindex    = glacierindex
        DEALLOCATE(glacierindex)
        ALLOCATE(glacierindex(ns))
        glacierindex(:)      = temp_glacierindex(indexarray(:))
        DEALLOCATE(temp_glacierindex)
      ENDIF

    IF(ALLOCATED(floodindex))THEN
      IF(.NOT.ALLOCATED(temp_lakeindex)) ALLOCATE(temp_lakeindex(nsfile))
      temp_lakeindex    = floodindex
      DEALLOCATE(floodindex)
      ALLOCATE(floodindex(ns))
      floodindex(:)      = temp_lakeindex(indexarray(:))
      DEALLOCATE(temp_lakeindex)
    ENDIF

    IF(ALLOCATED(watertransfer))THEN
      DO i = 1,nwtransfer
        watertransfer(i)%rcvindex = indexarray(watertransfer(i)%rcvindex) !zero for outside
      ENDDO
    ENDIF

    !Model parameters reformation, only subbasin dependent parameters need to be changed
    nbasinpar = 0
    DO i = 1,max_par
      IF(modparid(i)%deptype==m_bpar)  nbasinpar = MAX(nbasinpar,modparid(i)%parno)
    ENDDO
    IF(.NOT.ALLOCATED(temp_basinpar)) ALLOCATE(temp_basinpar(nbasinpar,nsfile))
    temp_basinpar = basinpar
    IF(ALLOCATED(basinpar)) DEALLOCATE(basinpar)
    ALLOCATE(basinpar(nbasinpar,ns))
    basinpar = temp_basinpar(:,indexarray)
    IF(ALLOCATED(temp_basinpar)) DEALLOCATE(temp_basinpar)

  END SUBROUTINE reform_inputdata_for_submodel
  
  !>Finds the corresponding index for a subid
  !--------------------------------------------
  INTEGER FUNCTION get_subid_index(id,ns)

    USE MODVAR, ONLY : basin

    !Argument declarations
    INTEGER, INTENT(IN)  :: id    !<subid
    INTEGER, INTENT(IN)  :: ns    !<number of subbasins
    
    !Local variables
    INTEGER j

    !>\b Algorithm \n
    !>Loop through subbasin id: to find the index of the current one.
    get_subid_index = 0
    DO j=1,ns
      IF(basin(j)%subid==id)THEN
        get_subid_index = j
        EXIT
      ENDIF
    ENDDO

  END FUNCTION get_subid_index

  !>Reads information about crops from file
  !>
  !>\b Consequences Module modvar variables cropdata, cropirrdata 
  !>and cropdataindex may be allocated and set.
  !----------------------------------------------------------
  SUBROUTINE load_cropdata(dir,n,status) 

    USE WORLDVAR, ONLY : fileunit_temp, &
                         maxcharpath, &
                         i_str,i_intg,i_real
    USE MODVAR, ONLY : cropindex,      &  !OUT
                       cropdata,       &  !OUT
                       cropirrdata,    &  !OUT
                       basin, &
                       conduct, &
                       simulate, &
                       modeloption, &
                       p_erosion, &
                       p_soilleakage, &
                       set_cropdataindex, &
                       initiate_cropdata, &
                       i_t1,i_ss
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_cropdata, &
                                    e_error,e_warning,e_info
    
    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir     !<File directory
    INTEGER, INTENT(OUT) :: n                !<Number of rows in file
    INTEGER, INTENT(OUT) :: status           !<Error status
    
    !Local parameters
    INTEGER, PARAMETER :: nskip = 1   !heading on row 1
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    INTEGER i
    INTEGER mcols                     !Actual number of coulmns in file
    INTEGER sumirr
    INTEGER ncols                     !Number of columns in file
    INTEGER, ALLOCATABLE :: code(:)   !Data type code
    INTEGER, ALLOCATABLE :: iindex(:) !Index integer data from file
    INTEGER, ALLOCATABLE :: rindex(:) !Index real data from file
    INTEGER, ALLOCATABLE :: xi(:,:)   !integer data from file
    INTEGER, ALLOCATABLE :: cropregion(:) !CropData region read from file
    INTEGER, ALLOCATABLE :: cropid(:) !CropData cropid read from file
    REAL, ALLOCATABLE    :: xr(:,:)   !real data from file
    CHARACTER (LEN=letters),ALLOCATABLE :: colstr(:)   !Content string of data from file
    CHARACTER(LEN=maxcharpath) infile             !Name of file 
    LOGICAL calcNPC   !Status of nutrient and OC simulation
    LOGICAL fileexist
    CHARACTER(LEN=150) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    !>\b Algorithm \n
    status = 0

    calcNPC = conduct%simN .OR. conduct%simP .OR. conduct%simC
    infile = TRIM(dir)//'CropData.txt'

    !>Check if file exist and if it is required
    INQUIRE(FILE=TRIM(infile),EXIST=fileexist)
    IF(.NOT.fileexist)THEN
      IF(calcNPC)THEN
        !N / P / C modelling require the file
        IF(modeloption(p_soilleakage)==0)THEN !Jag tror det räcker
          WRITE(6,*) ' ERROR: Missing CropData.txt for NP simulation'  
          status = 1
        ENDIF
      ENDIF
      IF(simulate%substance(i_ss).AND. modeloption(p_erosion)==0.AND. modeloption(p_soilleakage)==0)THEN
        !S modelling with default erosion require the file
        WRITE(6,*) ' ERROR: Missing CropData.txt for S simulation'
        propagate_str = 'Missing CropData.txt for S simulation'
        CALL propagate_external_msg(e_cropdata,e_error,propagate_str)
        status = 1
      ENDIF
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(infile)

    !>Count number of crops and data columns
    CALL count_data_rows(fileunit_temp,TRIM(infile),nskip,n,status)
    IF(status/=0)RETURN
    CALL count_data_cols(fileunit_temp,TRIM(infile),nskip,ncols,status)
    IF(status/=0)RETURN
    IF(.NOT.ALLOCATED(colstr)) ALLOCATE(colstr(ncols))
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(n,ncols))
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(n,ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))

    !>Allocate and initiate cropdata variables
    IF(.NOT.ALLOCATED(cropregion)) ALLOCATE(cropregion(n))   
    IF(.NOT.ALLOCATED(cropid)) ALLOCATE(cropid(n))   
    IF(.NOT.ALLOCATED(cropdata)) ALLOCATE(cropdata(n))   
    IF(.NOT.ALLOCATED(cropirrdata)) ALLOCATE(cropirrdata(n))
    CALL initiate_cropdata()

    !>Read column headings from file
    OPEN(UNIT=fileunit_temp,FILE=TRIM(infile),STATUS='old',ACTION='read')
    CALL read_column_headings(fileunit_temp,ncols,letters,colstr,mcols,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(infile)
      propagate_str = 'reading file: '//TRIM(infile)
      CALL propagate_external_msg(e_cropdata,e_error,propagate_str)
      RETURN
    ENDIF

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*ncols)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !>Code variables for easy finding of variable type
    DO i = 1,mcols
      SELECT CASE(colstr(i)(1:letters))
        CASE('cropid    ','reg       ')
          code(i) = i_intg
        CASE('fn1       ','fn2       ','fp1       ','fp2       ')
          code(i) = i_real
        CASE('mn1       ','mn2       ','mp1       ','mp2       ')
          code(i) = i_real
        CASE('fday1     ','fday2     ','mday1     ','mday2     ')
          code(i) = i_intg
        CASE('fdown1    ','fdown2    ','mdown1    ','mdown2    ')
          code(i) = i_real
        CASE('resn      ','resp      ','resc      ')
          code(i) = i_real
        CASE('resday    ')
          code(i) = i_intg
        CASE('resdown   ','resfast   ')
          code(i) = i_real
        CASE('up1       ','up2       ','up3       ')
          code(i) = i_real
        CASE('bd1       ','bd2       ','bd3       ','bd4       ','bd5       ')
          code(i) = i_intg
        CASE('ccmax1    ','ccmax2    ','gcmax1    ','gcmax2    ')
          code(i) = i_real
        CASE('kcbini    ','kcbmid    ','kcbend    ')
          code(i) = i_real
        CASE('dlref     ')
          code(i) = i_real
        CASE('plantday  ','lengthini ','lengthdev ','lengthmid ','lengthlate')
          code(i) = i_intg
        CASE('imm_start ','imm_end   ')
          code(i) = i_intg
        CASE('upupper   ','pnupr     ','daylength ','gddsow    ','basetemp  ')
          code(i) = i_real
        CASE('firstday  ')
          code(i) = i_intg
        CASE('tamount   ','tdown1    ','tdown2    ')
          code(i) = i_real
        CASE('tyear     ','tday      ','tnumdays  ','tdaydown  ')
          code(i) = i_intg
        CASE DEFAULT
          code(i)=i_str    !string, ignore
      END SELECT
      IF(code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(colstr(i)(1:letters))
      ENDIF
    ENDDO

    CALL propagate_external_msg(e_cropdata,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !>Read the data of the crops to matrix, first column is string which is skipped
    CALL read_basindata5(fileunit_temp,infile,ncols,n,ncols,code,rindex,iindex,xi,xr) 
    CLOSE(fileunit_temp)

    !>Set cropdata variables according to matrix with read data
    DO i = 1,mcols
      IF(colstr(i)(1:letters)=='cropid    ')   cropid(1:n)                = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='reg       ')   cropregion(1:n)            = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='fn1       ')   cropdata(1:n)%fertnamount1 = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='fn2       ')   cropdata(1:n)%fertnamount2 = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='fp1       ')   cropdata(1:n)%fertpamount1 = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='fp2       ')   cropdata(1:n)%fertpamount2 = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='mn1       ')   cropdata(1:n)%mannamount1  = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='mn2       ')   cropdata(1:n)%mannamount2  = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='mp1       ')   cropdata(1:n)%manpamount1  = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='mp2       ')   cropdata(1:n)%manpamount2  = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='fday1     ')   cropdata(1:n)%fertday1     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='fday2     ')   cropdata(1:n)%fertday2     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='mday1     ')   cropdata(1:n)%manday1      = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='mday2     ')   cropdata(1:n)%manday2      = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='fdown1    ')   cropdata(1:n)%fertdown1    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='fdown2    ')   cropdata(1:n)%fertdown2    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='mdown1    ')   cropdata(1:n)%mandown1     = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='mdown2    ')   cropdata(1:n)%mandown2     = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='resn      ')   cropdata(1:n)%resnamount   = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='resp      ')   cropdata(1:n)%respamount   = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='resc      ')   cropdata(1:n)%rescamount   = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='resday    ')   cropdata(1:n)%resdayno     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='resdown   ')   cropdata(1:n)%resdown      = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='resfast   ')   cropdata(1:n)%resfast      = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='up1       ')   cropdata(1:n)%uptake1      = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='up2       ')   cropdata(1:n)%uptake2      = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='up3       ')   cropdata(1:n)%uptake3      = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='bd1       ')   cropdata(1:n)%baredayno1   = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='bd2       ')   cropdata(1:n)%baredayno2   = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='bd3       ')   cropdata(1:n)%baredayno3   = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='bd4       ')   cropdata(1:n)%baredayno4   = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='bd5       ')   cropdata(1:n)%baredayno5   = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='ccmax1    ')   cropdata(1:n)%ccmax1       = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='ccmax2    ')   cropdata(1:n)%ccmax2       = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='gcmax1    ')   cropdata(1:n)%gcmax1       = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='gcmax2    ')   cropdata(1:n)%gcmax2       = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='kcbini    ')   cropirrdata(1:n)%kcbini    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='kcbmid    ')   cropirrdata(1:n)%kcbmid    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='kcbend    ')   cropirrdata(1:n)%kcbend    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='dlref     ')   cropirrdata(1:n)%dlref     = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='plantday  ')   cropirrdata(1:n)%plantingdayno = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='lengthini ')   cropirrdata(1:n)%lengthini     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='lengthdev ')   cropirrdata(1:n)%lengthdev     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='lengthmid ')   cropirrdata(1:n)%lengthmid     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='lengthlate')   cropirrdata(1:n)%lengthlate    = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='imm_start ')   cropirrdata(1:n)%imm_start     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='imm_end   ')   cropirrdata(1:n)%imm_end       = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='upupper   ')   cropdata(1:n)%uptakeupper      = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='pnupr     ')   cropdata(1:n)%PNuptakeRatio    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='daylength ')   cropdata(1:n)%daylength    = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='gddsow    ')   cropdata(1:n)%gddsow       = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='basetemp  ')   cropdata(1:n)%basetemp     = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='firstday  ')   cropdata(1:n)%firstday     = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='tamount   ')   cropdata(1:n)%T1amount         = xr(1:n,rindex(i))*100.   !kg/ha->kg/km2
      IF(colstr(i)(1:letters)=='tyear     ')   cropdata(1:n)%T1year           = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='tday      ')   cropdata(1:n)%T1day            = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='tnumdays  ')   cropdata(1:n)%T1numberofdays   = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='tdaydown  ')   cropdata(1:n)%T1daydown        = xi(1:n,iindex(i))
      IF(colstr(i)(1:letters)=='tdown1    ')   cropdata(1:n)%T1down1          = xr(1:n,rindex(i))
      IF(colstr(i)(1:letters)=='tdown2    ')   cropdata(1:n)%T1down2          = xr(1:n,rindex(i))    
    ENDDO
    !>Check if cropdata is needed?
    IF(.NOT.(calcNPC .OR. i_ss>0 .OR. i_t1>0)) DEALLOCATE(cropdata)
    sumirr = SUM(cropirrdata(1:n)%plantingdayno)+SUM(cropirrdata(1:n)%imm_start)
    IF(sumirr==0) DEALLOCATE(cropirrdata)

    IF(calcNPC .OR. i_ss>0 .OR. (i_t1>0) .OR. (sumirr>0))THEN
      !>Calculate index variable to find crops
      CALL set_cropdataindex(n,cropid,cropregion,cropindex)
    ENDIF

    !>Initialize irrigation variables
    IF(sumirr>0) CALL set_irrigation_season_end(cropirrdata)
    
    !Check that regions matching with CropData. TODO: elaborate
    IF(ALLOCATED(cropindex))THEN
      IF(MAXVAL(basin%region)>SIZE(cropindex,2))THEN
        WRITE(6,*) 'ERROR: Maximum number of regions differ between GeoData and CropData'
        propagate_str = 'Maximum number of regions differ between GeoData and CropData'
        CALL propagate_external_msg(e_cropdata,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
    ENDIF
    
    !Deallocate local arrays
    DEALLOCATE(xr,xi,iindex,rindex,code,colstr,cropid,cropregion)
    WRITE(6,*) 'Crop (vegetation) information loaded'
    
  END SUBROUTINE load_cropdata
  

  !>Calculate irrigation season and immersion season, and set end of
  !>season to be larger than start of season.
  !----------------------------------------------------------------
  SUBROUTINE set_irrigation_season_end(cid)

    USE MODVAR, ONLY : CROPIRRDATATYPE, & 
                       ncrop         
    
    !Argument declarations
    TYPE(CROPIRRDATATYPE),ALLOCATABLE,INTENT(INOUT) :: cid(:) !<irrigation data of crops
    
    !Local variables
    INTEGER k

    DO k = 1,ncrop   
      IF(cid(k)%plantingdayno>0)THEN
        cid(k)%season_end = cid(k)%plantingdayno + cid(k)%lengthini + &
               cid(k)%lengthdev + cid(k)%lengthmid + cid(k)%lengthlate
      ELSE
        cid(k)%season_end = 0
      ENDIF
      IF(cid(k)%imm_start>0)THEN
        IF(cid(k)%imm_end < cid(k)%imm_start) cid(k)%imm_end = cid(k)%imm_end + 365
      ENDIF
    ENDDO

  END SUBROUTINE set_irrigation_season_end

  !>Get command line argument: directory of info.txt and simulation sequence, 
  !>or read them from filedir.txt (old variant).
  !-----------------------------------------------------------------------
  SUBROUTINE get_hyss_arguments(dir,iseq,pflag)
    
    USE WORLDVAR, ONLY : fileunit_temp, &
                         maxcharpath

    !Argument declarations
    CHARACTER(LEN=maxcharpath), INTENT(OUT) :: dir  !<Directory for information about simulation (infodir)
    INTEGER, INTENT(OUT) :: iseq                    !<Sequence number (simsequence)
    LOGICAL, INTENT(OUT) :: pflag                   !<Sequence number to be used on par-file (parseq)
    
    !Local variables
    INTEGER i       !Argument number
    INTEGER narg    !Number of command line argument
    INTEGER st      !Status
    INTEGER pos,oldpos  !Position on line
    LOGICAL nostr
    CHARACTER(LEN=maxcharpath) argument,argi  !Command line argument i
    CHARACTER(LEN=500) line

    !>\b Algorithm \n
    iseq = 0
    pflag = .FALSE.
    pos = 1
    dir=''
    narg = COMMAND_ARGUMENT_COUNT()
    !>If no command line argument, read information from filedir.txt
    IF(narg==0)THEN
      OPEN(fileunit_temp,FILE = 'filedir.txt',STATUS = 'old',ACTION='read',ERR=200)
      READ(fileunit_temp,'(A)',END=300,ERR=300) line
      DO
        nostr = .FALSE.
        oldpos = pos
        CALL read_next_codestr_on_line(500,maxcharpath,pos,line,argument,nostr,'err',.TRUE.)
        IF(nostr) EXIT
        argi=TRIM(argument)
        SELECT CASE(argi)
        CASE('-sequence','-s')
          CALL read_next_codestr_on_line(500,maxcharpath,pos,line,argument,nostr,'err')
          READ(argument,*,END=101,ERR=101) iseq
        CASE('-infodir','-i')
          CALL read_next_codestr_on_line(500,maxcharpath,pos,line,argument,nostr,'err',.TRUE.)
          IF(.NOT.nostr)THEN
            dir=TRIM(argument)
          ELSE
            WRITE(*,*) 'ERROR reading infodir from filedir.txt'
          ENDIF
        CASE('-p')
          pflag = .TRUE.
        CASE DEFAULT
          IF(oldpos==1)THEN
            dir=argi
          ELSE
            WRITE(*,*) 'ERROR: No file path found in filedir.txt.'
            STOP 1
          ENDIF
        END SELECT
      ENDDO
      CLOSE(fileunit_temp)
      RETURN
    ENDIF
    
    !>Get command line argument
    i = 1
    DO WHILE(i<=narg)
      CALL GET_COMMAND_ARGUMENT(i,argument)
      argi=TRIM(argument)
      SELECT CASE(argi)
      CASE('-sequence','-s')
        i = i + 1
        CALL GET_COMMAND_ARGUMENT(i,argument)
        READ(argument,*,END=100,ERR=100) iseq
      CASE('-infodir','-i')
        i = i + 1
        CALL GET_COMMAND_ARGUMENT(i,argument,STATUS=st)
        IF(st==0)THEN
          dir=TRIM(argument)
        ELSE
          WRITE(*,*) 'ERROR reading infodir command line argument'
        ENDIF
      CASE('-p')
        pflag = .TRUE.
      CASE DEFAULT
        IF(i==1)THEN
          dir=argi
        ELSE
          WRITE(*,*) 'ERROR in command line argument'
          STOP 1
        ENDIF
      END SELECT
      i = i + 1
    ENDDO
    RETURN

    !Error handling
100 WRITE(*,*) 'ERROR reading sequence command line argument'      
    STOP 1
101 WRITE(*,*) 'ERROR reading sequence from filedir.txt'      
    STOP 1
200 WRITE(*,*) 'ERROR no command line argument and no filedir.txt found' 
    STOP 1
300 WRITE(*,*) 'ERROR reading filedir.txt' 
    STOP 1

  END SUBROUTINE get_hyss_arguments

  !\brief Reads file with information about the current simulation 
  !!
  !!Format of file: first on line is a coded string followed by the value 
  !!The string "!!" is used for comment rows and are not read
  !!
  !>\b Consequences The following worldvar module variables is set: readdaily,
  !> readobsid, writeload, readmatlab, writematlab, numoutstate, pstateoutdate,
  !>  bstateoutdate, ncrit, readformat, simsubmodel, resultseq, steplen, 
  !> forcingdata, resetstate. The following modvar 
  !> module variables is set: i_in,i_on,i_sp,i_pp,i_t1,i_t2,i_oc,simulatesubstances,
  !> conduct,conductload,conductregest,conductwarnon and modeloption.
  !--------------------------------------------------------------------
  SUBROUTINE load_coded_info(dir,status,date1,date2,date3,skip,step,nsubst,     &
                             stateinput, &
                             rsmax,rsnum,readstatedate, &
                             mdir,rdir,fdir,ldir,                           &
                             locupall,locupnone,subincrit) 

    USE WORLDVAR, ONLY : doens,doopt, &
                         fileunit_temp, &
                         maxcharpath, &
                         maxoutbasins, &
                         maxcritbasins, &
                         maxcrit, &
                         max_typeofperiods, &
                         maxclassingroup, &
                         comment_str, &
                         set_calibration, &
                         set_output, &
                         noutput, &   !OUT
                         set_maxmap, &
                         readdaily, &   !OUT
                         readobsid, &   !OUT
                         readpstime, & !OUT
                         writeload, &   !OUT
                         readmatlab, &   !OUT
                         writematlab, &   !OUT
                         weightsub, &   !OUT
                         usestop84, & !OUT
                         ncrit, &   !OUT
                         readformat, &   !OUT
                         readoutregion, &   !OUT
                         simsubmodel, &   !OUT
                         simsequence, &
                         resultseq, &   !OUT
                         i_d, &
                         steplen, &   !(OUT)
                         forcingdata, & !OUT
                         max_forcingdata, &
                         allocate_and_initialize_forcingdata_structure, &
                         i_sfobs,i_swobs, & 
                         i_tminobs,i_tmaxobs, & 
                         indatacheckonoff, &   !OUT
                         indatachecklevel, &   !OUT
                         doassimilation, &       !OUT
                         resetstate, &  !OUT
                         outstate, & !OUT
                         initialize_outstate, &
                         set_outstate_dates
    USE MODVAR, ONLY : i_in,i_on,i_sp,i_pp, &   !OUT
                       i_t1,i_t2,i_oc,i_ss,i_ae, &   !OUT
                       doupdate, &
                       conduct, & !OUT
                       conductxoms, & !OUT
                       conductwb, & !OUT
                       conductload, &  !OUT
                       conductregest, &  !OUT
                       soiliniwet, &  !OUT
                       conductwarning, & !OUT
                       simulate,simulatesubstances, & !OUT
                       max_outvar, &
                       classgroup2,&  !OUT
                       i_quseobs, &
                       i_qar,i_war, &
                       i_wendupd, &
                       i_tpcorr, &
                       i_tncorr, &
                       i_tploccorr, &
                       i_tnloccorr, &
                       i_cuseobs, &
                       dim_update, &
                       irrunlimited, &
                       allocate_initialize_modeloptions, &
                       initialize_simulation_configuration, &
                       modeloptionname, &
                       modeloption, &    !OUT
                       num_modelprocess, &   
                       p_connectivity, &
                       p_deepgroundwater, &
                       p_erosion, &
                       p_floodplain, &
                       p_frozensoil, &
                       p_glacierini, &
                       p_growthstart, &
                       p_infiltration, &
                       p_lakeriverice, & 
                       p_petmodel, &
                       p_soilleakage, &
                       p_surfacerunoff, &
                       p_snowdensity, &
                       p_snowevap, &
                       p_snowfall, &
                       p_snowfalldist, &
                       p_snowheat, &
                       p_snowmelt, &
                       p_swtemperature, &
                       p_wetland, &
                       simtimestep, simtimeunit
  USE READWRITE_ROUTINES, ONLY : convert_string_to_integer, &
                                 read_next_date_on_line, &
                                 print_output_information_to_logfile, &
                                 get_number_of_classgroups
  USE CONVERT, ONLY : integer_convert_to_string,&
                      real_to_str, &
                      scalar_lower_case
  USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_info_file, &
                                    e_error,e_warning,e_info

    !Argument declarations
    CHARACTER(LEN=maxcharpath), INTENT(IN) :: dir   !<File directory
    INTEGER, INTENT(OUT) :: status                  !<Status of subroutine
    TYPE(DateType), INTENT(OUT) :: date1            !<Begin date of simulation
    TYPE(DateType), INTENT(OUT) :: date2            !<End date of simulation
    TYPE(DateType), INTENT(OUT) :: date3            !<Begin date for criteria/output date
    INTEGER, INTENT(OUT) :: skip                    !<Length of warm-up period
    INTEGER, INTENT(OUT) :: step                    !<Length of simulation period
    INTEGER, INTENT(OUT) :: nsubst                  !<Number of substances simulated
    LOGICAL, INTENT(OUT) :: stateinput              !<Code for reading input state
    INTEGER, INTENT(IN)  :: rsmax                   !<Dimension dates for reading soil state
    INTEGER, INTENT(OUT) :: rsnum                   !<Number of dates for reading soil state
    TYPE(DateType), INTENT(OUT) :: readstatedate(rsmax) !<Dates for reading soil state variables
    CHARACTER(LEN=maxcharpath), INTENT(OUT) :: mdir           !<File directory model
    CHARACTER(LEN=maxcharpath), INTENT(OUT) :: rdir           !<File directory result
    CHARACTER(LEN=maxcharpath), INTENT(OUT) :: fdir           !<File directory forcing
    CHARACTER(LEN=maxcharpath), INTENT(OUT) :: ldir           !<File directory log
    LOGICAL, INTENT(OUT) :: locupall(dim_update)     !<Flag for update for all stations
    LOGICAL, INTENT(OUT) :: locupnone(dim_update)    !<Flag for update on no stations
    INTEGER, ALLOCATABLE, INTENT(INOUT) :: subincrit(:)      !<Subbasins to be included in criteria calculations
    
    !Local parameters
    INTEGER, PARAMETER :: linelen = 18000      
    INTEGER, PARAMETER :: maxgroups = 1000       !Maximum number of class groups that can be read
    CHARACTER (LEN=2), PARAMETER :: errstr = '##'  
    INTEGER, PARAMETER :: maxoutstates = 10     !Max number of dates for saving state
    
    !Local variables
    LOGICAL error
    LOGICAL onoff
    LOGICAL calibration
    LOGICAL nostrfound,notimefound
    LOGICAL exitbigloop
    LOGICAL cyclebigloop
    LOGICAL outvarallbasin(max_typeofperiods)
    LOGICAL icheckread,basinoutputicheck,regionoutputicheck,timeoutputicheck,classoutputicheck
    INTEGER io,i,iforc
    INTEGER temp
    INTEGER maxdim
    INTEGER intvalue
    CHARACTER(LEN=3), ALLOCATABLE :: c(:)
    CHARACTER(LEN=6), ALLOCATABLE :: critcgroup(:)
    INTEGER, ALLOCATABLE :: critc(:)
    INTEGER, ALLOCATABLE :: critr(:)
    INTEGER, ALLOCATABLE :: flowc(:)
    INTEGER, ALLOCATABLE :: flowr(:)
    INTEGER, ALLOCATABLE :: critareac(:)
    INTEGER, ALLOCATABLE :: critarear(:)
    REAL, ALLOCATABLE :: critweight(:)
    REAL, ALLOCATABLE :: critpar(:)
    INTEGER istate
    INTEGER ioutbasin(max_typeofperiods)
    INTEGER readbasins(max_typeofperiods,maxoutbasins)
    INTEGER icoutbasin(max_typeofperiods)
    INTEGER readcbasins(max_typeofperiods,maxoutbasins)
    INTEGER ioutregion(max_typeofperiods)
    INTEGER readregions(max_typeofperiods,maxoutbasins)
    INTEGER idefgroup,ndefgroup(maxgroups)
    INTEGER readdefgroups(maxgroups,maxclassingroup)
    INTEGER ioutgroups(maxgroups)
    LOGICAL classgroupallclass
    INTEGER j
    INTEGER ibasinoutputvar(max_typeofperiods)
    INTEGER iregionoutputvar(max_typeofperiods)
    INTEGER imapoutputvar(max_typeofperiods)
    INTEGER itimeoutputvar(max_typeofperiods)
    INTEGER iclassoutputvar(maxgroups)
    INTEGER ivarindex
    INTEGER varindex,flowtype
    INTEGER basinoutputperiod(max_typeofperiods)
    INTEGER regionoutputperiod(max_typeofperiods)
    INTEGER mapoutputperiod(max_typeofperiods)
    INTEGER timeoutputperiod(max_typeofperiods)
    INTEGER classoutputperiod(maxgroups)
    INTEGER basinoutputdecimal(max_typeofperiods)
    INTEGER regionoutputdecimal(max_typeofperiods)
    INTEGER mapoutputdecimal(max_typeofperiods)
    INTEGER timeoutputdecimal(max_typeofperiods)
    INTEGER classoutputdecimal(maxgroups)
    INTEGER basinoutputsignif(max_typeofperiods)
    INTEGER regionoutputsignif(max_typeofperiods)
    INTEGER mapoutputsignif(max_typeofperiods)
    INTEGER timeoutputsignif(max_typeofperiods)
    INTEGER classoutputsignif(maxgroups)
    INTEGER basinoutputvar(max_typeofperiods,max_outvar,2)
    INTEGER regionoutputvar(max_typeofperiods,max_outvar)
    INTEGER mapoutputvar(max_typeofperiods,max_outvar,2)
    INTEGER timeoutputvar(max_typeofperiods,max_outvar,2)
    INTEGER classoutputvar(maxgroups,max_outvar,2)
    LOGICAL classoutvarallbasin(maxgroups)
    TYPE(DateType) :: bstateoutdate(maxoutstates)     !Read dates for saving state
    INTEGER icrit,critperiod,critlimit
    INTEGER icritbasin
    INTEGER readcritbasins(maxoutbasins)
    INTEGER tstep,indx !Time step length [stepunit] and string index (used with stepstr)
    INTEGER linepos
    INTEGER icheck,areagg
    CHARACTER (LEN=linelen) :: line
    CHARACTER (LEN=16) :: code,code2,code3
    CHARACTER (LEN=10) :: strvalue
    CHARACTER (LEN=16) :: strdate,strdate1,strdate2,strdate3
    CHARACTER (LEN=16), ALLOCATABLE :: strstates(:),strrstates(:)
    CHARACTER (LEN=8)  :: namestr
    CHARACTER (LEN=6)  :: varstr
    CHARACTER (LEN=6)  :: defgroupname(maxgroups)
    CHARACTER (LEN=6)  :: readgroupsnames(maxgroups,maxgroups)
    CHARACTER (LEN=6)  :: defaultgroupsnames(maxgroups)
    CHARACTER (LEN=3)  :: yesnostr
    CHARACTER (LEN=3)  :: critstr
    CHARACTER (LEN=maxcharpath) :: filedir
    CHARACTER (LEN=maxcharpath+8) :: filename
    CHARACTER (LEN=4)  :: stepunit      !Time step unit; mo,d,h or min
    CHARACTER (LEN=10) :: stepstr       !Temporary storage for time step string
    LOGICAL, ALLOCATABLE :: critcond(:) !flag that this criteria should only be used conditionally /DG
    REAL, ALLOCATABLE :: critthres(:)   !acceptance criteria for conditional criteria /DG
    
    CHARACTER(LEN=250) :: propagate_str

    status = 0
    error = .FALSE.
    ncrit = maxcrit
    ALLOCATE(c(ncrit),critweight(ncrit),critc(ncrit),flowc(ncrit),    &
             critr(ncrit),flowr(ncrit),critpar(ncrit), &
             critcond(ncrit),critthres(ncrit), &
             critareac(ncrit),critarear(ncrit),critcgroup(ncrit))
    ncrit = 0
    !initialize the conditional criteria and threshold
    critcond = .FALSE.
    critthres = 0.
    critcgroup = ''

    !Open file      
    filename=TRIM(dir)//'info.txt'
    OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read')
    WRITE(6,*) 'File opened: ', TRIM(filename)

    !Default values
    calibration    = .FALSE.
    doens          = .FALSE.
    doassimilation = .FALSE.
    stateinput     = .FALSE.
    writematlab    = .FALSE.
    readmatlab     = .FALSE.
    readpstime     = .FALSE.
    outvarallbasin = .FALSE.
    exitbigloop    = .FALSE.
    simsubmodel    = .FALSE.
    simulatesubstances  = .FALSE.
    doupdate       = .FALSE.
    locupall       = .FALSE.
    locupnone      = .FALSE.
    readdaily      = .FALSE.
    writeload      = .FALSE.
    readobsid      = .TRUE. 
    usestop84      = .FALSE.
    resetstate     = .FALSE.
    irrunlimited   = .FALSE.                !Unlimited irrigation, defaults to False
    soiliniwet     = .FALSE.
    simulate%soildepthstretch = .FALSE.
    conductregest  = .FALSE.
    resultseq      = .FALSE.  
    IF(simsequence>0) resultseq = .TRUE.  
    CALL allocate_and_initialize_forcingdata_structure()
    readoutregion  = .FALSE.
    conductxoms    = .FALSE.
    conductwb      = .FALSE.
    CALL allocate_initialize_modeloptions()
    CALL initialize_simulation_configuration()
    conductload    = .FALSE.
    conductwarning    = .TRUE.
    CALL initialize_outstate()
    bstateoutdate  = DateType(0,0,0,0,0)
    date3          = DateType(0,0,0,0,0)
    readstatedate  = DateType(0,0,0,0,0)
    ncrit          = 0
    readformat     = 0         !ascii
    nsubst         = 0
    i_in=0;i_on=0;i_sp=0;i_pp=0;i_t1=0;i_t2=0;i_oc=0;i_ss=0;i_ae=0
    mdir = ''
    rdir = ''
    fdir = ''
    ldir = ''
    ioutbasin          = 0
    icoutbasin         = 0
    ioutregion         = 0
    ioutgroups         = 0
    idefgroup          = 0
    ndefgroup          = 0
    defgroupname = ''
    ibasinoutputvar    = 0
    iregionoutputvar   = 0
    imapoutputvar      = 0
    itimeoutputvar     = 0
    iclassoutputvar     = 0
    basinoutputvar     = 0
    regionoutputvar    = 0
    mapoutputvar       = 0
    timeoutputvar      = 0
    classoutputvar      = 0
    basinoutputperiod  = 1   !daily
    regionoutputperiod = 1   !daily
    mapoutputperiod    = 5   !mean over simulation period
    timeoutputperiod   = 1   !daily
    classoutputperiod   = 1   !daily
    basinoutputdecimal = 1
    regionoutputdecimal = 1
    mapoutputdecimal   = 1
    timeoutputdecimal  = 3
    classoutputdecimal  = 1
    basinoutputsignif  = 0
    regionoutputsignif = 0
    mapoutputsignif    = 0
    timeoutputsignif   = 0
    classoutputsignif   = 0
    classoutvarallbasin = .FALSE.
    classgroupallclass = .FALSE.
    icritbasin          = 0
    critperiod         = i_d  !for calvarper
    critlimit          = 3    !for calvarlim
    readcritbasins     = 0
    rsnum              = 0
    tstep              = 1    ! Default time step length is 1 ..
    stepunit           = 'd'  ! .. day

    !Read content of file      
    DO
      READ(fileunit_temp,600,END=200,ERR=800,IOSTAT=io) line
      linepos = 1
      CALL read_next_codestr_on_line(linelen,16,linepos,line,code,nostrfound,errstr)    !Read code
      IF(code(1:2)==errstr)THEN
        error = .TRUE.
        code=line(1:16)
        EXIT
      ENDIF
      IF(code(1:2)==comment_str) CYCLE
      IF(nostrfound) CYCLE  !empty row
      cyclebigloop = .FALSE.

      !Read date   
      IF(code(1:5)=='bdate' .OR. &
         code(1:5)=='cdate' .OR. &
         code(1:5)=='edate' .OR. &
         code(1:14)=='resetstatedate' .OR. &
         code(1:12)=='outstatedate')THEN
        CALL read_next_date_on_line(linelen,16,linepos,line,strdate,nostrfound,notimefound,errstr)
        IF(nostrfound.OR.strdate(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        SELECT CASE(code)
        CASE ('bdate')
          CALL string_convert_to_DateType(strdate,date1)
          CYCLE
        CASE ('cdate')
          CALL string_convert_to_DateType(strdate,date3)
          CYCLE
        CASE ('edate')
          CALL string_convert_to_DateType(strdate,date2)
          CYCLE
        CASE ('outstatedate')
          CALL scalar_lower_case(strdate)
          IF(strdate(1:3)=='all')THEN
            outstate%doall = .TRUE.
            CYCLE
          ELSEIF(strdate(1:6)=='period')THEN
            outstate%doperiod = .TRUE.
            IF(strdate(9:9)<=CHAR(57) .AND. strdate(9:9)>=CHAR(48))THEN   !0-9 !Reading "period" with date-read can give to long strdate return extra letters to line
              linepos=linepos-6
            ELSE
              linepos = linepos-2
            ENDIF
            CALL read_next_date_on_line(linelen,16,linepos,line,strdate,nostrfound,notimefound,errstr)
            IF(nostrfound.OR.strdate(1:2)==errstr)THEN
              WRITE(6,*) 'ERROR: No dates given for outstatedate period'
              propagate_str = 'Not correct date for outstatedate period in info.txt'
              CALL propagate_external_msg(e_info_file,e_error,propagate_str)
              status = 1
              RETURN
            ENDIF
            CALL string_convert_to_DateType(strdate,bstateoutdate(1))
            CALL read_next_date_on_line(linelen,16,linepos,line,strdate,nostrfound,notimefound,errstr)
            IF(nostrfound.OR.strdate(1:2)==errstr)THEN
              WRITE(6,*) 'ERROR: No dates given for outstatedate period'
              propagate_str = 'Not correct date for outstatedate period in info.txt'
              CALL propagate_external_msg(e_info_file,e_error,propagate_str)
              status = 1
              RETURN
            ENDIF
            CALL string_convert_to_DateType(strdate,bstateoutdate(2))
            CYCLE
          ELSE    !given dates
            DO
              outstate%numdates = outstate%numdates + 1
              IF(outstate%numdates>maxoutstates) THEN
                WRITE(6,*) 'ERROR: Too many outstatedate in info.txt'
                propagate_str = 'Too many outstatedate in info.txt'
                CALL propagate_external_msg(e_info_file,e_error,propagate_str)
                status = 1
                RETURN
              ENDIF
              CALL string_convert_to_DateType(strdate,bstateoutdate(outstate%numdates))
              CALL read_next_date_on_line(linelen,16,linepos,line,strdate,nostrfound,notimefound,errstr)
              IF(nostrfound.OR.strdate(1:2)==errstr)THEN
                EXIT
              ENDIF
            ENDDO
            CYCLE
          ENDIF
        CASE ('resetstatedate')
          DO
            rsnum = rsnum + 1
            IF(rsnum>rsmax) THEN
              WRITE(6,*) 'ERROR: Too many readstatedate in info.txt'
              status = 1
              RETURN
            ENDIF
            CALL string_convert_to_DateType(strdate,readstatedate(rsnum))
            CALL read_next_date_on_line(linelen,16,linepos,line,strdate,nostrfound,notimefound,errstr)
            IF(nostrfound.OR.strdate(1:2)==errstr)THEN
              EXIT
            ENDIF
          ENDDO
          CYCLE
        END SELECT
      ENDIF

      !Read time step, if present
      IF(code(1:10)=='steplength')THEN
        CALL read_next_codestr_on_line(linelen,10,linepos,line,stepstr,nostrfound,errstr,.TRUE.)
        IF(nostrfound)THEN   ! No error here if not defined, only using default values
          WRITE(6,'(A60,I2,A3)') &
                  'WARNING: No value for steplength found, using default values',tstep,stepunit
        ELSE
          indx=SCAN(stepstr,'dhm') ! Looking for unit: d,h or min
          IF(indx.GT.0)THEN
            READ(stepstr(1:indx-1),'(I4)',ERR=801) tstep
            stepunit=stepstr(indx:LEN(stepstr))
          ELSE
            WRITE(6,'(A65,I2,A3)') &
                   'WARNING: No valid unit for steplength found, using default values',tstep,stepunit
          ENDIF
        ENDIF
        CYCLE
      ENDIF

      !Read yes or no coded variables
      DO iforc = 1,max_forcingdata
        IF(code(1:12)==forcingdata(iforc)%infocode)THEN
          status = set_yesno_variable_from_line(linelen,linepos,line,errstr,forcingdata(iforc)%readfile)
          IF(status/=0)THEN
            error = .TRUE.
            exitbigloop = .TRUE.
          ELSE
            cyclebigloop = .TRUE.
          ENDIF
          EXIT
        ENDIF
      ENDDO
      IF(exitbigloop) EXIT
      IF(cyclebigloop) CYCLE
      IF(code(1:12)=='assimilation'.OR. &
         code(1:11)=='calibration' .OR. &
         code(1:11)=='parensemble' .OR. &
         code(1:9) =='weightsub'   .OR. &
         code(1:9) =='readdaily'   .OR. &
         code(1:9) =='printload'   .OR. &
         code(1:8) =='submodel'    .OR. &
         code(1:7) =='instate'     .OR. &
         code(1:8) =='resseqnr'    .OR. &
         code(1:12)=='irrunlimited'.OR. &
         code(1:9) =='readobsid'   .OR. &
         code(1:10)=='readpstime'  .OR. &
         code(1:9) =='usestop84'   .OR. &
         code(1:13)=='printwaterbal' .OR. &
         code(1:7)=='warning' .OR. &
         code(1:10)=='soiliniwet' .OR. &
         code(1:11)=='soilstretch' .OR. &
         code(1:11)=='regestimate' .OR. &
         code(1:13)=='readoutregion' .OR. &
         code(1:13)=='readxomsfiles')THEN
        CALL read_next_codestr_on_line(linelen,3,linepos,line,yesnostr,nostrfound,errstr)
        IF(nostrfound.OR.yesnostr(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        IF(yesnostr(1:1)=='Y' .OR. yesnostr(1:1)=='y' .OR.    &
           yesnostr(1:1)=='J' .OR. yesnostr(1:1)=='j')THEN
          onoff = .TRUE.
        ELSEIF(yesnostr(1:1)=='N' .OR. yesnostr(1:1)=='n')THEN
          onoff = .FALSE.
        ELSE
          error = .TRUE.
          EXIT
        ENDIF
        SELECT CASE(code)
        CASE ('assimilation')
          doassimilation = onoff
        CASE ('calibration')
          calibration = onoff
        CASE ('parensemble')
          doens = onoff
        CASE ('weightsub')
          weightsub = onoff
        CASE ('readdaily')
          readdaily = onoff
        CASE ('printload')
          writeload = onoff
        CASE ('submodel')
          simsubmodel = onoff
        CASE ('instate')
          stateinput = onoff
        CASE ('resseqnr')
          resultseq = onoff
        CASE ('readobsid')
          readobsid = onoff
        CASE ('readpstime')
          readpstime = onoff
        CASE ('usestop84')
          usestop84 = onoff
        CASE ('readoutregion')
          readoutregion = onoff
        CASE ('readxomsfiles')
          conductxoms = onoff
        CASE ('printwaterbal')
          conductwb = onoff
        CASE ('warning')
          conductwarning = onoff
        CASE ('irrunlimited')
          irrunlimited = onoff
        CASE ('soiliniwet')
          soiliniwet = onoff
        CASE ('soilstretch')
          simulate%soildepthstretch = onoff
        CASE ('regestimate')
          conductregest = onoff
        END SELECT
        CYCLE
      ENDIF

      !Read integer coded variables
      IF(code(1:10)=='readformat' .OR. &
         code(1:11)=='writeformat' .OR. &
         code(1:11)=='indatacheck')THEN
        CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
        IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        READ(strvalue,*,ERR=801) intvalue
        IF(code(1:10)=='readformat')THEN
          IF(intvalue==1) readmatlab = .TRUE.     
        ELSEIF(code(1:11)=='writeformat')THEN
          IF(intvalue==1) writematlab = .TRUE.
        ELSEIF(code(1:16)=='indatachecklevel')THEN
          indatachecklevel = intvalue
        ELSEIF(code(1:16)=='indatacheckonoff')THEN
          indatacheckonoff = intvalue
        ENDIF
        CYCLE
      ENDIF

      !Read optional model structures coded with integers
      IF(code(1:11)=='modeloption')THEN
        CALL read_next_codestr_on_line(linelen,16,linepos,line,code2,nostrfound,errstr)
        IF(nostrfound.OR.code2(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
        IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        READ(strvalue,*,ERR=801) intvalue
        DO ivarindex = 1,num_modelprocess
          IF(code2==modeloptionname(ivarindex))THEN
            modeloption(ivarindex) = intvalue
            IF(ivarindex==p_lakeriverice)THEN
              !Make sure T2 temperature model is switched on if lake and river ice model is used
              IF(i_t2.LE.0 .AND. modeloption(p_lakeriverice).GE.1)THEN
                nsubst=nsubst+1
                i_t2 = nsubst
                conduct%simT2 = .TRUE.
              ENDIF
            ENDIF
            cyclebigloop = .TRUE.
            EXIT
          ENDIF
          !No matching modeloption found. Continue to get caught before end of loop
        ENDDO
        IF(cyclebigloop) CYCLE
      ENDIF

      !Read substances to be simulated
      IF(code(1:9) =='substance')THEN
        DO
          CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
          IF(strvalue(1:2)==errstr)THEN
            error = .TRUE.
            exitbigloop = .TRUE.
            EXIT
          ENDIF
          IF(nostrfound)THEN
            cyclebigloop = .TRUE.
            EXIT
          ENDIF
          SELECT CASE(strvalue(1:1))
          CASE('N','n')
            i_in = nsubst+1
            i_on = nsubst+2
            nsubst=nsubst+2
            conduct%simN = .TRUE.
          CASE('P','p')
            i_sp = nsubst+1
            i_pp = nsubst+2
            nsubst=nsubst+2
            conduct%simP = .TRUE.
          CASE('C','c')
            nsubst=nsubst+1
            i_oc = nsubst
            conduct%simC = .TRUE.
          CASE('T','t')
            SELECT CASE(strvalue(1:2))
            CASE('T1','t1')
              nsubst=nsubst+1
              i_t1 = nsubst
              conduct%simT1 = .TRUE.
            CASE('T2','t2')
              !Make sure T2 is only initiated once (see lake and river ice setting above)
              IF(i_t2.LE.0)THEN
                nsubst=nsubst+1
                i_t2 = nsubst
                conduct%simT2 = .TRUE.
                !Is T2 model relevant without lake and river ice model?
              ENDIF
            END SELECT
          CASE('S','s')
            i_ss = nsubst+1
            i_ae = nsubst+2
            nsubst=nsubst+2
            conduct%simS = .TRUE.
          END SELECT
        ENDDO
        IF(exitbigloop)EXIT
        IF(cyclebigloop)CYCLE
      ENDIF

      !Read directory paths
      IF(code(1:8)=='modeldir' .OR. &
         code(1:10)=='forcingdir'.OR. &
         code(1:9)=='resultdir' .OR. &
         code(1:6)=='logdir')THEN
        CALL read_next_codestr_on_line(linelen,maxcharpath,linepos,line,filedir,nostrfound,errstr,.TRUE.)
        IF(nostrfound.OR.filedir(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        IF(code(1:8)=='modeldir') mdir = filedir
        IF(code(1:9)=='resultdir') rdir = filedir
        IF(code(1:10)=='forcingdir') fdir = filedir
        IF(code(1:6)=='logdir') ldir = filedir
        CYCLE
      ENDIF

      !Read output information
      IF(code(1:11)=='basinoutput' .OR. &
         code(1:9) =='mapoutput'   .OR. &
         code(1:10)=='timeoutput'  .OR. &
         code(1:11)=='classoutput'  .OR. &
         code(1:12)=='regionoutput') THEN
        CALL read_next_codestr_on_line(linelen,16,linepos,line,code2,nostrfound,errstr)
        IF(code2(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        IF(nostrfound) CYCLE
        icheck = 1
        icheckread = .FALSE.
        READ(code2,*,ERR=25) icheck
        icheckread = .TRUE.
        CALL read_next_codestr_on_line(linelen,16,linepos,line,code2,nostrfound,errstr)
        IF(code2(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        IF(nostrfound) CYCLE
25      CONTINUE
        IF(code2(1:11)=='definegroup')THEN
          CALL read_next_codestr_on_line(linelen,8,linepos,line,namestr,nostrfound,errstr,.TRUE.)
          IF(namestr(1:2)==errstr)THEN
            error = .TRUE.
            WRITE(6,*)  'ERROR: In reading definegroup for ',TRIM(code)
            EXIT
          ENDIF
          IF(nostrfound)THEN
            cyclebigloop = .TRUE.
          ENDIF
          IF(namestr(1:8)=='allclass')THEN    !Should be allclass in info.txt
            classgroupallclass = .TRUE.
            cyclebigloop = .TRUE.
          ENDIF
          IF(cyclebigloop)CYCLE
          idefgroup = idefgroup + 1
          defgroupname(idefgroup) = namestr(1:6)
          DO
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(strvalue(1:2)==errstr)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: In reading definegroup for ',TRIM(code)
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(nostrfound)THEN
              cyclebigloop = .TRUE.
              EXIT
            ENDIF
            CALL convert_string_to_integer(10,strvalue,intvalue,status) 
            IF(status/=0)THEN
              error = .TRUE.
              exitbigloop = .TRUE.
            ENDIF  
            ndefgroup(idefgroup) = ndefgroup(idefgroup) + 1
            IF(ndefgroup(idefgroup)>maxclassingroup)THEN
              WRITE(6,*) 'Warning: Number of "classoutput definegroup" is larger than max',maxclassingroup
              WRITE(6,*) 'Warning: Excess classes will be ignored'
            ELSE
              readdefgroups(idefgroup,ndefgroup(idefgroup)) = intvalue
            ENDIF
          ENDDO
          IF(exitbigloop)EXIT
          IF(cyclebigloop)CYCLE
        ENDIF
        IF(code2(1:8)=='allbasin')THEN
          IF(code(1:11)=='basinoutput')THEN
            outvarallbasin(icheck) = .TRUE.
          ELSEIF(code(1:11)=='classoutput')THEN
            classoutvarallbasin(icheck) = .TRUE.
          ELSEIF(code(1:12)=='regionoutput'.OR.code(1:9)=='mapoutput'.OR.code(1:10)=='timeoutput')THEN
            WRITE(6,*) 'Warning: Code allbasin not used for mapoutput, timeoutput or regiooutput. Check info.txt'
          ENDIF
          CYCLE
        ENDIF
        IF(code2(1:10)=='meanperiod')THEN
          CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
          IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          READ(strvalue,*,ERR=801) intvalue
          IF(code(1:11)=='basinoutput')THEN
            basinoutputperiod(icheck) = intvalue
            basinoutputicheck = icheckread
          ELSEIF(code(1:9)=='mapoutput')THEN
            mapoutputperiod(icheck) = intvalue
          ELSEIF(code(1:10)=='timeoutput')THEN
            timeoutputperiod(icheck) = intvalue
            timeoutputicheck = icheckread
          ELSEIF(code(1:11)=='classoutput')THEN
            classoutputperiod(icheck) = intvalue
            classoutputicheck = icheckread
          ELSEIF(code(1:12)=='regionoutput')THEN
            regionoutputperiod(icheck) = intvalue
            regionoutputicheck = icheckread
          ENDIF
          CYCLE
        ENDIF
        IF(code2(1:8)=='decimals')THEN
          CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
          IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          READ(strvalue,*,ERR=801) intvalue
          IF(code(1:11)=='basinoutput')THEN
            basinoutputdecimal(icheck) = intvalue
          ELSEIF(code(1:9)=='mapoutput')THEN
            mapoutputdecimal(icheck) = intvalue
          ELSEIF(code(1:10)=='timeoutput')THEN
            timeoutputdecimal(icheck) = intvalue
          ELSEIF(code(1:11)=='classoutput')THEN
            classoutputdecimal(icheck) = intvalue
          ELSEIF(code(1:12)=='regionoutput')THEN
            regionoutputdecimal(icheck) = intvalue
          ENDIF
          CYCLE
        ENDIF
        IF(code2(1:11)=='signfigures')THEN
          CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
          IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          READ(strvalue,*,ERR=801) intvalue
          IF(intvalue<4) WRITE(6,*) 'WARNING: Less than 4 signfigures of '//TRIM(code(1:12))//' will destroy missing value.'
          IF(code(1:11)=='basinoutput')THEN
            basinoutputsignif(icheck) = intvalue
          ELSEIF(code(1:9)=='mapoutput')THEN
            mapoutputsignif(icheck) = intvalue
          ELSEIF(code(1:10)=='timeoutput')THEN
            timeoutputsignif(icheck) = intvalue
          ELSEIF(code(1:11)=='classoutput')THEN
            classoutputsignif(icheck) = intvalue
          ELSEIF(code(1:12)=='regionoutput')THEN
            regionoutputsignif(icheck) = intvalue
          ENDIF
          CYCLE
        ENDIF
        IF(code2(1:8)=='variable')THEN
          DO
            CALL read_next_codestr_on_line(linelen,6,linepos,line,varstr,nostrfound,errstr)
            IF(varstr(1:2)==errstr)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: In variable for ',TRIM(code)
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(nostrfound)THEN
              cyclebigloop = .TRUE.
              EXIT
            ENDIF
            status = find_variable_index_type(varstr,varindex,flowtype,areagg)
            IF(status>0)THEN
              WRITE(6,*)  'ERROR: In variable index calculation: ', varstr,' for ',TRIM(code)
              error = .TRUE.
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(code(1:11)=='basinoutput')THEN
              ibasinoutputvar(icheck) = ibasinoutputvar(icheck) + 1
              IF(ibasinoutputvar(icheck)>max_outvar)THEN
                WRITE(6,*) 'ERROR: To many basinoutput variables, max:',max_outvar
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              IF(areagg==2)THEN
                WRITE(6,*) 'ERROR: No regional variables allowed for basinoutput: ',varstr
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              basinoutputvar(icheck,ibasinoutputvar(icheck),1) = varindex
              basinoutputvar(icheck,ibasinoutputvar(icheck),2) = areagg
            ELSEIF(code(1:9)=='mapoutput')THEN
              imapoutputvar(icheck) = imapoutputvar(icheck) + 1
              IF(imapoutputvar(icheck)>max_outvar)THEN
                WRITE(6,*) 'ERROR: To many mapoutput variable, max:',max_outvar
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              mapoutputvar(icheck,imapoutputvar(icheck),1) = varindex
              mapoutputvar(icheck,imapoutputvar(icheck),2) = areagg
            ELSEIF(code(1:10)=='timeoutput')THEN
              itimeoutputvar(icheck) = itimeoutputvar(icheck) + 1
              IF(itimeoutputvar(icheck)>max_outvar)THEN
                WRITE(6,*) 'ERROR: To many timeoutput variable, max:',max_outvar
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              timeoutputvar(icheck,itimeoutputvar(icheck),1) = varindex
              timeoutputvar(icheck,itimeoutputvar(icheck),2) = areagg
            ELSEIF(code(1:11)=='classoutput')THEN
              iclassoutputvar(icheck) = iclassoutputvar(icheck) + 1
              IF(iclassoutputvar(icheck)>max_outvar)THEN
                WRITE(6,*) 'ERROR: To many classoutput variable, max:',max_outvar
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              IF(areagg==1)THEN
                WRITE(6,*) 'ERROR: No upstream variables allowed for classoutput: ',varstr
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              classoutputvar(icheck,iclassoutputvar(icheck),1) = varindex
              classoutputvar(icheck,iclassoutputvar(icheck),2) = areagg
            ELSEIF(code(1:12)=='regionoutput')THEN
              iregionoutputvar(icheck) = iregionoutputvar(icheck) + 1
              IF(iregionoutputvar(icheck)>max_outvar)THEN
                WRITE(6,*) 'ERROR: To many regionoutput variables, max:',max_outvar
                error = .TRUE.
                exitbigloop = .TRUE.
                EXIT
              ENDIF
              regionoutputvar(icheck,iregionoutputvar(icheck)) = varindex
            ENDIF
          ENDDO
          IF(exitbigloop)EXIT
          IF(cyclebigloop)CYCLE
        ENDIF
        IF(code2(1:8)=='subbasin')THEN
          DO
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(strvalue(1:2)==errstr)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: In subbasin for ',TRIM(code)
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(nostrfound)THEN
              cyclebigloop = .TRUE.
              EXIT
            ENDIF
            CALL convert_string_to_integer(10,strvalue,intvalue,status) 
            IF(status/=0)THEN
              error = .TRUE.
              exitbigloop = .TRUE.
              EXIT
            ENDIF  
            IF(code(1:11)=='basinoutput')THEN
              ioutbasin(icheck) = ioutbasin(icheck) + 1
              IF(ioutbasin(icheck)>maxoutbasins)THEN
                WRITE(6,*) 'Warning: Number of "basinoutput subbasin" is larger than max',maxoutbasins
                WRITE(6,*) 'Warning: Excess subbasins will be ignored'
              ELSE
                readbasins(icheck,ioutbasin(icheck)) = intvalue
              ENDIF
            ELSEIF(code(1:11)=='classoutput')THEN            
              icoutbasin(icheck) = icoutbasin(icheck) + 1
              IF(icoutbasin(icheck)>maxoutbasins)THEN
                WRITE(6,*) 'Warning: Number of "classoutput subbasin" is larger than max',maxoutbasins
                WRITE(6,*) 'Warning: Excess subbasins will be ignored'
              ELSE
                readcbasins(icheck,icoutbasin(icheck)) = intvalue
              ENDIF
            ENDIF
          ENDDO
          IF(exitbigloop)EXIT
          IF(cyclebigloop)CYCLE
        ENDIF
        IF(code2(1:9)=='outregion')THEN
          DO
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(strvalue(1:2)==errstr)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: In outregion for ',TRIM(code)
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(nostrfound)THEN
              cyclebigloop = .TRUE.
              EXIT
            ENDIF
            CALL convert_string_to_integer(10,strvalue,intvalue,status) 
            IF(status/=0)THEN
              error = .TRUE.
              exitbigloop = .TRUE.
              EXIT
            ENDIF  
            ioutregion(icheck) = ioutregion(icheck) + 1
            IF(ioutregion(icheck)>maxoutbasins)THEN
              WRITE(6,*) 'Warning: Number of "regionoutput outregion" is larger than max',maxoutbasins
              WRITE(6,*) 'Warning: Excess output regions will be ignored'
            ELSE
              readregions(icheck,ioutregion(icheck)) = intvalue
            ENDIF
          ENDDO
          IF(exitbigloop)EXIT
          IF(cyclebigloop)CYCLE
        ENDIF
        IF(code2(1:5)=='group')THEN  !Temporary own name
          DO
            CALL read_next_codestr_on_line(linelen,6,linepos,line,varstr,nostrfound,errstr)
            IF(varstr(1:2)==errstr)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: In reading group for ',TRIM(code)
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(nostrfound)THEN
              cyclebigloop = .TRUE.
              EXIT
            ENDIF
            ioutgroups(icheck) = ioutgroups(icheck) + 1
            readgroupsnames(icheck,ioutgroups(icheck)) = varstr
          ENDDO
          IF(exitbigloop)EXIT
          IF(cyclebigloop)CYCLE
        ENDIF
      ENDIF

      !Read update information
      IF(code(1:6)=='update') THEN
        CALL read_next_codestr_on_line(linelen,16,linepos,line,code2,nostrfound,errstr)
        IF(nostrfound.OR.code2(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF

        !Read information for updating with qobs
        IF(code2(1:7)=='quseobs') THEN
          doupdate(i_quseobs) = .TRUE. 
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr)
          IF(code3(1:10)=='allstation')THEN
            locupall(i_quseobs) = .TRUE.
          ENDIF
          IF(code3(1:9)=='nostation')THEN
            locupnone(i_quseobs) = .TRUE.
          ENDIF
          CYCLE
        ELSEIF(code2(1:3)=='qar') THEN
          doupdate(i_qar) = .TRUE. 
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr)          
          IF(code3(1:9)=='nostation')THEN
            locupnone(i_qar) = .TRUE.
          ENDIF
          CYCLE
        !Read information for updating with wstr
        ELSEIF(code2(1:7)=='wendupd') THEN
          doupdate(i_wendupd) = .TRUE. 
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr) !'wstr'
          IF(nostrfound.OR.code3(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr)
          IF(code3(1:10)=='allstation')THEN
            locupall(i_wendupd) = .TRUE.
          ENDIF
          IF(code3(1:9)=='nostation')THEN
            locupnone(i_wendupd) = .TRUE.
          ENDIF
          CYCLE
        ELSEIF(code2(1:3)=='war') THEN
          doupdate(i_war) = .TRUE. 
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr) !'wstr'
          IF(nostrfound.OR.code3(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr)          
          IF(code3(1:9)=='nostation')THEN
            locupnone(i_war) = .TRUE.
          ENDIF
          CYCLE
        !Read information for updating nitrogen and phosphorus concentrations
        ELSEIF(code2(1:6)=='tpcorr') THEN
          doupdate(i_tpcorr) = .TRUE. 
          CYCLE
        ELSEIF(code2(1:6)=='tncorr') THEN
          doupdate(i_tncorr) = .TRUE. 
          CYCLE
        ELSEIF(code2(1:9)=='tploccorr') THEN
          doupdate(i_tploccorr) = .TRUE. 
          CYCLE
        ELSEIF(code2(1:9)=='tnloccorr') THEN
          doupdate(i_tnloccorr) = .TRUE. 
          CYCLE
        ELSEIF(code2(1:7)=='cuseobs') THEN
          doupdate(i_cuseobs) = .TRUE. 
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr)
          IF(code3(1:10)=='allstation')THEN
            locupall(i_cuseobs) = .TRUE.
          ENDIF
          IF(code3(1:9)=='nostation')THEN
            locupnone(i_cuseobs) = .TRUE.
          ENDIF
          CYCLE
        ENDIF
      ENDIF

      !Read criteria information
      IF(code(1:4)=='crit')THEN
        CALL read_next_codestr_on_line(linelen,16,linepos,line,code2,nostrfound,errstr)
        IF(nostrfound.OR.code2(1:2)==errstr)THEN
          error = .TRUE.
          EXIT
        ENDIF
        IF(code2(1:10)=='meanperiod')THEN
          CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
          IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          READ(strvalue,*,ERR=801) critperiod
          CYCLE
        ELSEIF(code2(1:9)=='datalimit')THEN
          CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
          IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          READ(strvalue,*,ERR=801) critlimit
          CYCLE
        ELSEIF(code2(1:8)=='subbasin')THEN
          DO
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(strvalue(1:2)==errstr)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: for ',TRIM(code),' ',TRIM(code2)
              exitbigloop = .TRUE.
              EXIT
            ENDIF
            IF(nostrfound)THEN
              cyclebigloop = .TRUE.
              EXIT
            ENDIF
            CALL convert_string_to_integer(10,strvalue,intvalue,status) 
            IF(status/=0)THEN
              error = .TRUE.
              WRITE(6,*)  'ERROR: for ',TRIM(code),' ',TRIM(code2)
              exitbigloop = .TRUE.
            ENDIF  
            icritbasin = icritbasin + 1
            IF(icritbasin>maxcritbasins)THEN
              WRITE(6,*) 'Warning: Number of "crit subbasin" is larger than max',maxcritbasins
              WRITE(6,*) 'Warning: Excess subbasins will be ignored'
            ELSE
              readcritbasins(icritbasin) = intvalue
            ENDIF
          ENDDO
          IF(exitbigloop)EXIT
          IF(cyclebigloop)CYCLE
        ELSE
          READ(code2,*,ERR=801) icrit
          IF(icrit>maxcrit)THEN
            WRITE(6,*)  'ERROR: Too many criteria, max: ',maxcrit
            error = .TRUE.
            EXIT
          ENDIF
          ncrit = MAX(ncrit,icrit)
          CALL read_next_codestr_on_line(linelen,16,linepos,line,code3,nostrfound,errstr)
          IF(nostrfound.OR.code3(1:2)==errstr)THEN
            error = .TRUE.
            EXIT
          ENDIF
          IF(code3(1:9)=='criterion')THEN
            CALL read_next_codestr_on_line(linelen,3,linepos,line,critstr,nostrfound,errstr)
            IF(nostrfound.OR.critstr(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            c(icrit) = critstr
            CYCLE
          ELSEIF(code3(1:9)=='cvariable')THEN
            CALL read_next_codestr_on_line(linelen,6,linepos,line,varstr,nostrfound,errstr)
            IF(nostrfound.OR.varstr(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            status = find_variable_index_type(varstr,varindex,flowtype,areagg)
            IF(status>0)THEN
              WRITE(6,*)  'ERROR in variable index calculation: ', varstr,' for ',TRIM(code3)
              error = .TRUE.
              EXIT
            ENDIF
            critc(icrit) = varindex
            flowc(icrit) = flowtype
            critareac(icrit) = areagg
            CYCLE
          ELSEIF(code3(1:9)=='cgroup')THEN
            CALL read_next_codestr_on_line(linelen,6,linepos,line,varstr,nostrfound,errstr)
            IF(nostrfound.OR.varstr(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            critcgroup(icrit) = varstr
            CYCLE
          ELSEIF(code3(1:9)=='rvariable')THEN
            CALL read_next_codestr_on_line(linelen,6,linepos,line,varstr,nostrfound,errstr)
            IF(nostrfound.OR.varstr(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            status = find_variable_index_type(varstr,varindex,flowtype,areagg)
            IF(status>0)THEN
              WRITE(6,*)  'ERROR in variable index calculation: ', varstr,' for ',TRIM(code3)
              error = .TRUE.
              EXIT
            ENDIF
            critr(icrit) = varindex
            flowr(icrit) = flowtype
            critarear(icrit) = areagg
            CYCLE
          ELSEIF(code3(1:6)=='weight')THEN
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            READ(strvalue,*,ERR=801) critweight(icrit)
            CYCLE
          ELSEIF(code3(1:9)=='parameter')THEN
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            READ(strvalue,*,ERR=801) critpar(icrit)
            CYCLE
          ELSEIF(code3(1:11)=='conditional')THEN
            critcond(icrit) = .TRUE.
            CALL read_next_codestr_on_line(linelen,10,linepos,line,strvalue,nostrfound,errstr)
            IF(nostrfound.OR.strvalue(1:2)==errstr)THEN
              error = .TRUE.
              EXIT
            ENDIF
            READ(strvalue,*,ERR=801) critthres(icrit)
            CYCLE
          ENDIF
        ENDIF
      ENDIF

      WRITE(6,*) 'WARNING: Unknown code on info.txt line: ',TRIM(line)
    ENDDO
                
    IF(error)THEN
      WRITE(6,*) 'ERROR: reading info.txt (',TRIM(filename),'). Code=',code
      propagate_str = 'reading info.txt ('//TRIM(filename)//'). Code='//code
      CALL propagate_external_msg(e_info_file,e_error,propagate_str)
      status = 1
      CLOSE(fileunit_temp)
      RETURN
    ENDIF

200 CONTINUE
    CLOSE(fileunit_temp)
    WRITE(6,*) 'File read  : ', TRIM(filename)

    ! Check consistency between optional model structure and optional forcing data input
    IF(.NOT.forcingdata(i_sfobs)%readfile .AND. modeloption(p_snowfall).EQ.1)THEN
      WRITE(6,*) 'ERROR: (info.txt) modeloption SNOWFALLMODEL can not be 1 if READSFOBS is N - please provide input file SFobs.txt and change option READSFOBS to Y!'
      propagate_str = 'SNOWFALLMODEL can not be 1 if READSFOBS is N - provide input file SFobs.txt and change option READSFOBS to Y!'
      CALL propagate_external_msg(e_info_file,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    IF(.NOT.forcingdata(i_swobs)%readfile .AND. .NOT.forcingdata(i_tminobs)%readfile .AND. .NOT.forcingdata(i_tmaxobs)%readfile .AND. modeloption(p_petmodel).GE.3)THEN
      WRITE(6,*) 'WARNING: model option PETMODEL', modeloption(p_petmodel), 'requires shortwave radiation data (SWobs.txt) and/or Tmin and Tmax data. Model will run, but derives diurnal temperature range from clear sky turbidity, which might cause strange results....' 
    ENDIF
    IF(.NOT.forcingdata(i_tminobs)%readfile .AND. .NOT.forcingdata(i_tmaxobs)%readfile .AND. modeloption(p_petmodel).GE.4)THEN
      WRITE(6,*) 'WARNING: model option PETMODEL', modeloption(p_petmodel), 'requires Tmin data to estimate relative humidity (assuming actual vapour pressure = saturation pressure at Tmin). Model will run, but derives diurnal temperature range from clear sky turbidity, which might cause strange results.... (btw RHUM data will be optional in future releases)' 
    ENDIF
    !Check compatible simulation settings
    IF((modeloption(p_lakeriverice)>=1.AND.i_t2==0) .OR. (modeloption(p_lakeriverice)==0.AND.i_t2>0))THEN
      WRITE(6,*) 'ERROR: lakerivericemodel requires simulation of water temperature (T2), and vise versa'
      propagate_str = 'lakerivericemodel requires simulation of water temperature (T2), and vise versa'
      CALL propagate_external_msg(e_info_file,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    IF(calibration .AND. conductregest)THEN
      WRITE(6,*) 'ERROR: Automatic calibration is not possible with reg_par.txt.'
      propagate_str = 'Automatic calibration is not possible with reg_par.txt.'
      CALL propagate_external_msg(e_info_file,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    IF(modeloption(p_floodplain)==1.AND.nsubst>0)THEN
      WRITE(6,*) 'WARNING: floodmodel 1 is not tested for substance simulations'
    ENDIF
    IF(modeloption(p_swtemperature)==1.AND.i_t2==0)THEN
      WRITE(6,*) 'ERROR: model option swtemperature 1 require T2 simulation'
      propagate_str = 'model option swtemperature 1 require T2 simulation'
      CALL propagate_external_msg(e_info_file,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    IF(simulate%soildepthstretch.AND.(stateinput.OR.outstate%numdates>0.OR.outstate%doall.OR.outstate%doperiod))THEN
      WRITE(6,*) 'WARNING: function soildepthstretch is not recommended to be used with saved state,'
      WRITE(6,*) 'WARNING: because there is no possibility to check if the state is compatible with model set-up (par).'
    ENDIF
    IF(modeloption(p_wetland)==2.AND.i_t1>0)THEN
      WRITE(6,*) 'WARNING: wetland model is not developed for general substance simulations'
    ENDIF
    IF(doassimilation)THEN
      IF(calibration)THEN
        WRITE(6,*) 'WARNING: calibration and assimilation are both ON, which is not allowed -> assimilation is kept ON and calibration is set OFF.'
        calibration = .FALSE.
      ENDIF
      IF(doupdate(i_quseobs) .OR. & 
        doupdate(i_wendupd) .OR. &
        doupdate(i_war) .OR. &
        doupdate(i_tpcorr) .OR. & 
        doupdate(i_tncorr) .OR. & 
        doupdate(i_tploccorr) .OR. & 
        doupdate(i_tnloccorr))THEN
        WRITE(6,*) 'WARNING: assimilation and updating are both ON - make sure you dont unintentially try to assimilate the same data twice with different methods!'
      ENDIF
    ENDIF
    IF(outstate%numdates>0.AND.simsubmodel)THEN
      IF(conductwarning) WRITE(6,*) 'WARNING: State can not be saved when submodel is simulated'
    ENDIF
!    IF(modeloption(p_surfacerunoff)==3.OR.modeloption(p_surfacerunoff)==4)THEN
!      WRITE(6,*) 'ERROR: surfacerunoff model option 3 and 4 are not available yet.'
!      STOP 1
!    ENDIF
       
    !Some calculations
    temp = set_timestep_variables(tstep,stepunit,date1)    
    IF(date3%Year==0.AND.date3%Month==0.AND.date3%Day==0.AND.date3%Hour==0.AND.date3%Minute==0) date3=date1      
    skip = period_length(date1,date3)         !Calculate length of warmup period
    step = period_length(date1,AddDates(date2,steplen)) !Calculate length of simulation period
    CALL handle_relative_directories(dir,mdir,rdir,fdir,ldir)
    IF(.NOT.readdaily)THEN   
      IF(step>26*366)THEN
        WRITE(6,*)
        WRITE(6,*) 'WARNING: Simulation period >26 year'
        WRITE(6,*) 'WARNING: Use readdaily if problem with saving PT to memory'
        WRITE(6,*)
      ENDIF
    ENDIF
    !Set simulation configuration variables
    ALLOCATE(simulate%substance(0:nsubst))
    simulate%substance(0) = .FALSE.
    IF(nsubst>0)THEN
      simulatesubstances = .TRUE.
      simulate%substance(1:nsubst) = .TRUE.
    ENDIF
    IF(modeloption(p_wetland)==1) conduct%riverwetland = .TRUE.
    IF(modeloption(p_infiltration)==1 .OR. modeloption(p_infiltration)==3) conduct%icelens = .TRUE.
    IF(modeloption(p_lakeriverice)==1 .OR. modeloption(p_lakeriverice)==2) conduct%lakeriverice = .TRUE.
    IF(modeloption(p_snowheat)>=1) conduct%snowheat = .TRUE.
    IF(modeloption(p_connectivity)>=1) conduct%connectivity = .TRUE.
    conduct%qbank = conduct%simN.OR.conduct%simP.OR.conduct%simS.OR.conduct%simT1 
    IF(modeloption(p_soilleakage)==2 .OR. modeloption(p_soilleakage)==3) conduct%traveltime = .TRUE.
    
    !Preparations for output
    DO i = 1,max_typeofperiods
      IF(outvarallbasin(i))THEN
        ioutbasin(i) = -1
      ENDIF
    ENDDO
    maxdim = MAX(MAXVAL(ibasinoutputvar),MAXVAL(imapoutputvar),MAXVAL(itimeoutputvar))
    CALL get_number_of_classgroups(classgroupallclass,maxgroups,ioutgroups,idefgroup)
    noutput = count_number_of_output(max_typeofperiods,maxgroups,ibasinoutputvar,imapoutputvar,itimeoutputvar,iregionoutputvar,iclassoutputvar,ioutgroups)
    io = 0
    i=1
    DO
      IF(i>max_typeofperiods)EXIT
      IF(ibasinoutputvar(i)==0)EXIT
      io=io+1
      CALL check_output_variables(max_outvar,ibasinoutputvar(i),basinoutputvar(i,:,1),basinoutputvar(i,:,2))
      temp = set_output(1,io,noutput,max_outvar,basinoutputvar(i,:,1),    &
         ibasinoutputvar(i),basinoutputperiod(i),    &
         basinoutputdecimal(i),basinoutputsignif(i),basinoutputicheck,basinoutputvar(i,:,2),maxoutbasins,ioutbasin(i),readbasins(i,:))
      i=i+1
    ENDDO
    i=1
    DO
      IF(i>max_typeofperiods)EXIT
      IF(imapoutputvar(i)==0)EXIT
      IF(i>1)THEN
        WRITE(6,*) 'ERROR: Only one output period for mapoutput is allowed.'
        WRITE(6,*) 'ERROR: the following outputs are ignored.'
        EXIT
      ENDIF
      io=io+1
      CALL check_output_variables(max_outvar,imapoutputvar(i),mapoutputvar(i,:,1),mapoutputvar(i,:,2))
      temp = set_output(2,io,noutput,max_outvar,mapoutputvar(i,:,1),    &
         imapoutputvar(i),mapoutputperiod(i),    &
         mapoutputdecimal(i),mapoutputsignif(i),.FALSE.,mapoutputvar(i,:,2))
      IF(mapoutputperiod(i)<4)THEN
        WRITE(6,*) 'WARNING: Mapoutput files use lot of memory and should not be used for shorter' 
        WRITE(6,*) 'WARNING: meanperiod than year (4), unless a short simulation period is used.'
      ENDIF
      i=i+1
    ENDDO
    i=1
    DO
      IF(i>max_typeofperiods)EXIT
      IF(itimeoutputvar(i)==0)EXIT
      io=io+1
      CALL check_output_variables(max_outvar,itimeoutputvar(i),timeoutputvar(i,:,1),timeoutputvar(i,:,2))
      temp = set_output(3,io,noutput,max_outvar,timeoutputvar(i,:,1),    &
         itimeoutputvar(i),timeoutputperiod(i),    &
         timeoutputdecimal(i),timeoutputsignif(i),timeoutputicheck,timeoutputvar(i,:,2))
      i=i+1
    ENDDO
    i=1
    DO
      IF(i>max_typeofperiods)EXIT
      IF(iregionoutputvar(i)==0)EXIT
      io=io+1
      IF(ioutregion(i)>0)THEN
        temp = set_output(4,io,noutput,max_outvar,regionoutputvar(i,:),   &
         iregionoutputvar(i),regionoutputperiod(i),       &
         regionoutputdecimal(i),regionoutputsignif(i),regionoutputicheck,maxa=maxoutbasins,na=ioutregion(i),area=readregions(i,:))
      ELSE
        temp = set_output(4,io,noutput,max_outvar,regionoutputvar(i,:),   &
         iregionoutputvar(i),regionoutputperiod(i),       &
         regionoutputdecimal(i),regionoutputsignif(i),regionoutputicheck)
      ENDIF
      i=i+1
    ENDDO
    !Set structure with defined classgroups for output
    IF(.NOT.ALLOCATED(classgroup2)) ALLOCATE(classgroup2(idefgroup))
    IF(.NOT.classgroupallclass)THEN
      DO i = 1,idefgroup
        classgroup2(i)%name = defgroupname(i)
        classgroup2(i)%ncgslcs = ndefgroup(i)
        IF(.NOT.ALLOCATED(classgroup2(i)%slcs)) ALLOCATE(classgroup2(i)%slcs(classgroup2(i)%ncgslcs))
        classgroup2(i)%slcs = readdefgroups(i,1:classgroup2(i)%ncgslcs)
      ENDDO
    ELSE
      DO i = 1,idefgroup
        defaultgroupsnames(i) = 'CG'//integer_convert_to_string(i,4)
        classgroup2(i)%name = defaultgroupsnames(i)
        classgroup2(i)%ncgslcs = 1
        IF(.NOT.ALLOCATED(classgroup2(i)%slcs)) ALLOCATE(classgroup2(i)%slcs(classgroup2(i)%ncgslcs))
        classgroup2(i)%slcs = i
      ENDDO
    ENDIF
    i=1
    DO
      IF(i>maxgroups)EXIT
      IF(iclassoutputvar(i)==0)EXIT
      io=io+1
      CALL check_output_variables(max_outvar,iclassoutputvar(i),classoutputvar(i,:,1),classoutputvar(i,:,2))
      IF(classoutvarallbasin(i))THEN
        IF(ioutgroups(i)>0)THEN
          io = io - 1
          DO j=1,ioutgroups(i)
            io = io + 1
            IF(.NOT.classgroupallclass)THEN
              temp = set_output(5,io,noutput,max_outvar,classoutputvar(i,:,1),    &
              iclassoutputvar(i),classoutputperiod(i),    &
              classoutputdecimal(i),classoutputsignif(i),classoutputicheck,classoutputvar(i,:,2),classgroup2=readgroupsnames(i,j))
            ELSE
              temp = set_output(5,io,noutput,max_outvar,classoutputvar(i,:,1),    &
              iclassoutputvar(i),classoutputperiod(i),    &
              classoutputdecimal(i),classoutputsignif(i),classoutputicheck,classoutputvar(i,:,2),classgroup2=defaultgroupsnames(j))
            ENDIF
          ENDDO
        ELSE
          WRITE(6,*) 'Warning: No class group given for output. Skipped!'
          io = io - 1
        ENDIF
      ELSE
        IF(icoutbasin(i)==0)THEN
          WRITE(6,*) 'Warning: No subbasins given for classoutput', i,'. Skipped!'
          io = io - 1
        ELSEIF(ioutgroups(i)>0)THEN
          io = io - 1
          DO j=1,ioutgroups(i)
            io = io + 1
            IF(.NOT.classgroupallclass)THEN
            temp = set_output(6,io,noutput,max_outvar,classoutputvar(i,:,1),    &
              iclassoutputvar(i),classoutputperiod(i),    &
              classoutputdecimal(i),classoutputsignif(i),classoutputicheck,classoutputvar(i,:,2),maxa=maxoutbasins, &
              na=icoutbasin(i),area=readcbasins(i,:),classgroup2=readgroupsnames(i,j))
            ELSEIF(classgroupallclass)THEN
            temp = set_output(6,io,noutput,max_outvar,classoutputvar(i,:,1),    &
              iclassoutputvar(i),classoutputperiod(i),    &
              classoutputdecimal(i),classoutputsignif(i),classoutputicheck,classoutputvar(i,:,2),maxa=maxoutbasins, &
              na=icoutbasin(i),area=readcbasins(i,:),classgroup2=defaultgroupsnames(j))
            ENDIF
          ENDDO
        ELSE
          WRITE(6,*) 'Warning: No class group given for output. Skipped!'
          io = io - 1
        ENDIF
      ENDIF
      i=i+1
    ENDDO
    
    temp = set_maxmap(step-skip+1,mapoutputperiod(1))   !Only one mapoutput allowed
    CALL check_outputfiles_for_illegal_input(status)
    IF (status /= 0) THEN
      propagate_str = 'illegal input for output files'
      CALL propagate_external_msg(e_info_file,e_error,propagate_str)
      RETURN
    END IF
    CALL set_outstate_dates(maxoutstates,bstateoutdate)    !Calculate previous time step for output of state
    IF(rsnum>0) CALL check_if_date_in_period(rsmax,rsnum,readstatedate,date1,date2,resetstate)  !Check for reset state
    temp = set_calibration(calibration,ncrit,c(1:ncrit),critcgroup(1:ncrit),critperiod,critlimit,   &
         critweight(1:ncrit),critc(1:ncrit),critr(1:ncrit),flowc(1:ncrit),   &   !NOTE only computed variables flow and areagg is saved, combination of different type of variables may go wrong
         critpar(1:ncrit),critcond(1:ncrit),critthres(1:ncrit),critareac(1:ncrit))
    IF(icritbasin>0)THEN
      ALLOCATE(subincrit(icritbasin))
      subincrit = readcritbasins(1:icritbasin)
    ENDIF
    IF(doens.AND.doopt)THEN
      WRITE(6,*) 'ERROR: Automatic calibration and parameter ensemble simulation'
      WRITE(6,*) 'ERROR: cannot be done simultaneously. Check info.txt.'
      STOP 1
    ENDIF

    !Dates to formatted strings for hyss.log
    CALL format_date(date1,'yyyy-mm-dd HH:MM',strdate1)
    CALL format_date(date2,'yyyy-mm-dd HH:MM',strdate2)
    CALL format_date(date3,'yyyy-mm-dd HH:MM',strdate3)
    IF(outstate%numdates>0) ALLOCATE(strstates(outstate%numdates))
    DO istate=1,outstate%numdates
      CALL format_date(bstateoutdate(istate),'yyyy-mm-dd HH:MM',strstates(istate))
    ENDDO
    IF(rsnum>0) ALLOCATE(strrstates(rsnum))
    DO istate=1,rsnum
      CALL format_date(readstatedate(istate),'yyyy-mm-dd HH:MM',strrstates(istate))
    ENDDO

    !Check consistency between optional model structure and time step (temporary not working)
    IF(.NOT.(steplen==datetype(0,0,1,0,0)))THEN
      IF(modeloption(p_petmodel).GE.1)THEN
        WRITE(6,*)  'ERROR: model option PETMODEL', modeloption(p_petmodel), 'does not work with other time step than day (to come in future release)'
        propagate_str = 'PETMODEL '//real_to_str(REAL(modeloption(p_petmodel)))//' does not work with other time step than day (to come in future release)'
        CALL propagate_external_msg(e_info_file,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF  
      IF(i_t2.GE.1 .OR. modeloption(p_lakeriverice).GE.1)THEN
        WRITE(6,*) 'ERROR: modeloption lakeriverice and water temperature model does not work with other time step than day (to come in future release)'
        propagate_str = 'modeloption lakeriverice and water temperature model does not work with other time step than day (to come in future release)'
        CALL propagate_external_msg(e_info_file,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
      IF(.NOT.forcingdata(i_swobs)%readfile.AND.modeloption(p_snowmelt)==2)THEN
        WRITE(6,*) 'ERROR: modeloption snowmeltmodel=2 require SWobs.txt to work with other time step than day (for now)'
        propagate_str = 'modeloption snowmeltmodel=2 require SWobs.txt to work with other time step than day (for now)'
        CALL propagate_external_msg(e_info_file,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
      IF(conductxoms)THEN
        WRITE(6,*) 'ERROR: XobsXOMS files can only be used for daily time steps (for now)'
        propagate_str = 'XobsXOMS files can only be used for daily time steps (for now)'
        CALL propagate_external_msg(e_info_file,e_error,propagate_str)
        status = 1
        RETURN
      ENDIF
    ENDIF
    simtimestep = tstep
    simtimeunit = stepunit
    WRITE(6,*) 'Simulation information loaded (info.txt)'

    !Write information about simulation to hyss.log  !TODO: Move this output to later in code (after basin PETmodel read?)
    WRITE(6,*)
    WRITE(6,*)'--------Information about the simulation---------'
    WRITE(6,*)'Current model set-up:  ',TRIM(mdir)
    IF(fdir/=mdir) WRITE(6,*)'and forcing data:      ',TRIM(fdir)
    WRITE(6,*)'Simulation sequence:   ',simsequence
    WRITE(6,*)
    WRITE(6,*)'Simulation begin date  ',strdate1
    WRITE(6,*)'Crit/Output start date ',strdate3
    WRITE(6,*)'Simulation end date    ',strdate2
    WRITE(6,'(A23,I2,A3)') 'Simulation steplength ',tstep,stepunit
    IF(calibration) WRITE(6,*)'Calibration run'
    IF(doassimilation) WRITE(6,*)'Assimilation run'
    WRITE(6,*)'Number of substances modelled',nsubst
    WRITE(6,*)'Model options:'
    WRITE(6,*)'               PET model',modeloption(p_petmodel)
    WRITE(6,*)'          Snowfall model',modeloption(p_snowfall)
    WRITE(6,*)'      Snowfalldist model',modeloption(p_snowfalldist)
    WRITE(6,*)'          Snowmelt model',modeloption(p_snowmelt)
    WRITE(6,*)'          Snowevap model',modeloption(p_snowevap)
    WRITE(6,*)'          Snowheat model',modeloption(p_snowheat)
    WRITE(6,*)'       Snowdensity model',modeloption(p_snowdensity)
    WRITE(6,*)' Frozen soil water flows',modeloption(p_frozensoil)
    WRITE(6,*)'     Lakeriver ice model',modeloption(p_lakeriverice)
    WRITE(6,*)'  Glacier initialization',modeloption(p_glacierini)
    WRITE(6,*)'  Deep groundwater model',modeloption(p_deepgroundwater)
    WRITE(6,*)'        Floodplain model',modeloption(p_floodplain)
    WRITE(6,*)'    Surface runoff model',modeloption(p_surfacerunoff)
    WRITE(6,*)'      Infiltration model',modeloption(p_infiltration)
    WRITE(6,*)'           Wetland model',modeloption(p_wetland)
    WRITE(6,*)'Surface water temp model',modeloption(p_swtemperature)
    WRITE(6,*)'     Growth season model',modeloption(p_growthstart)
    WRITE(6,*)'           Erosion model',modeloption(p_erosion)
    WRITE(6,*)'      Soil leakage model',modeloption(p_soilleakage)
    WRITE(6,*)'      Ilake connectivity',modeloption(p_connectivity)
    WRITE(6,*)
    CALL print_output_information_to_logfile(6)
    IF(writeload.AND.(conduct%simN.OR.conduct%simP))THEN
      WRITE(6,*) 'Yearly loads will be saved'
      conductload = .TRUE.
    ELSEIF(writeload)THEN
      WRITE(6,*) 'Yearly loads will not be saved. Need to simulate N or P.'
      writeload = .FALSE.
    ENDIF
    IF(stateinput) WRITE(6,*)'Initial state from file will be used.'
    DO i=1,rsnum
      WRITE(6,*)'Soil states will be read for: ',strrstates(i)
    ENDDO
    IF(outstate%doall)THEN
      WRITE(6,*)'State variables will be saved for all time steps.'
    ELSEIF(outstate%doperiod)THEN
      WRITE(6,*)'State variables will be saved for all time steps during period.'
    ELSEIF(outstate%numdates>0)THEN
      WRITE(6,*)'State variables will be saved for specified dates: '
      WRITE(6,*) ((strstates(i)//CHAR(32)),i=1,outstate%numdates)
    ENDIF
    WRITE(6,*)'-------------------------------------------------'
    WRITE(6,*)

    IF(ALLOCATED(c)) DEALLOCATE(c)
    IF(ALLOCATED(critweight)) DEALLOCATE(critweight)
    IF(ALLOCATED(critc)) DEALLOCATE(critc)
    IF(ALLOCATED(critr)) DEALLOCATE(critr)
    IF(ALLOCATED(flowc)) DEALLOCATE(flowc)
    IF(ALLOCATED(flowr)) DEALLOCATE(flowr)
    IF(ALLOCATED(critareac)) DEALLOCATE(critareac)
    IF(ALLOCATED(critarear)) DEALLOCATE(critarear)
    IF(ALLOCATED(critpar)) DEALLOCATE(critpar)
    IF(ALLOCATED(strstates)) DEALLOCATE(strstates)
    IF(ALLOCATED(strrstates)) DEALLOCATE(strrstates)
    IF(ALLOCATED(critcond)) DEALLOCATE(critcond)
    IF(ALLOCATED(critthres)) DEALLOCATE(critthres)

600 FORMAT(A18000)    !linelen
    RETURN

800 CONTINUE
    status = 1
    WRITE(6,*) 'ERROR: reading info.txt. io=',io
    propagate_str = 'reading info.txt. io='//real_to_str(REAL(io))
    CALL propagate_external_msg(e_info_file,e_error,propagate_str)
    CLOSE(fileunit_temp)
    RETURN
801 CONTINUE
    status = 1
    WRITE(6,*) 'ERROR: reading line of info.txt.'
    WRITE(6,*) 'ERROR: on line: ',TRIM(line)
    propagate_str = 'reading line of info.txt.'
    CALL propagate_external_msg(e_info_file,e_error,propagate_str)
    CLOSE(fileunit_temp)
    RETURN

  END SUBROUTINE load_coded_info

  !>Check relative paths in info.txt and make them to relative infodir.
  !------------------------------------------------------------------------
  SUBROUTINE handle_relative_directories(dir,mdir,rdir,fdir,ldir)
  
    USE WORLDVAR, ONLY : maxcharpath
    
    !Argument declarations
    CHARACTER(LEN=maxcharpath),INTENT(IN) :: dir     !<Info file directory
    CHARACTER(LEN=maxcharpath),INTENT(INOUT) :: mdir !<File directory model (possibly relative to info file directory)
    CHARACTER(LEN=maxcharpath),INTENT(INOUT) :: rdir !<File directory result (possibly relative to info file directory)
    CHARACTER(LEN=maxcharpath),INTENT(INOUT) :: fdir !<File directory forcing (possibly relative to info file directory)
    CHARACTER(LEN=maxcharpath),INTENT(INOUT) :: ldir !<File directory log (possibly relative to info file directory)
  
    !Local variable
    CHARACTER(LEN=maxcharpath) :: temp_dir
    
    !>Check for relative path
    IF(mdir=='')THEN
      mdir = dir
    ELSEIF(mdir(1:1)=='.')THEN
      IF(mdir(2:2)=='.')THEN
        temp_dir=TRIM(dir)//mdir(1:LEN(mdir))
        mdir=''
        mdir=temp_dir
      ELSE
        temp_dir=TRIM(dir)//mdir(3:LEN(mdir))
        mdir=''
        mdir=temp_dir
      ENDIF
    ENDIF
    IF(rdir=='')THEN
      rdir = dir
    ELSEIF(rdir(1:1)=='.')THEN
      IF(rdir(2:2)=='.')THEN
        temp_dir=TRIM(dir)//rdir(1:LEN(rdir))
        rdir=''
        rdir=temp_dir
      ELSE
        temp_dir=TRIM(dir)//rdir(3:LEN(rdir))
        rdir=''
        rdir=temp_dir
      ENDIF
    ENDIF
    IF(fdir=='')THEN
      fdir = mdir
    ELSEIF(fdir(1:1)=='.')THEN
      IF(fdir(2:2)=='.')THEN
        temp_dir=TRIM(dir)//fdir(1:LEN(fdir))
        fdir=''
        fdir=temp_dir
      ELSE
        temp_dir=TRIM(dir)//fdir(3:LEN(fdir))
        fdir=''
        fdir=temp_dir
      ENDIF
    ENDIF
    IF(ldir=='')THEN
      ldir = dir
    ELSEIF(ldir(1:1)=='.')THEN
      IF(ldir(2:2)=='.')THEN
        temp_dir=TRIM(dir)//ldir(1:LEN(ldir))
        ldir=''
        ldir=temp_dir
      ELSE
        temp_dir=TRIM(dir)//ldir(3:LEN(ldir))
        ldir=''
        ldir=temp_dir
      ENDIF
    ENDIF
  
  END SUBROUTINE handle_relative_directories

  !>Read Y/J/N code from line to set logical variable
  !------------------------------------------------------------------------
  INTEGER FUNCTION set_yesno_variable_from_line(linelen,linepos,line,errstr,onoff)
  
    !Argument declarations
    INTEGER,INTENT(IN) :: linelen    !<length of line
    INTEGER,INTENT(INOUT) :: linepos   !<current position on line
    CHARACTER(LEN=linelen), INTENT(IN) :: line  !<line to be read
    CHARACTER(LEN=*), INTENT(IN) :: errstr    !<value of str if error
    LOGICAL,INTENT(OUT) :: onoff  !<value of yesno variable
  
    !Local variables
    INTEGER status
    LOGICAL nostrfound
    CHARACTER(LEN=3) yesnostr
    
      status = 0
      CALL read_next_codestr_on_line(linelen,3,linepos,line,yesnostr,nostrfound,errstr)
      IF(nostrfound.OR.yesnostr(1:2)==errstr)THEN
        status = 1
      ELSEIF(yesnostr(1:1)=='Y' .OR. yesnostr(1:1)=='y' .OR.    &
             yesnostr(1:1)=='J' .OR. yesnostr(1:1)=='j')THEN
        onoff = .TRUE.
      ELSEIF(yesnostr(1:1)=='N' .OR. yesnostr(1:1)=='n')THEN
        onoff = .FALSE.
      ELSE
        status = 1
      ENDIF
      set_yesno_variable_from_line = status
          
  END FUNCTION set_yesno_variable_from_line

  !>Count number of outputs (periods) for region output
  !>
  !>\b Consequences Module worldvar variable noutput is set by return value
  !------------------------------------------------------------------------
  INTEGER FUNCTION count_number_of_output(dim1,dim2,arr1,arr2,arr3,arr4,arr5,arr7)

    INTEGER, INTENT(IN) :: dim1        !<dimension of array
    INTEGER, INTENT(IN) :: dim2        !<dimension of array
    INTEGER, INTENT(IN) :: arr1(dim1)   !<array with number of variables for output of type 1
    INTEGER, INTENT(IN) :: arr2(dim1)   !<array with number of variables for output of type 2
    INTEGER, INTENT(IN) :: arr3(dim1)   !<array with number of variables for output of type 3
    INTEGER, INTENT(IN) :: arr4(dim1)   !<array with number of variables for output of type 4
    INTEGER, INTENT(IN) :: arr5(dim2)   !<array with number of variables for output of type 5 or 6
    INTEGER, INTENT(IN) :: arr7(dim2)   !<array with number of variables for output of type 5 or 6

    !Local variables 
    INTEGER i,noutput
    
    !> \b Algorithm \n
    !>Count number of outputs with set variables, i.e. used for this simulation
    noutput = 0
    DO i = 1,dim1
      IF(arr1(i)>0) noutput = noutput + 1
      IF(arr2(i)>0) noutput = noutput + 1
      IF(arr3(i)>0) noutput = noutput + 1
      IF(arr4(i)>0) noutput = noutput + 1
    ENDDO
    DO i = 1,dim2
      IF(arr5(i)>0.AND.arr7(i)>0)THEN
        noutput = noutput + arr7(i)
      ENDIF
    ENDDO
    count_number_of_output = noutput

  END FUNCTION count_number_of_output

  !>Checks for incompatible output
  !------------------------------------------------------------------------
  SUBROUTINE check_output_variables(dimout,n,varid,aggarea)
  
  USE MODVAR, ONLY : outvarid
    
    !Argument declarations
    INTEGER, INTENT(IN)    :: dimout       !<dimension of variable arrays
    INTEGER, INTENT(INOUT) :: n            !<number of output variables of this type
    INTEGER, INTENT(INOUT) :: varid(dimout)    !<variables for output
    INTEGER, INTENT(INOUT) :: aggarea(dimout)  !<area aggregation type of variable (subbasin=0,upstream=1,region=2)

    !Local variables 
    INTEGER i,nnew,ivar,varidnew(dimout),aggareanew(dimout)
    LOGICAL skipped(n)
    
    !> \b Algorithm \n
    !>Check outvar information for non-calculatable upstream variables
    nnew = n
    skipped=.FALSE.
    DO i = 1,n
      IF(aggarea(i)==1)THEN   !upstream variables checked
        IF(outvarid(varid(i))%basearea==0)THEN
          !Wrong used variable, remove
          nnew = nnew - 1
          skipped(i)=.TRUE.
          WRITE(6,*) 'WARNING: Erronous output variable up'//outvarid(varid(i))%shortname//' removed from output'
        ENDIF
      ENDIF
    ENDDO
    
    IF(n>nnew)THEN
      !>Change output variable information
      ivar = 0
      DO i = 1,n
        IF(.NOT.skipped(i))THEN
          ivar = ivar + 1
          varidnew(ivar)   = varid(i)
          aggareanew(ivar) = aggarea(i)
        ENDIF
      ENDDO
      !>Set output
      n = nnew
      varid = varidnew
      aggarea = aggareanew
    ENDIF

  END SUBROUTINE check_output_variables

  !>Saves information on variables to calculate for output
  !------------------------------------------------------------------------
  SUBROUTINE set_outvar(nout,noutvar,noutvarclass)

    USE MODVAR, ONLY : max_outvar, &
                       outvarindex, &  !(OUT)
                       outvarclassindex, & !(OUT)
                       outvarstatus !OUT

    !Argument declaration
    INTEGER, INTENT(IN) :: nout         !<number of output file type to be set
    INTEGER, INTENT(OUT) :: noutvar     !<number of output variables (so far)
    INTEGER, INTENT(OUT) :: noutvarclass     !<number of output variables for class output (so far)

    INTEGER status,i
    
    status = set_outvar_output(nout,noutvar,noutvarclass)
    status = set_outvar_crit(noutvar,noutvarclass)
    
    DO i = 1, max_outvar
      IF(outvarindex(i)>0.OR.outvarclassindex(i)>0) outvarstatus(i) = .TRUE.
    ENDDO

  END SUBROUTINE set_outvar

  !>Saves information on variables to write to output files
  !>
  !> \b Consequences Module variable outvarinfotemp and outvarindex is allocated.
  !------------------------------------------------------------------------
  INTEGER FUNCTION set_outvar_output(nout,noutvar,noutvarclass)

    USE WORLDVAR, ONLY : output, & !OUT
                         outvarinfotemp, & !OUT
                         outvarclassinfotemp !OUT
    USE MODVAR, ONLY : max_noutvar, &
                       max_outvar, &
                       outvarindex, &  !OUT
                       outvarclassindex, & !OUT
                       outvarstatus, & !OUT
                       get_classgroup_classes
    
    !Argument declaration
    INTEGER, INTENT(IN) :: nout         !<number of output file type to be set
    INTEGER, INTENT(OUT) :: noutvar     !<number of output variables (so far)
    INTEGER, INTENT(OUT) :: noutvarclass     !<number of output variables for class output (so far)

    INTEGER i,iout,nclassoutput,nslc
    INTEGER,ALLOCATABLE :: classindex(:)
    
    !>\b Algorithm \n
    set_outvar_output  = 0
    
    !>Count number of classoutputs
    nclassoutput = 0
    DO iout = 1,nout
      IF(output(iout)%fileformat==5.OR.output(iout)%fileformat==6)THEN
        nclassoutput = nclassoutput + 1
      ENDIF
    ENDDO

    !>Initialize and allocate variables holding outvar information
    noutvar = 0 !Initialize here, will be changed in subroutines
    noutvarclass = 0 !Initialize here, will be changed in subroutines
    IF(.NOT.ALLOCATED(outvarinfotemp)) ALLOCATE(outvarinfotemp(max_noutvar))  !temporary allocation, big structure
    IF(.NOT.ALLOCATED(outvarclassinfotemp)) ALLOCATE(outvarclassinfotemp(max_noutvar))  !temporary allocation, big structure
    IF(.NOT.ALLOCATED(outvarindex)) ALLOCATE(outvarindex(max_outvar))
    outvarindex = 0 !default is turned off
    IF(.NOT.ALLOCATED(outvarclassindex)) ALLOCATE(outvarclassindex(max_outvar))
    outvarclassindex = 0 !default is turned off
    IF(.NOT.ALLOCATED(outvarstatus)) ALLOCATE(outvarstatus(max_outvar))
    outvarstatus = .FALSE. !default is off
    
    !>For every output file type...
    DO iout = 1,nout
      !>and every variable..
      !>\li Set outvar information
      IF(output(iout)%fileformat==5.OR.output(iout)%fileformat==6)THEN
        DO i = 1,output(iout)%nvar
          CALL get_classgroup_classes(output(iout)%gcgroupname,nslc,classindex)
          CALL set_outvar_for_variable_new(output(iout)%variable(i)%idindex,output(iout)%variable(i)%areaagg,output(iout)%variable(i)%ovindex,noutvar,noutvarclass,nslc,classindex)
        ENDDO
      ELSE
        DO i = 1,output(iout)%nvar
          CALL set_outvar_for_variable_new(output(iout)%variable(i)%idindex,output(iout)%variable(i)%areaagg,output(iout)%variable(i)%ovindex,noutvar)
        ENDDO
      ENDIF
    ENDDO
    IF(ALLOCATED(classindex)) DEALLOCATE(classindex)

  END FUNCTION set_outvar_output

  !>Saves information on output variables to calculate for use in criteria calculations
  !>
  !> \b Consequences Module variable acccalvar, outvarinfotemp, noutvar and 
  !>outvarindex is changed.
  !------------------------------------------------------------------------
  INTEGER FUNCTION set_outvar_crit(noutvar,noutvarclass)

    USE MODVAR, ONLY : get_classgroup_classes
    USE WORLDVAR, ONLY : nacrit, &
                         acccalvar
    
    !Argument declarations
    INTEGER, INTENT(INOUT) :: noutvar  !<number of output variables to be set (so far)
    INTEGER, INTENT(OUT) :: noutvarclass     !<number of output variables for class output (so far)
    
    !Local variables
    INTEGER ia,nslc
    INTEGER,ALLOCATABLE :: classindex(:)
    
    !> \b Algorithm \n
    set_outvar_crit  = 0
    
    DO ia = 1,nacrit
      IF(acccalvar(ia)%cgname=='')THEN
        CALL set_outvar_for_variable_new(acccalvar(ia)%comp,acccalvar(ia)%areaagg,acccalvar(ia)%compoutvar,noutvar)
        CALL set_outvar_for_variable_new(acccalvar(ia)%rec,acccalvar(ia)%areaagg,acccalvar(ia)%recoutvar,noutvar)
      ELSE
        CALL get_classgroup_classes(acccalvar(ia)%cgname,nslc,classindex)
        CALL set_outvar_for_variable_new(acccalvar(ia)%comp,acccalvar(ia)%areaagg,acccalvar(ia)%compoutvar,noutvar,noutvarclass,nslc,classindex)
        CALL set_outvar_for_variable_new(acccalvar(ia)%rec,acccalvar(ia)%areaagg,acccalvar(ia)%recoutvar,noutvar)
      ENDIF
    ENDDO
    IF(ALLOCATED(classindex)) DEALLOCATE(classindex)
    
  END FUNCTION set_outvar_crit

  !>\brief Reads file with information about submodel for the current
  !run and gets the coupling to the model set-up
  !!
  !!Format of file: first row is number of subbasins, and the
  !!following holds the subid of these subbasins
  !--------------------------------------------------------------------
  SUBROUTINE load_submodel_info(dir,submodelrun,msub,status)

    USE WORLDVAR, ONLY : fileunit_temp,   &
                         ibasemodel
    USE MODVAR, ONLY : nsub_basemodel, &
                       basin

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir  !<File directory
    LOGICAL, INTENT(IN)  :: submodelrun  !<Submodel is supposed to be run
    INTEGER, INTENT(OUT) :: msub         !<Number of subbasins of submodel
    INTEGER, INTENT(OUT) :: status       !<Error status
    
    !Local variables
    LOGICAL fileexist
    INTEGER i,j,io
    CHARACTER (LEN=100) filename
    INTEGER, ALLOCATABLE :: selectedaro(:)

    status = 0

    !File exist for simulation with submodel?
    filename=TRIM(dir)//'pmsf.txt'
    INQUIRE(FILE=filename, EXIST=fileexist)
    IF(submodelrun.AND.(.NOT.fileexist))THEN
      WRITE(6,*) 'ERROR: File is missing:',TRIM(filename)
      status = 1
      RETURN
    ENDIF

    !Read subbasins to be simulated
    IF(submodelrun)THEN
      WRITE(6,*) 'File opened:',TRIM(filename)
      OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read')
      READ(fileunit_temp,*,ERR=200,IOSTAT=io) msub
      IF(.NOT.ALLOCATED(selectedaro)) ALLOCATE(selectedaro(msub))
      READ(fileunit_temp,*,ERR=200,IOSTAT=io) selectedaro
      CLOSE(fileunit_temp)
    ELSE
      msub = nsub_basemodel
    ENDIF

    !Set reordering of subbasins if necessary
    IF(.NOT.ALLOCATED(ibasemodel)) ALLOCATE(ibasemodel(msub))
    ibasemodel(:)=-9999
    IF(submodelrun)THEN
      DO i = 1,nsub_basemodel
        DO j = 1,msub
          IF(basin(i)%subid==selectedaro(j))THEN
            ibasemodel(j)=i
            EXIT
          ENDIF
        ENDDO
      ENDDO
      !Check if all subbasins were found
      j=0 
      DO i=1,msub 
        IF(ibasemodel(i)==-9999) THEN
          IF(j==0) THEN
            WRITE(6,*) ' '
            WRITE(6,*) '---------------------------------------------------'
            WRITE(6,*) 'ERROR Subbasins in pmsf.txt not found i GeoData.txt'
            WRITE(6,*) '       SUBID:       PMSF#'
            j=1
          ENDIF
          WRITE(6,*) selectedaro(i), i
        ENDIF
      ENDDO
      IF(j==1) THEN
        status = 1
        RETURN
      ENDIF
      IF(ALLOCATED(selectedaro)) DEALLOCATE(selectedaro)   
      WRITE(6,*) 'Submodel information loaded'

    ELSE
      !Set index straight if no submodel is simulated
      DO i = 1,nsub_basemodel
        ibasemodel(i)=i
      ENDDO
    ENDIF

    !Write information to hyss.log
    IF(submodelrun)THEN
      WRITE(6,*)
      WRITE(6,*)'-----Information about the simulation (cont.)----'
      WRITE(6,*)'Submodel defined by file: ',TRIM(filename)
      WRITE(6,*)'Number of chosen subbasins to simulate',msub
      WRITE(6,*)'-------------------------------------------------'
      WRITE(6,*)
    ENDIF
    RETURN

200 CONTINUE
    WRITE(6,*) 'ERROR: reading file:', TRIM(filename)
    WRITE(6,*) 'ERROR: io=',io
    status = 1
    RETURN

  END SUBROUTINE load_submodel_info

  !>Turns on calculation of choosen variable
  !------------------------------------------------------------------------
  SUBROUTINE set_outvar_for_variable_new(varid,areaagg,ovindex,noutvar,noutvarclass,nclasses,classes)
    
    USE MODVAR, ONLY : outvarindex, & !OUT
                       outvarclassindex, &  !OUT
                       outvarid, &
                       max_noutvar
    USE WORLDVAR, ONLY : OUTVARINFOTYPE, &
                         outvarinfotemp !OUT
    USE CONVERT, ONLY : real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_set_outvar_for_var, &
                                    e_error,e_warning,e_info

    !Argument declaration
    INTEGER, INTENT(IN) :: varid    !<index in outvarid of current variable
    INTEGER, INTENT(IN) :: areaagg  !<aggregation of variable (0=subbasin average, 1=upstream, 2=region)
    INTEGER, INTENT(OUT) :: ovindex !<index of current variable in outvar
    INTEGER, INTENT(INOUT) :: noutvar !<number of output variables to be set (so far)
    INTEGER, INTENT(INOUT),OPTIONAL :: noutvarclass !<number of class output variables to be calculated (so far)
    INTEGER, INTENT(IN),OPTIONAL :: nclasses !<number of classes included in this variable (0=all, default)
    INTEGER, INTENT(IN),OPTIONAL :: classes(:) !<which classes included in this variable

    !Local variables 
    INTEGER ioutvar,i2,idindex,icurrent,icurrent2,ic
    LOGICAL found,found2,found3,found4,classfound
    INTEGER ncl
    INTEGER ioutvarclass
    INTEGER, ALLOCATABLE :: classindex(:)
    CHARACTER(LEN=150) :: propagate_str
    
    !> \b Algorithm \n
    IF(noutvar>max_noutvar-5) THEN
      WRITE(6,*) 'WARNING: A lot of output variables has been chosen. Program may crash.'
      propagate_str = 'A lot of output variables has been chosen. Program may crash. Warning limit: max_noutvar-5 = '//real_to_str(REAL(max_noutvar-5))
      CALL propagate_external_msg(e_set_outvar_for_var,e_warning,propagate_str)
    ENDIF
    
    !Initiations
    ncl = 0
    IF(PRESENT(noutvarclass)) ioutvarclass = noutvarclass
    IF(PRESENT(nclasses)) ncl = nclasses
    ALLOCATE(classindex(ncl))
    classindex = 0
    IF(PRESENT(classes))THEN
      classindex = classes
    ENDIF
    ioutvar = noutvar
    found = .FALSE.
    
    !Check if the variable already is in outvar
    DO i2=1,ioutvar
      IF(outvarinfotemp(i2)%idindex == varid)THEN
        IF(outvarinfotemp(i2)%areaagg==areaagg)THEN
          IF(ncl==0)THEN
            found = .TRUE.
            ovindex = i2
            EXIT
          ELSEIF(outvarinfotemp(i2)%nclasses==ncl)THEN
            classfound = .TRUE.
            DO ic=1,ncl
              IF(outvarinfotemp(i2)%slcclass(ic)/=classindex(ic))THEN
                classfound = .FALSE.
                EXIT
              ENDIF
            ENDDO
            IF(classfound)THEN
              found = .TRUE.
              ovindex = i2
              EXIT
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    IF(.NOT.found)THEN
      !If not, add the variable to outvar...
      ioutvar = ioutvar + 1
      icurrent = ioutvar
      idindex = varid
      ALLOCATE(outvarinfotemp(icurrent)%slcclass(ncl))
      IF(areaagg==0)THEN
        IF(ncl==0)THEN
          outvarindex(idindex) = icurrent
        ELSE
          IF(outvarclassindex(idindex)==0)THEN
            ioutvarclass = ioutvarclass + 1
            outvarclassindex(idindex) = ioutvarclass
          ENDIF
          !TODO: Anything else need to be saved?
        ENDIF
      ENDIF
      ovindex = icurrent
      outvarinfotemp(icurrent) = OUTVARINFOTYPE(idindex,areaagg,ioutvar,0,ncl,classindex,0)
      !..together with its water-variable if applicable
      IF(outvarid(idindex)%water>0)THEN
        found2 = .FALSE.
        DO i2=1,ioutvar-1
          IF(outvarinfotemp(i2)%idindex == outvarid(idindex)%water)THEN
            IF(outvarinfotemp(i2)%areaagg==areaagg)THEN
              IF(ncl==0)THEN
                found2 = .TRUE.
                outvarinfotemp(icurrent)%ovindexwater = i2
                EXIT
              ELSEIF(outvarinfotemp(i2)%nclasses==ncl)THEN
                classfound = .TRUE.
                DO ic=1,ncl
                  IF(outvarinfotemp(i2)%slcclass(ic)/=classindex(ic))THEN
                    classfound = .FALSE.
                    EXIT
                  ENDIF
                ENDDO
                IF(classfound)THEN
                  found2 = .TRUE.
                  outvarinfotemp(icurrent)%ovindexwater = i2
                  EXIT
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF(.NOT.found2)THEN    !Add the corresponding water variable
          ioutvar = ioutvar + 1
          icurrent2 = ioutvar
          idindex = outvarid(idindex)%water
          ALLOCATE(outvarinfotemp(icurrent2)%slcclass(ncl))
          IF(areaagg==0)THEN
            IF(ncl==0)THEN
              outvarindex(idindex) = icurrent2
            ELSE
              IF(outvarclassindex(idindex)==0)THEN
                ioutvarclass = ioutvarclass + 1
                outvarclassindex(idindex) = ioutvarclass
              ENDIF
            ENDIF
          ENDIF
          outvarinfotemp(icurrent2) = OUTVARINFOTYPE(idindex,areaagg,ioutvar,0,ncl,classindex,0)
          outvarinfotemp(icurrent)%ovindexwater = ioutvar
          !...If the variable is an average of a large area, add the subbasin average of the water variable
          IF(outvarinfotemp(icurrent2)%areaagg>0)THEN
            found3 = .FALSE.
            DO i2=1,icurrent2-1
              IF(outvarinfotemp(i2)%idindex == idindex)THEN
                IF(outvarinfotemp(i2)%areaagg==0)THEN
                  IF(outvarinfotemp(i2)%nclasses==ncl)THEN
                    classfound = .TRUE.
                    DO ic=1,ncl
                      IF(outvarinfotemp(i2)%slcclass(ic)/=classindex(ic))THEN
                        classfound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF(classfound)THEN
                      found3 = .TRUE.
                      outvarinfotemp(icurrent2)%ovindex0 = i2
                      EXIT
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            IF(.NOT.found3)THEN   !Add the non-area aggregated version of the water variable
              ioutvar = ioutvar + 1
              ALLOCATE(outvarinfotemp(ioutvar)%slcclass(ncl))
              IF(ncl==0)THEN
                outvarindex(idindex) = ioutvar
              ELSE
                IF(outvarclassindex(idindex)==0)THEN
                  ioutvarclass = ioutvarclass + 1
                  outvarclassindex(idindex) = ioutvarclass
                ENDIF
              ENDIF
              outvarinfotemp(icurrent2)%ovindex0 = ioutvar
              outvarinfotemp(ioutvar) = OUTVARINFOTYPE(idindex,0,ioutvar,0,ncl,classindex,0)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      !Check if non-area aggregated version of the variable is already in outvar
      IF(outvarinfotemp(icurrent)%areaagg>0)THEN
        found3 = .FALSE.
        DO i2=1,ioutvar-1
          IF(outvarinfotemp(i2)%idindex == varid)THEN
            IF(outvarinfotemp(i2)%areaagg==0)THEN
              IF(outvarinfotemp(i2)%nclasses==ncl)THEN
                classfound = .TRUE.
                DO ic=1,ncl
                  IF(outvarinfotemp(i2)%slcclass(ic)/=classindex(ic))THEN
                    classfound = .FALSE.
                    EXIT
                  ENDIF
                ENDDO
                IF(classfound)THEN
                  found3 = .TRUE.
                  outvarinfotemp(icurrent)%ovindex0 = i2
                  EXIT
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF(.NOT.found3)THEN
          !If not, add the non-area aggregated version variable to outvar...
          ioutvar = ioutvar + 1
          icurrent2 = ioutvar
          idindex = varid
          ALLOCATE(outvarinfotemp(icurrent2)%slcclass(ncl))
          IF(ncl==0)THEN
            outvarindex(idindex) = ioutvar    !replaces the outvarindex(idindex) with the one to areagg=0
          ELSE
            IF(outvarclassindex(idindex)==0)THEN
              ioutvarclass = ioutvarclass + 1
              outvarclassindex(idindex) = ioutvarclass
            ENDIF
          ENDIF
          outvarinfotemp(icurrent2) = OUTVARINFOTYPE(idindex,0,ioutvar,0,ncl,classindex,0)
          outvarinfotemp(icurrent)%ovindex0 = ioutvar !replace with the one with areagg=0
          IF(outvarid(idindex)%water>0)THEN
            found4 = .FALSE.
            DO i2=1,ioutvar-1
              IF(outvarinfotemp(i2)%idindex == outvarid(idindex)%water)THEN
                IF(outvarinfotemp(i2)%nclasses==ncl)THEN
                  classfound = .TRUE.
                  DO ic=1,ncl
                    IF(outvarinfotemp(i2)%slcclass(ic)/=classindex(ic))THEN
                      classfound = .FALSE.
                      EXIT
                    ENDIF
                  ENDDO
                  IF(classfound)THEN
                    found4 = .TRUE.
                    outvarinfotemp(icurrent2)%ovindexwater = i2
                    EXIT
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            IF(.NOT.found4)THEN   !maybe I already added this one above
              ioutvar = ioutvar + 1
              idindex = outvarid(idindex)%water
              ALLOCATE(outvarinfotemp(ioutvar)%slcclass(ncl))
              IF(ncl==0)THEN
                outvarindex(idindex) = ioutvar
              ELSE
                IF(outvarclassindex(idindex)==0)THEN
                  ioutvarclass = ioutvarclass + 1
                  outvarclassindex(idindex) = ioutvarclass
                ENDIF
              ENDIF
              outvarinfotemp(ioutvar) = OUTVARINFOTYPE(idindex,0,ioutvar,0,ncl,classindex,0)
              outvarinfotemp(icurrent2)%ovindexwater = ioutvar
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    
    !>Increase the counter of number of output variables to calculate
    noutvar = ioutvar
    IF(ALLOCATED(classindex)) DEALLOCATE(classindex)
    IF(PRESENT(noutvarclass)) noutvarclass = ioutvarclass

  END SUBROUTINE set_outvar_for_variable_new

  !>Preparation for subbasin and regional output files
  !>Preparation of outputs regarding subbasins and regions. Selected 
  !>subbasins/regions for subbasin and regional output files and criteria
  !>calculations.
  !--------------------------------------------------------------------
  SUBROUTINE prepare_subbasin_output(critbasins,status)

    USE MODVAR, ONLY : basin, &
                       nsub
    USE WORLDVAR, ONLY : output, &
                         noutput, &
                         noutreg, &
                         outregion, &
                         subforcrit !OUT
    
    !Argument declarations
    INTEGER, ALLOCATABLE, INTENT(INOUT) :: critbasins(:)     !<Subid of subbasins selected for criteria calculations
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local variables
    INTEGER i,isub,io,ia,ireg,iaccept,ilast
    INTEGER na
    INTEGER,ALLOCATABLE :: tempoutareas(:)

    status = 0
   
    !>Find and set index of areas to be written as subbasin- or region-files
    !>(areas was given as ids) and reduce them to only those in (sub-)model
    DO io=1,noutput
      na = output(io)%narea
      IF(na==-1)THEN   !all out
        IF(output(io)%fileformat==4)THEN
          na = noutreg
          IF(ALLOCATED(output(io)%areaindex)) DEALLOCATE(output(io)%areaindex)
          ALLOCATE(output(io)%areaindex(na))
          IF(ALLOCATED(tempoutareas)) DEALLOCATE(tempoutareas)
          ALLOCATE(tempoutareas(na))
          tempoutareas(1:na) = outregion(1:na)%outregid
          iaccept = 0
          DO ia = 1,na
            DO ireg = 1,noutreg
              IF(tempoutareas(ia)==outregion(ireg)%outregid .AND. &
                 outregion(ireg)%nsubbasin>0)THEN
                iaccept = iaccept + 1
                output(io)%areaindex(iaccept) = ireg
                EXIT
              ENDIF
            ENDDO
            IF(ireg>noutreg)THEN
              WRITE(6,*) 'Warning: regionoutput region', tempoutareas(ia), 'not found in model'
            ENDIF
          ENDDO
          IF(iaccept<na) output(io)%narea = iaccept
        ENDIF
      ELSEIF(na>0)THEN
        IF(ALLOCATED(tempoutareas)) DEALLOCATE(tempoutareas)
        ALLOCATE(tempoutareas(na))
        tempoutareas(1:na) = output(io)%areaindex !subid or outregid
        IF(output(io)%fileformat==1.OR.output(io)%fileformat==6)THEN
          iaccept = 0
          DO ia = 1,na
            DO isub = 1,nsub
              IF(tempoutareas(ia)==basin(isub)%subid)THEN
                iaccept = iaccept + 1
                output(io)%areaindex(iaccept) = isub
                EXIT
              ENDIF
            ENDDO
            IF(isub>nsub)THEN
              IF(output(io)%fileformat==1) WRITE(6,*) 'Warning: basinoutput subbasin', tempoutareas(ia), 'not in model'
              IF(output(io)%fileformat==6) WRITE(6,*) 'Warning: classoutput subbasin', tempoutareas(ia), 'not in model'
            ELSE
              ilast = iaccept
              DO isub = 1,ilast - 1
                IF(output(io)%areaindex(isub)==output(io)%areaindex(ilast))THEN   !Check for doublets
                  iaccept = iaccept - 1 
                ENDIF
              ENDDO
            ENDIF
          ENDDO
          IF(iaccept<na) output(io)%narea = iaccept
        ELSEIF(output(io)%fileformat==4)THEN
          iaccept = 0
          DO ia = 1,na
            DO ireg = 1,noutreg
              IF(tempoutareas(ia)==outregion(ireg)%outregid .AND. &
                 outregion(ireg)%nsubbasin>0)THEN
                iaccept = iaccept + 1
                output(io)%areaindex(iaccept) = ireg
                EXIT
              ENDIF
            ENDDO
            IF(ireg>noutreg)THEN
              WRITE(6,*) 'Warning: regionoutput region', tempoutareas(ia), 'not found in model'
            ELSE
              ilast = iaccept
              DO ireg = 1,ilast - 1
                IF(output(io)%areaindex(ireg)==output(io)%areaindex(ilast))THEN   !Check for doublets
                  iaccept = iaccept - 1 
                ENDIF
              ENDDO
            ENDIF
          ENDDO
          IF(iaccept<na) output(io)%narea = iaccept
        ENDIF
      ELSE  !na=0
        IF(output(io)%fileformat==1)THEN
          WRITE(6,*) 'Warning: no output areas found for some subbasinoutput'
        ELSEIF(output(io)%fileformat==4)THEN
          WRITE(6,*) 'Warning: no output areas found for some regionoutput'
        ENDIF
      ENDIF
    ENDDO
    IF(ALLOCATED(tempoutareas)) DEALLOCATE(tempoutareas)
    
    !>Find and set index of subbasins to be excluded from criteria calculation
    IF(ALLOCATED(critbasins))THEN
      IF(ALLOCATED(subforcrit)) DEALLOCATE(subforcrit)
      ALLOCATE(subforcrit(nsub))
      subforcrit = .FALSE.
      DO isub = 1,nsub
        DO i = 1,SIZE(critbasins)
          IF(critbasins(i)==basin(isub)%subid)THEN
            subforcrit(isub) = .TRUE.
            EXIT
          ENDIF
        ENDDO
      ENDDO
      DEALLOCATE(critbasins)
    ENDIF

  END SUBROUTINE prepare_subbasin_output

  !>Load file with information on output regions; Outregions.txt
  !
  !>\b Consequences Module worldvar variable outregion is allocated 
  !>and set, and variable noutreg is set.
  !--------------------------------------------------------------------
  SUBROUTINE load_output_regions(dir,nreg,status)

    USE MODVAR, ONLY : basin,            &
                       nsub
    USE WORLDVAR, ONLY : i_str,       &
                         i_intg,      &
                         i_real,      &
                         maxcharpath, &
                         readoutregion
    USE WORLDVAR, ONLY : noutreg,  &    !OUT
                         outregion, &   !OUT
                         fileunit_temp
    USE CONVERT, ONLY : real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_load_output_regions, &
                                    e_error,e_warning,e_info
    
    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: dir   !<File directory
    INTEGER, INTENT(OUT) :: nreg           !<Number of output regions
    INTEGER, INTENT(OUT) :: status         !<Error status

    !Local constants
    INTEGER, PARAMETER :: letters = 10  !Max number of letters (read) in heading
    
    !Local variables
    INTEGER ncols,nrows                !size of data in file
    INTEGER mcol,nmax
    INTEGER i,j,isub,ix
    LOGICAL fileex                     !Status of file
    CHARACTER(LEN=maxcharpath) infile             !Name of irrigation characteristics file 
    INTEGER, ALLOCATABLE :: xi(:,:)               !Integer data read from file
    INTEGER, ALLOCATABLE :: code(:)               !Code for column variable
    INTEGER, ALLOCATABLE :: rindex(:)             !Index for column real variables
    INTEGER, ALLOCATABLE :: iindex(:)             !Index for column integer variables
    REAL, ALLOCATABLE    :: xr(:,:)               !Real data read from file
    INTEGER, ALLOCATABLE :: readsubid(:,:)        !Subid in file
    REAL, ALLOCATABLE    :: readweight(:,:)       !Weights in file
    CHARACTER(LEN=letters), ALLOCATABLE :: str(:)      !File column content string
    CHARACTER(LEN=100) :: propagate_str                  !Propagate string
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str_extra  !Propagate string extra

    status = 0
    nreg = 0
    infile = TRIM(dir)//'Outregions.txt'
    IF(.NOT.readoutregion) RETURN

    !Check if file exist
    INQUIRE(FILE=TRIM(infile),EXIST=fileex)
    IF(.NOT.fileex)THEN
      WRITE(6,*) 'Error: No Outregions.txt file found.'
      propagate_str = 'No Outregions.txt file found: '//TRIM(infile)
      CALL propagate_external_msg(e_load_output_regions,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF
    WRITE(6,*) 'File opened: ', TRIM(infile)

    !Count number of columns in OutregionData
    CALL count_data_cols(fileunit_temp,TRIM(infile),0,ncols,status)
    IF(status/=0)RETURN

    !Count number of regions in OutregionData
    CALL count_data_rows(fileunit_temp,TRIM(infile),1,nrows,status)
    IF(status/=0)RETURN
    IF(nrows==0)THEN
      WRITE(6,*) 'ERROR: No data in Outregions.txt file found.'
      propagate_str = 'No data in Outregions.txt file found.'
      CALL propagate_external_msg(e_load_output_regions,e_error,propagate_str)
      status = 1
      RETURN
    ENDIF

    !Allocate and initiate local variables
    IF(.NOT.ALLOCATED(xi)) ALLOCATE(xi(nrows,ncols))  
    IF(.NOT.ALLOCATED(code)) ALLOCATE(code(ncols))  
    IF(.NOT.ALLOCATED(rindex)) ALLOCATE(rindex(ncols))
    IF(.NOT.ALLOCATED(iindex)) ALLOCATE(iindex(ncols))
    IF(.NOT.ALLOCATED(xr)) ALLOCATE(xr(nrows,ncols))
    IF(.NOT.ALLOCATED(str)) ALLOCATE(str(ncols)) 
    ALLOCATE(readsubid(nsub,nrows))   !nsub = nsub_base
    ALLOCATE(readweight(nsub,nrows))
    readsubid = 0

    !Open file
    OPEN(UNIT = fileunit_temp,FILE = TRIM(infile), STATUS = 'old', ACTION='read')     

    !Read the column headings from file
    CALL read_column_headings(fileunit_temp,ncols,letters,str,mcol,status)
    IF(status.NE.0) THEN
      WRITE(6,*) 'ERROR reading file: ',TRIM(infile)
      propagate_str = 'reading file: '//TRIM(infile)
      CALL propagate_external_msg(e_load_output_regions,e_error,propagate_str)
      RETURN
    ENDIF

    !>If not allocated: allocate and initialize outregion
    noutreg = nrows !module variable
    IF(noutreg>nsub)THEN
      WRITE(6,*) 'ERROR: HYSS-HYPE can not handle more than nsub output regions'
      propagate_str = 'HYSS-HYPE can not handle more than '//real_to_str(REAL(nsub))//' output regions.'
      CALL propagate_external_msg(e_load_output_regions,e_error,propagate_str)
      status = 1
      return
    ENDIF
    nreg = nrows    !subroutine argument
    IF(.NOT.ALLOCATED(outregion)) ALLOCATE(outregion(noutreg))

    IF(.NOT.ALLOCATED(propagate_str_extra)) ALLOCATE(CHARACTER(LEN=(13*ncols)) :: propagate_str_extra)
    propagate_str_extra = 'columns ignored: -'

    !Code variables for easy finding of variable type
    code=i_str    !string, ignore
    DO i = 1,ncols
      IF(str(i)(1:letters)=='outregid  ') code(i) = i_intg
      IF(str(i)(1:letters)=='xcoord    ') code(i) = i_real
      IF(str(i)(1:letters)=='ycoord    ') code(i) = i_real
      IF(str(i)(1:letters)=='zcoord    ') code(i) = i_real
      IF(str(i)(1:letters)=='area      ') code(i) = i_real
      IF(str(i)(1:letters)=='obsfunc   ') code(i) = i_intg
      IF(str(i)(1:6)=='weight')      code(i) = i_real
      IF(str(i)(1:5)=='subid')       code(i) = i_intg
      IF (code(i) == i_str) THEN
        propagate_str_extra = propagate_str_extra//','//TRIM(str(i)(1:letters))
      END IF
    ENDDO

    CALL propagate_external_msg(e_load_output_regions,e_info,propagate_str_extra,REAL(code))
    IF(ALLOCATED(propagate_str_extra)) DEALLOCATE(propagate_str_extra)

    !Read all data
    CALL read_basindata5(fileunit_temp,infile,ncols,nrows,ncols,code,rindex,iindex,xi,xr)

    CLOSE(UNIT=fileunit_temp)

    isub = 0
    DO i = 1,ncols
      IF(str(i)(1:letters)=='outregid  ') outregion(1:nrows)%outregid = xi(1:nrows,iindex(i))
      IF(str(i)(1:letters)=='xcoord    ') outregion(1:nrows)%xcoord   = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='ycoord    ') outregion(1:nrows)%ycoord   = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='zcoord    ') outregion(1:nrows)%zcoord   = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='area      ') outregion(1:nrows)%area     = xr(1:nrows,rindex(i))
      IF(str(i)(1:letters)=='obsfunc   ') outregion(1:nrows)%obsfunc  = xi(1:nrows,iindex(i))
      IF(str(i)(1:5)=='subid')THEN
        isub = isub + 1
        readsubid(isub,1:nrows) = xi(1:nrows,iindex(i))
      ENDIF
      IF(str(i)(1:6)=='weight')THEN
        readweight(isub,1:nrows) = xr(1:nrows,rindex(i))
      ENDIF
    ENDDO
    nmax = isub
    
    !Allocate and find subbasin index and set outregion components
    DO i = 1,noutreg
      IF(outregion(i)%obsfunc==0)THEN !obsfunc == 0: standard method, regoutvar = weighted sum of subbasin outvar
        outregion(i)%nsubbasin = nmax
        DO j = nmax,1,-1
          IF(readsubid(j,i)==0) outregion(i)%nsubbasin = outregion(i)%nsubbasin-1
        ENDDO
        IF(.NOT.ALLOCATED(outregion(i)%subindex)) ALLOCATE(outregion(i)%subindex(outregion(i)%nsubbasin))
        IF(.NOT.ALLOCATED(outregion(i)%weight)) ALLOCATE(outregion(i)%weight(outregion(i)%nsubbasin))
        outregion(i)%subindex = 0
        outregion(i)%weight = readweight(1:outregion(i)%nsubbasin,i)
        DO isub=1,outregion(i)%nsubbasin
          DO ix=1,nsub
            IF(readsubid(isub,i)==basin(ix)%subid)THEN
              outregion(i)%subindex(isub) = ix
              EXIT
            ENDIF
          ENDDO
        ENDDO
      ELSE !obsfunc >= 0: regional observation functions, typically weighted sum of some other regionalized outvars
        SELECT CASE(outregion(i)%obsfunc)
        CASE(1) !obsfunc==1: local reservoir inflow, rgqcin[outregid_i] = w1 * rgclrf[outregid_1] + w2 * (rgclrp[outregid_2] - rgclre[outregid_2])
          outregion(i)%nsubbasin = 2 !this observation function requires weights for exactly two other outregions, but we use the subbasin subindex and weight to store the information
          IF(.NOT.ALLOCATED(outregion(i)%subindex)) ALLOCATE(outregion(i)%subindex(outregion(i)%nsubbasin))
          IF(.NOT.ALLOCATED(outregion(i)%weight)) ALLOCATE(outregion(i)%weight(outregion(i)%nsubbasin))
          
          !weights directly from the outregion.txt input
          outregion(i)%subindex = 0
          outregion(i)%weight = readweight(1:outregion(i)%nsubbasin,i)
          
          !subindex will now store the outregion index
          DO isub=1,2
            DO ix=1,noutreg
              IF(readsubid(isub,i)==outregion(ix)%outregid)THEN
                outregion(i)%subindex(isub) = ix
                EXIT
              ENDIF
            ENDDO
          ENDDO
        CASE DEFAULT
          outregion(i)%nsubbasin = 0
        ENDSELECT
      ENDIF
    ENDDO

    !Deallocate local variables
    IF(ALLOCATED(xi)) DEALLOCATE(xi)  
    IF(ALLOCATED(code)) DEALLOCATE(code)  
    IF(ALLOCATED(rindex)) DEALLOCATE(rindex)
    IF(ALLOCATED(iindex)) DEALLOCATE(iindex)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)
    IF(ALLOCATED(str)) DEALLOCATE(str) 
    IF(ALLOCATED(readsubid)) DEALLOCATE(readsubid) 
    IF(ALLOCATED(readweight)) DEALLOCATE(readweight)

    WRITE(6,*) 'Output region information loaded'

  END SUBROUTINE load_output_regions
  

  !>Writes the files with data in format suitable for mapping.
  !--------------------------------------------------------------------
  SUBROUTINE save_mapfiles(dir,ntime,iens,runens,allens)

    USE WORLDVAR, ONLY : maptime,     &
                         writematlab, &
                         resultseq,   &
                         simsequence, &
                         fileunit_temp, &
                         output,      &
                         outregion,   &
                         noutput,     &
                         noutreg,     &
                         i_t,i_d,i_w,i_m,i_y,i_s
    USE MODVAR, ONLY : basin, &
                       i_sum, &
                       nsub, &
                       outvarid
    USE READWRITE_ROUTINES, ONLY : write_dataline, &
                                   write_commentline

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir  !<File directory
    INTEGER, INTENT(IN) :: ntime         !<Number of values in vector = number of timesteps
    INTEGER, INTENT(IN) :: iens          !<Current simulation
    LOGICAL, INTENT(IN) :: runens        !<Flag for ensemble simulation
    LOGICAL, INTENT(IN) :: allens        !<Flag for writing all ensemble results

    !Local variables
    INTEGER io,ivar
    INTEGER itime
    INTEGER isub
    INTEGER nn         !File prefix number
    CHARACTER(LEN=29) filename  !Current filename
    CHARACTER(LEN=20) var
    CHARACTER(LEN=40) var2
    CHARACTER(LEN=30) unit
    REAL x(ntime)
    INTEGER lt,lout
    CHARACTER (LEN=16)   t
    CHARACTER (LEN=200) comment
    CHARACTER (LEN=1800000) outtxt
    LOGICAL writeperiod

    DO io = 1,noutput
      IF(output(io)%fileformat==2)THEN
        DO ivar = 1,output(io)%nvar
          IF(resultseq.AND.simsequence>0 .OR. runens)THEN  !Ex. mapCOUT_005.txt
            IF(resultseq.AND.simsequence>0) nn=simsequence
            IF(runens) nn=iens
            CALL create_filename_for_variable(filename,'map',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix2=nn,suffix2format=allens)
          ELSE  !Ex. mapCOUT.txt
            CALL create_filename_for_variable(filename,'map',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg)
          ENDIF
          var=outvarid(output(io)%variable(ivar)%idindex)%longname
          IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
            var2='regional average of '//var
          ELSEIF(output(io)%variable(ivar)%areaagg==1)THEN    !upstream output
            var2='upstream average of '//var
          ELSE
            var2=var
          ENDIF
          unit = get_unit_string(io,ivar)
          OPEN(file=TRIM(dir)//filename,unit=fileunit_temp,status='unknown',form='formatted',ACTION='write')

          !Write heading
          IF(writematlab)THEN
            comment = '%This is a file with '//TRIM(var2)//' in '//TRIM(unit)//' for GIS mapping'
          ELSE
            comment = '!!This is a file with '//TRIM(var2)//' in '//TRIM(unit)//' for GIS mapping'
          ENDIF
          CALL write_commentline(fileunit_temp,comment)
          outtxt = ''
          IF(writematlab)THEN
            IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
              outtxt(1:10) = '%OUTREGID'//CHAR(44)
              lout = 10
            ELSE
              outtxt(1:7) = '%SUBID'//CHAR(44)
              lout = 7
            ENDIF
          ELSE
            IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
              outtxt(1:11) = 'OUTREGID'//CHAR(44)
              lout = 11
            ELSE
              outtxt(1:6) = 'SUBID'//CHAR(44)
              lout = 6
            ENDIF
          ENDIF
          IF(output(io)%period==i_s) THEN
            writeperiod=.TRUE.
          ELSE
            writeperiod=.FALSE.
          ENDIF
          DO itime = 1,ntime
            t = ''
            t = TRIM(maptime(itime))
            t = ADJUSTL(t)
            lt = LEN_TRIM(t)
            outtxt(lout+1:lout+lt) = t(1:lt)
            lout = lout+lt
            IF(itime < ntime) THEN
              outtxt(lout+1:lout+1) = CHAR(44)   !Also the last one necessary
              lout = lout+1                      !for reading in "free format"
            ENDIF
          ENDDO
          WRITE(fileunit_temp,'(a)') outtxt(1:lout) 

          !Write data
          IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
            DO isub=1,noutreg
              x=output(io)%accdata%value(isub,ivar,1:ntime)
              CALL write_dataline(fileunit_temp,ntime,x,output(io)%decimal,output(io)%signfig,&
                    0,',',0,writematlab,id=outregion(isub)%outregid)
            ENDDO
          ELSE
            DO isub=1,nsub
              x=output(io)%accdata%value(isub,ivar,1:ntime)
              CALL write_dataline(fileunit_temp,ntime,x,output(io)%decimal,output(io)%signfig,&
                    0,',',0,writematlab,id=basin(isub)%subid)
            ENDDO
          ENDIF
          CLOSE(fileunit_temp)
        ENDDO
      ENDIF
    ENDDO

    RETURN
  END SUBROUTINE save_mapfiles

  !>Writes subbasin/output region assessment to file subassX.txt for each pair of compared variables
  !--------------------------------------------------------------------
  SUBROUTINE write_subbasin_assessment(filedir,numsub,na,sas2,iens,runens)

    USE WORLDVAR, ONLY : acccalvar,     &
                         calvarper,     &
                         outregion,     &
                         noutreg,       &
                         fileunit_temp, &
                         writematlab,   &
                         resultseq,     &
                         simsequence,   &
                         outperiodname, &
                         maxsubass
    USE MODVAR, ONLY :   outvarid,      &
                         nsub,          &
                         basin,         &
                         missing_value      
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: filedir       !<File directory
    INTEGER, INTENT(IN) :: numsub                 !<dimension of performance measures (areas)
    INTEGER, INTENT(IN) :: na             !<dimension of performance measures (variables)
    REAL, INTENT(IN)    :: sas2(numsub,maxsubass,na)  !<assessment values
    INTEGER, INTENT(IN) :: iens                   !<Current simulation
    LOGICAL, INTENT(IN) :: runens                 !<Flag for ensemble simulation

    !Local parameters
    INTEGER, PARAMETER :: ndec = 4      !Number of decimals to be written (rounding to ndec decimals will occurr)
    
    !Local variables
    INTEGER ia         !Criteria number
    INTEGER isub       !Subbasin
    INTEGER nn         !File suffix number
    CHARACTER(LEN=20) filename
    REAL x(maxsubass)  !Values
    CHARACTER(LEN=1) tal

    !>\b Algorithm \n
    tal = '0'
    !>For every pair of variables to be compared 
    DO ia = 1, na
      !> \li Determine the filename and open a file
      tal = CHAR(ICHAR(tal)+1)    !tal=i
      IF(tal==':') tal = 'A'      !for more than 9 criterias use letters
      IF(resultseq.AND.simsequence>0 .OR. runens)THEN  !Ex. subass1_005.txt
        IF(resultseq.AND.simsequence>0) nn=simsequence
        IF(runens) nn=iens
        filename(1:8) = 'subass'//tal//'_'
        WRITE(filename(9:11),601) nn
        filename(12:15) = '.txt'
      ELSE  !Ex. subass1.txt
        filename='subass'//tal//'.txt'
      ENDIF
601   FORMAT(I3.3)
      OPEN(file=TRIM(filedir)//filename,unit=fileunit_temp,status='unknown',form='formatted',ACTION='write')
      
      !> \li Write heading to file
      IF(acccalvar(ia)%areaagg==0)THEN
        IF(writematlab)THEN
          WRITE(fileunit_temp,'(a56,a2,a13,a4,a2,a4,a8,a6)')    &
              '%Subbasin assessment. Criteria is calculated for period ',outperiodname(calvarper),'. Variables: ',outvarid(acccalvar(ia)%rec)%shortname,', ',outvarid(acccalvar(ia)%comp)%shortname,' Unit: ',outvarid(acccalvar(ia)%comp)%shortunit
          WRITE(fileunit_temp,'(a)') '%SUBID'//CHAR(9)//'NSE'//CHAR(9)//'CC'//CHAR(9)//'RE(%)'//CHAR(9)//'RSDE(%)'//CHAR(9)//'Sim'//CHAR(9)//'Rec'//CHAR(9)//'SDSim'//CHAR(9)//'SDRec'//CHAR(9)//'MAE'//CHAR(9)//'RMSE'//CHAR(9)//'Bias'//CHAR(9)//'SDE'//CHAR(9)//'KGE'//CHAR(9)//'KGESD'//CHAR(9)//'KGEM'//CHAR(9)//'NRMSE'//CHAR(9)//'NSEW'
        ELSE
          WRITE(fileunit_temp,'(a57,a2,a13,a4,a2,a4,a8,a6)')    &
                '!!Subbasin assessment. Criteria is calculated for period ',outperiodname(calvarper),'. Variables: ',outvarid(acccalvar(ia)%rec)%shortname,', ',outvarid(acccalvar(ia)%comp)%shortname,' Unit: ',outvarid(acccalvar(ia)%comp)%shortunit
          WRITE(fileunit_temp,'(a)') 'SUBID'//CHAR(9)//'NSE'//CHAR(9)//'CC'//CHAR(9)//'RE(%)'//CHAR(9)//'RSDE(%)'//CHAR(9)//'Sim'//CHAR(9)//'Rec'//CHAR(9)//'SDSim'//CHAR(9)//'SDRec'//CHAR(9)//'MAE'//CHAR(9)//'RMSE'//CHAR(9)//'Bias'//CHAR(9)//'SDE'//CHAR(9)//'KGE'//CHAR(9)//'KGESD'//CHAR(9)//'KGEM'//CHAR(9)//'NRMSE'//CHAR(9)//'NSEW'
        ENDIF
      ELSEIF(acccalvar(ia)%areaagg==2)THEN
        IF(writematlab)THEN
          WRITE(fileunit_temp,'(a56,a2,a15,a4,a4,a4,a8,a6)')    &
                '%Subbasin assessment. Criteria is calculated for period ',outperiodname(calvarper),'. Variables: rg',outvarid(acccalvar(ia)%rec)%shortname,', rg',outvarid(acccalvar(ia)%comp)%shortname,' Unit: ',outvarid(acccalvar(ia)%comp)%shortunit
          WRITE(fileunit_temp,'(a)') '%OUTREGID'//CHAR(9)//'NSE'//CHAR(9)//'CC'//CHAR(9)//'RE(%)'//CHAR(9)//'RSDE(%)'//CHAR(9)//'Sim'//CHAR(9)//'Rec'//CHAR(9)//'SDSim'//CHAR(9)//'SDRec'//CHAR(9)//'MAE'//CHAR(9)//'RMSE'//CHAR(9)//'Bias'//CHAR(9)//'SDE'//CHAR(9)//'KGE'//CHAR(9)//'KGESD'//CHAR(9)//'KGEM'//CHAR(9)//'NRMSE'//CHAR(9)//'NSEW'
        ELSE
          WRITE(fileunit_temp,'(a57,a2,a15,a4,a4,a4,a8,a6)')    &
                '!!Subbasin assessment. Criteria is calculated for period ',outperiodname(calvarper),'. Variables: rg',outvarid(acccalvar(ia)%rec)%shortname,', rg',outvarid(acccalvar(ia)%comp)%shortname,' Unit: ',outvarid(acccalvar(ia)%comp)%shortunit
          WRITE(fileunit_temp,'(a)') 'OUTREGID'//CHAR(9)//'NSE'//CHAR(9)//'CC'//CHAR(9)//'RE(%)'//CHAR(9)//'RSDE(%)'//CHAR(9)//'Sim'//CHAR(9)//'Rec'//CHAR(9)//'SDSim'//CHAR(9)//'SDRec'//CHAR(9)//'MAE'//CHAR(9)//'RMSE'//CHAR(9)//'Bias'//CHAR(9)//'SDE'//CHAR(9)//'KGE'//CHAR(9)//'KGESD'//CHAR(9)//'KGEM'//CHAR(9)//'NRMSE'//CHAR(9)//'NSEW'
        ENDIF
      ENDIF
      
      !> \li Write data to file
      IF(acccalvar(ia)%areaagg==0)THEN
        DO isub=1,nsub
          x=sas2(isub,1:maxsubass,ia)
          IF(x(6)/=missing_value) CALL write_dataline(fileunit_temp,maxsubass,x,ndec,0,0,CHAR(9),0,writematlab,id=basin(isub)%subid)  !Rec (x(6)) decides
        ENDDO
      ELSEIF(acccalvar(ia)%areaagg==1)THEN
        !Add upstream variables?
      ELSEIF(acccalvar(ia)%areaagg==2)THEN
        DO isub=1,noutreg
          x=sas2(isub,1:maxsubass,ia)
          IF(x(6)/=missing_value) CALL write_dataline(fileunit_temp,maxsubass,x,ndec,0,0,CHAR(9),0,writematlab,id=outregion(isub)%outregid)
        ENDDO
      ENDIF
      CLOSE(fileunit_temp)
    ENDDO

  END SUBROUTINE write_subbasin_assessment

  !>Writes the simulation assessment to hyss file and separate file
  !--------------------------------------------------------------------
  SUBROUTINE write_simulation_assessment(dir,iens,n,optcrit,performance,runens,ccrit,cthres)

    USE WORLDVAR, ONLY : acccalvar,   &
                         calvarper,   &
                         resultseq,   &
                         maxperf,     &
                         simsequence, &
                         outperiodname, &
                         i_rnse,i_snse,  &
                         i_mnse,         &
                         i_rmae,i_sre,   &  
                         i_rre,i_mre,    &
                         i_rra,i_sra,    &
                         i_mra,i_tau,    &
                         i_mdnse,i_mdra, &
                         i_mstdre,i_mcc, &
                         i_mdkg,i_akg, &
                         i_asckg,i_mabsre, &
                         i_mnrmse,i_mnw, &
                         i_snr,i_smb,    &
                         i_numrc,i_nummc, &
                         fileunit_temp, &
                         ncrit, calvar
    USE COMPOUT, ONLY : find_acrit_corresponding_to_crit
    USE MODVAR, ONLY :   outvarid

    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: dir           !<File directory
    INTEGER, INTENT(IN)          :: iens          !<Current simulation
    INTEGER, INTENT(IN)          :: n             !<Dimension of performance measures
    REAL, INTENT(IN)             :: optcrit       !<Opimization criterion value
    REAL, INTENT(IN)             :: performance(maxperf,n)  !Perfomance criteria
    LOGICAL, INTENT(IN)          :: runens        !<Flag for ensemble simulation
    REAL, OPTIONAL, INTENT(IN)   :: ccrit         !<conditional criteria
    REAL, OPTIONAL, INTENT(IN)   :: cthres        !<conditional criteria threshold
 
    !Local variables
    INTEGER i,iac
    INTEGER nn    !file suffix number
    REAL x(ncrit)
    CHARACTER(LEN=20) filename
    CHARACTER(LEN=15) fmtstr,fmtstr3

    !Write simulation assessment to log-file
    !Write data to log-file; objective function value
    WRITE(6,*)
    IF(PRESENT(cthres))THEN
       WRITE(6,*) 'Total criteria value: ',optcrit, ' Conditional criteria value: ',ccrit,', threshold: ', cthres
    ELSE
       WRITE(6,*) 'Total criteria value: ',optcrit, ' Conditional criteria value: -9999, threshold: -9999'
    ENDIF
    
    !Write data to log-file; for each pair of variables
    DO i = 1, n
       WRITE(6,*)
       IF(acccalvar(i)%areaagg==0)THEN
         WRITE(6,*) 'Variables: ',outvarid(acccalvar(i)%rec)%shortname,', ',outvarid(acccalvar(i)%comp)%shortname
       ELSEIF(acccalvar(i)%areaagg==1)THEN
         WRITE(6,*) 'Variables: up',outvarid(acccalvar(i)%rec)%shortname,', up',outvarid(acccalvar(i)%comp)%shortname
       ELSEIF(acccalvar(i)%areaagg==2)THEN
         WRITE(6,*) 'Variables: rg',outvarid(acccalvar(i)%rec)%shortname,', rg',outvarid(acccalvar(i)%comp)%shortname
       ENDIF
       WRITE(6,*) 'Period: ',outperiodname(calvarper)
       WRITE(6,*) 'Regional NSE:',performance(i_rnse,i)
       WRITE(6,*) 'Regional  RA:',performance(i_rra,i)
       WRITE(6,*) 'Regional  RE:',performance(i_rre,i)
       WRITE(6,*) 'Regional MAE:',performance(i_rmae,i)
       WRITE(6,*) 'Average  NSE:',performance(i_mnse,i)
       WRITE(6,*) 'Average   RA:',performance(i_mra,i)
       WRITE(6,*) 'Average   RE:',performance(i_mre,i)
       WRITE(6,*) 'Average RSDE:',performance(i_mstdre,i)
       WRITE(6,*) 'Average   CC:',performance(i_mcc,i)
       WRITE(6,*) 'Average  ARE:',performance(i_mabsre,i)
       WRITE(6,*) 'Average  KGE:',performance(i_akg,i)
       WRITE(6,*) 'Aver scalKGE:',performance(i_asckg,i)
       WRITE(6,*) 'Spatial  NSE:',performance(i_snse,i)
       WRITE(6,*) 'Spatial   RA:',performance(i_sra,i)
       WRITE(6,*) 'Spatial   RE:',performance(i_sre,i)
       WRITE(6,*) 'Spatial Bias:',performance(i_smb,i)
       WRITE(6,*) 'Spatial RMSE:',performance(i_snr,i)
       WRITE(6,*) 'Kendalls Tau:',performance(i_tau,i)
       WRITE(6,*) 'Median   NSE:',performance(i_mdnse,i)
       WRITE(6,*) 'Median    RA:',performance(i_mdra,i)
       WRITE(6,*) 'Median   KGE:',performance(i_mdkg,i)
       WRITE(6,*) 'Median NRMSE:',performance(i_mnrmse,i)
       WRITE(6,*) 'Mean    NSEW:',performance(i_mnw,i)
       WRITE(6,*) 'Number of data for regional criterion:',performance(i_numrc,i)
       WRITE(6,*) 'Number of areas in mean/median criterion:',performance(i_nummc,i)
    ENDDO
    WRITE(6,*)

    !Open simulation assessment file (simass-file)
    IF(resultseq.AND.simsequence>0 .OR. runens)THEN  !Ex. simass_005.txt
       IF(resultseq.AND.simsequence>0) nn=simsequence
       IF(runens) nn=iens
       filename(1:7) = 'simass_'
       WRITE(filename(8:10),601) nn
       filename(11:14) = '.txt'
    ELSE
       filename='simass.txt'
    ENDIF
601 FORMAT(I3.3)
    OPEN(FILE=TRIM(dir)//filename,UNIT=fileunit_temp,STATUS='unknown',FORM='formatted',ACTION='write')
    !Write heading
    WRITE(fileunit_temp,*) 'Simulation assessment all variables/criterions used'

    !Write data to simass-file; objective function value
    WRITE(fileunit_temp,*) 'Simulation number: ',iens
    IF(PRESENT(cthres))THEN
      WRITE(fileunit_temp,*) 'Total criteria value: ',optcrit, ' Conditional criteria value: ',ccrit,', threshold: ', cthres
    ELSE
      WRITE(fileunit_temp,*) 'Total criteria value: ',optcrit, ' Conditional criteria value: -9999, threshold: -9999'
    ENDIF
    
    !Write data to simass-file; individual criteria in objective function
    IF(ncrit>0)THEN
      DO i = 1, ncrit   !All criteria included in objective function
        CALL find_acrit_corresponding_to_crit(i,iac)
        SELECT CASE (calvar(i)%crit)
        CASE('TAU')
          x(i) = performance(i_tau,iac)
        CASE('RRA')
          x(i) = performance(i_rra,iac)
        CASE('SRA')
          x(i) = performance(i_sra,iac)
        CASE('SR2')
          x(i) = performance(i_snse,iac)
        CASE('RR2')
          x(i) = performance(i_rnse,iac)
        CASE('MR2')
          x(i) = performance(i_mnse,iac)
        CASE('MD2')
          x(i) = performance(i_mdnse,iac)
        CASE('MRA')
          x(i) = performance(i_mra,iac)
        CASE('MDA')
          x(i) = performance(i_mdra,iac)
        CASE('RRE')
          x(i) = performance(i_rre,iac)
        CASE('MRE')
          x(i) = performance(i_mre,iac)
        CASE('MAR')
          x(i) = performance(i_mabsre,iac)
        CASE('MCC')
          x(i) = performance(i_mcc,iac)
        CASE('MKG')
          x(i) = performance(i_mdkg,iac)
        CASE('AKG')
          x(i) = performance(i_akg,iac)
        CASE('ASK')
          x(i) = performance(i_asckg,iac)
        CASE('MRS')
          x(i) = performance(i_mstdre,iac)
        CASE('MNR')
          x(i) = performance(i_mnrmse,iac) 
        CASE('MNW')
          x(i) = performance(i_mnw,iac)
        CASE('SNR')
          x(i) = performance(i_snr,iac)
        CASE('SMB')
          x(i) = performance(i_smb,iac)
        END SELECT  
      ENDDO
      WRITE(fileunit_temp,*)
      WRITE(fileunit_temp,'(A22)')  'Individual criterions'
      WRITE(fmtstr3,'(A1,I2.2,A9)') '(',ncrit,'(A13,I2))'
      WRITE(fmtstr,'(A1,I2.2,A6)') '(',ncrit,'F15.6)'
      WRITE(fileunit_temp,fmtstr3) ('        crit ',i,i=1,ncrit)
      WRITE(fileunit_temp,fmtstr) x
    ENDIF
    
    !Write data to simass-file; for each pair of variables
    DO i = 1, n
       WRITE(fileunit_temp,*)
       IF(acccalvar(i)%areaagg==0)THEN
         WRITE(fileunit_temp,*) 'Variables: ',outvarid(acccalvar(i)%rec)%shortname,', ',outvarid(acccalvar(i)%comp)%shortname
       ELSEIF(acccalvar(i)%areaagg==1)THEN
         WRITE(fileunit_temp,*) 'Variables: up',outvarid(acccalvar(i)%rec)%shortname,', up',outvarid(acccalvar(i)%comp)%shortname
       ELSEIF(acccalvar(i)%areaagg==2)THEN
         WRITE(fileunit_temp,*) 'Variables: rg',outvarid(acccalvar(i)%rec)%shortname,', rg',outvarid(acccalvar(i)%comp)%shortname
       ENDIF
       WRITE(fileunit_temp,*) 'Period: ',outperiodname(calvarper)
       WRITE(fileunit_temp,*) 'Regional NSE:',performance(i_rnse,i)
       WRITE(fileunit_temp,*) 'Regional  RA:',performance(i_rra,i)
       WRITE(fileunit_temp,*) 'Regional  RE:',performance(i_rre,i)
       WRITE(fileunit_temp,*) 'Regional MAE:',performance(i_rmae,i)
       WRITE(fileunit_temp,*) 'Average  NSE:',performance(i_mnse,i)
       WRITE(fileunit_temp,*) 'Average   RA:',performance(i_mra,i)
       WRITE(fileunit_temp,*) 'Average   RE:',performance(i_mre,i)
       WRITE(fileunit_temp,*) 'Average RSDE:',performance(i_mstdre,i)
       WRITE(fileunit_temp,*) 'Average   CC:',performance(i_mcc,i)
       WRITE(fileunit_temp,*) 'Average  ARE:',performance(i_mabsre,i)
       WRITE(fileunit_temp,*) 'Average  KGE:',performance(i_akg,i)
       WRITE(fileunit_temp,*) 'Aver scalKGE:',performance(i_asckg,i)
       WRITE(fileunit_temp,*) 'Spatial  NSE:',performance(i_snse,i)
       WRITE(fileunit_temp,*) 'Spatial   RA:',performance(i_sra,i)
       WRITE(fileunit_temp,*) 'Spatial   RE:',performance(i_sre,i)
       WRITE(fileunit_temp,*) 'Spatial Bias:',performance(i_smb,i)
       WRITE(fileunit_temp,*) 'Spatial RMSE:',performance(i_snr,i)
       WRITE(fileunit_temp,*) 'Kendalls Tau:',performance(i_tau,i)
       WRITE(fileunit_temp,*) 'Median   NSE:',performance(i_mdnse,i)
       WRITE(fileunit_temp,*) 'Median    RA:',performance(i_mdra,i)
       WRITE(fileunit_temp,*) 'Median   KGE:',performance(i_mdkg,i)
       WRITE(fileunit_temp,*) 'Median NRMSE:',performance(i_mnrmse,i)
       WRITE(fileunit_temp,*) 'Mean    NSEW:',performance(i_mnw,i)
       WRITE(fileunit_temp,*) 'Number of data for regional criterion:',performance(i_numrc,i)
       WRITE(fileunit_temp,*) 'Number of areas in mean/median criterion:',performance(i_nummc,i)
    ENDDO
    WRITE(fileunit_temp,*)
    CLOSE(fileunit_temp)
    
  END SUBROUTINE write_simulation_assessment

  !>Manage output files; opens files and write heading
  !--------------------------------------------------------------------
  SUBROUTINE prepare_outputfiles(dir,n,na,iens,runens,allsim,ensstat)

    USE MODELMODULE, ONLY : open_modeldefined_outputfiles
    
    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir !<Result file directory
    INTEGER, INTENT(IN) :: n            !<Number of subbasins
    INTEGER, INTENT(IN) :: na           !<Number of aquifers
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    LOGICAL, INTENT(IN) :: runens       !<Flag for using ensemble number of ensemble simulation
    LOGICAL, INTENT(IN) :: allsim       !<Flag for writing all simulation results
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files

    !CALL check_files_for_doubles()
    IF(PRESENT(ensstat))THEN
      CALL open_subbasinfiles(dir,n,iens,runens,allsim,ensstat) !and regional
      CALL open_timefiles(dir,n,iens,runens,allsim,ensstat)
      CALL open_timefiles_classes(dir,n,iens,runens,allsim,ensstat)
    ELSE
      CALL open_subbasinfiles(dir,n,iens,runens,allsim) !and regional and basinclass
      CALL open_timefiles(dir,n,iens,runens,allsim)
      CALL open_timefiles_classes(dir,n,iens,runens,allsim)
    ENDIF
    CALL open_modeldefined_outputfiles(dir,n,na)

  END SUBROUTINE prepare_outputfiles
  
  !>Manage output files; close files
  !--------------------------------------------------------------------
  SUBROUTINE close_outputfiles(n,na,iens,ensstat)

    USE MODELMODULE, ONLY : close_modeldefined_outputfiles
    
    !Argument declarations
    INTEGER, INTENT(IN) :: n            !<Number of subbasins
    INTEGER, INTENT(IN) :: na           !<Number of aquifers
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files

    IF(PRESENT(ensstat).AND. iens>1)THEN
      CALL close_subbasinfiles(n,iens,ensstat)
      CALL close_timefiles(iens,ensstat)
      CALL close_timefiles_class(iens,ensstat)
    ELSE
      CALL close_subbasinfiles(n,iens)
      CALL close_timefiles(iens)
      CALL close_timefiles_class(iens)
    ENDIF
    CALL close_modeldefined_outputfiles(na)

  END SUBROUTINE close_outputfiles
  
  !>Opens files for subbasin and output region printout, including classoutput
  !--------------------------------------------------------------------
  SUBROUTINE open_subbasinfiles(dir,n,iens,runens,allens,ensstat)

    USE WORLDVAR, ONLY : output,    &
                         noutput,   &
                         outregion, &
                         noutreg,   &
                         maxclassingroup, &
                         outvarinfo, &
                         outperiodname, &
                         writematlab,     &
                         resultseq,       &
                         simsequence,     &
                         fileunit_get, &
                         fid_assim_ens
    USE MODVAR, ONLY : outvarid,     &
                       basin, &
                       nsub, &
                       get_classgroup_classes
    USE READWRITE_ROUTINES, ONLY : write_commentline

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir !<Result file directory
    INTEGER, INTENT(IN) :: n            !<Number of subbasins
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    LOGICAL, INTENT(IN) :: runens       !<Flag for ensemble simulation
    LOGICAL, INTENT(IN) :: allens       !<Flag for writing all ensemble results, i.e. 6 characters for integer
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files

    !Local variables
    INTEGER i,io,ivar,isb,ireg
    INTEGER nn                  !file suffix number
    INTEGER nselected           !number of selected subbasins for output
    INTEGER funit               !fileunit for subbasin output
    INTEGER ispecific           !order number of specific output type
    INTEGER nslc                !number of classes in group
    INTEGER,ALLOCATABLE :: classindex(:)  !classes in group
    CHARACTER(LEN=28) str       !filename
    CHARACTER(LEN=4) var
    CHARACTER(LEN=6) var2
    CHARACTER(LEN=6) varunit
    INTEGER lt,ls,lout1,lout2
    CHARACTER(LEN=16)   t,s
    CHARACTER(LEN=2000) outtxt,outtxt2           !max_outvar*7
    CHARACTER(LEN=200) commentheading
    CHARACTER(LEN=150) classes
    CHARACTER(LEN=20) fmtstr

    !Preparations if several ensemble results are to be written
    IF(PRESENT(ensstat))THEN
      IF(.NOT.ALLOCATED(fid_assim_ens))THEN
        nn = 0
        DO io=1,noutput
          IF(output(io)%fileformat==1)THEN
            nselected = nsub
            IF(output(io)%narea>0) nselected = output(io)%narea
            nn = MAX(nn,nselected)
          ELSEIF(output(io)%fileformat==4)THEN
            nselected = noutreg
            IF(output(io)%narea>0) nselected = output(io)%narea
            nn = MAX(nn,nselected)
          ELSEIF(output(io)%fileformat==6)THEN
            nselected = 0
            IF(output(io)%narea>0) nselected = output(io)%narea   !narea=0 means timefile
            nn = MAX(nn,nselected)
          ELSEIF(output(io)%fileformat==3)THEN
            nn = MAX(nn,output(io)%nvar)
          ELSEIF(output(io)%fileformat==5)THEN
            nn = MAX(nn,output(io)%nvar)
          ENDIF
        ENDDO
        ALLOCATE(fid_assim_ens(noutput,ensstat+1,nn))
        fid_assim_ens=1
      ENDIF
    ENDIF
    
    !Open subbasin files
    !ispecific = 0
    DO io=1,noutput
      IF(output(io)%fileformat==1)THEN
        IF(output(io)%narea==0) CYCLE     !no areas for this output, skip
        !ispecific = ispecific + 1
        !Preparation of heading
        IF(writematlab)THEN
          lout1 = 6
          lout2 = 7
          outtxt(1:lout1) = '%DATE'//CHAR(9)
          outtxt2(1:lout2) = '%UNITS'//CHAR(9)
        ELSE
          lout1 = 5
          lout2 = 6
          outtxt(1:lout1) = 'DATE'//CHAR(9)
          outtxt2(1:lout2) = 'UNITS'//CHAR(9)
        ENDIF
        DO ivar = 1,output(io)%nvar
          var  = outvarid(output(io)%variable(ivar)%idindex)%shortname
          !varunit = outvarid(output(io)%variable(ivar)%idindex)%shortunit
          IF(output(io)%variable(ivar)%areaagg==1)THEN    !upstream output
            var2='up'//var
            varunit = outvarid(output(io)%variable(ivar)%idindex)%upunit
          ELSE
            var2=var
            varunit = outvarid(output(io)%variable(ivar)%idindex)%shortunit
          ENDIF
          WRITE(t,'(a16)') var2
          WRITE(s,'(a16)') varunit
          t = ADJUSTL(t)
          s = ADJUSTL(s)
          lt = LEN_TRIM(t)
          ls = LEN_TRIM(s)
          outtxt(lout1+1:lout1+lt) = t(1:lt)
          outtxt2(lout2+1:lout2+ls) = s(1:ls)
          lout1 = lout1+lt
          lout2 = lout2+ls
          IF(ivar < output(io)%nvar) THEN
            outtxt(lout1+1:lout1+1) = CHAR(9)
            outtxt2(lout2+1:lout2+1) = CHAR(9) !Also the last one necessary
            lout1 = lout1+1                    !for reading in "free format"
            lout2 = lout2+1
          ENDIF
        ENDDO

        nselected = n
        IF(output(io)%narea>0) nselected = output(io)%narea
        IF(.NOT.ALLOCATED(output(io)%fileunit)) ALLOCATE(output(io)%fileunit(nselected))
        DO i = 1,nselected
          isb = i
          IF(output(io)%narea>0) isb = output(io)%areaindex(i)
          IF(resultseq.AND.simsequence>0 .OR. runens)THEN
            IF(resultseq.AND.simsequence>0) nn=simsequence
            IF(runens) nn=iens
            CALL create_filename_for_basin(str,basin(isb)%subid,output(io)%useperiodname,outperiodname(output(io)%period),nn,allens)
          ELSE
            CALL create_filename_for_basin(str,basin(isb)%subid,output(io)%useperiodname,outperiodname(output(io)%period))
          ENDIF
          funit = fileunit_get()
          IF(PRESENT(ensstat))THEN
            fid_assim_ens(io,iens,i) = funit  !save for later
          ELSE
            output(io)%fileunit(i) = funit
          ENDIF
          !Open and write heading to file
          OPEN(UNIT=funit,FILE=TRIM(dir)//TRIM(str),status='unknown',form='formatted',ACTION='write')
          WRITE(funit,'(a)') outtxt(1:lout1)
          WRITE(funit,'(a)') outtxt2(1:lout2)
        ENDDO
      ENDIF
    ENDDO
    

    !Output region files
    DO io=1,noutput
      IF(output(io)%fileformat==4)THEN
        IF(output(io)%narea==0) CYCLE   !no areas for this output, skip
        !Preparation of heading of region files
        outtxt='';outtxt2=''
        IF(writematlab)THEN
          lout1 = 6
          lout2 = 7
          outtxt(1:lout1) = '%DATE'//CHAR(9)
          outtxt2(1:lout2) = '%UNITS'//CHAR(9)
        ELSE
          lout1 = 5
          lout2 = 6
          outtxt(1:lout1) = 'DATE'//CHAR(9)
          outtxt2(1:lout2) = 'UNITS'//CHAR(9)
        ENDIF
        DO ivar = 1,output(io)%nvar
          var  = outvarid(outvarinfo(output(io)%variable(ivar)%ovindex)%idindex)%shortname
          varunit = outvarid(outvarinfo(output(io)%variable(ivar)%ovindex)%idindex)%shortunit
          var2='rg'//var
          WRITE(t,'(a16)') var2
          WRITE(s,'(a16)') varunit
          t = ADJUSTL(t)
          s = ADJUSTL(s)
          lt = LEN_TRIM(t)
          ls = LEN_TRIM(s)
          outtxt(lout1+1:lout1+lt) = t(1:lt)
          outtxt2(lout2+1:lout2+ls) = s(1:ls)
          lout1 = lout1+lt
          lout2 = lout2+ls
          IF(ivar < output(io)%nvar) THEN
            outtxt(lout1+1:lout1+1) = CHAR(9)
            outtxt2(lout2+1:lout2+1) = CHAR(9) !Also the last one necessary
            lout1 = lout1+1                    !for reading in "free format"
            lout2 = lout2+1
          ENDIF
        ENDDO

        nselected = noutreg
        IF(output(io)%narea>0) nselected = output(io)%narea
        IF(.NOT.ALLOCATED(output(io)%fileunit)) ALLOCATE(output(io)%fileunit(nselected))
        DO i = 1,nselected
          ireg = i
          IF(output(io)%narea>0) ireg = output(io)%areaindex(i)
          IF(resultseq.AND.simsequence>0 .OR. runens)THEN
            IF(resultseq.AND.simsequence>0) nn=simsequence
            IF(runens) nn=iens
            CALL create_filename_for_basin(str,outregion(ireg)%outregid,output(io)%useperiodname,outperiodname(output(io)%period),nn,allens)
          ELSE
            CALL create_filename_for_basin(str,outregion(ireg)%outregid,output(io)%useperiodname,outperiodname(output(io)%period))
          ENDIF
          funit = fileunit_get()
          IF(PRESENT(ensstat))THEN
            fid_assim_ens(io,iens,i) = funit  !save for later
          ELSE
            output(io)%fileunit(i) = funit
          ENDIF
          !Open and write headings to file
          OPEN(UNIT=funit,FILE=TRIM(dir)//TRIM(str),status='unknown',form='formatted',ACTION='write')
          WRITE(funit,'(a)') outtxt(1:lout1)
          WRITE(funit,'(a)') outtxt2(1:lout2)
        ENDDO
      ENDIF
    ENDDO

    !Open subbasin files for classoutput
    ispecific = 0
    DO io=1,noutput
      IF(output(io)%fileformat==6)THEN
        ispecific = ispecific + 1
        !Preparation of heading
        IF(writematlab)THEN
          lout1 = 6
          lout2 = 7
          outtxt(1:lout1) = '%DATE'//CHAR(9)
          outtxt2(1:lout2) = '%UNITS'//CHAR(9)
        ELSE
          lout1 = 5
          lout2 = 6
          outtxt(1:lout1) = 'DATE'//CHAR(9)
          outtxt2(1:lout2) = 'UNITS'//CHAR(9)
        ENDIF
        DO ivar = 1,output(io)%nvar
          var = outvarid(output(io)%variable(ivar)%idindex)%shortname
          varunit = outvarid(output(io)%variable(ivar)%idindex)%shortunit
          var2=var
          WRITE(t,'(a16)') var2
          WRITE(s,'(a16)') varunit
          t = ADJUSTL(t)
          s = ADJUSTL(s)
          lt = LEN_TRIM(t)
          ls = LEN_TRIM(s)
          outtxt(lout1+1:lout1+lt) = t(1:lt)
          outtxt2(lout2+1:lout2+ls) = s(1:ls)
          lout1 = lout1+lt
          lout2 = lout2+ls
          IF(ivar < output(io)%nvar) THEN
            outtxt(lout1+1:lout1+1) = CHAR(9)
            outtxt2(lout2+1:lout2+1) = CHAR(9) !Also the last one necessary
            lout1 = lout1+1                    !for reading in "free format"
            lout2 = lout2+1
          ENDIF
        ENDDO
        classes=''
        fmtstr=''
        WRITE(fmtstr,'(a1,I3,a5)') '(',maxclassingroup,'(I3))'
        CALL get_classgroup_classes(output(io)%gcgroupname,nslc,classindex)
        WRITE(classes,fmtstr) classindex(1:nslc)  !Works for slc<100

        nselected = 0
        IF(output(io)%narea>0) nselected = output(io)%narea   !narea=0 means timefile
        IF(.NOT.ALLOCATED(output(io)%fileunit)) ALLOCATE(output(io)%fileunit(nselected))
        DO i = 1,nselected
          isb = output(io)%areaindex(i)
          IF(resultseq.AND.simsequence>0 .OR. runens)THEN
            IF(resultseq.AND.simsequence>0) nn=simsequence
            IF(runens) nn=iens
            IF(output(io)%gcgroupname/='')THEN  !Ex. 0000001_CGA.txt
              CALL create_filename_for_basin(str,basin(isb)%subid,output(io)%useperiodname,outperiodname(output(io)%period),suffix2=nn,suffix2format=allens,suffix4=output(io)%gcgroupname)
            ELSE
              CALL create_filename_for_basin(str,basin(isb)%subid,output(io)%useperiodname,outperiodname(output(io)%period),suffix2=nn,suffix2format=allens,suffix3=ispecific)
            ENDIF
          ELSE
            IF(output(io)%gcgroupname/='')THEN
              CALL create_filename_for_basin(str,basin(isb)%subid,output(io)%useperiodname,outperiodname(output(io)%period),suffix4=output(io)%gcgroupname)
            ELSE
             CALL create_filename_for_basin(str,basin(isb)%subid,output(io)%useperiodname,outperiodname(output(io)%period),suffix3=ispecific)
            ENDIF
          ENDIF
          funit = fileunit_get()
          IF(PRESENT(ensstat))THEN
            fid_assim_ens(io,iens,i) = funit
          ELSE
            output(io)%fileunit(i) = funit
          ENDIF
          !Open and write comment and headings to file
          OPEN(UNIT=funit,FILE=TRIM(dir)//TRIM(str),status='unknown',form='formatted')
          IF(writematlab)THEN
            commentheading = '%This is a file with variables grouped for classes ('//TRIM(classes)//')'
          ELSE
            commentheading = '!!This is a file with variables grouped for classes ('//TRIM(classes)//')'
          ENDIF
          CALL write_commentline(funit,commentheading)
          WRITE(funit,'(a)') outtxt(1:lout1)
          WRITE(funit,'(a)') outtxt2(1:lout2)
        ENDDO
      ENDIF
    ENDDO
    IF(ALLOCATED(classindex)) DEALLOCATE(classindex)

  END SUBROUTINE open_subbasinfiles

  !>Calculate and write subbasin files
  !--------------------------------------------------------------------
  SUBROUTINE write_subbasinfiles(io,idt,ndt,cd)

    USE LIBDATE, ONLY : DateType
    USE WORLDVAR, ONLY : output, &
                         writematlab, &
                         outstartdate
    USE MODVAR, ONLY : nsub
    USE COMPOUT, ONLY : compute_basinoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    TYPE(DateType), INTENT(IN) :: cd    !<timestep date-time

    !Local variables
    INTEGER ia,n   !loop index over selected subbasins and number of selected basins
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: x(:)    !Help variable for continous print out

    IF(output(io)%narea==-1)THEN
      n = nsub
    ELSE
      n = output(io)%narea
    ENDIF
    IF(output(io)%nvar>0)THEN  !Write period mean for selected subbasins
      ALLOCATE(x(output(io)%nvar))
      DO ia = 1,n
        CALL compute_basinoutput(cd,io,ia,output(io)%nvar,x,pwrite,idt,ndt,1)
        IF(pwrite) CALL write_dataline(output(io)%fileunit(ia),output(io)%nvar,x,output(io)%decimal,  &
                        output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,odate=outstartdate)
      ENDDO
    ENDIF
    IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE write_subbasinfiles

  !>Calculate and write subbasin files (special for assimilation)
  !>Writes sequences' files in parallel
  !--------------------------------------------------------------------
  SUBROUTINE write_subbasinfiles_in_parallel(io,idt,ndt,iens,cd)

    USE LIBDATE, ONLY : DateType
    USE WORLDVAR, ONLY : output,      &
                         writematlab,     &
                         outstartdate,    &
                         fid_assim_ens
    USE MODVAR, ONLY : nsub
    USE COMPOUT, ONLY : compute_basinoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    TYPE(DateType), INTENT(IN) :: cd    !<timestep date-time

    !Local variables
    INTEGER ia,n
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: x(:)    !Help variable for continous print out

    IF(output(io)%narea==-1)THEN
      n = nsub
    ELSE
      n = output(io)%narea
    ENDIF
    IF(output(io)%nvar>0)THEN  !Write period mean for selected subbasins
      ALLOCATE(x(output(io)%nvar))
      DO ia = 1,n
        CALL compute_basinoutput(cd,io,ia,output(io)%nvar,x,pwrite,idt,ndt,iens)
        IF(pwrite) CALL write_dataline(fid_assim_ens(io,iens,ia),output(io)%nvar,x,output(io)%decimal,  &
                        output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,odate=outstartdate)
       ENDDO
     ENDIF
     IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE write_subbasinfiles_in_parallel

  !>Calculate and write output region files
  !--------------------------------------------------------------------
  SUBROUTINE write_regionfiles(io,idt,ndt,cd)

    USE LIBDATE, ONLY : DateType
    USE WORLDVAR, ONLY : output, &
                         noutreg, &
                         writematlab, &
                         outstartdate
    USE COMPOUT, ONLY : compute_regionoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time step
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time steps
    TYPE(DateType), INTENT(IN) :: cd    !<Current date-time

    !Local variables
    INTEGER i,ir,nselected
    LOGICAL pwrite              !Flag for period end, time to write to file
    REAL,ALLOCATABLE :: x(:)    !Data to be printed

    ALLOCATE(x(output(io)%nvar))
    nselected = noutreg
    IF(output(io)%narea>=0) nselected = output(io)%narea
    DO i = 1,nselected
      ir = i
      IF(output(io)%narea>0) ir = output(io)%areaindex(i)
      CALL compute_regionoutput(cd,io,ir,output(io)%nvar,idt,ndt,1,pwrite,x)
      IF(pwrite) CALL write_dataline(output(io)%fileunit(i),output(io)%nvar,x,output(io)%decimal,  &
                      output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,odate=outstartdate)
    ENDDO
    IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE write_regionfiles

  !>Calculate and write output region files
  !--------------------------------------------------------------------
  SUBROUTINE write_regionfiles_in_parallel(io,idt,ndt,iens,cd)

    USE LIBDATE, ONLY : DateType
    USE WORLDVAR, ONLY : output, &
                         noutreg,       &
                         writematlab,     &
                         outstartdate,    &
                         fid_assim_ens
    USE COMPOUT, ONLY : compute_regionoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time step
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time steps
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    TYPE(DateType), INTENT(IN) :: cd    !<Current date-time

    !Local variables
    INTEGER i,ir,nselected
    LOGICAL pwrite              !Flag for period end, time to write to file
    REAL,ALLOCATABLE :: x(:)    !Data to be printed

    ALLOCATE(x(output(io)%nvar))
    nselected = noutreg
    IF(output(io)%narea>=0) nselected = output(io)%narea
    DO i = 1,nselected
      ir = i
      IF(output(io)%narea>0) ir = output(io)%areaindex(i)
      CALL compute_regionoutput(cd,io,ir,output(io)%nvar,idt,ndt,iens,pwrite,x)
      IF(pwrite) CALL write_dataline(fid_assim_ens(io,iens,i),output(io)%nvar,x,output(io)%decimal,  &
                      output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,odate=outstartdate)
    ENDDO
    IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE write_regionfiles_in_parallel

  !>Calculate and write subbasin files
  !--------------------------------------------------------------------
  SUBROUTINE write_subbasinfiles_class(io,idt,ndt,cd)

    USE LIBDATE, ONLY : DateType
    USE WORLDVAR, ONLY : output, &
                         writematlab, &
                         outstartdate
    USE COMPOUT, ONLY : compute_basinoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    TYPE(DateType), INTENT(IN) :: cd    !<timestep date-time

    !Local variables
    INTEGER ia,n   !loop index over selected subbasins and number of selected basins
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: x(:)    !Help variable for continous print out

    IF(output(io)%nvar>0)THEN  !Write period mean for selected subbasins
      ALLOCATE(x(output(io)%nvar))
      n = 0
      IF(output(io)%narea>0)THEN
        n = output(io)%narea
      ENDIF
      DO ia = 1,n
        CALL compute_basinoutput(cd,io,ia,output(io)%nvar,x,pwrite,idt,ndt,1)
        IF(pwrite) CALL write_dataline(output(io)%fileunit(ia),output(io)%nvar,x,output(io)%decimal,  &
                        output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,odate=outstartdate)
       ENDDO
     ENDIF
     IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE write_subbasinfiles_class

  !>Calculate and write subbasin files (special for assimilation)
  !>Writes sequences' files in parallel
  !--------------------------------------------------------------------
  SUBROUTINE write_subbasinfiles_class_in_parallel(io,idt,ndt,iens,cd)

    USE LIBDATE, ONLY : DateType
    USE WORLDVAR, ONLY : output, &
                         writematlab, &
                         outstartdate, &
                         fid_assim_ens
    USE COMPOUT, ONLY : compute_basinoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    TYPE(DateType), INTENT(IN) :: cd    !<timestep date-time

    !Local variables
    INTEGER ia,n
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: x(:)    !Help variable for continous print out

    IF(output(io)%nvar>0)THEN  !Write period mean for selected subbasins
      ALLOCATE(x(output(io)%nvar))
      n = 0
      IF(output(io)%narea>0)THEN
        n = output(io)%narea
      ENDIF
      DO ia = 1,n
        CALL compute_basinoutput(cd,io,ia,output(io)%nvar,x,pwrite,idt,ndt,iens)
        IF(pwrite) CALL write_dataline(fid_assim_ens(io,iens,ia),output(io)%nvar,x,output(io)%decimal,  &
                        output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,odate=outstartdate)
       ENDDO
     ENDIF
     IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE write_subbasinfiles_class_in_parallel

  !>Close files for subbasin and output region printout
  !--------------------------------------------------------------------
  SUBROUTINE close_subbasinfiles(n,iens,ensstat)
  
    USE WORLDVAR, ONLY : output, &
                         fid_assim_ens, &
                         fileunit_free, &
                         noutreg,noutput
    
    !Argument declarations
    INTEGER, INTENT(IN) :: n      !<Number of subbasins
    INTEGER, INTENT(IN) :: iens   !<Current simulation
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files

    !Local variables
    INTEGER i,io,funit,nselected

    DO io=1,noutput
      IF(output(io)%fileformat==1)THEN
        nselected = n
        IF(output(io)%narea>=0) nselected = output(io)%narea
        DO i = 1,nselected
          IF(PRESENT(ensstat))THEN
            funit = fid_assim_ens(io,iens,i)
          ELSE
            funit = output(io)%fileunit(i)
          ENDIF
          CLOSE(funit)
          CALL fileunit_free(funit)
        ENDDO
      ELSEIF(output(io)%fileformat==4)THEN
        nselected = noutreg
        IF(output(io)%narea>=0) nselected = output(io)%narea
        DO i = 1,nselected
          IF(PRESENT(ensstat))THEN
            funit = fid_assim_ens(io,iens,i)
          ELSE
            funit = output(io)%fileunit(i)
          ENDIF
          CLOSE(funit)
          CALL fileunit_free(funit)
        ENDDO
      ELSEIF(output(io)%fileformat==6)THEN
        nselected = output(io)%narea
        DO i = 1,nselected
          IF(PRESENT(ensstat))THEN
            funit = fid_assim_ens(io,iens,i)
          ELSE
            funit = output(io)%fileunit(i)
          ENDIF
          CLOSE(funit)
          CALL fileunit_free(funit)
        ENDDO
      ENDIF
    ENDDO

  END SUBROUTINE close_subbasinfiles

  !>Check output files for illegal variables
  !--------------------------------------------------------------------
  SUBROUTINE check_outputfiles_for_illegal_input(status)
  
    USE WORLDVAR, ONLY : output,noutput
    USE MODVAR, ONLY : outvarid
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_output_illegal_input, &
                                    e_error,e_warning,e_info

    !Argument declarations
    INTEGER, INTENT(OUT) :: status      !<status returned from function

    !Local variables
    INTEGER io,ivar,ivar2

    status = 0

    !Check for double variable output
    DO io = 1, noutput
      IF(output(io)%fileformat==2 .OR. output(io)%fileformat==3 .OR. output(io)%fileformat==5)THEN
        DO ivar = 1,output(io)%nvar
          DO ivar2 = ivar+1,output(io)%nvar
            IF(outvarid(output(io)%variable(ivar)%idindex)%shortname==outvarid(output(io)%variable(ivar2)%idindex)%shortname &
                 .AND. output(io)%variable(ivar)%areaagg==output(io)%variable(ivar2)%areaagg)THEN
              IF(output(io)%fileformat==2) WRITE(6,*) 'ERROR: Same variable multiple times in mapoutput. Variable: '//TRIM(outvarid(output(io)%variable(ivar)%idindex)%shortname)
              IF(output(io)%fileformat==3) WRITE(6,*) 'ERROR: Same variable multiple times in timeoutput. Variable: '//TRIM(outvarid(output(io)%variable(ivar)%idindex)%shortname)
              IF(output(io)%fileformat==5) WRITE(6,*) 'ERROR: Same variable multiple times in classoutput. Variable: '//TRIM(outvarid(output(io)%variable(ivar)%idindex)%shortname)
              status = 1
              RETURN
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO

    !Check classoutput basinfile for regional variables (not allowed)
    DO io = 1, noutput
      IF(output(io)%fileformat==6)THEN
        DO ivar = 1,output(io)%nvar
          IF(output(io)%variable(ivar)%areaagg==2)THEN
            WRITE(6,*) 'ERROR: Regional variable cannot be printed as classoutput subbasin. Variable: rg'//TRIM(outvarid(output(io)%variable(ivar)%idindex)%shortname)
            status = 1
            RETURN
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  END SUBROUTINE check_outputfiles_for_illegal_input

  !>Opens files for timeserie printout
  !--------------------------------------------------------------------
  SUBROUTINE open_timefiles(dir,n,iens,runens,allens,ensstat)
  
    USE WORLDVAR, ONLY : outregion,noutreg, &
                         outperiodname,  &
                         writematlab,       &
                         resultseq,         &
                         simsequence,       &
                         i_t,i_h,i_d,i_w,i_m,i_y,i_s,       &
                         fileunit_get, &
                         fid_assim_ens, &
                         output,noutput
    USE MODVAR, ONLY : outvarid, &
                       i_sum, &
                       basin, &
                       nsub
    USE READWRITE_ROUTINES, ONLY : write_integer_header, &
                                   write_commentline

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir !<File directory
    INTEGER, INTENT(IN) :: n            !<Number of data columns (subbasins)
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    LOGICAL, INTENT(IN) :: runens       !<Flag for ensemble run
    LOGICAL, INTENT(IN) :: allens       !<Flag for writing all ensemble results
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files

    !Local variables
    INTEGER io,ivar
    INTEGER nn                  !file suffix2 number
    INTEGER nselected
    INTEGER funit
    CHARACTER(LEN=29) filename
    CHARACTER(LEN=20) var
    CHARACTER(LEN=40) var2
    CHARACTER(LEN=30) unit
    INTEGER lout
    CHARACTER (LEN=200) commentheading
    CHARACTER (LEN=200) outtxt

    !Preparations if several ensemble results are to be written
    IF(PRESENT(ensstat))THEN
      IF(.NOT.ALLOCATED(fid_assim_ens))THEN
        nn = 0
        DO io=1,noutput
          IF(output(io)%fileformat==1)THEN
            nselected = nsub
            IF(output(io)%narea>0) nselected = output(io)%narea
            nn = MAX(nn,nselected)
          ELSEIF(output(io)%fileformat==4)THEN
            nselected = noutreg
            IF(output(io)%narea>0) nselected = output(io)%narea
            nn = MAX(nn,nselected)
          ELSEIF(output(io)%fileformat==6)THEN
            nselected = 0
            IF(output(io)%narea>0) nselected = output(io)%narea
            nn = MAX(nn,nselected)
          ELSEIF(output(io)%fileformat==3)THEN
            nn = MAX(nn,output(io)%nvar)
          ELSEIF(output(io)%fileformat==5)THEN
            nn = MAX(nn,output(io)%nvar)
          ENDIF
        ENDDO
        ALLOCATE(fid_assim_ens(noutput,ensstat+1,nn))
        fid_assim_ens=1
      ENDIF
    ENDIF

    !Open files
    DO io = 1, noutput
      IF(output(io)%fileformat==3)THEN
        IF(.NOT.ALLOCATED(output(io)%fileunit)) ALLOCATE(output(io)%fileunit(output(io)%nvar))
        DO ivar = 1,output(io)%nvar
          !Find variable and file
          IF(resultseq.AND.simsequence>0 .OR. runens)THEN  !Ex. timeCOUT_005.txt
            IF(resultseq.AND.simsequence>0) nn=simsequence
            IF(runens) nn=iens
            IF(output(io)%useperiodname)THEN
              CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix1=outperiodname(output(io)%period),suffix2=nn,suffix2format=allens)
            ELSE
              CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix2=nn,suffix2format=allens)
            ENDIF
          ELSE  !Ex. timeCOUT.txt
            IF(output(io)%useperiodname)THEN
              CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix1=outperiodname(output(io)%period))
            ELSE
              CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg)
            ENDIF
          ENDIF

          var=outvarid(output(io)%variable(ivar)%idindex)%longname
          IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
            var2='regional average of '//var
          ELSEIF(output(io)%variable(ivar)%areaagg==1)THEN    !upstream output
            var2='upstream average of '//var
          ELSE
            var2=var
          ENDIF
          unit = get_unit_string(io,ivar)
          funit = fileunit_get()
          IF(PRESENT(ensstat))THEN
            fid_assim_ens(io,iens,ivar) = funit  !save for later
          ELSE
            output(io)%fileunit(ivar) = funit
          ENDIF

          !Open file
          OPEN(UNIT=funit,FILE=TRIM(dir)//TRIM(filename),STATUS='unknown',FORM='formatted',ACTION='write',ERR=900)

          !Write headings
          IF(writematlab)THEN
            commentheading = '%This is a file with timeseries of '//TRIM(var2)//' in '//TRIM(unit)
            CALL write_commentline(funit,commentheading)
            outtxt(1:6) = '%DATE'//CHAR(9)
            lout = 6
            IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
              CALL write_integer_header(funit,noutreg,outregion(:)%outregid,outtxt(1:lout))
            ELSE
              CALL write_integer_header(funit,n,basin(:)%subid,outtxt(1:lout))
            ENDIF
          ELSE
            commentheading = '!!This is a file with timeseries of '//TRIM(var2)//' in '//TRIM(unit)
            CALL write_commentline(funit,commentheading)
            outtxt(1:5) = 'DATE'//CHAR(9)
            lout = 5
            IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
              CALL write_integer_header(funit,noutreg,outregion(:)%outregid,outtxt(1:lout))
            ELSE
              CALL write_integer_header(funit,n,basin(:)%subid,outtxt(1:lout))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    RETURN
    
900 WRITE(6,*) 'ERROR: Open file '//TRIM(dir)//TRIM(filename)//' for writing.'
    STOP 1 

  END SUBROUTINE open_timefiles

  !>Opens files for timeserie printout of classes
  !--------------------------------------------------------------------
  SUBROUTINE open_timefiles_classes(dir,n,iens,runens,allens,ensstat)
  
    USE WORLDVAR, ONLY : outregion,noutreg, &
                         outperiodname, &
                         writematlab, &
                         resultseq, &
                         simsequence, &
                         maxclassingroup, &
                         i_t,i_h,i_d,i_w,i_m,i_y,i_s, &
                         fid_assim_ens, &
                         fileunit_get, &
                         output,noutput
    USE MODVAR, ONLY : outvarid, &
                       i_sum, &
                       basin, &
                       get_classgroup_classes
    USE READWRITE_ROUTINES, ONLY : write_integer_header, &
                                   write_commentline

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir !<File directory
    INTEGER, INTENT(IN) :: n            !<Number of data columns (subbasins)
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    LOGICAL, INTENT(IN) :: runens       !<Flag for ensemble run
    LOGICAL, INTENT(IN) :: allens       !<Flag for writing all ensemble results
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files
    
    !Local variables
    INTEGER io,ivar
    INTEGER nn                  !file suffix2 number
    INTEGER ispecific,funit
    INTEGER nslc
    INTEGER, ALLOCATABLE :: classindex(:)
    CHARACTER(LEN=29) filename
    CHARACTER(LEN=20) var,fmtstr
    CHARACTER(LEN=40) var2
    CHARACTER(LEN=30) unit
    INTEGER lout
    CHARACTER (LEN=150) classes
    CHARACTER (LEN=200) commentheading
    CHARACTER (LEN=1700000) outtxt

    !Open files
    ispecific = 0
    DO io = 1, noutput
      IF(output(io)%fileformat==5)THEN
        IF(.NOT.ALLOCATED(output(io)%fileunit)) ALLOCATE(output(io)%fileunit(output(io)%nvar))
        ispecific = ispecific + 1
        DO ivar = 1,output(io)%nvar
          !Find variable and file
          IF(resultseq.AND.simsequence>0 .OR. runens)THEN
            IF(resultseq.AND.simsequence>0) nn=simsequence
            IF(runens) nn=iens
            IF(output(io)%gcgroupname/='')THEN  !Ex. timeCOUT_CGA_005.txt
              CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix1=outperiodname(output(io)%period),suffix2=nn,suffix2format=allens,suffix4=output(io)%gcgroupname)
            ELSE
              CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix1=outperiodname(output(io)%period),suffix2=nn,suffix2format=allens,suffix3=ispecific)
            ENDIF
          ELSEIF(output(io)%gcgroupname/='')THEN  !Ex. timeCOUT_CGA.txt
            CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix1=outperiodname(output(io)%period),suffix4=output(io)%gcgroupname)
          ELSE  !Ex. timeCOUT_C001.txt
            CALL create_filename_for_variable(filename,'time',outvarid(output(io)%variable(ivar)%idindex)%shortname,output(io)%variable(ivar)%areaagg,suffix1=outperiodname(output(io)%period),suffix3=ispecific)
          ENDIF

          var=outvarid(output(io)%variable(ivar)%idindex)%longname
          IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
            var2='regional average of '//var
          ELSEIF(output(io)%variable(ivar)%areaagg==1)THEN    !upstream output
            var2='upstream average of '//var
          ELSE
            var2=var
          ENDIF
          classes=''
          fmtstr=''
          WRITE(fmtstr,'(a1,I3,a5)') '(',maxclassingroup,'(I3))'
          CALL get_classgroup_classes(output(io)%gcgroupname,nslc,classindex)
          WRITE(classes,fmtstr) classindex(1:nslc)  !Works for slc<100
          unit = get_unit_string(io,ivar)
          funit = fileunit_get()
          IF(PRESENT(ensstat))THEN
            fid_assim_ens(io,iens,ivar) = funit
          ELSE
            output(io)%fileunit(ivar) = funit
          ENDIF

          !Open file
          OPEN(UNIT=funit,FILE=TRIM(dir)//TRIM(filename),STATUS='unknown',FORM='formatted',ERR=900)

          !Write headings
          IF(writematlab)THEN
            commentheading = '%This is a file with timeseries of '//TRIM(var2)//' in classes ('//TRIM(classes)//') in '//TRIM(unit)
            CALL write_commentline(funit,commentheading)
            outtxt(1:6) = '%DATE'//CHAR(9)
            lout = 6
            IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
              CALL write_integer_header(funit,noutreg,outregion(:)%outregid,outtxt(1:lout))
            ELSE
              CALL write_integer_header(funit,n,basin(:)%subid,outtxt(1:lout))
            ENDIF
          ELSE
            commentheading = '!!This is a file with timeseries of '//TRIM(var2)//' in classes ('//TRIM(classes)//') in '//TRIM(unit)
            CALL write_commentline(funit,commentheading)
            outtxt(1:5) = 'DATE'//CHAR(9)
            lout = 5
            IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
              CALL write_integer_header(funit,noutreg,outregion(:)%outregid,outtxt(1:lout))
            ELSE
              CALL write_integer_header(funit,n,basin(:)%subid,outtxt(1:lout))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    IF(ALLOCATED(classindex)) DEALLOCATE(classindex)
    RETURN
    
900 WRITE(6,*) 'ERROR: Open file '//TRIM(dir)//TRIM(filename)//' for writing.'
    STOP 1 

  END SUBROUTINE open_timefiles_classes

  !>Create file name string for subbasin- or outregion file
  !--------------------------------------------------------------------
  SUBROUTINE create_filename_for_basin(filename,basinid,usesuffix1,suffix1,suffix2,suffix2format,suffix3,suffix4)
  
    !Argument declarations
    CHARACTER(LEN=*),INTENT(OUT) :: filename
    INTEGER, INTENT(IN) :: basinid  !<subid or outregid; to be name of file
    LOGICAL,INTENT(IN) :: usesuffix1      !<status of using suffix1
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: suffix1   !<string file suffix
    INTEGER,OPTIONAL,INTENT(IN) :: suffix2            !<integer file suffix
    LOGICAL,OPTIONAL,INTENT(IN) :: suffix2format      !<use 6 character suffix instead of three
    INTEGER,OPTIONAL,INTENT(IN) :: suffix3            !<integer suffix for classoutput
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: suffix4   !<classgroup name suffix
  
    INTEGER iout,namelen
  
    filename = ''
    WRITE(filename(1:7),600) basinid
    iout=7
    IF(PRESENT(suffix3))THEN
      filename(iout+1:iout+2)='_C'
      WRITE(filename(iout+3:iout+5),601) suffix3
      iout = iout+5
    ENDIF
    IF(usesuffix1)THEN
      filename(iout+1:iout+1) = '_'
      filename(iout+2:iout+3) = TRIM(ADJUSTL(suffix1))
      iout=iout+3
    ENDIF
    IF(PRESENT(suffix2))THEN
      filename(iout+1:iout+1)='_'
      IF(PRESENT(suffix2format))THEN
        IF(suffix2format)THEN
          WRITE(filename(iout+2:iout+7),602) suffix2   !6 character sequence number
          iout = iout+7
        ELSE
          WRITE(filename(iout+2:iout+4),601) suffix2
          iout = iout+4
        ENDIF
      ELSE
        WRITE(filename(iout+2:iout+4),601) suffix2
        iout = iout+4
      ENDIF
    ENDIF
    IF(PRESENT(suffix4))THEN
      filename(iout+1:iout+1)='_'
      namelen = LEN(TRIM(ADJUSTL(suffix4)))
      filename(iout+2:iout+1+namelen) = TRIM(ADJUSTL(suffix4))
      iout = iout+1+namelen
    ENDIF
    filename(iout+1:iout+4)='.txt'
    iout = iout+4

600 FORMAT(I7.7)
601 FORMAT(I3.3)
602 FORMAT(I6.6)
       
  END SUBROUTINE create_filename_for_basin
  
  !>Create file name string for time- or map file
  !--------------------------------------------------------------------
  SUBROUTINE create_filename_for_variable(filename,outputtype,variablename,areaagg,suffix1,suffix2,suffix2format,suffix3,suffix4)
  
  USE CONVERT, ONLY : scalar_upper_case
  
    !Argument declarations
    CHARACTER(LEN=*),INTENT(OUT) :: filename
    CHARACTER(LEN=*),INTENT(IN) :: outputtype  !<time- or map-file
    CHARACTER(LEN=4),INTENT(IN) :: variablename !<code for variable; to be name of file
    INTEGER, INTENT(IN) :: areaagg  !<subbasin, upstream or output region variable
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: suffix1   !<string file suffix
    INTEGER,OPTIONAL,INTENT(IN) :: suffix2            !<integer file suffix
    LOGICAL,OPTIONAL,INTENT(IN) :: suffix2format      !<use 6 character suffix instead of three
    INTEGER,OPTIONAL,INTENT(IN) :: suffix3            !<integer suffix for classoutput
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: suffix4   !<classgroup name suffix
  
    INTEGER iout,namelen
    CHARACTER(LEN=4) :: capitalname
  
    filename = ''
    iout = LEN(outputtype)
    filename(1:iout)=outputtype(1:iout)
    IF(areaagg==2)THEN
      filename(iout+1:iout+2) = 'RG'
      iout=iout+2
    ELSEIF(areaagg==1)THEN
      filename(iout+1:iout+2) = 'UP'
      iout=iout+2
    ENDIF
    capitalname = variablename
    CALL scalar_upper_case(capitalname)
    filename(iout+1:iout+4) = capitalname
    iout = iout+4
    IF(PRESENT(suffix3))THEN
      filename(iout+1:iout+2)='_C'
      WRITE(filename(iout+3:iout+5),603) suffix3
      iout = iout+5
    ENDIF
    IF(PRESENT(suffix1))THEN
      filename(iout+1:iout+1)='_'
      filename(iout+2:iout+3) = TRIM(ADJUSTL(suffix1))
      iout = iout+3
    ENDIF
    IF(PRESENT(suffix2))THEN
      filename(iout+1:iout+1)='_'
      IF(PRESENT(suffix2format))THEN
        IF(suffix2format)THEN
          WRITE(filename(iout+2:iout+7),602) suffix2   !6 character sequence number
          iout = iout+7
        ELSE
          WRITE(filename(iout+2:iout+4),601) suffix2
          iout = iout+4
        ENDIF
      ELSE
        WRITE(filename(iout+2:iout+4),601) suffix2
        iout = iout+4
      ENDIF
    ENDIF
    IF(PRESENT(suffix4))THEN
      filename(iout+1:iout+1)='_'
      namelen = LEN(TRIM(ADJUSTL(suffix4)))
      filename(iout+2:iout+1+namelen) = TRIM(ADJUSTL(suffix4))
      iout = iout+1+namelen
    ENDIF
    filename(iout+1:iout+4)='.txt'
    iout = iout+4

601    FORMAT(I3.3)
602    FORMAT(I6.6)
603    FORMAT(I3.3)

  END SUBROUTINE create_filename_for_variable
  
  !>Calculate time aggregate of time-output and writes to file
  !--------------------------------------------------------------------
  SUBROUTINE write_timefiles_class(io,idt,ndt,cd)
  
    USE WORLDVAR, ONLY : writematlab,       &
                         output, &
                         noutreg, &
                         outstartdate
    USE MODVAR, ONLY : nsub
    USE COMPOUT, ONLY : compute_timeoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline
    USE LIBDATE, ONLY : DateType

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    TYPE(DateType), INTENT(IN) :: cd    !<current date-time
    
    !Local variables
    INTEGER ivar,n
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: y(:)    !Help variable for continous print out

    IF(.NOT.ALLOCATED(y)) ALLOCATE(y(nsub))
    DO ivar = 1, output(io)%nvar
      IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
        n = noutreg
      ELSE
        n = nsub
      ENDIF
      CALL compute_timeoutput(cd,io,ivar,n,y,pwrite,idt,ndt,1)
      IF(pwrite) CALL write_dataline(output(io)%fileunit(ivar),n,y,output(io)%decimal,  &
                      output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,  &
                      odate=outstartdate)
    ENDDO 
    IF(ALLOCATED(y)) DEALLOCATE(y)

  END SUBROUTINE write_timefiles_class

  !>Calculate time aggregate of time-output and writes to file
  !--------------------------------------------------------------------
  SUBROUTINE write_timefiles(io,idt,ndt,cd)
  
    USE WORLDVAR, ONLY : writematlab, &
                         output, &
                         noutreg, &
                         outstartdate
    USE MODVAR, ONLY : nsub
    USE COMPOUT, ONLY : compute_timeoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline
    USE LIBDATE, ONLY : DateType

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    TYPE(DateType), INTENT(IN) :: cd    !<current date-time
    
    !Local variables
    INTEGER ivar,n
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: y(:)    !Help variable for continous print out

    IF(.NOT.ALLOCATED(y)) ALLOCATE(y(nsub))
    DO ivar = 1, output(io)%nvar
      IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
        n = noutreg
      ELSE
        n = nsub
      ENDIF
      CALL compute_timeoutput(cd,io,ivar,n,y,pwrite,idt,ndt,1)
      
      IF(pwrite) CALL write_dataline(output(io)%fileunit(ivar),n,y,output(io)%decimal,  &
                      output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,  &
                      odate=outstartdate)
    ENDDO 
    IF(ALLOCATED(y)) DEALLOCATE(y)

  END SUBROUTINE write_timefiles

  !>Calculate time aggregate of time-output and writes to file (special 
  !>for data assimilation)
  !--------------------------------------------------------------------
  SUBROUTINE write_timefiles_class_in_parallel(io,idt,ndt,iens,cd)
  
    USE WORLDVAR, ONLY : writematlab, &
                         output, &
                         noutreg, &
                         outstartdate, &
                         fid_assim_ens
    USE MODVAR, ONLY : nsub
    USE COMPOUT, ONLY : compute_timeoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline
    USE LIBDATE, ONLY : DateType

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    TYPE(DateType), INTENT(IN) :: cd    !<current date-time
    
    !Local variables
    INTEGER ivar,n
    LOGICAL pwrite              !Flag for periodend, time to write to file
    REAL,ALLOCATABLE :: y(:)    !Help variable for continous print out

    IF(.NOT.ALLOCATED(y)) ALLOCATE(y(nsub))
    DO ivar = 1, output(io)%nvar
      IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
        n = noutreg
      ELSE
        n = nsub
      ENDIF
      CALL compute_timeoutput(cd,io,ivar,n,y,pwrite,idt,ndt,iens)
      IF(pwrite) CALL write_dataline(fid_assim_ens(io,iens,ivar),n,y,output(io)%decimal,  &
                      output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,  &
                      odate=outstartdate)
    ENDDO 
    IF(ALLOCATED(y)) DEALLOCATE(y)

  END SUBROUTINE write_timefiles_class_in_parallel

  !>Calculate time aggregate of time-output and writes to file (special 
  !>for data assimilation)
  !--------------------------------------------------------------------
  SUBROUTINE write_timefiles_in_parallel(io,idt,ndt,iens,cd)
  
    USE WORLDVAR, ONLY : output,        &
                         writematlab,       &
                         noutreg,           &
                         outstartdate,      &
                         fid_assim_ens
    USE MODVAR, ONLY : nsub
    USE COMPOUT, ONLY : compute_timeoutput
    USE READWRITE_ROUTINES, ONLY : write_dataline
    USE LIBDATE, ONLY : DateType

    !Argument declarations
    INTEGER, INTENT(IN) :: io           !<Current output
    INTEGER, INTENT(IN) :: idt          !<Current time
    INTEGER, INTENT(IN) :: ndt          !<Maximum simulation time
    INTEGER, INTENT(IN) :: iens         !<Current simulation
    TYPE(DateType), INTENT(IN) :: cd     !<current date-time
    
    !Local variables
    INTEGER ivar,n
    LOGICAL pwrite              !Flag for period end, time to write to file
    REAL,ALLOCATABLE :: y(:)    !Help variable for continous print out

    IF(.NOT.ALLOCATED(y)) ALLOCATE(y(nsub))
    DO ivar = 1, output(io)%nvar
      IF(output(io)%variable(ivar)%areaagg==2)THEN    !region output
        n = noutreg
      ELSE
        n = nsub
      ENDIF
      CALL compute_timeoutput(cd,io,ivar,n,y,pwrite,idt,ndt,iens)
      IF(pwrite) CALL write_dataline(fid_assim_ens(io,iens,ivar),n,y,output(io)%decimal,  &
                      output(io)%signfig,output(io)%period,CHAR(9),0,writematlab,d=cd,  &
                      odate=outstartdate)
    ENDDO 
    IF(ALLOCATED(y)) DEALLOCATE(y)

  END SUBROUTINE write_timefiles_in_parallel

  !>Close files for timeserie output
  !--------------------------------------------------------------------
  SUBROUTINE close_timefiles(iens,ensstat)
  
    USE WORLDVAR, ONLY : output,  &
                         noutput, &
                         fileunit_free, &
                         fid_assim_ens

    !Argument declarations
    INTEGER, INTENT(IN) :: iens  !<Current simulation
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files
    
    !Local variables
    INTEGER io,ivar
    INTEGER funit

    DO io=1,noutput
      IF(output(io)%fileformat==3)THEN
        DO ivar = 1, output(io)%nvar
          IF(PRESENT(ensstat).AND.iens>1)THEN
            funit = fid_assim_ens(io,iens,ivar)
          ELSE
            funit = output(io)%fileunit(ivar)
          ENDIF
          CLOSE(funit)
          CALL fileunit_free(funit)            
        ENDDO
      ENDIF
    ENDDO

  END SUBROUTINE close_timefiles

  !>Close files for timeserie output for classes
  !--------------------------------------------------------------------
  SUBROUTINE close_timefiles_class(iens,ensstat)
  
    USE WORLDVAR, ONLY : fid_assim_ens, &
                         fileunit_free, &
                         noutput, &
                         output

    !Argument declarations
    INTEGER, INTENT(IN) :: iens  !<Current simulation
    INTEGER, OPTIONAL, INTENT(IN) :: ensstat  !<Flag for writing parallell ensemble files

    !Local variables
    INTEGER io,ivar,funit

    DO io=1,noutput
      IF(output(io)%fileformat==5)THEN
        DO ivar = 1, output(io)%nvar
          IF(PRESENT(ensstat).AND.iens>1)THEN
            funit = fid_assim_ens(io,iens,ivar)
          ELSE
            funit = output(io)%fileunit(ivar)
          ENDIF
          CLOSE(funit)
          CALL fileunit_free(funit)            
          !CLOSE(output(io)%fileunit(ivar))
          !CALL fileunit_free(output(io)%fileunit(ivar))
        ENDDO
      ENDIF
    ENDDO

  END SUBROUTINE close_timefiles_class

  !>Read output test values from file
  !--------------------------------------------------------------------
  SUBROUTINE load_otest(dir,name,status)

    USE WORLDVAR, ONLY : fileunit_temp, &
                         comment_str, &
                         allocate_outvar_test, &
                         outvartest
    USE MODVAR, ONLY : simtimestep, simtimeunit
    USE CONVERT, ONLY : int_to_str,real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_otest,e_info

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN)  :: dir           !<File directory
    CHARACTER(LEN=*), INTENT(IN)  :: name          !<File name
    INTEGER,          INTENT(OUT) :: status        !<Error status of subroutine

    !Local variables
    CHARACTER (LEN=220) filename
    CHARACTER (LEN=10) varstr            !string with variable name
    CHARACTER(LEN=1024) line
    LOGICAL fileexist
    INTEGER nline,nvars                  !lines of interest in file
    INTEGER nvalues                      !number of values read from line
    REAL values(2)                       !values of parameter read from file
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str   !Propagate string

    status = 0
    filename=TRIM(dir)//TRIM(name)
    INQUIRE(FILE = filename, EXIST = fileexist)

    IF(fileexist)THEN
      !>Open file and read heading
      WRITE(6,*) 'File opened: ', TRIM(filename)
      OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read')

      ! 256 bytes for not read parameters should be enough I think.
      IF(.NOT.ALLOCATED(propagate_str)) ALLOCATE(CHARACTER(LEN=256) :: propagate_str)
      propagate_str = 'testing output ('//TRIM(int_to_str(simtimestep))//TRIM(simtimeunit)//'): -'

      nvars = 1
      DO
        READ(fileunit_temp,'(a)',END=10) line
        IF(line(1:2)==comment_str) CYCLE
        CALL read_parameterline(line,SIZE(values),varstr,values,nvalues)    !read one line with parameter values
        IF(varstr=='') CYCLE    !except empty line and cycle
        nvars = nvars + 1
      ENDDO

10    CONTINUE
      CALL allocate_outvar_test(nvars)
      REWIND fileunit_temp

      !>Read parameter values from file
      nline = 1
      DO
        READ(fileunit_temp,'(a)',END=110) line
        IF(line(1:2)==comment_str) CYCLE
        CALL read_parameterline(line,SIZE(values),varstr,values,nvalues)    !read one line with parameter values
        IF(varstr == '' .OR. SIZE(values) /= nvalues)CYCLE    !empty line or wrong number of values, cycle
        outvartest(nline)%shortname = varstr
        outvartest(nline)%minvalue = values(1)
        outvartest(nline)%maxvalue = values(2)
        propagate_str = TRIM(propagate_str)//','//TRIM(varstr)//TRIM(ADJUSTL(real_to_str(values(1))))//'-'//TRIM(ADJUSTL(real_to_str(values(2))))
        nline = nline + 1
      ENDDO

110   CONTINUE
      CALL propagate_external_msg(e_otest,e_info,TRIM(propagate_str))
      CLOSE(fileunit_temp)
      IF(ALLOCATED(propagate_str)) DEALLOCATE(propagate_str)
      WRITE(6,*) 'Model output test loaded (otest.txt)'
    ENDIF

  END SUBROUTINE load_otest

  !>Read parameter values from file par.txt
  !>
  !>\b Consequences The modvar module variables soilpar, landpar, genpar,
  !> basinpar, regpar, lakedatapar and monthpar will be (re)set.
  !--------------------------------------------------------------------
  SUBROUTINE load_parameters(dir,nsbase,indexarray,status,seqflag,iens)

    USE WORLDVAR, ONLY : fileunit_temp, &
                         comment_str, &
                         maxcharpath, &
                         get_seq_filename
    USE MODVAR, ONLY : soilpar,    &   !OUT, parameters depending on soil type
                       landpar,    &   !OUT, parameters depending on land use
                       genpar,     &   !OUT, parameters not dependent
                       basinpar,   &   !OUT, parameters depending on subbbasin
                       regpar,     &   !OUT, parameters depending on parregion
                       monthpar, &   !OUT, parameters depending on month
                       modparid, &   !definition of model parameters
                       regiondivision, &
                       nregions, &
                       max_par, &
                       m_gpar, &
                       m_bpar, &
                       m_spar, &
                       m_lpar, &
                       m_rpar, &
                       m_ldpar, &
                       m_mpar, &
                       nluse,   &
                       nsoil
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_par,e_error,e_warning,e_info

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN)  :: dir   !<File directory
    INTEGER, INTENT(IN)  :: nsbase         !<Number of subbasins basemodel (nsub_basemodel/nsub) 
!    INTEGER, INTENT(IN)  :: ns             !<Number of subbasins submodel  (nsub_basemodel/nsub) FIXED: parfile want nsub_base (but HYPE has no basinpar),lakepar want nsub_base first time but nsub later, for calibration they are thesame
    INTEGER, INTENT(IN)  :: indexarray(nsbase)   !<Index for basemodel
    INTEGER, INTENT(OUT) :: status         !<Error status of subroutine
    LOGICAL, INTENT(IN), OPTIONAL :: seqflag        !<Flag for using sequence number in par-file name (parseq)
    INTEGER, INTENT(IN), OPTIONAL :: iens           !<Index of ensemble simulation, 0 for ordinary par.txt
    
    !Local parameters
    CHARACTER(LEN=7),PARAMETER :: parfile = 'par.txt'   !Default name of parameter file 
    
    !Local variables
    CHARACTER (LEN=maxcharpath+20) filename !parameter file with path
    CHARACTER (LEN=11) fname        !current parameter file name
    CHARACTER (LEN=10) varstr       !string with variable name
    CHARACTER(LEN=18000) line
    INTEGER j
    INTEGER dim
    INTEGER dimcheck                !current parameter supposed size
    INTEGER nline                   !line number in file
    INTEGER nvalues                 !number of values read from line
    INTEGER nbasinpar               !number of basinparameters in model
    INTEGER varindex                !parameter index in array
    REAL,ALLOCATABLE :: values(:)   !values of parameter read from file
    REAL,ALLOCATABLE :: temp_basinpar(:,:)   !basinpar values read from file
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str,propagate_str2   !Propagate string for unknown parameters

    !>\b Algorithm \n
    !Initialisations
    status = 0

    !>Get filename and open par-file
    fname = parfile
    IF(PRESENT(seqflag))THEN
      IF(seqflag) CALL get_seq_filename(fname)
    ENDIF
    IF(PRESENT(iens))THEN
      IF(iens>0) CALL add_number_to_filename(iens,fname)
    ENDIF
    filename=TRIM(dir)//TRIM(fname)
    WRITE(6,*) 'File opened: ', TRIM(filename)
    OPEN(UNIT = fileunit_temp,FILE = filename,STATUS = 'old',ACTION='read',ERR=101)

    !>Allocate and initiate parameter variables
    dim = MAX(1,nsbase,nsoil,nluse,MAXVAL(nregions),12)
    nbasinpar = SIZE(basinpar,1)
    IF(.NOT.ALLOCATED(values)) ALLOCATE(values(dim))
    IF(.NOT.ALLOCATED(temp_basinpar)) ALLOCATE(temp_basinpar(nbasinpar,nsbase))

    ! 256 bytes for not read parameters should be enough I think.
    IF(.NOT.ALLOCATED(propagate_str2)) ALLOCATE(CHARACTER(LEN=100) :: propagate_str2)
    IF(.NOT.ALLOCATED(propagate_str)) ALLOCATE(CHARACTER(LEN=256) :: propagate_str)
    propagate_str = 'parameters not recognized: -'

    !>Read parameter values from file
    nline = 1
    DO 
      READ(fileunit_temp,'(a)',END=100) line
      IF(line(1:2)==comment_str)CYCLE
      CALL read_parameterline(line,dim,varstr,values,nvalues)    !read one line with parameter values
      IF(varstr=='')CYCLE    !except empty line and cycle

      !Find corresponding varindex
      varindex = 0
      DO j = 1,max_par
        IF(varstr==modparid(j)%shortname)THEN
          IF(modparid(j)%deptype/=m_ldpar)THEN 
            varindex = j
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF(varindex==0)THEN
        WRITE(6,*) 'WARNING: unknown parameter name on line. Parameter ',TRIM(varstr),' ignored.'
        propagate_str=propagate_str//','//TRIM(varstr)
      ELSE
        !Find and check variable dimension and set parameter values
        SELECT CASE(modparid(varindex)%deptype)
        CASE(m_gpar)
          dimcheck = 1                   !genpar
        CASE(m_bpar)
          dimcheck = nsbase              !basinpar
        CASE(m_spar)
          dimcheck = nsoil
        CASE(m_lpar)
          dimcheck = nluse
        CASE(m_rpar)
          dimcheck = nregions(regiondivision(modparid(varindex)%parno))
        CASE(m_mpar)
          dimcheck = 12
        CASE DEFAULT
          WRITE(6,*) 'ERROR in code. Unknown parameter dependence type'
          propagate_str2='Unknown parameter dependence type'//','//TRIM(varstr)
          CALL propagate_external_msg(e_par,e_error,propagate_str2)
          status = 1
          RETURN
        END SELECT
        IF(dimcheck /= nvalues)THEN
          IF(dimcheck<nvalues)then
            nvalues = dimcheck
            propagate_str2='too many values in parameter file for '//TRIM(varstr)
            CALL propagate_external_msg(e_par,e_warning,propagate_str2)
            WRITE(6,*) 'WARNING: '//TRIM(propagate_str2)
          ELSE
            propagate_str2='too few values in parameter file for '//TRIM(varstr)
            CALL propagate_external_msg(e_par,e_error,propagate_str2)
            WRITE(6,*) 'ERROR: '//TRIM(propagate_str2)
            status = 1
            RETURN
          ENDIF
        ENDIF
        SELECT CASE(modparid(varindex)%deptype)
        CASE(m_gpar)
          genpar(modparid(varindex)%parno) = values(1)
        CASE(m_bpar)
          temp_basinpar(modparid(varindex)%parno,1:nvalues) = values(1:nvalues)
        CASE(m_spar)
          soilpar(modparid(varindex)%parno,1:nvalues) = values(1:nvalues)
        CASE(m_lpar)
          landpar(modparid(varindex)%parno,1:nvalues) = values(1:nvalues)
        CASE(m_rpar)
          regpar(modparid(varindex)%parno,1:nvalues) = values(1:nvalues)
        CASE(m_mpar)
          monthpar(modparid(varindex)%parno,1:nvalues) = values(1:nvalues)
        END SELECT
      ENDIF

      nline = nline + 1
    ENDDO
100 CONTINUE
    CLOSE(fileunit_temp)
    
    !>Handle submodel for basin parameters
    IF(SIZE(basinpar,1)>0)THEN  !any basinpar defined?
      IF(SIZE(basinpar,2)==SIZE(temp_basinpar,2))THEN
        basinpar = temp_basinpar
      ELSE    !simsubmodel, call in ensemble loop
        basinpar = temp_basinpar(:,indexarray)
      ENDIF
    ENDIF
    
    WRITE(6,*) 'Model parameters loaded'
    IF(ALLOCATED(values)) DEALLOCATE(values)
    IF(ALLOCATED(temp_basinpar)) DEALLOCATE(temp_basinpar)
    CALL propagate_external_msg(e_par,e_info,propagate_str)
    IF(ALLOCATED(propagate_str)) DEALLOCATE(propagate_str)

    RETURN
    
101 CONTINUE
    WRITE(6,*) 'ERROR: Opening file ',TRIM(filename)
    STOP 1
    
  END SUBROUTINE load_parameters
 
  !>Reads file with intructions on calibration and parameters to be calibrated 
  !>and saves them in worldvar-arrays. Also reads file for instructions and 
  !>parameters for parameter ensemble simulation.
  !>
  !>\b Consequences Module worldvar variables optparmin, optparmax, optparprecision,
  !> optparid, parindex, optim, dimpar, numoptimpar will be allocated and set.
  !--------------------------------------------------------------------
  SUBROUTINE load_optpar(dir) 

    USE WORLDVAR, ONLY : doopt,doens, &
                         optparmin, optparmax, & 
                         optparprecision, optparid, &
                         dimpar,maxoptpar, &
                         optim, &
                         count_optim_par, &
                         numoptimpar, &
                         fileunit_temp
    USE MODVAR, ONLY : modparid,max_par, &
                       nsoil,nluse,nsub,nregions

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir    !<File directory

    !Local variables
    INTEGER j, caseFlag, io
    INTEGER nskip                   !number of lines with headings
    INTEGER nline                   !line number in file
    INTEGER nvalues                 !number of values read from line
    INTEGER numpar                  !number of different parameters to be calibrated (parameter names in optpar.txt)
    INTEGER varindex                !index for parameter in varstr
    LOGICAL filefound    !checked file existance
    REAL,ALLOCATABLE :: values(:)   !values of parameter read from file (different soil or landuse)
    REAL,ALLOCATABLE :: par(:,:)    !Tempary storage of parameter value 
    CHARACTER (LEN=210) filename
    CHARACTER (LEN=10) varstr       !string with parameter name
    CHARACTER(LEN=18000) line       !line in file
    CHARACTER(LEN=2) taskchar       !code for optimation task
    CHARACTER(LEN=1) flagChar       !character string for one-character flags (Y/N)

    !Local parameters
    INTEGER, PARAMETER :: ninfolines = 20

    !>\b Algorithm \n
    !>Check for file
    filename=TRIM(dir)//'optpar.txt'
    INQUIRE(FILE = filename, EXIST=filefound)
    IF(.NOT.filefound)THEN
      IF(doens)THEN
        optim%task_runens = .TRUE.
        IF(optim%task_writesim) optim%task_writesim = .FALSE.   !Safety. Need to turn this off to go to ensemble_loop
        RETURN
      ELSE
        WRITE(6,*) 'ERROR: File optpar.txt not found',TRIM(filename)
        STOP 1
      ENDIF
    ENDIF
    
    !>Initiation of variables for optimization parameters
    dimpar = MAX(1,nsub,nsoil,nluse,MAXVAL(nregions))
    IF(.NOT.ALLOCATED(optparmin)) ALLOCATE(optparmin(maxoptpar,dimpar)) !worldvar
    IF(.NOT.ALLOCATED(optparmax)) ALLOCATE(optparmax(maxoptpar,dimpar)) !worldvar
    IF(.NOT.ALLOCATED(optparprecision)) ALLOCATE(optparprecision(maxoptpar,dimpar))   !worldvar
    optparmin = 0.
    optparmax = 0.
    IF(.NOT.ALLOCATED(values)) ALLOCATE(values(dimpar))           !local
    IF(.NOT.ALLOCATED(par)) ALLOCATE(par(maxoptpar,dimpar))       !local

    !>Open and read simulation settings from optpar.txt
    WRITE(6,*) 'File opened: ', TRIM(filename)
    OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read',ERR=198)
    nskip = 1
    DO j = 1,nskip   !Skip heading
      READ(fileunit_temp,*)
    ENDDO
    nline = nskip + 1

    DO j = 1, ninfolines     !Read lines with optimisation information
      READ(fileunit_temp,'(a)',END=100,ERR=199,IOSTAT=io) line

      IF(line(1:4)=='task')THEN
        READ(line(6:16000),*,END=102,ERR=200) taskchar
        SELECT CASE(taskchar)
        CASE('MC')
          optim%task_MC        = .TRUE.
        CASE('BP')
          optim%task_boundps   = .TRUE.
        CASE('SC')                        !Scanning mode for 2-parameter problems, execute an organised sampling of crit(param)  [Fred, 06.10.11]
          optim%task_Scanning  = .TRUE.
        CASE('SM')                        !Stage-wise centering Monte Carlo routine  [Fred, 26.08.10]
          optim%task_stageMC   = .TRUE.
        CASE('BN')                        !Brent optimisation routine, with new line search  [Fred, 27.11.10]
          optim%task_BrentNew  = .TRUE.
        CASE('SD')                        !Steepest descent method  [Fred, 08.10.11]
          optim%task_stpstDesc = .TRUE.
        CASE('Q1')                        !Quasi-Newton DFP gradient-based optimisation routine  [Fred, 14.07.10]
          optim%task_DFP       = .TRUE.
        CASE('Q2')                        !Quasi-Newton BFGS gradient-based optimisation routine  [Fred, 09.12.10]
          optim%task_BFGS      = .TRUE.
        CASE('WA') 
          optim%task_writeall  = .TRUE.   !Write performance result for all ensembles (MC-methods)
        CASE('WS') 
          optim%task_writesim  = .TRUE.   !Write simulation result for all ensembles (MC-methods)
        CASE('DE')
          optim%task_DEMC      = .TRUE.   !Differential-Evolution Markov Chain method [David, 2013.02.11]
        CASE('AS') 
          optim%task_ensall    = .TRUE.   !Parameter ensemble simulation based of allsims.txt
        CASE('BS') 
          optim%task_ensbest   = .TRUE.   !Parameter ensemble simulation based of bestsims.txt
        END SELECT
      ENDIF

102   IF(line(1:6)=='num_mc')THEN
        READ(line(8:16000),*,END=103,ERR=200) optim%nruns_MC
      ENDIF

103   IF(line(1:7)=='num_ens')THEN
        READ(line(9:16000),*,END=104,ERR=200) optim%nruns_best
      ENDIF

104   IF(line(1:8)=='num_bpmc')THEN
        READ(line(10:16000),*,END=105,ERR=200) optim%nruns_MCI
      ENDIF

105   IF(line(1:9)=='num_bpmax')THEN
        READ(line(11:16000),*,END=106,ERR=200) optim%nruns_MCImax
      ENDIF

106   IF(line(1:10)=='num_stages')THEN                        ! Read 'num_stages' (amount of successive zooming MC stages) for zoom MC routine  [Fred, 26.08.10]
        READ(line(11:16000),*,END=107,ERR=200) optim%nruns_stages
      ENDIF

107   IF(line(1:8)=='num_zoom')THEN                           ! Read numerical parameter 'num_zoom' (zooming factor) for zoom MC routine  [Fred, 26.08.10]
        READ(line(11:16000),*,END=108,ERR=200) optim%nruns_zoom       ! Checked later (in stage MC procedure) that it is not larger than 1
      ENDIF

108   IF(line(1:10)=='num_dbgMod')THEN                        ! Flag for debug scenario (0 = HYPE, see optim.f90 for alternatives)
        READ(line(11:16000),*,END=109,ERR=200) optim%cal_debugCase
      ENDIF

109   IF(line(1:7)=='cal_log')THEN                            ! Y/N flag to write or not the file "calibration.log", that contains all details on the calibration routine
        READ(line(11:16000),*,END=110,ERR=200) flagChar
        IF(flagChar == 'N')THEN
          optim%cal_log = .FALSE.
        ELSEIF(flagChar .NE. 'N' .AND. flagChar .NE. 'Y')THEN
          WRITE(6,*) 'ERROR: flag to enable/disable writing calibration.log must be either Y or N'
          STOP 1
        ENDIF
      ENDIF

      !Numerical parameter for interruption of non-MC methods [Fred; checked for consistency with optim-type on 29.09.11]
110   IF(line(1:10)=='num_maxItr')THEN                        ! Max amount of iterations allowed
        READ(line(11:16000),*,END=111,ERR=200) optim%cal_maxIterat
      ENDIF

111   IF(line(1:10)=='num_maxTim')THEN                        ! Max amout of time (hours) allowed to calibration routine
        READ(line(11:16000),*,END=112,ERR=200) optim%cal_maxTime
      ENDIF

112   IF(line(1:10)=='num_criItr')THEN                        ! Amount of last optimisation iterations taken into account for criteria improvement monitoring
        READ(line(11:16000),*,END=113,ERR=200) optim%cal_improvCritIter
      ENDIF

113   IF(line(1:10)=='num_criTol')THEN                        ! Tolerance to consider criteria as optimised (delta/mean of criteria over the last 'num_critIt' iterations)
        READ(line(11:16000),*,END=114,ERR=200) optim%cal_improvCritTol
      ENDIF

114   IF(line(1:10)=='num_parItr')THEN                        ! Amount of last optimisation iterations taken into account for parameter improvement monitoring
        READ(line(11:16000),*,END=115,ERR=200) optim%cal_improvParamIter
      ENDIF

      !Numerical parameters for Quasi-Newton method [Fred]
115   IF(line(1:10)=='QN_nrmTol')THEN                         ! Tolerance for gradient norm to be considered zero
        READ(line(11:16000),*,END=116,ERR=200) optim%QN_flatTol
      ENDIF

116   IF(line(1:10)=='QN_pctDerv')THEN                        ! Offset (percentage of parameter value) for numerical derivative
        READ(line(11:16000),*,END=117,ERR=200) optim%QN_factorDeriv
      ENDIF

117   IF(line(1:10)=='QN_stencil')THEN                        ! Stencil type
        READ(line(11:16000),*,END=118,ERR=200) optim%QN_stencil       ! QN optimisation algorithm checkes that it is either 2, 4, 6 or 8
      ENDIF

118   IF(line(1:10)=='QN_lambMax')THEN                        ! Factor containing lambda, to avoid taking points for gradient outside allowed parameter space
        READ(line(11:16000),*,END=119,ERR=200) optim%QN_lambdaMaxFac
      ENDIF

119   IF(line(1:10)=='QN_lambAcc')THEN                        ! Factor increasing the step length proposed by QN algorithms (case lambda = 1 to be replaced by lambdaAccel), in order to allow for faster iteration progression
        READ(line(11:16000),*,END=120,ERR=200) optim%QN_lambdaAccel
      ENDIF

120   IF(line(1:10)=='BR_diagStp')THEN                        ! Flag to disable diagonal step at the end of each Brent iteration
        READ(line(11:16000),*,END=122,ERR=200) flagChar
        IF(flagChar == 'N')THEN
          optim%Brent_diagonalStep = .FALSE.
        ELSEIF(flagChar .NE. 'N' .AND. flagChar .NE. 'Y')THEN
          WRITE(6,*) 'ERROR: flag to enable/disable diagonal Brent step must be either Y or N'
          STOP 1
        ENDIF
      ENDIF

      !Numerical parameters for line search [Fred]
122   IF(line(1:10)=='lnS_maxItr')THEN                        ! Maximum amount of allowed line search iterations
        READ(line(11:16000),*,END=123,ERR=200) optim%lineSearch_maxIter
      ENDIF

123   IF(line(1:7)=='lnS_tol')THEN                            ! Tolerance for numerical line search
        READ(line(11:16000),*,END=124,ERR=200) optim%lineSearch_tol
      ENDIF

124   IF(line(1:9)=='scan_numx')THEN                          ! Amount of scanning points in the dimension of the 1st parameter
        READ(line(11:16000),*,END=125,ERR=200) optim%scan_xpoints
      ENDIF

125   IF(line(1:9)=='scan_numy')THEN                          ! Amount of scanning points in the dimension of the 2nd parameter
        READ(line(11:16000),*,END=126,ERR=200) optim%scan_ypoints
      ENDIF

      !Parameters for DE-MC [David, 2013-02-11]
126   IF(line(1:9)=='DEMC_ngen')THEN                          ! Number of generations in DE-MC simulation
        READ(line(11:16000),*,END=127,ERR=200) optim%DEMC_ngen
      ENDIF

127   IF(line(1:9)=='DEMC_npop')THEN                          ! Number of populations in DE-MC simulation
        READ(line(11:16000),*,END=128,ERR=200) optim%DEMC_npop
      ENDIF
128   IF(line(1:15)=='DEMC_gammascale')THEN                   ! Scaling of the default gamma factor (2.38/sqrt(2*npar)) in DE-MC
        READ(line(17:16000),*,END=129,ERR=200) optim%DEMC_gammascale
      ENDIF
129   IF(line(1:14)=='DEMC_crossover')THEN                     !Crossover probability in DE-MC simulation
        READ(line(16:16000),*,END=130,ERR=200) optim%DEMC_crossover
      ENDIF
130   IF(line(1:10)=='DEMC_sigma')THEN                         !Standard deviation of sample perturbations in DE-MC simulation
        READ(line(12:16000),*,END=131,ERR=200) optim%DEMC_sigma
      ENDIF
131   IF(line(1:12)=='DEMC_accprob')THEN                         !Standard deviation of sample perturbations in DE-MC simulation
        READ(line(14:16000),*,END=133,ERR=200) optim%DEMC_accprob
      ENDIF

133   CONTINUE
    ENDDO

    nline = nline + ninfolines 

    !>Set tasks for calibration
    IF(doopt)THEN
      IF(optim%task_MC .OR. optim%task_boundps .OR. optim%task_stageMC) optim%task_runens = .TRUE.
      IF(optim%task_DEMC)                       optim%task_runens = .TRUE.
      IF(optim%task_DEMC)                       optim%nruns_best = optim%DEMC_npop+1 !Number of populations corresponds to the number of best, add 1 to store median of the populations as "the best" NO1
      IF(.NOT. optim%task_runens) optim%nruns_best = 1  
      IF(optim%task_writesim .AND. .NOT.(optim%task_MC .OR. optim%task_boundps .OR. optim%task_DEMC))THEN
        WRITE(6,*) 'Warning: It is only allowed to write all simulation results for some of the '
        WRITE(6,*) 'Warning: MonteCarlo methods. Write all simulation results is turned off'
        optim%task_writesim = .FALSE.
      ENDIF
      IF(optim%task_DEMC.AND.optim%DEMC_npop<3)THEN
        WRITE(6,*) 'ERROR: The DEMC optimization routine needs at least 3 populations.'
        WRITE(6,*) 'ERROR: This one has',optim%DEMC_npop,'. Check optpar.txt.'
        STOP 1
      ENDIF
      optim%nruns_simloop = optim%nruns_best
    ENDIF
    
    !>Set tasks for parameter ensemble simulation
    IF(doens)THEN
      optim%task_runens = .TRUE.
      IF(optim%task_writesim) optim%task_writesim = .FALSE.   !Safety. Need to turn this off to go to ensemble_loop
      !optim%nruns_simloop will be set later in main.f90
    ENDIF
    
    !>Continue to read parameters
    numpar = 0
    caseFlag = 1
    DO 
      READ(fileunit_temp,'(a)',END=140,ERR=199,IOSTAT=io) line
      CALL read_parameterline(line,dimpar,varstr,values,nvalues)    !read one line with parameter values
      IF(varstr=='')CYCLE    !end of file
      varindex = 0
      DO j = 1,max_par                !find index of parameter
        IF(varstr==modparid(j)%shortname)THEN
          varindex = j
          EXIT
        ENDIF
      ENDDO
      IF(varindex==0)THEN             !check if parameter index found
        WRITE(6,*) 'ERROR: unknown variable on line: ', nline
        WRITE(6,*) 'ERROR: check file',filename
        STOP 1
      ENDIF
      IF(caseFlag == 1) THEN      !First occurence of parameter: minimum values
        numpar = numpar + 1
        optparmin(numpar,1:nvalues) = values(1:nvalues)
        optparid(numpar)=varindex
        caseFlag = 2
      ELSEIF(caseFlag == 2) THEN  !Second occurence of parameter: maximum values
        optparmax(numpar,1:nvalues) = values(1:nvalues)
        caseFlag = 3
      ELSEIF(caseFlag == 3) THEN  !Third occurence of parameter: calibration decimal precision for parameter
        optparprecision(numpar,1:nvalues) = values(1:nvalues)
        caseFlag = 1
      ENDIF

      nline = nline + 1
    ENDDO

140 CONTINUE
    CLOSE(fileunit_temp)

    !>Sort min and max values
    WHERE(optparmin>optparmax)
      par=optparmax
      optparmax=optparmin
      optparmin=par
    ENDWHERE

    !>Set and check worldvar numoptimpar = amount of model parameters to be optimized
    CALL count_optim_par(maxoptpar,dimpar)
    IF(numoptimpar == 0) THEN
      WRITE(6,*) 'ERROR: only zero-length model parameters intervals found in optpar.txt!'
      STOP 1 
    ENDIF

    IF(doopt.AND.(.NOT.(optim%task_MC .OR. optim%task_boundps .OR. optim%task_Scanning .OR. optim%task_stageMC .OR. optim%task_BrentNew .OR. optim%task_stpstDesc .OR. optim%task_DFP .OR. optim%task_BFGS .OR. optim%task_DEMC))) THEN     ! [Fred 06.10.11] Added task_Scanning to checklist
      WRITE(6,*) 'ERROR: no task for calibration set in optpar.txt'
      STOP 1
    ENDIF
    IF(ALLOCATED(values)) DEALLOCATE(values)
    IF(ALLOCATED(par)) DEALLOCATE(par)
    WRITE(6,*) 'Calibration configuration loaded (optpar.txt)'
    RETURN

100 WRITE(6,*) 'End of file during read, optpar.txt' 
    CLOSE(fileunit_temp)
    STOP 1
198 WRITE(6,*) 'ERROR: open optpar.txt'
    CLOSE(fileunit_temp)
    STOP 1
199 WRITE(6,*) 'ERROR: reading optpar.txt, io=',io
    CLOSE(fileunit_temp)
    STOP 1
200 WRITE(6,*) 'ERROR: reading line of optpar.txt: ',line
    CLOSE(fileunit_temp)
    STOP 1

  END SUBROUTINE load_optpar

  !>Reads file with intructions on parameter ensemble simulation.
  !>
  !--------------------------------------------------------------------
  SUBROUTINE read_parameter_ensemble_task(dir,taskAS,taskBS) 

    USE WORLDVAR, ONLY :fileunit_temp

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir    !<File directory
    LOGICAL, INTENT(OUT) :: taskAS !<task parameter ensemble with allsim.txt
    LOGICAL, INTENT(OUT) :: taskBS !<task parameter ensemble with bestsims.txt

    !Local variables
    INTEGER j, io
    INTEGER nskip                   !number of lines with headings
    LOGICAL filefound   !checked file existance
    CHARACTER (LEN=210) filename
    CHARACTER(LEN=18000) line       !line in file
    CHARACTER(LEN=2) taskchar       !code for optimation task

    !Local parameters
    INTEGER, PARAMETER :: ninfolines = 20

    !>\b Algorithm \n
    !>Default values
    taskAS=.FALSE.
    taskBS=.FALSE.
    
    !>Check for file
    filename=TRIM(dir)//'optpar.txt'
    INQUIRE(FILE = filename, EXIST=filefound)
    IF(.NOT.filefound)THEN
      !WRITE(6,*) 'File optpar.txt not found',TRIM(filename)
      !WRITE(6,*) 'Parameter ensemble simulation with par_NNN.txt.'
      RETURN
    ENDIF
    
    !>Open and read simulation settings from optpar.txt
    WRITE(6,*) 'File opened: ', TRIM(filename)
    OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read')
    nskip = 1
    DO j = 1,nskip   !Skip heading
      READ(fileunit_temp,*)
    ENDDO

    DO j = 1, ninfolines     !Read lines with optimisation information
      READ(fileunit_temp,'(a)',END=100,ERR=199,IOSTAT=io) line

      IF(line(1:4)=='task')THEN
        READ(line(6:16000),*,END=102,ERR=200) taskchar
        SELECT CASE(taskchar)
        CASE('AS') 
          taskAS    = .TRUE.   !Parameter ensemble simulation based of allsims.txt
        CASE('BS') 
          taskBS   = .TRUE.   !Parameter ensemble simulation based of bestsims.txt
        END SELECT
      ENDIF
102   CONTINUE
    ENDDO

    CLOSE(fileunit_temp)
    RETURN

100 WRITE(6,*) 'End of file during read, optpar.txt' 
    CLOSE(fileunit_temp)
    STOP 1
199 WRITE(6,*) 'ERROR: reading optpar.txt, io=',io
    CLOSE(fileunit_temp)
    STOP 1
200 WRITE(6,*) 'ERROR: reading line of optpar.txt: ',line
    CLOSE(fileunit_temp)
    STOP 1

  END SUBROUTINE read_parameter_ensemble_task

  !>Reads parameters to be calibrated to find maximum dimension of river states
  !--------------------------------------------------------------------
  SUBROUTINE calculate_special_optpar_parameters(dir,velindex,dampindex,rivvel,damp) 

    USE WORLDVAR, ONLY : fileunit_temp
    USE MODVAR, ONLY : modparid,nsoil,nluse,nsub,nregions

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir    !<file directory
    INTEGER, INTENT(IN)  :: velindex   !<index of rivvel in modparid
    INTEGER, INTENT(IN)  :: dampindex  !<index of damp in modparid
    REAL, INTENT(INOUT)  :: rivvel   !<lower river velocity boundary
    REAL, INTENT(InOUT)  :: damp     !<lower damp boundary

    !Local variables
    INTEGER j, caseFlag, io
    INTEGER nskip                   !number of lines with headings
    INTEGER nline                   !line number in file
    INTEGER localdimpar             !maximal dimention of parameters
    INTEGER nvalues                 !number of values read from line
    LOGICAL fexist                  !presence of file
    REAL minvalv,maxvalv,minvald,maxvald
    REAL,ALLOCATABLE :: values(:)   !values of parameter read from file (different soil or landuse)
    CHARACTER (LEN=210) filename
    CHARACTER (LEN=10) varstr       !string with parameter name
    CHARACTER(LEN=18000) line       !line in file

    !Local parameters
    INTEGER, PARAMETER :: ninfolines = 20

    !Initiate default value
    !rivvel = 9999.; damp = 9999.   !unreasonable large values (set outside)
    minvald = 0.; maxvald = 0.
    minvalv = 0.; maxvalv = 0.
    
    !Check file presence
    filename=TRIM(dir)//'optpar.txt'
    INQUIRE(FILE=TRIM(filename),EXIST=fexist)
    IF(.NOT.fexist) RETURN
    
    !Initiation of variables for reading parameters
    localdimpar = MAX(1,nsub,nsoil,nluse,MAXVAL(nregions))
    IF(.NOT.ALLOCATED(values)) ALLOCATE(values(localdimpar))           !local

    OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read')
    nskip = 1
    DO j = 1,nskip   !Skip heading
      READ(fileunit_temp,*,ERR=199,IOSTAT=io)
    ENDDO
    nline = nskip + 1
    DO j = 1, ninfolines     !Skip lines with optimisation information
      READ(fileunit_temp,'(a)',END=100,ERR=199,IOSTAT=io) line
    ENDDO
    nline = nline + ninfolines 

    caseFlag = 1
    DO 
      READ(fileunit_temp,'(a)',END=140,ERR=199,IOSTAT=io) line
      CALL read_parameterline(line,localdimpar,varstr,values,nvalues)    !read one line with parameter values
      IF(varstr=='')CYCLE    !end of file should be handled by the read call

      IF(varstr==modparid(velindex)%shortname)THEN
        IF(caseFlag == 1) THEN      !First occurence of parameter: minimum values
          minvalv = values(1)
          caseFlag = 2
        ELSEIF(caseFlag == 2) THEN  !Second occurence of parameter: maximum values
          maxvalv = values(1)
          caseFlag = 3
        ELSEIF(caseFlag == 3) THEN  !Third occurence of parameter: calibration decimal precision for parameter
          caseFlag = 1
        ENDIF
      ENDIF
      IF(varstr==modparid(dampindex)%shortname)THEN
        IF(caseFlag == 1) THEN      !First occurence of parameter: minimum values
          minvald = values(1)
          caseFlag = 2
        ELSEIF(caseFlag == 2) THEN  !Second occurence of parameter: maximum values
          maxvald = values(1)
          caseFlag = 3
        ELSEIF(caseFlag == 3) THEN  !Third occurence of parameter: calibration decimal precision for parameter
          caseFlag = 1
        ENDIF
      ENDIF

      nline = nline + 1
    ENDDO

140 CONTINUE
    CLOSE(fileunit_temp)

    !Set output
    IF(minvalv>0. .OR. maxvalv>0.) rivvel = MIN(minvalv,maxvalv)
    IF(minvald>0. .OR. maxvald>0.) damp = MIN(minvald,maxvald)
    IF(ALLOCATED(values)) DEALLOCATE(values)
    RETURN
    
100 WRITE(6,*) 'End of file during read optpar.txt for river dimension' 
    CLOSE(fileunit_temp)
    STOP 1
199 WRITE(6,*) 'ERROR: reading optpar.txt for river dimension, io=',io
    CLOSE(fileunit_temp)
    STOP 1

  END SUBROUTINE calculate_special_optpar_parameters

  !>Reads parameters of ensemble simulation to find maximum dimension of river states
  !--------------------------------------------------------------------
  SUBROUTINE calculate_special_partxt_parameters(dir,nruns,velindex,dampindex,rivvel,damp) 

    USE WORLDVAR, ONLY : fileunit_temp,comment_str
    USE MODVAR, ONLY : modparid,nsoil,nluse,nsub,nregions

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir    !<file directory
    INTEGER, INTENT(IN)  :: nruns      !<number of ensembles
    INTEGER, INTENT(IN)  :: velindex   !<index of rivvel in modparid
    INTEGER, INTENT(IN)  :: dampindex  !<index of damp in modparid
    REAL, INTENT(INOUT)  :: rivvel   !<lower river velocity boundary
    REAL, INTENT(INOUT)  :: damp     !<lower damp boundary

    !Local variables
    INTEGER io,iens
    INTEGER localdimpar             !maximal dimention of parameters
    INTEGER nvalues                 !number of values read from line
    REAL,ALLOCATABLE :: values(:)   !values of parameter read from file (different soil or landuse)
    CHARACTER (LEN=210) filename
    CHARACTER (LEN=3) iensstr       !string with ensemble number
    CHARACTER (LEN=10) varstr       !string with parameter name
    CHARACTER(LEN=18000) line       !line in file

    !Initiate default value
    rivvel = 9999.; damp = 9999.   !unreasonable large values

    !Initiation of variables for reading parameters
    localdimpar = MAX(1,nsub,nsoil,nluse,MAXVAL(nregions))
    IF(.NOT.ALLOCATED(values)) ALLOCATE(values(localdimpar))           !local
    
    !>Check whole ensemble's par files
    DO iens = 1, nruns
      WRITE(iensstr,'(I3.3)') iens
      filename=TRIM(dir)//'par_'//iensstr//'.txt'
      OPEN(UNIT = fileunit_temp,FILE = filename,ERR=101, STATUS = 'old', ACTION='read')

      !>Read each parameter from file
      DO 
        READ(fileunit_temp,'(a)',END=100,ERR=199,IOSTAT=io) line
        IF(line(1:2)==comment_str)CYCLE
        CALL read_parameterline(line,localdimpar,varstr,values,nvalues)    !read one line with parameter values

        !>Find rivvel and damp values
        IF(varstr==modparid(velindex)%shortname)THEN
          rivvel = MIN(rivvel,values(1))
        ENDIF
        IF(varstr==modparid(dampindex)%shortname)THEN
          damp = MIN(damp,values(1))
        ENDIF
      ENDDO
100   CONTINUE
      CLOSE(fileunit_temp)
    ENDDO
    
    IF(rivvel==9999.) rivvel = 1 !not set in any par-file
    IF(damp==9999.)   damp   = 0 !not set in any par-file
    
    IF(ALLOCATED(values)) DEALLOCATE(values)
    RETURN
    
101 CONTINUE
    WRITE(6,*) 'ERROR: Opening file ',TRIM(filename)
    STOP 1
    
199 WRITE(6,*) 'ERROR: reading ',TRIM(filename),' for river dimension, io=',io
    CLOSE(fileunit_temp)
    STOP 1

  END SUBROUTINE calculate_special_partxt_parameters

  !>Save optimal values of optimized parameters to file respar.txt
  !-----------------------------------------------------------------
  SUBROUTINE save_respar(dir,numpar,n) 

    USE WORLDVAR, ONLY : optparid, &
                         parindex, &
                         fileunit_temp
    USE MODVAR, ONLY   : modparid, & 
                         nluse, &
                         nsoil, &
                         nregions

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir    !<File directory
    INTEGER, INTENT(IN)          :: numpar !<number of parameters that has been calibrated
    INTEGER, INTENT(IN)          :: n      !<number of subbasins
    
    !Local variables 
    INTEGER i,j
    INTEGER dim                     !max number of values for parameter
    INTEGER num                     !number of values for parameter
    INTEGER ndec
    CHARACTER (LEN=210) filename
    CHARACTER (LEN=10)  numparstr   !number of parameter values
    CHARACTER (LEN=50)  form_str    !format for writing parameter
    INTEGER varindex                !array-index of parameter
    LOGICAL written                 !status of parameter
    REAL,ALLOCATABLE :: par(:)      !parameter values

    !>\b Algoritm \n
    !>Allocate and initiate local variables
    dim = MAX(1,n,nsoil,nluse,MAXVAL(nregions))
    IF(.NOT.ALLOCATED(par)) ALLOCATE(par(dim))

    !>Open file for writing parameter and write heading
    filename=TRIM(dir)//'respar.txt'
    OPEN(UNIT = fileunit_temp,FILE = filename,STATUS = 'unknown',ACTION='write')
    WRITE(fileunit_temp,'(A64)') '!!Optimal value of parameters found during automatic calibration'    !heading

    !>For every calibrated parameter:
    ndec = 7    !7 decimals for parameter values
    DO i = 1,numpar
      varindex = optparid(parindex(i,1))
      written = .FALSE.
      DO j = 1, i-1
        IF(varindex==optparid(parindex(j,1))) written = .TRUE.       !Check if parameter already written to file
      ENDDO
      IF(.NOT. written)THEN
        !>\li Get current parameter value
        CALL get_parametervalues(varindex,n,dim,par,num)
        !>\li Write parameter to file
        form_str=''
        WRITE(numparstr,'(I10)') num   !Integer to character
        form_str = '(A10,'//TRIM(ADJUSTL(numparstr))//'(1x,F16.'//CHAR(ndec+48)//'))'
        WRITE(fileunit_temp,form_str) modparid(varindex)%shortname,par(1:num)       !write parameter values to file
      ENDIF
    ENDDO

    CLOSE(fileunit_temp)
    IF(ALLOCATED(par)) DEALLOCATE(par)

  END SUBROUTINE save_respar

  !>Collect parameter values from model variables
  !--------------------------------------------------------------
  SUBROUTINE get_parametervalues(varindex,n,dim,values,num)

    USE MODVAR, ONLY : soilpar,  &
                       landpar,  &
                       genpar,   &
                       regpar,   &
                       basinpar, &
                       monthpar, &
                       modparid, &
                       m_gpar,   &
                       m_bpar,   &
                       m_spar,   &
                       m_lpar,   &
                       m_rpar,   &
                       m_mpar, &
                       regiondivision, &
                       nluse, &
                       nsoil, &
                       nregions

    !Argument declarations
    INTEGER, INTENT(IN)  :: varindex    !<model parameter index
    INTEGER, INTENT(IN)  :: n           !<number of subbasins
    INTEGER, INTENT(IN)  :: dim         !<max number of parameter values (soil types/land uses/subbasins)
    REAL, INTENT(OUT)    :: values(dim) !<parameter values
    INTEGER, INTENT(OUT) :: num         !<number of parameter values (soil types/land uses/subbasins) that are used

    !>\b Algoritm \n
    !>Depending on variable type: Find variable dimension and parameter values
    IF(modparid(varindex)%deptype==m_gpar)THEN
      num = 1                    !genpar
      values(1:num) = genpar(modparid(varindex)%parno)
    ELSEIF(modparid(varindex)%deptype==m_bpar)THEN
      num = n                    !basinpar
      values(1:num) = basinpar(modparid(varindex)%parno,1:num)
    ELSEIF(modparid(varindex)%deptype==m_spar)THEN
      num = nsoil
      values(1:num) = soilpar(modparid(varindex)%parno,1:num)
    ELSEIF(modparid(varindex)%deptype==m_lpar)THEN
      num = nluse
      values(1:num) = landpar(modparid(varindex)%parno,1:num)
    ELSEIF(modparid(varindex)%deptype==m_rpar)THEN
      num = nregions(regiondivision(modparid(varindex)%parno))
      values(1:num) = regpar(modparid(varindex)%parno,1:num)
    ELSEIF(modparid(varindex)%deptype==m_mpar)THEN
      num = 12
      values(1:num) = monthpar(modparid(varindex)%parno,1:num)
    ENDIF

  END SUBROUTINE get_parametervalues

  !>Initiate accumulation variables for printout
  !>
  !>\b Consequences Module worldvar variables accdata_classload, 
  !> accdata_basinload, maptime, tmap, output are set.
  !------------------------------------------------------------------
  SUBROUTINE initiate_output_routines() 

    USE WORLDVAR, ONLY : accdata_classload, &  !OUT
                         accdata_basinload, &  !OUT
                         maptime, &  !OUT
                         tmap, &  !OUT
                         writeload, &
                         output, & !OUT
                         noutput
    USE MODVAR, ONLY : numsubstances                     

    !Local variables
    INTEGER io

    DO io = 1,noutput
      IF(output(io)%nvar>0)THEN
        output(io)%accdata%value = 0.
        output(io)%accdata%help = 0.
        output(io)%accdata%nok = 1 !=.TRUE.
        IF(output(io)%fileformat==2)THEN
          maptime = ''
          tmap = 1
        ENDIF
      ENDIF
    ENDDO
    
    IF(writeload)THEN
      IF(numsubstances>0)THEN
        accdata_classload = 0.
        accdata_basinload = 0.
      ENDIF
    ENDIF

  END SUBROUTINE initiate_output_routines

  !>Initiate output variables to missing value
  !>
  !>\b Consequences Module modvar variables firstoutstep and outvar is set.
  !-------------------------------------------------------------
  SUBROUTINE initiate_outvar(idt)

    USE MODVAR,   ONLY : outvar, & !OUT
                         missing_value, &
                         firstoutstep !OUT
    USE WORLDVAR, ONLY : dtskip      

    INTEGER, INTENT(IN) :: idt  !<current timestep 

    firstoutstep = .FALSE. 
    IF(idt==dtskip+1) firstoutstep = .TRUE.      

    outvar = missing_value

  END SUBROUTINE initiate_outvar

  !>Add information of test of output
  !----------------------------------------------------------------
  SUBROUTINE set_outvar_test_information(n)

    USE MODVAR,  ONLY: outvarid
    USE WORLDVAR, ONLY : outvarinfo,&
                         outvartest

    INTEGER, INTENT(IN) :: n   !<number of output variables 

    INTEGER i,j
    
    DO i = 1,n
      !update with the output testing index if any
      !This wont work, because several outvarinfo can have the same shortname (depending on aggregation)
      outvarinfo(i)%tstindex = 0
      IF(.NOT.ALLOCATED(outvartest)) CYCLE
      DO j = 1,SIZE(outvartest)
        IF(outvartest(j)%shortname == outvarid(outvarinfo(i)%idindex)%shortname) THEN
          outvarinfo(i)%tstindex = j    !test the first found!
          EXIT
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE set_outvar_test_information

  !>Set concentrations of waters with zero volume to missing_value and
  !>calculate regional and upstream average for output.
  !>
  !>\b Consequences Module modvar variable outvar may be set.
  !------------------------------------------------------------------
  SUBROUTINE revise_outvar(idt)

    USE WORLDVAR, ONLY : noutreg, &
                         outvartest, &
                         outvarinfo, &
                         outregion
    USE MODVAR, ONLY : basin, &
                       outvarid, &
                       missing_value, &
                       i_wmean, &
                       nsub, &
                       noutvar, & 
                       outvar, & !OUT
                       outvarclassindex, &
                       xoregobsi,xoregindex
    USE COMPOUT, ONLY : calculate_region_average, &
                        calculate_upstream_average, &
                        calculate_class_average, &
                        calculate_class_weighted_average, &
                        calculate_region_obsfunc
    USE CONVERT, ONLY : int_to_str,&
                        real_to_str
    USE MODEL_TEST_ROUTINES, ONLY : propagate_external_msg,e_output_range_violation, &
                                    e_error,e_warning,e_info

    !Argument declarations
    INTEGER, INTENT(IN)  :: idt  !<current timestep index

    !Local variables
    INTEGER isb,ir,iout,itst,iid
    REAL y
    CHARACTER(LEN=:),ALLOCATABLE :: propagate_str   !Propagate string
    LOGICAL do_propagate

    ! 1024 bytes for output variable with range violations should be enough I think.
    IF(.NOT.ALLOCATED(propagate_str)) ALLOCATE(CHARACTER(LEN=1024) :: propagate_str)
    propagate_str = 'range violations: '

    !>Calculate outvar as class average for non-aggregated class outvars
    DO iout = 1,noutvar
      IF(outvarinfo(iout)%nclasses>0)THEN
        IF(outvarinfo(iout)%areaagg==0)THEN
          IF(outvarinfo(iout)%ovindexwater==0)THEN
            CALL calculate_class_average(outvarclassindex(outvarinfo(iout)%idindex), &
                                         outvarinfo(iout)%nclasses,outvarinfo(iout)%slcclass,outvar(:,iout))
          ELSE
            CALL calculate_class_weighted_average(outvarclassindex(outvarinfo(iout)%idindex), &
                         outvarclassindex(outvarinfo(outvarinfo(iout)%ovindexwater)%idindex), &
                         outvarinfo(iout)%nclasses,outvarinfo(iout)%slcclass,outvar(:,iout))
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !>Calculate new outvar for regional variables
    DO iout = 1,noutvar
      IF(outvarinfo(iout)%areaagg==2)THEN
        DO ir = 1,noutreg
          IF(xoregindex(outvarinfo(iout)%idindex,ir)>0)THEN
            y=xoregobsi(xoregindex(outvarinfo(iout)%idindex,ir))
          ELSE
            CALL calculate_region_average(ir,outvarinfo(iout)%ovindex0,y)
          ENDIF
          outvar(ir,iout) = y    !noutreg assumed less than nsub
        ENDDO
      ENDIF
    ENDDO
    
    !Calculate new outvar using regional observation functions (outregion(ireg)%obsfunc>0)
    DO ir = 1,noutreg
      IF(outregion(ir)%obsfunc>0)THEN
        CALL calculate_region_obsfunc(ir)
      ENDIF
    ENDDO
  
    !>Calculate new outvar for upstream variables
    DO iout = 1,noutvar
      IF(outvarinfo(iout)%areaagg==1)THEN
        CALL calculate_upstream_average(outvarinfo(iout)%ovindex0,outvarid(outvarinfo(iout)%idindex)%basearea,outvar(:,iout))
      ENDIF
    ENDDO

    !>Set computed concentrations of waters with zero volume to missing_value
    DO iout = 1,noutvar
      IF(outvarinfo(iout)%ovindexwater>0)THEN
        IF(outvarid(outvarinfo(iout)%idindex)%vartype==i_wmean)THEN
          DO isb = 1, nsub
            IF(outvar(isb,outvarinfo(iout)%ovindexwater)==0.) outvar(isb,iout) = missing_value
          ENDDO
        ENDIF
      ENDIF
    ENDDO

    !>Do not do any output testing if not allocated
    IF(.NOT.ALLOCATED(outvartest)) RETURN

    DO iout = 1,noutvar
      itst = outvarinfo(iout)%tstindex
      IF(itst > 0) THEN
        IF(ANY(outvar(:,iout) > outvartest(itst)%maxvalue) .OR. &
           ANY(outvar(:,iout) < outvartest(itst)%minvalue)) THEN
          propagate_str='timestep: '//TRIM(ADJUSTL(int_to_str(idt)))
          do_propagate = .FALSE.
          DO iid = 1, SIZE(outvar(:,iout))
            IF ((outvar(iid,iout) /= missing_value) .AND. &
                ((outvar(iid,iout) > outvartest(itst)%maxvalue) .OR. &
                (outvar(iid,iout) < outvartest(itst)%minvalue))) THEN
              propagate_str=TRIM(propagate_str)//', subid:'//TRIM(ADJUSTL(int_to_str(basin(iid)%subid)))//'->value:'//TRIM(ADJUSTL(real_to_str(REAL(outvar(iid,iout)))))
              do_propagate = .TRUE.
            ENDIF
          ENDDO
          IF(do_propagate) THEN
            CALL propagate_external_msg(e_output_range_violation,e_error,TRIM(propagate_str))
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    IF(ALLOCATED(propagate_str)) DEALLOCATE(propagate_str)

  END SUBROUTINE revise_outvar

  !>\brief Get the proper unit for output variables summed over a period.
  !---------------------------------------------------------------------
  FUNCTION get_unit_string(io,ivar) RESULT(unit)

    USE WORLDVAR, ONLY : output, &
                         i_h,i_t,i_d,i_w,i_m,i_y,i_s
    USE MODVAR, ONLY : outvarid, i_sum
    
    !Argument declarations
    INTEGER, INTENT(IN) :: io     !<current output
    INTEGER, INTENT(IN) :: ivar   !<current output variable
    CHARACTER(LEN=30) unit        !< \retval the unit spelled out for current period

    !< Get unit of current variable
    unit=''
    unit=outvarid(output(io)%variable(ivar)%idindex)%longunit
    !>If summed variable; add the current period
    IF(outvarid(output(io)%variable(ivar)%idindex)%vartype==i_sum)THEN
      SELECT CASE(output(io)%period)
      CASE(i_h)
        unit=TRIM(unit)//' per hour'
      CASE(i_t)
        unit=TRIM(unit)//' per timestep'
      CASE(i_d)
        unit=TRIM(unit)//' per day'
      CASE(i_w)
        unit=TRIM(unit)//' per week'
      CASE(i_m)
        unit=TRIM(unit)//' per month'
      CASE(i_y,i_s)
        unit=TRIM(unit)//' per year'
      END SELECT
    ENDIF

  END FUNCTION get_unit_string

  !>Write to file result from the nsim best MonteCarlo simulations
  !--------------------------------------------------------------------
  SUBROUTINE save_ensemble_simulations(dir,numpar,nperf,numcrit,&
       nsim,optcrit,performance,parameters)

    USE WORLDVAR, ONLY : writematlab,      &
         filename_best, &
         fileunit_temp

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir       !<File directory
    INTEGER, INTENT(IN) :: numpar             !<Number of optimised parameters
    INTEGER, INTENT(IN) :: nperf              !<Number of performance measures
    INTEGER, INTENT(IN) :: numcrit            !<Number of optimised variables
    INTEGER, INTENT(IN) :: nsim               !<Number of simulations to be saved
    REAL, INTENT(IN) :: optcrit(nsim)         !<Optimation criterion for simulations
    REAL, INTENT(IN) :: performance(nsim,nperf,numcrit) !<Performance measures for simulations
    REAL, INTENT(IN) :: parameters(nsim,numpar) !<Parameter values for simulations
   
    !Local variables 
    INTEGER i

    !Open files
    OPEN(file=TRIM(dir)//filename_best,unit=fileunit_temp,status='unknown',form='formatted',ACTION='write')
    !Write heading
    CALL write_ensemble_simulations_heading(fileunit_temp,numpar,nperf,numcrit,.FALSE.)
    !Write data
    DO i = 1,nsim
       CALL write_simulation_results_perf_and_par(fileunit_temp,i,numpar,numpar,nperf,numcrit,&
            optcrit(i),performance(i,:,:),parameters(i,:),writematlab)
    ENDDO
    !End routine
    CLOSE(fileunit_temp)

  END SUBROUTINE save_ensemble_simulations

  !>Prepare a file to writes result from all MonteCarlo simulations
  !--------------------------------------------------------------------
  SUBROUTINE prepare_save_all_simulations(dir,filename,funit,numpar,nperf,numcrit)

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir       !<File directory
    CHARACTER(LEN=*), INTENT(IN) :: filename  !<Filename
    INTEGER, INTENT(IN) :: funit           !<Unit to be connected to file
    INTEGER, INTENT(IN) :: numpar          !<Number of optimised parameters
    INTEGER, INTENT(IN) :: nperf           !<Number of performance measures
    INTEGER, INTENT(IN) :: numcrit         !<Number of optimised variables

    !Open file
    OPEN(file=TRIM(dir)//filename,unit=funit,status='unknown',form='formatted',ACTION='write')
    !Write heading
    CALL write_ensemble_simulations_heading(funit,numpar,nperf,numcrit,.TRUE.)

  END SUBROUTINE prepare_save_all_simulations

  !>Write heading to file for MonteCarlo simulations
  !--------------------------------------------------------------------
  SUBROUTINE write_ensemble_simulations_heading(funit,numpar,nperf,numcrit,popflag)

    USE WORLDVAR, ONLY : performance_name,       &
                         writematlab,      &
                         optparid,      &
                         parindex, &
                         optim  
    USE MODVAR, ONLY :   modparid

    !Argument declarations
    INTEGER, INTENT(IN) :: funit    !<File unit
    INTEGER, INTENT(IN) :: numpar   !<Number of optimised parameters
    INTEGER, INTENT(IN) :: nperf    !<Number of performance measures
    INTEGER, INTENT(IN) :: numcrit  !<Number of optimised variables
    LOGICAL, INTENT(IN) :: popflag  !<Flag for population columns included
    
    !Local variables
    INTEGER i,j,index
    INTEGER lt,lout
    CHARACTER (LEN=16)  t
    CHARACTER (LEN=800) outtxt     !heading
    CHARACTER (LEN=1)   sep        !separator

    !Write heading
    sep = ','
    IF(writematlab)THEN
       outtxt(1:4) = '%NO'//sep       !i
       outtxt(5:9) = 'CRIT'//sep     !optcrit
       lout = 9
    ELSE
       outtxt(1:3) = 'NO'//sep       !i
       outtxt(4:8) = 'CRIT'//sep     !optcrit
       lout = 8
    ENDIF
    DO i = 1,numcrit                !performance measures
       DO j = 1,nperf
          t  = performance_name(j)
          t  = ADJUSTL(t)
          lt = LEN_TRIM(t)
          outtxt(lout+1:lout+lt+1) = t(1:lt)//sep
          lout = lout+lt+1
       ENDDO
    ENDDO
    DO i = 1,numpar                 !parameters
       index = optparid(parindex(i,1))
       t = modparid(index)%shortname
       t  = ADJUSTL(t)
       lt = LEN_TRIM(t)
       outtxt(lout+1:lout+lt+1) = t(1:lt)//sep
       lout = lout+lt+1
    ENDDO
    !IF DE-MC simulation, also print out the generation and population indeces
    IF(popflag .AND. optim%task_DEMC)THEN
       outtxt(lout+1:lout+4+1) = 'jpop'//sep
       lout = lout+4+1
       outtxt(lout+1:lout+4+1) = 'igen'//sep
       lout = lout+4+1
       outtxt(lout+1:lout+4+1) = 'iacc'//sep
       lout = lout+4+1
    ENDIF
    WRITE(funit,'(a)') outtxt(1:lout-1)

  END SUBROUTINE write_ensemble_simulations_heading

  !>Write the performance result and used parameter values from the last
  !>simulations to a file.
  !--------------------------------------------------------------------
  SUBROUTINE write_simulation_results_perf_and_par(funit,i,mpar,numpar,nperf,&
       numcrit,optcrit,performance,parameters,mlab,jpop,igen,iacc)

    USE READWRITE_ROUTINES, ONLY : write_dataline

    INTEGER, INTENT(IN) :: funit         !<File unit
    INTEGER, INTENT(IN) :: i             !<Simulation number
    INTEGER, INTENT(IN) :: mpar          !<Dimension of parameters
    INTEGER, INTENT(IN) :: numpar        !<Number of optimised parameters
    INTEGER, INTENT(IN) :: nperf         !<Number of performance measures
    INTEGER, INTENT(IN) :: numcrit       !<Number of optimised variables
    REAL, INTENT(IN) :: optcrit          !<Value of optimation criterion for the simulation
    REAL, INTENT(IN) :: performance(nperf,numcrit)  !<performance criteria for the simulation
    REAL, INTENT(IN) :: parameters(mpar)  !<Parameter values for the simulation
    LOGICAL, INTENT(IN) :: mlab           !<MATLAB format for print out
    INTEGER, OPTIONAL, INTENT(IN) :: jpop !<Population Index in DE-MC simulation
    INTEGER, OPTIONAL, INTENT(IN) :: igen !<Generation Index in DE-MC simulation
    INTEGER, OPTIONAL, INTENT(IN) :: iacc !<Acceptance Index in DE-MC simulation

    !Local variables
    REAL, ALLOCATABLE :: x(:)
    INTEGER n          !Number of columns with data to be written
    INTEGER ndec       !Number of decimals to be written (rounding to ndec decimals)

    !Write data
    ndec=8
    n = 1 + nperf*numcrit + numpar
    IF(PRESENT(iacc)) n = n+3
    IF(.NOT.ALLOCATED(x)) ALLOCATE(x(n))
    IF(PRESENT(iacc))THEN
       x = (/optcrit,performance(:,:),parameters(1:numpar),REAL(jpop),REAL(igen),REAL(iacc)/)
    ELSE
       x = (/optcrit,performance(:,:),parameters(1:numpar)/)
    ENDIF
    CALL write_dataline(funit,n,x,ndec,0,0,',',0,mlab,id=i)
    DEALLOCATE(x)

  END SUBROUTINE write_simulation_results_perf_and_par

  !>Count number of parameter sets used for ensemble simulation 
  !--------------------------------------------------------------------
  SUBROUTINE count_ensemble_simulations(use_bestsims,use_allsim,nens)

    USE WORLDVAR, ONLY : filename_best,filename_MC,&
                         fileunit_get,fileunit_free,&
                         maxcharpath,modeldir

    !Argument declarations
    LOGICAL, INTENT(IN)  :: use_bestsims   !<Task count bestsims.txt
    LOGICAL, INTENT(IN)  :: use_allsim     !<Task count allsim.txt
    INTEGER, INTENT(OUT) :: nens           !<Number of parameter ensembles
   
    !Local variables 
    INTEGER irow    !parameter set number
    INTEGER funit   !available file unit
    LOGICAL status  !file status, exist?
    CHARACTER(LEN=maxcharpath) :: filename

    !>\b Algorithm \n
    !>Find file with parameter set(s)
    IF(use_bestsims)THEN
      filename = TRIM(modeldir)//TRIM(filename_best)
    ELSEIF(use_allsim)THEN
      filename = TRIM(modeldir)//TRIM(filename_MC)
    ELSE
      filename = TRIM(modeldir)//'par_001.txt'
    ENDIF
    INQUIRE(FILE=filename,EXIST=status)
    IF(.NOT.status)THEN
      WRITE(6,*) 'ERROR: No file with parameters for emsemble simulation found'
      WRITE(6,*) 'ERROR: Missing file',TRIM(filename)
      STOP 1
    ENDIF

    !>If file with parameter ensemble sets
    IF(use_bestsims.OR.use_allsim)THEN
      !>\li Open file and skip heading
      funit = fileunit_get()
      OPEN(FILE=TRIM(filename),UNIT=funit,STATUS='old',FORM='formatted',ACTION='read',ERR=199)
      READ(funit,*)
    
      !>\li Read number of lines
      irow = 0
      DO
        READ(funit,*,END=100,ERR=200)
        irow = irow + 1
      ENDDO
100   nens = irow
    
      !>\li End routine
      CLOSE(funit)
      CALL fileunit_free(funit)
      RETURN
      
    !If several files with parameter ensembles
    ELSE
      CALL count_parfile_simulations(nens)
      RETURN
    ENDIF
    
199 WRITE(6,*) 'ERROR: Opening file',TRIM(filename)
    STOP 1
200 WRITE(6,*) 'ERROR: Reading file',TRIM(filename)
    STOP 1

  END SUBROUTINE count_ensemble_simulations

  !>Count number of parameter files ensemble simulation 
  !--------------------------------------------------------------------
  SUBROUTINE count_parfile_simulations(nens)

    USE WORLDVAR, ONLY : maxcharpath,modeldir

    !Argument declarations
    INTEGER, INTENT(OUT) :: nens   !<Number of parameter ensembles
   
    !Local variables 
    INTEGER ipar    !parameter set number
    LOGICAL status  !file status, exist?
    CHARACTER(LEN=maxcharpath) :: filename,filename1

    !>\b Algorithm \n
    !>Find file with parameter set(s)
    filename = TRIM(modeldir)//'par_001.txt'
    INQUIRE(FILE=filename,EXIST=status)
    IF(.NOT.status)THEN
      WRITE(6,*) 'ERROR: No file with parameters for emsemble simulation found'
      WRITE(6,*) 'ERROR: Missing file',TRIM(filename)
      STOP 1
    ENDIF

    !Count the existing files
    ipar = 1
    DO
      filename = ''
      WRITE(filename1,'(A4,I3.3,A4)') 'par_',ipar,'.txt'
      filename = TRIM(modeldir)//TRIM(filename1)
      INQUIRE(FILE=filename,EXIST=status)
      IF(.NOT.status)EXIT
      ipar = ipar + 1
    ENDDO
    nens = ipar - 1
    
  END SUBROUTINE count_parfile_simulations

  !>Load parameters used for ensemble simulation from file
  !--------------------------------------------------------------------
  SUBROUTINE load_ensemble_simulations(dir,fname,numpar,nperf,numcrit,&
       nsim,parameters)

    USE WORLDVAR, ONLY : fileunit_temp

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir       !<File directory
    CHARACTER(LEN=*), INTENT(IN) :: fname     !<File name
    INTEGER, INTENT(IN) :: numpar             !<Number of optimised parameters
    INTEGER, INTENT(IN) :: nperf              !<Number of performance measures
    INTEGER, INTENT(IN) :: numcrit            !<Number of optimised variables
    INTEGER, INTENT(IN) :: nsim               !<Number of simulations to be saved
    REAL, INTENT(INOUT) :: parameters(nsim,numpar) !<Parameter values for simulations
   
    !Local variables 
    INTEGER i
    REAL optcrit(nsim)         !Optimation criterion for simulations
    REAL performance(nsim,nperf,numcrit) !Performance measures for simulations

    !Open file and skip heading
    OPEN(file=TRIM(dir)//TRIM(fname),unit=fileunit_temp,status='old',form='formatted',ACTION='read',ERR=100)
    READ(fileunit_temp,*)
    
    !Read data
    DO i = 1,nsim
      CALL read_simulation_perf_and_par(fileunit_temp,i,numpar,numpar,nperf,numcrit,&
            optcrit(i),performance(i,:,:),parameters(i,:))
    ENDDO
    
    !End routine
    WRITE(6,*) 'File loaded: ',TRIM(dir)//TRIM(fname)
    CLOSE(fileunit_temp)
    RETURN

    !Error handling
100 WRITE(6,*) 'ERROR: Open parameter file: ',TRIM(dir)//TRIM(fname)
    STOP 1

  END SUBROUTINE load_ensemble_simulations

  !>Read the performance result and used parameter values from the one
  !>simulation (one row) from a file.
  !--------------------------------------------------------------------
  SUBROUTINE read_simulation_perf_and_par(funit,i,mpar,numpar,nperf,&
       numcrit,optcrit,performance,parameters)

    INTEGER, INTENT(IN) :: funit         !<File unit
    INTEGER, INTENT(IN) :: i             !<Simulation number
    INTEGER, INTENT(IN) :: mpar          !<Dimension of parameters
    INTEGER, INTENT(IN) :: numpar        !<Number of optimised parameters
    INTEGER, INTENT(IN) :: nperf         !<Number of performance measures
    INTEGER, INTENT(IN) :: numcrit       !<Number of optimised variables
    REAL, INTENT(INOUT) :: optcrit          !<Value of optimation criterion for the simulation
    REAL, INTENT(INOUT) :: performance(nperf,numcrit)  !<performance criteria for the simulation
    REAL, INTENT(OUT)   :: parameters(mpar)  !<Parameter values for the simulation

    !Local variables
    REAL, ALLOCATABLE :: x(:)
    INTEGER j
    INTEGER id
    INTEGER np1,np2    !First and last column with parameter values
    INTEGER n          !Number of columns with data

    !Default output
    parameters = 0.
    optcrit = 0.
    performance = 0.
    
    !Write data
    !ndec=8  !hardcoded in corresponding write routine
    np1 = 1 + nperf*numcrit + 1
    np2 = 1 + nperf*numcrit + numpar
    n = 1 + nperf*numcrit + numpar
    IF(.NOT.ALLOCATED(x)) ALLOCATE(x(n))
    READ(funit,*) id,x
    IF(id == i)THEN
      !ok
    ELSE
      WRITE(6,*) 'ERROR:reading allsim'
      STOP 1
    ENDIF
    optcrit = x(1)
    DO j=1,numcrit
      performance(1:nperf,j) = x(2+nperf*(j-1):nperf*j+1)
    ENDDO
    parameters(1:numpar) = x(np1:np2)
    IF(ALLOCATED(x)) DEALLOCATE(x)

  END SUBROUTINE read_simulation_perf_and_par

  !>Save the loads for the last year to files. Subroutine is called once per year.
  !>
  !>\b Consequences Module worldvar variables accdata_classload and 
  !>accdata_basinload are zeroed.
  !--------------------------------------------------------------------
  SUBROUTINE save_loadfiles(dir,year)

    USE WORLDVAR, ONLY : accdata_classload, &     !OUT
                         accdata_basinload, &     !OUT
                         fileunit_temp
    USE MODVAR, ONLY : basin,               &
                       numsubstances,       &
                       substance_name, &
                       nclass,              &
                       i_t2, &
                       nsub,                &
                       max_classoutvar,     &
                       max_basinoutvar,     &
                       loadheadings
    USE READWRITE_ROUTINES, ONLY : write_dataline

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir   !<File directory
    INTEGER, INTENT(IN) :: year           !<Current year

    !Local variables
    INTEGER i       ! substance number       
    INTEGER j       ! class number
    INTEGER k       ! heading number in 'Headings'
    INTEGER l       ! heading number 'Prepheadings'
    INTEGER s       ! subbasin number
    INTEGER subnr   ! subbasin id
    INTEGER ffunit   ! file unit
    INTEGER nvalues   ! number of values in xload
    REAL xload(max_classoutvar*nclass+max_basinoutvar) !input to write_tab_sep
    CHARACTER (LEN=7) str !file name
    CHARACTER (LEN=11),DIMENSION(max_classoutvar*nclass+max_basinoutvar+1) :: headings ! Array of headings for writing to file

    !Preparation of heading
    headings(:)='0'
    WRITE(headings(1),'(a7,a)') loadheadings(1),CHAR(9)                     !subbasin number
    DO l=2,max_classoutvar+1
      DO j=1,nclass
        k=(l-2)*nclass+j+1 ! position in Headings
        WRITE(headings(k),'(a6,a1,I3.3,a)') loadheadings(l),'_',j,CHAR(9)     !class dependent
      ENDDO
    ENDDO
    DO l=max_classoutvar+2,max_classoutvar+max_basinoutvar+1 
      k=max_classoutvar*nclass+(l-max_classoutvar)
      WRITE(headings(k),'(a6,a)') loadheadings(l),CHAR(9)                   !subbasin dependent
    ENDDO

    !Save data to files
    ffunit=fileunit_temp
    DO i=1,numsubstances
!      IF (i==i_in.OR.i==i_on.OR.i==i_sp.OR.i==i_pp)THEN
      IF(i/=i_t2)THEN
        WRITE(str,'(I4,a,a2)') year,'_',substance_name(i)
        OPEN(UNIT=ffunit,FILE=TRIM(dir)//TRIM(ADJUSTL(str))//'.txt',status='unknown',form='formatted',ACTION='write')
        WRITE(ffunit,'(8014a11)') headings     !Write headings to file
        DO s=1,nsub
          !Write data to a row array
          DO k=1,max_classoutvar
            xload(((k-1)*nclass+1):(k*nclass))=accdata_classload(1:nclass,k,i,s)
          ENDDO
          xload((max_classoutvar*nclass+1):(max_classoutvar*nclass)+max_basinoutvar)=accdata_basinload(i,:,s)
          nvalues=max_classoutvar*nclass+max_basinoutvar
          subnr = basin(s)%subid
          CALL write_dataline(ffunit,nvalues,xload,3,0,0,CHAR(9),0,.FALSE.,id=subnr) 
        ENDDO ! end subarea loop (s)
        CLOSE(ffunit)
      ENDIF
    ENDDO !end substance lop (i)

    !Reset accumulations to zero at end of year 
    accdata_classload(:,:,:,:) = 0.0  
    accdata_basinload(:,:,:)   = 0.0 

  END SUBROUTINE save_loadfiles

END MODULE
