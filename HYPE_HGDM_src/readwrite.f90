!> \file readwrite.f90
!> Contains module readwrite_routines.

!>Module for reading and writing to files
MODULE READWRITE_ROUTINES

  !Copyright 2011-2018,2020 SMHI
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
  !----------------------------------------------------------------------------

  USE COMPOUT, ONLY : find_variable_index_type
  USE CONVERT, ONLY : lower_case, &
                      scalar_lower_case, &
                      string_convert_to_DateType, &
                      int_to_str
  USE LIBDATE, ONLY : DateType, &
                      format_date, &
                      OPERATOR(.EQ.), &
                      TimeLag, &
                      OPERATOR(+), &
                      OPERATOR(-), &
                      OPERATOR(.LE.)

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: check_file_station_id_order, &
            find_reorder_index, &
            check_station, &
            check_xobs, &
            read_headings_pointsource_timeseries, &
            prepare_read_matrix, &
            check_obs_timeperiod, &
            read_matrix_line, &
            read_matrix, &
            write_integer_header, &
            write_dataline, &
            write_commentline, &
            write_sepsep, &
            write_mathsep, &
            count_data_cols, &
            count_data_rows, &
            get_number_of_classgroups, &
            read_geoclass, &
            read_basindata5, &
            read_basindata6, &
            read_column_headings, &
            read_column_headings2, &
            read_next_codestr_on_line, &
            read_next_date_on_line, &
            read_next_column, &
            convert_string_to_real, &
            convert_string_to_integer, &
            add_number_to_filename, &
            read_array_from_file,  &
            read_array_from_file2,  &
            write_array_to_file2,  &
            write_array_to_file,  &
            read_parameterline,  &
            log_progress, &
            print_output_information_to_logfile
       
  CONTAINS

  !>Opens a file with observations and checks if the column ids is the same 
  !>as wanted by the model set up. Returns a index array with reorder information.
  !>Used for forcing data files (e.g. Pobs.txt)
  !>and LeakageData.txt
  !----------------------------------------------------------------------------
  SUBROUTINE check_file_station_id_order(funit,infile,ns,geostn,ptindex,ncols,status) 

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit           !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of file to be read
    INTEGER, INTENT(IN)  :: ns              !<Total number of stations/subbasins
    INTEGER, INTENT(IN)  :: geostn(ns)      !<Subbasin identification number in model (GeoData.txt or ForcKey.txt)
    INTEGER, INTENT(OUT) :: ptindex(ns)     !<Index table for order of stations in file compared to GeoData
    INTEGER, INTENT(OUT) :: ncols           !<Number of columns/timeseries in file
    INTEGER, INTENT(OUT) :: status          !<error number

    !Local variables
    CHARACTER(LEN=100) str
    INTEGER nskip
    INTEGER, ALLOCATABLE :: stn(:)  !Identification numbers in obs-file

    !Start of subroutine
    status = 0
    nskip = 0

    !Read stations in file
    CALL count_data_cols(funit,infile,nskip,ncols,status)
    IF(status.NE.0) RETURN
    ncols = ncols - 1     !Number of timeseries
    IF(.NOT.ALLOCATED(stn)) ALLOCATE(stn(ncols))
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read')
    READ(funit,*) str,stn
    CLOSE(funit)

    !Check if the stations are the same as in geostn and save order
    CALL find_reorder_index(infile,ns,geostn,ncols,stn,.TRUE.,ptindex,status)
    IF(status.NE.0) RETURN
    IF(ALLOCATED(stn)) DEALLOCATE(stn)

  END SUBROUTINE check_file_station_id_order

  !>Calculate the index correspondence of two id-arrays. 
  !>Check if content of arrays is the same if flagged.
  !-----------------------------------------------------------------------------------------
  SUBROUTINE find_reorder_index(file,n,oldstn,n2,newstn,allflag,aindex,status) 

    !Argument declarations
    CHARACTER (LEN=*), INTENT(IN) :: file !<Name of entity whose array (newstn) is checked (for error message)
    INTEGER, INTENT(IN)  :: n             !<Size of original array
    INTEGER, INTENT(IN)  :: oldstn(n)     !<Original array
    INTEGER, INTENT(IN)  :: n2            !<Size of new array
    INTEGER, INTENT(IN)  :: newstn(n2)    !<New array with possible different number of and order of elements
    LOGICAL, INTENT(IN)  :: allflag       !<Status: all stations is to be matched, error if not
    INTEGER, INTENT(OUT) :: aindex(n)     !<Index table for order of newstn compared to oldstn
    INTEGER, INTENT(OUT) :: status        !<Error status
    
    !Local variables
    INTEGER i,j

    !>\b Algoritm \n
    status = 0
    aindex   = 0

    !Check if the newstn are the same as in oldstn and save order
    !>If the size of arrays is the same: for every element
    IF(n==n2)THEN
      DO i = 1, n
        !>\li Check if identical element in other array
        IF(oldstn(i)==newstn(i)) THEN
          aindex(i) = i
        ELSE
        !>\li Else check if element found in other array
          DO j = 1, n
            IF(oldstn(i)==newstn(j)) THEN
              aindex(i) = j
              EXIT
            ENDIF
          ENDDO
          !>\li If not found and that is error write error message and return
          IF(allflag.AND.aindex(i)==0)THEN
            WRITE(6,*) 
            WRITE(6,*)    'ERROR:'
            WRITE(6,600) ' ERROR: the subbasin with obsid ',oldstn(i),' is missing in '//file
            status = 1
            RETURN
          ENDIF
        ENDIF
      ENDDO
    ELSE
    !>Elseif the size of arrays is not the same: for every element
      DO i = 1, n
        !>\li Check if element found in other array
        DO j = 1, n2
          IF(oldstn(i)==newstn(j)) THEN
            aindex(i) = j
            EXIT
          ENDIF
        ENDDO
        !>\li If not found and that is error write error message and return
        IF(allflag.AND.aindex(i)==0)THEN
          WRITE(6,*) 
          WRITE(6,*)    'ERROR:'
          WRITE(6,600) ' ERROR: the subbasin with obsid ',oldstn(i),' is missing in '//file
          status = 1
          RETURN
        ENDIF
      ENDDO
    ENDIF

600 FORMAT(A32,I7,A100)

  END SUBROUTINE find_reorder_index

  !>Opens a file with time series of observations and count and check stations and 
  !>save their order. Used for Qobs- and XobsXOMS-files.
  !-------------------------------------------------------------------------------
  SUBROUTINE check_station(funit,infile,ns,nskip,geostn,nobsstn,oindex,status) 

    USE MODVAR, ONLY : conductwarning

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit           !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of file to be read
    INTEGER, INTENT(IN)  :: ns              !<Number of subbasins
    INTEGER, INTENT(IN)  :: nskip           !<Number of comment row to skip before station row
    INTEGER, INTENT(IN)  :: geostn(ns)      !<subbasin id in GeoData.txt
    INTEGER, INTENT(OUT) :: nobsstn         !<Number of columns in file
    INTEGER, INTENT(OUT) :: oindex(ns)      !<Index to find correct station in matrix
    INTEGER, INTENT(OUT) :: status          !<error number
    
    !Local variables
    CHARACTER(LEN=100) str
    INTEGER i,j,k,io
    INTEGER ncols
    INTEGER,ALLOCATABLE :: stn(:)            !Station number in current file
    LOGICAL found

    !Start of subroutine
    status = 0
    oindex = 0
  
    !Calculate number of columns, handle more than number of subbasins
    CALL count_data_cols(funit,infile,nskip,ncols,status)
    IF(status/=0)RETURN
    nobsstn = ncols - 1
    IF(.NOT.ALLOCATED(stn)) ALLOCATE(stn(nobsstn))

    !Read stations
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read',ERR=200)
    DO i = 1,nskip  !Skip comment rows
      READ(funit,*,ERR=201,IOSTAT=io)
    ENDDO    
    READ(funit,*,ERR=201,IOSTAT=io) str,stn(1:nobsstn)
    CLOSE(funit)

    !Find station corresponding subbasin
    DO j = 1,nobsstn                        !find station order
      IF(stn(j)>0)THEN
      found=.FALSE.
      DO k=1,ns
        IF(stn(j).EQ.geostn(k)) THEN
          oindex(k)=j                       !save the order
          nobsstn = j   !count again to reduce the size if last columns is not in model setup
          found=.TRUE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT.found .AND. conductwarning)THEN
        WRITE(6,*) 
        WRITE(6,*) 'WARNING:'
        WRITE(6,*) 'WARNING: Station number in '//TRIM(infile)//' not in '
        WRITE(6,*) 'WARNING: accordance with station number in GeoData.txt'
        WRITE(6,600) ' WARNING: the ',j,' column, station number ',stn(j)
      ENDIF
      ELSE
        CYCLE
      ENDIF
    ENDDO
    IF(SUM(oindex)==0)THEN
      WRITE(6,*) 
      IF(conductwarning) WRITE(6,*) 'WARNING: No stations found in '//TRIM(infile)//' which are in '
      IF(conductwarning) WRITE(6,*) 'WARNING: accordance with station number in GeoData.txt'
      status = 3
      RETURN
    ENDIF
    IF(nobsstn/=ncols-1)THEN
      IF(conductwarning) WRITE(6,*) 'INFO: Number of columns read in '//TRIM(infile)//' reduced.'
    ENDIF
    IF(ALLOCATED(stn)) DEALLOCATE(stn)

600 FORMAT(A14,I8,A24,I8)
    RETURN
200 WRITE(6,*) 'Error: open file',TRIM(infile)
    status = 1
    RETURN
201 WRITE(6,*) 'Error: reading file',TRIM(infile)
    WRITE(6,*) 'Error: io=',io
    status = 1
    RETURN

  END SUBROUTINE check_station

  !>Opens a file with observations, reads which variables it contains 
  !>and save that information. Used for Xobs.txt and Xoregobs.txt
  !-------------------------------------------------------------------------------
  SUBROUTINE check_xobs(funit,infile,ncols,varinfo,status) 

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit             !<File unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile   !<Name of file to be read
    INTEGER, INTENT(IN)  :: ncols             !<Total number of columns
    INTEGER, INTENT(OUT) :: varinfo(ncols,2)  !<xobs variable numbers and subbasin id
    INTEGER, INTENT(OUT) :: status            !<error number
    
    !Local variables
    CHARACTER(LEN=6) varname(ncols)     !Short name of variable in Xobs.txt
    CHARACTER(LEN=100) str
    INTEGER j,timeagg,areagg,io
    INTEGER varnumber(ncols)            !Outvaridnumber of variable for column in Xobs.txt
    INTEGER daroid(ncols)               !Subbasin identification number for corresponding column in Xobs.txt

    !Start of subroutine
    status = 0
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read',ERR=200)
    READ(funit,*,ERR=201,IOSTAT=io)                       !Skip one comment line
    READ(funit,*,ERR=201,IOSTAT=io) str,varname(1:ncols)  !Read variable shortname
    READ(funit,*,ERR=201,IOSTAT=io) str,daroid(1:ncols)           !Read variable subbasin id/outregion id
    
    !Find outvarid index of read variables
    DO j=1,ncols
      status = find_variable_index_type(varname(j),varnumber(j),timeagg,areagg)
      IF(status/=0)THEN
        WRITE(6,*) 'ERROR: non recognizable variable (',TRIM(varname(j)),') in file ',TRIM(infile)
        RETURN
      ENDIF
    ENDDO
    
    !Set output to variable names and subbasins
    varinfo(:,1) = varnumber(:)
    varinfo(:,2) = daroid(:)

    CLOSE(funit)
    RETURN
200 WRITE(6,*) 'Error: open file',TRIM(infile)
    status = 1
    RETURN
201 WRITE(6,*) 'Error: reading file',TRIM(infile)
    WRITE(6,*) 'Error: io=',io
    status = 1
    RETURN

  END SUBROUTINE check_xobs

  !>Opens a file with pointsource time series, reads which variables it contains 
  !>and save that information. 
  !-------------------------------------------------------------------------------
  SUBROUTINE read_headings_pointsource_timeseries(funit,infile,ncols,varname,varinfo,status) 

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit             !<File unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile   !<Name of file to be read
    INTEGER, INTENT(IN)  :: ncols             !<Total number of columns
    CHARACTER(LEN=10), INTENT(OUT) :: varname(ncols)    !<column variable name
    INTEGER, INTENT(OUT) :: varinfo(ncols)    !<column variable pointsource id
    INTEGER, INTENT(OUT) :: status            !<error number
    
    !Local variables
    CHARACTER(LEN=100) str

    !Start of subroutine
    status = 0
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read',ERR=200)
    READ(funit,*,ERR=201) str,varname(1:ncols)   !Read variable shortname; flow, INconc
    READ(funit,*,ERR=201) str,varinfo(1:ncols)   !Read variable psid
    
    CLOSE(funit)
    RETURN
200 WRITE(6,*) 'Error: open file',TRIM(infile)
    status = 1
    RETURN
201 WRITE(6,*) 'Error: reading file',TRIM(infile)
    status = 1
    RETURN

  END SUBROUTINE read_headings_pointsource_timeseries

  !>Opens a file with observations and reads to starting date
  !-------------------------------------------------------------------------------
  SUBROUTINE prepare_read_matrix(fileunit,infile,nskip,bdate,status) 

    !Argument declarations
    INTEGER, INTENT(IN)  :: fileunit              !<Fileunit for infile
    CHARACTER (LEN=*), INTENT(IN) :: infile       !<Name of file to be read
    INTEGER, INTENT(IN)  :: nskip                 !<Number of rows to skip in each file
    TYPE(DateType), INTENT(IN)  :: bdate          !<Begin date
    INTEGER, INTENT(OUT) :: status              !<error number
    
    !Local variables
    LOGICAL nostrfound,notimefound
    INTEGER j,pos,io
    TYPE(DateType) d              
    CHARACTER (LEN=2), PARAMETER :: errstr = '99'
    CHARACTER(LEN=16)  d2         !Date string yyyy-mm-dd[ hh:mm]
    CHARACTER(LEN=20)  line       !Beginning of line in file

    !Start of subroutine
    status = 0
    OPEN(UNIT = fileunit,FILE = infile, STATUS = 'old',ACTION='read',ERR=610)

    DO j = 1,nskip   !Skip lines
      READ(fileunit,*,ERR=611,IOSTAT=io)
    ENDDO

    !Skip to simulation starting date
    DO 
      READ(fileunit,600,ERR=611,END=612,IOSTAT=io) line
      pos = 1
      CALL read_next_date_on_line(20,16,pos,line,d2,nostrfound,notimefound,errstr)
      CALL string_convert_to_DateType(d2,d)
      IF(d.EQ.bdate) EXIT               
    ENDDO
    BACKSPACE(fileunit)

600 FORMAT(A20)
    RETURN
610 WRITE(6,*) 'Error: open file',TRIM(infile)
    status = 1
    RETURN
611 WRITE(6,*) 'Error: reading file',TRIM(infile)
    WRITE(6,*) 'Error: io=',io
    status = 1
    RETURN
612 WRITE(6,*) 'Error: end of file reading',TRIM(infile)
    WRITE(6,*) 'Error: io=',io
    status = 1
    RETURN

  END SUBROUTINE prepare_read_matrix

  !>Opens a file with observations and reads the first and last datetime
  !>Can handle timesteply, monthly and yearly files.
  !-------------------------------------------------------------------------------
  SUBROUTINE check_obs_timeperiod(fileunit,infile,nskip,fileperiod,bdate,edate,&
                filestartdate,fileenddate,notimefound,status) 

  USE MODVAR, ONLY : conductwarning
  USE WORLDVAR, ONLY : steplen, &
                       i_t,i_m,i_y
  USE TIMEROUTINES, ONLY : day_of_month, &
                           numdays_of_year
  
    !Argument declarations
    INTEGER, INTENT(IN)  :: fileunit              !<Fileunit
    CHARACTER (LEN=*), INTENT(IN) :: infile       !<Name of file
    INTEGER, INTENT(IN)  :: nskip                 !<Number of rows to skip in file
    INTEGER, INTENT(IN)  :: fileperiod            !<Timestep of file (e.g timesteply or monthly)
    TYPE(DateType), INTENT(IN)  :: bdate          !<Begin date of simulation
    TYPE(DateType), INTENT(IN)  :: edate          !<End date of simulation 
    TYPE(DateType), INTENT(OUT) :: filestartdate  !<Start date in file 
    TYPE(DateType), INTENT(OUT) :: fileenddate    !<End date in file 
    LOGICAL, INTENT(OUT) :: notimefound           !<Status of datetime format
    INTEGER, INTENT(OUT) :: status                !<error number
    
    !Local variables
    INTEGER j,pos,io
    CHARACTER(LEN=16)  d2,line,line2,line3        !Date string yyyy-mm-dd[ hh:mm]
    CHARACTER(LEN=5), PARAMETER :: errstr='error'
    LOGICAL nostrfound
    TYPE(DateType) :: seconddate,diffDT,oneday
    REAL diff

    !Start of subroutine
    status = 0
    OPEN(UNIT = fileunit,FILE = infile, STATUS = 'old',ACTION='read',ERR=902)
    DO j = 1,nskip   !Skip lines
      READ(fileunit,*,ERR=903,IOSTAT=io)
    ENDDO

    !Read first date and second date in file
    READ(fileunit,'(A16)',ERR=903,IOSTAT=io) line
    pos = 1
    CALL read_next_date_on_line(16,16,pos,line,d2,nostrfound,notimefound,errstr) 
    IF(nostrfound)THEN 
      WRITE(6,*) 'ERROR: reading file '//TRIM(infile)//' for checking date. No date found in beginning of first data line.'
      status = 1
      RETURN
    ENDIF
    CALL string_convert_to_DateType(d2,filestartdate) 
    READ(fileunit,'(A16)',END=910,ERR=903,IOSTAT=io) line2
    pos = 1
    CALL read_next_date_on_line(16,16,pos,line2,d2,nostrfound,notimefound,errstr) 
    IF(nostrfound)THEN 
      WRITE(6,*) 'ERROR: reading file '//TRIM(infile)//' for checking date. No date found in beginning of second data line.'
      status = 1
      RETURN
    ENDIF
    CALL string_convert_to_DateType(d2,seconddate) 

    IF(fileperiod==i_t)THEN
      !Check time step in file against simulation step length
      diff = TimeLag(seconddate,filestartdate)
      diffDT = DateType(0,0,INT(diff),NINT((diff-INT(diff))*24.),0)
      IF(diff>0 .AND. (.NOT.(steplen.EQ.diffDT)))THEN
        WRITE(6,*)
        WRITE(6,*) 'ERROR: Time step of file '//TRIM(infile)//&
               ' not in accordance with simulation time step.'
        WRITE(6,*) 'File timestep:',diff,diffDT
        WRITE(6,*) 'Info given timestep:',steplen
        CLOSE(fileunit)
        status = 1
        RETURN
      ENDIF
    ENDIF
    
    !Read last date in file
    line3 = ''
    DO 
      READ(fileunit,'(A16)',ERR=901,END=900,IOSTAT=io) line3
    ENDDO
900 pos = 1
    IF(line3=='') line3 = line2   !Only two dates
    CALL read_next_date_on_line(16,16,pos,line3,d2,nostrfound,notimefound,errstr) 
    IF(nostrfound)THEN
      WRITE(6,*) 'ERROR: reading file '//TRIM(infile)//' for checking date. No date found in beginning of last line.'
      status = 1
      RETURN
    ENDIF
910 CALL string_convert_to_DateType(d2,fileenddate)

    !Change the fileenddate.
    !For time series of longer time period (than time step), e.g. monthly 
    !time series, the data cover the period to the end of the last 
    !period, while the date in the file is the first of the period.
    IF(fileperiod==i_m)THEN
      !Add one month
      oneday = DateType(0,0,1,0,0)
      DO j=1,day_of_month(fileenddate%Year,fileenddate%Month) - 1
        fileenddate = fileenddate + oneday
      ENDDO
    ELSEIF(fileperiod==i_y)THEN
      !Add one year
      oneday = DateType(0,0,1,0,0)
      DO j=1,numdays_of_year(fileenddate) - 1
        fileenddate = fileenddate + oneday
      ENDDO
    ENDIF

    !Check file time period against simulation time period
    IF(filestartdate.LE.bdate .AND. edate.LE.fileenddate)THEN
      !ok
    ELSE
      !not ok
      status = 2
      IF(conductwarning)THEN
        WRITE(6,*)
        WRITE(6,*) 'WARNING:'
        WRITE(6,*) 'WARNING: Time period of file '//TRIM(infile)//&
             ' not in accordance with simulation time period.'
        WRITE(6,*) 'WARNING:       File time period: ',filestartdate,'-',fileenddate
        WRITE(6,*) 'WARNING: Simulation time period: ',bdate,'-',edate
        WRITE(6,*)
      ENDIF
    ENDIF

    CLOSE(fileunit)
    RETURN

901 WRITE(6,*) 'ERROR: reading file '//TRIM(infile)//' for ending date'
    WRITE(6,*) 'ERROR: last read line: ',d2
    WRITE(6,*) 'ERROR: io=',io
    status = 1
    CLOSE(fileunit)
    RETURN
902 WRITE(6,*) 'ERROR: open file '//TRIM(infile)
    status = 1
    RETURN
903 WRITE(6,*) 'ERROR: reading file '//TRIM(infile)
    WRITE(6,*) 'ERROR: io=',io
    status = 1
    CLOSE(fileunit)
    RETURN

  END SUBROUTINE check_obs_timeperiod

  !>Reads a line of a matrix of values
  !-------------------------------------------------------------------------------
  SUBROUTINE read_matrix_line(fileunit,ncols,d,y,miss,intformat) 
    
  USE WORLDVAR, ONLY : timeformat

    !Argument declarations
    INTEGER, INTENT(IN)  :: fileunit  !<Fileunit for file being read
    INTEGER, INTENT(IN)  :: ncols     !<Total number of columns
    TYPE(DateType), INTENT(OUT) :: d  !<Date (usually days)
    REAL, INTENT(OUT)    :: y(ncols)  !<Matrix with data for current day
    REAL, INTENT(IN)     :: miss      !<missing value
    LOGICAL, INTENT(IN)  :: intformat !<Date given as integer
    
    !Local variables
    CHARACTER(LEN=16)  d2,d3       !Date yyyy-mm-dd[ hh:mm]

    y = miss
    IF(intformat)THEN
      READ(fileunit,*) d2,y
      CALL string_convert_to_DateType(d2,d)
    ELSEIF(timeformat==0)THEN
      READ(fileunit,*) d2,y
      CALL string_convert_to_DateType(d2,d)
    ELSEIF(timeformat==1)THEN
      READ(fileunit,*) d2,d3,y    
      d2(12:16) = d3(1:5)
      CALL string_convert_to_DateType(d2,d)
    ENDIF

  END SUBROUTINE read_matrix_line

  !>Reads a matrix of values in file
  !-------------------------------------------------------------------------------
  SUBROUTINE read_matrix(funit,dimrows,ncols,edate,nrows,tx,xall,miss,intformat) 
    
  USE WORLDVAR, ONLY : timeformat

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit   !<Unit of file to be read
    INTEGER, INTENT(IN)  :: dimrows !<Maximum number of rows
    INTEGER, INTENT(IN)  :: ncols   !<Total number of columns
    TYPE(DateType), INTENT(IN)  :: edate  !<End date of simulation
    INTEGER, INTENT(OUT) :: nrows         !<Number of rows (number of data read in chosen time period)
    TYPE(DateType), INTENT(OUT) :: tx(dimrows)  !<Dates 
    REAL, INTENT(OUT)    :: xall(dimrows,ncols) !<Matrix with all data in chosen time period
    REAL, INTENT(IN)     :: miss                !<Missing value
    LOGICAL, INTENT(IN)  :: intformat           !<Date given as integer
    
    !Local variables
    TYPE(DateType) d              !Date
    INTEGER irow
    CHARACTER(LEN=16) d2,d3       !Date yyyy-mm-dd[ hh:mm]
    REAL y(ncols)                 !Data on last read row

    irow = 0                  !Read rows until ending date
    xall = 0.
    DO
      y = miss
      IF(intformat)THEN
        READ(funit,*) d2,y
        CALL string_convert_to_DateType(d2,d)
      ELSEIF(timeformat==0)THEN
        READ(funit,*) d2,y
        CALL string_convert_to_DateType(d2,d)
      ELSEIF(timeformat==1)THEN
        READ(funit,*) d2,d3,y    
        d2(12:16) = d3(1:5)
        CALL string_convert_to_DateType(d2,d)
      ENDIF
      IF(d.LE.edate) THEN
        irow = irow+1         !row
        tx(irow) = d          !date
        xall(irow,:) = y(:)   !data
      ENDIF
      IF(d.EQ.edate) EXIT
    ENDDO
    nrows = irow

  END SUBROUTINE read_matrix

  !>\brief Print header with integer ids, separated by TAB (=CHAR(9))
  !----------------------------------------------------------------------
  SUBROUTINE write_integer_header(funit,n,columnid,firstcolumn)

    USE WORLDVAR, ONLY : maxlinelength
    
    !Argument declarations
    INTEGER, INTENT(IN) :: funit       !<current file unit
    INTEGER, INTENT(IN) :: n           !<number of columns (after first)
    INTEGER, INTENT(IN) :: columnid(n) !<id of columns
    CHARACTER(LEN=*),INTENT(IN) :: firstcolumn    !<first column header
    
    !Local variables
    INTEGER j,lt,lout
    CHARACTER (LEN=16) t
    CHARACTER (LEN=maxlinelength) outtxt

    lout=LEN(firstcolumn)
    outtxt(1:lout)=firstcolumn
    DO j = 1,n
      WRITE(t,'(i16)') columnid(j)
      t = ADJUSTL(t)
      lt = LEN_TRIM(t)
      outtxt(lout+1:lout+lt) = t(1:lt)
      lout = lout+lt
      IF(j < n) THEN
        outtxt(lout+1:lout+1) = CHAR(9)    !Also the last one necessary
        lout = lout+1                      !for reading in "free format"
      ENDIF
    ENDDO
    WRITE(funit,'(a)') outtxt(1:lout)     !Write heading to file

  END SUBROUTINE write_integer_header
  
  !>\brief Print one line, separated by TAB (=CHAR(9)) or selected
  !!separator and with ndec decimals or nsig signficiant figures
  !----------------------------------------------------------------------
  SUBROUTINE write_dataline(iout,n,x,ndec,nsig,per,sep,abb,mlab,id,d,odate)

    USE WORLDVAR, ONLY : i_t,i_d,i_w,i_m,i_y,i_s, &
                         timeformat
    
    !Argument declarations
    INTEGER, INTENT(IN) :: iout !<Unit for print-out (e.g. 6)
    INTEGER, INTENT(IN) :: n    !<Number of values in vector x
    REAL, INTENT(IN)    :: x(n) !<Values
    INTEGER, INTENT(IN) :: ndec !<Number of decimals to be written (rounding to ndec decimals will occurr)
    INTEGER, INTENT(IN) :: nsig !<Number of significant figures to be written (alternative to ndec)
    INTEGER, INTENT(IN) :: per  !<Time period for date print out or integer print out
    CHARACTER(LEN=1), INTENT(IN) :: sep !<Separator in file, e.g ',' or char(9) [=TAB]
    INTEGER, INTENT(IN) :: abb          !<Switch for abbreviated format (0=no,e.g 0.1 stays 0.1; 1=yes, e.g 0.1 becomes .1)
    LOGICAL, INTENT(IN) :: mlab         !<Switch for MATLAB format (FALSE ,e.g dates with -; TRUE e.g % before title row)
    INTEGER, OPTIONAL, INTENT(IN) :: id !<ID to be used as column 1 (e.g. subid or ensemble number)
    TYPE(DateType), OPTIONAL, INTENT(IN) :: d     !<Date to be used as column 1
    TYPE(DateType), OPTIONAL, INTENT(IN) :: odate !<Start date for output period
    
    !Local variables
    CHARACTER (LEN=16)    t
    TYPE(DateType) :: aweek

    IF(PRESENT(id))THEN !First column consists of some sort of ID
      WRITE(t,'(I10)') id
    ELSEIF(PRESENT(d))THEN !First column consists of dates
      SELECT CASE(per)
      !CASE(i_h)  !Not included yet
      CASE(i_s)       !Write period
        IF(mlab)THEN
          WRITE(t,'(i4,i4)') odate%Year,d%Year           
        ELSE
          WRITE(t,'(i4,a,i4)') odate%Year,'-',d%Year
        ENDIF
      CASE(i_t,i_d) 
        IF(mlab)THEN
          IF(timeformat==0)THEN
            CALL format_date(d,'yyyymmdd',t)
          ELSE
            CALL format_date(d,'yyyymmddHHMM',t)
          ENDIF
        ELSE
          IF(timeformat==0)THEN
            CALL format_date(d,'yyyy-mm-dd',t)
          ELSE
            CALL format_date(d,'yyyy-mm-dd HH:MM',t)
          ENDIF
        ENDIF
      CASE(i_w) !Weekly output
        aweek = DateType(0,0,6,0,0)
        IF(mlab)THEN
          CALL format_date(d-aweek,'yyyymmdd',t)
        ELSE
          CALL format_date(d-aweek,'yyyy-mm-dd',t)
        ENDIF
      CASE(i_m) !Monthly output
        IF(mlab)THEN
          CALL format_date(d,'yyyymm',t)
        ELSE
          CALL format_date(d,'yyyy-mm',t)
        ENDIF
      CASE(i_y) !Year, no special treatment of matlab files
        CALL format_date(d,'yyyy',t)
      END SELECT
    ELSE
      WRITE(6,*) 'HYSS ERROR: Need to specify either id or date (d) when calling write_dataline'
      STOP 2
    ENDIF

    IF(nsig==0)THEN
      CALL write_sepsep(iout,n,x,ndec,t,sep,abb)
    ELSE
      CALL write_mathsep(iout,n,x,nsig,t,sep)
    ENDIF

  END SUBROUTINE write_dataline

  !>\brief Print one line, left adjusted
  !----------------------------------------------------------------------
  SUBROUTINE write_commentline(funit,comment)

    !Argument declarations
    INTEGER, INTENT(IN) :: funit !<file unit for print-out
    CHARACTER(LEN=*), INTENT(IN) :: comment !<comment to be written
    
    !Local variables
    CHARACTER(LEN=10) format_str,commentlength

      format_str=''
      WRITE(commentlength,'(I10)') LEN(TRIM(comment))   !Integer to character
      format_str = '(A'//TRIM(ADJUSTL(commentlength))//')'
      WRITE(funit,format_str) comment

  END SUBROUTINE write_commentline

  !>\brief Mathematical print-out, separated by selected separator
  !----------------------------------------------------------------------
  SUBROUTINE write_mathsep(iout,n,x,nsig,textin,separator)

    USE WORLDVAR, ONLY : maxlinelength

    !Argument declarations
    INTEGER, INTENT(IN) :: iout           !<File unit for print-out
    INTEGER, INTENT(IN) :: n              !<Number of values in vector x
    REAL, INTENT(IN)    :: x(n)           !<Values
    INTEGER, INTENT(IN) :: nsig           !<Number of significant figures to be written (rounding will occurr)
    CHARACTER(LEN=16), INTENT(IN) :: textin !<Date or other id for first column
    CHARACTER(LEN=1), INTENT(IN) :: separator   !<Separator in file, e.g ',' or char(9) [=TAB]
    
    !Local variables
    INTEGER i,lt,lout
    CHARACTER (LEN=8)     form_f
    CHARACTER (LEN=16)    t
    CHARACTER (LEN=maxlinelength) outtxt           !(16+1)*200000

    t = textin
    t = ADJUSTL(t)
    lt = LEN_TRIM(t)
    outtxt(1:lt+1) = t(1:lt)//separator

    lout = lt+1
    form_f = '(ES16.'//CHAR(nsig+48)//')'

    !Go through all data and check they are integer, real of missing (-9999):
    DO i = 1,n
      WRITE(t,form_f) x(i)    !Gives specified rounding
      t = ADJUSTL(t)
      lt = LEN_TRIM(t)
      outtxt(lout+1:lout+lt) = t(1:lt)
      lout = lout+lt
      IF(i < n) THEN
        outtxt(lout+1:lout+1) = separator        !Also the last one necessary
        lout = lout+1                      !for reading in "free format"
      ENDIF
    ENDDO
    WRITE(iout,'(a)') outtxt(1:lout)

  END SUBROUTINE write_mathsep

  !>\brief Compressed print-out, separated by selected separator
  !----------------------------------------------------------------------
  SUBROUTINE write_sepsep(iout,n,x,ndec,textin,separator,abb)

    USE WORLDVAR, ONLY : maxlinelength

    !Argument declarations
    INTEGER, INTENT(IN) :: iout !<Unit for print-out
    INTEGER, INTENT(IN) :: n    !<Number of values in vector x
    REAL, INTENT(IN)    :: x(n) !<Values to be written
    INTEGER, INTENT(IN) :: ndec !<Number of decimals to be written (rounding to ndec decimals will occurr)
    CHARACTER(LEN=16), INTENT(IN) :: textin !<Date or other id for first column
    CHARACTER(LEN=1), INTENT(IN) :: separator  !<Separator in file, e.g ',' or char(9) [=TAB]
    INTEGER, INTENT(IN) :: abb  !<Switch for abbreviated format (0=no,e.g 0.1 stays 0.1; 1=yes, e.g 0.1 becomes .1)
    
    !Local variables
    INTEGER i,j,k,lt,lout
    CHARACTER (LEN=7)     form_f
    CHARACTER (LEN=16)    t
    CHARACTER (LEN=maxlinelength) outtxt           !(16+1)*200000

    t = textin
    t = ADJUSTL(t)
    lt = LEN_TRIM(t)
    outtxt(1:lt+1) = t(1:lt)//separator

    lout = lt+1
    form_f = '(f16.'//CHAR(ndec+48)//')'

    !Go through all data and check they are integer, real of missing (-9999):
    DO i = 1,n
       WRITE(t,form_f) x(i)    !Gives specified rounding
       IF(t(16:16) == '.') t(16:16) = ' '
       !Delete redundant zeroes at the end
       DO j = 16,1,-1
          IF(t(j:j) == '0') THEN
             t(j:j) = ' '
          ELSE
             EXIT
          ENDIF
       ENDDO
       !Delete unnecessary decimal points
       k = INDEX(t,'. ')
       IF(k > 0) THEN      !Integer
          t(k:k) = ' '
       ELSE
          !Delete insignificant zeroes if that option is set:
          IF(abb == 1)THEN
             k = INDEX(t,' 0.')
             IF(k > 0) THEN
                t(k+1:k+1) = ' '  !Ex) 0.1 -> .1
             ELSE
                k = INDEX(t,' -0.')
                IF(k > 0) THEN
                   t(k+1:k+3) = ' -.' !Ex) -0.1 -> -.1
                ENDIF
             ENDIF
          ENDIF
       ENDIF
       t = ADJUSTL(t)
       lt = LEN_TRIM(t)
       outtxt(lout+1:lout+lt) = t(1:lt)
       lout = lout+lt
       IF(i < n) THEN
          outtxt(lout+1:lout+1) = separator        !Also the last one necessary
          lout = lout+1                      !for reading in "free format"
       ENDIF
    ENDDO
    WRITE(iout,'(a)') outtxt(1:lout)

  END SUBROUTINE write_sepsep

  !>\brief Reads a matrix of data, count the number of columns
  !-------------------------------------------------------------------------------
  SUBROUTINE count_data_cols(funit,infile,nskip, ncols,status)

    USE WORLDVAR, ONLY : maxlinelength
 
    !Argument declarations
    INTEGER, INTENT(IN)  :: funit           !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of file to be read
    INTEGER, INTENT(IN)  :: nskip           !<Number of comment rows to skip in the beginning
    INTEGER, INTENT(OUT) :: ncols           !<Total number of columns
    INTEGER, INTENT(OUT) :: status          !<Error number
    
    !Local variables
    CHARACTER(LEN=maxlinelength) line
    CHARACTER(LEN=20) fmt601
    INTEGER i,j,io
    INTEGER linelen

    !Start of subroutine
    status = 0
    fmt601 = '(A'//TRIM(int_to_str(maxlinelength))//')'
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old', ACTION = 'read', ERR=610)

    IF(nskip>0) THEN !Skips nskip rows with comments
      DO i=1,nskip
        READ(funit,*,ERR=612,IOSTAT=io) line                             
      ENDDO
    ENDIF

    !Read row and calculate number of columns
    READ(funit,fmt601,ERR=611,IOSTAT=io) line
    j=0
    linelen = LEN_TRIM(line)
    DO i = 1,linelen
      IF(line(i:i)==CHAR(32).OR.line(i:i)==CHAR(9).OR.line(i:i)==CHAR(13).OR.line(i:i)==CHAR(10))THEN
        !skip - space, tab, CR or LF
      ELSE
        IF((line(i+1:i+1)==CHAR(32).OR.line(i+1:i+1)==CHAR(9).OR.   &
            line(i+1:i+1)==CHAR(13).OR.line(i+1:i+1)==CHAR(10)) .AND.     & 
            ((line(i:i)>=CHAR(48).AND.line(i:i)<=CHAR(57)).OR.(line(i:i)>=CHAR(65)   &
            .AND.line(i:i)<=CHAR(90)) .OR.  &
            (line(i:i)>=CHAR(97).AND.line(i:i)<=CHAR(122))) ) THEN
          j=j+1              !this char is number or letter and next is tab,space,CR or LF
        ENDIF
      ENDIF
    ENDDO
    ncols = j 

    CLOSE(funit)
    RETURN
610 WRITE(6,*) 'ERROR open file: ', TRIM(infile)
    status = 1
    RETURN
611 WRITE(6,*) 'ERROR reading headings in file: ', TRIM(infile)
    WRITE(6,*) 'ERROR io=',io
    CLOSE(funit)
    status = 1
    RETURN
612 WRITE(6,*) 'ERROR reading lines to skip in file: ', TRIM(infile)
    WRITE(6,*) 'ERROR io=',io
    CLOSE(funit)
    status = 1
    RETURN

  END SUBROUTINE count_data_cols

  !>\brief Reads a matrix of data, count the number of rows
  !---------------------------------------------------------------------------------------------
  SUBROUTINE count_data_rows(funit,infile,nskip,n,status) 

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit            !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile  !<Name of file to be read
    INTEGER, INTENT(IN)  :: nskip            !<Number of rows to skip
    INTEGER, INTENT(OUT) :: n                !<Number of data rows
    INTEGER, INTENT(OUT) :: status           !<Error status
    
    !Local variables
    INTEGER i,io
    CHARACTER(LEN=80) line

    !Open file
    status = 0
    OPEN(UNIT = funit,FILE = infile,STATUS = 'old',ACTION = 'read',ERR=301)

    !Read heading and comment lines in the beginning of the file
    DO i=1,nskip
      READ(funit,*,END=200,ERR=300,IOSTAT=io) line    
    ENDDO

    !Read class information
    n = 0
    DO
      READ(funit,*,END=200,ERR=300,IOSTAT=io) line
      n = n + 1
    ENDDO

200 CONTINUE
    CLOSE(UNIT = funit)
    RETURN

300 CONTINUE
    CLOSE(UNIT = funit)
    WRITE(6,*) 'ERROR: reading ',TRIM(infile)
    WRITE(6,*) 'ERROR: io=',io
    status = 1
    RETURN
301 CONTINUE
    WRITE(6,*) 'ERROR: opening ',TRIM(infile)
    status = 1
    RETURN

  END SUBROUTINE count_data_rows

  !>\brief Read GeoClass.txt and return number of classes
  !---------------------------------------------------------------------------------------------
  SUBROUTINE get_number_of_classgroups(readnclass,dim,outgroups,ndefgroup)
  
    USE WORLDVAR, ONLY : fileunit_temp, &
                         modeldir
    
    !Argument declarations
    LOGICAL, INTENT(IN) :: readnclass      !<Flag for setting class groups from GeoClass
    INTEGER, INTENT(IN) :: dim             !<Max number of outputs
    INTEGER, INTENT(INOUT) :: outgroups(dim)  !<Number of classgroups for each output
    INTEGER, INTENT(INOUT) :: ndefgroup    !<Number of defined class groups
    
    INTEGER,PARAMETER :: maxgccol = 13      !Maximum number of data columns in GeoClass, TODO:FLYTTA TILL WORLDVAR/MODAVR
    CHARACTER(LEN=12),PARAMETER :: file = 'GeoClass.txt'

    !Local variables
    REAL,ALLOCATABLE :: x(:,:)   !data from Geoclass-file

    IF(readnclass)THEN
      !>Count number of classes
      CALL read_geoclass(fileunit_temp,TRIM(modeldir)//file,maxgccol,x,ndefgroup)
      IF(ALLOCATED(x)) DEALLOCATE(x)
      outgroups = ndefgroup
    ENDIF

  END SUBROUTINE get_number_of_classgroups

  !>\brief Reads a matrix of characteristics for classes
  !---------------------------------------------------------------------------------------------
  SUBROUTINE read_geoclass(funit,infile,n,x,dmax)

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit           !<Unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile !<Name of file to be read
    INTEGER, INTENT(IN)  :: n               !<Maximum number of identifications
    REAL,ALLOCATABLE,INTENT(OUT) :: x(:,:)  !<Identification numbers
    INTEGER, INTENT(OUT) :: dmax            !<Number of combinations read
    
    !Local variables 
    INTEGER d,m,nslc,status,io
    REAL    y(n)                 !Data on row
    CHARACTER(LEN=80) line

    !>\b Algoritm \n
    !>Count number of classes (actually nslc include all rows)
    WRITE(6,*) 'File opened: ', TRIM(infile)
    CALL count_data_rows(funit,infile,0,nslc,status)
    IF(status/=0) STOP 1
    
    !>Allocate variable for holding class information
    IF(.NOT.ALLOCATED(x)) ALLOCATE(x(n,nslc))
    
    !>Read information of GeoClass.txt
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read',ERR=202) 
    !Skip comment lines in the beginning of the file
    DO 
      READ(funit,'(a)',END=200) line    
      IF(line(1:1)=='!')THEN 
      ELSE
        EXIT
      ENDIF
    ENDDO

    !Read class information (requires slc in order in file)
    d = 0
    DO
      y = 0
      READ(line,*,END=200,ERR=201) d,y(1:n-3)
      m = n - 3 + NINT(y(n-3))
      READ(line,*,END=200,ERR=201) d,y(1:m)
      x(:,d)=y(:)
      DO
        READ(funit,'(a)',END=200,ERR=201,IOSTAT=io) line
          IF(line(1:1)=='!')THEN
            CYCLE
          ELSE
            EXIT
          ENDIF
      ENDDO
    ENDDO

200 CONTINUE
    CLOSE(funit)
    dmax = d
    IF(d==0)THEN
      WRITE(6,*) 'Error: No classes found in ',TRIM(infile)
      STOP 1
    ENDIF
    RETURN
    
201 CONTINUE
    WRITE(6,*) 'Error reading ',TRIM(infile)
    WRITE(6,*) 'Error io=',io
    CLOSE(funit)
    STOP 1
    RETURN
202 CONTINUE
    WRITE(6,*) 'Error opening ',TRIM(infile)
    STOP 1
    RETURN

  END SUBROUTINE read_geoclass

  !>\brief Reads the data section from tab-separated file. Used for
  !!GeoData/LakeData/update.txt and many more
  !----------------------------------------------------------------------
  SUBROUTINE read_basindata5(funit,fname,maxcol,n,mcols,code,rindex,iindex,xi,xr) 
    
  USE WORLDVAR, ONLY : i_str,i_intg,i_real

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit          !<Unit for file
    CHARACTER(LEN=*), INTENT(IN) :: fname  !<Error filename
    INTEGER, INTENT(IN)  :: maxcol         !<Maximum number of data columns
    INTEGER, INTENT(IN)  :: n              !<Number of subbasins (rows)
    INTEGER, INTENT(IN)  :: mcols          !<Actual number of columns
    INTEGER, INTENT(IN)  :: code(maxcol)   !<Code for column variable type
    INTEGER, INTENT(OUT) :: rindex(maxcol) !<Index for column real variables
    INTEGER, INTENT(OUT) :: iindex(maxcol) !<Index for column integer variables
    INTEGER, INTENT(OUT) :: xi(n,maxcol)   !<Integer data read from file
    REAL   , INTENT(OUT) :: xr(n,maxcol)   !<Real data read from file
    
    !Local parameters
    INTEGER, PARAMETER :: linelen = 18000
    INTEGER, PARAMETER :: strlen  = 100   !Max length of column content
    
    !Local variables
    INTEGER isub
    INTEGER i,ij,rj,io
    INTEGER status                     !Error status
    INTEGER pos
    CHARACTER(LEN=strlen)  :: comstr
    CHARACTER(LEN=linelen) :: line
    CHARACTER(LEN=20) fmt600

    fmt600 = '(A'//TRIM(int_to_str(linelen))//')'
    
    !Read all data
    rindex = 0;iindex=0
    DO isub = 1,n
      rj = 1;ij = 1
      READ(funit,fmt600,ERR=610,IOSTAT=io) line
      pos = 1
      DO i = 1,mcols
        !Read a string on line
        CALL read_next_column(linelen,strlen,line,comstr,pos,status)
        IF(status.NE.0)THEN
          IF(status==2 .AND. i==mcols)THEN
            !this is ok, last column
          ELSE
            IF(status==2) WRITE(6,*) 'ERROR reading column, column wider than string'
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub,'column ',i
            STOP 1
          ENDIF
        ENDIF
        !Convert read string to value (real or integer)
        IF(code(i)==i_real)THEN
          CALL convert_string_to_real(strlen,comstr,xr(isub,rj),status) 
          IF(status.NE.0)THEN
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub
            STOP 1
          ENDIF
          rindex(i) = rj
          rj = rj + 1
        ELSEIF(code(i)==i_intg)THEN
          CALL convert_string_to_integer(strlen,comstr,xi(isub,ij),status) 
          IF(status.NE.0)THEN
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub
            STOP 1
          ENDIF
          iindex(i) = ij
          ij = ij + 1
        ENDIF
      ENDDO
    ENDDO
!
!600 FORMAT(A18000)
    RETURN
    
610 WRITE(6,*) 'ERROR: reading line:',line
    WRITE(6,*) 'in file',TRIM(fname)
    WRITE(6,*) 'ERROR io=',io
    STOP 1
    RETURN

  END SUBROUTINE read_basindata5

  !>\brief Reads the data section from tab-separated file. Also string columns. 
  !!Used for PointSourceData.txt
  !----------------------------------------------------------------------
  SUBROUTINE read_basindata6(funit,fname,maxcol,n,mcols,dimstr,code,rindex,iindex,sindex,xi,xr,xs) 
    
    USE WORLDVAR, ONLY : i_str,i_intg,i_real

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit          !<Unit for file
    CHARACTER(LEN=*), INTENT(IN) :: fname  !<Error filename
    INTEGER, INTENT(IN)  :: maxcol         !<Maximum number of data columns
    INTEGER, INTENT(IN)  :: n              !<Number of subbasins (rows)
    INTEGER, INTENT(IN)  :: mcols          !<Actual number of columns
    INTEGER, INTENT(IN)  :: dimstr         !<Max length of string values
    INTEGER, INTENT(IN)  :: code(maxcol)   !<Code for column variable type
    INTEGER, INTENT(OUT) :: rindex(maxcol) !<Index for column real variables
    INTEGER, INTENT(OUT) :: iindex(maxcol) !<Index for column integer variables
    INTEGER, INTENT(OUT) :: sindex(maxcol) !<Index for column string variables
    INTEGER, INTENT(OUT) :: xi(n,maxcol)   !<Integer data read from file
    REAL   , INTENT(OUT) :: xr(n,maxcol)   !<Real data read from file
    CHARACTER(LEN=dimstr), INTENT(OUT) :: xs(n,maxcol)   !<String data read from file
    
    !Local parameters
    INTEGER, PARAMETER :: linelen = 18000
    INTEGER, PARAMETER :: strlen  = 100   !Max length of column content
    
    !Local variables
    INTEGER isub
    INTEGER i,ij,rj,sj
    INTEGER status                     !Error status
    INTEGER pos
    CHARACTER(LEN=strlen)  :: comstr
    CHARACTER(LEN=linelen) :: line
    CHARACTER(LEN=20) fmt600

    fmt600 = '(A'//TRIM(int_to_str(linelen))//')'

    !Read all data
    rindex = 0;iindex=0;sindex=0
    DO isub = 1,n
      rj = 1;ij = 1;sj = 1
      READ(funit,fmt600) line
      pos = 1
      DO i = 1,mcols
        !Read a string on line
        CALL read_next_column(linelen,strlen,line,comstr,pos,status)
        IF(status.NE.0)THEN
          IF(status==2 .AND. i==mcols)THEN
            !this is ok, last column
          ELSE
            IF(status==2) WRITE(6,*) 'ERROR reading column, column wider than string'
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub,'colnr ',i
            STOP 1
          ENDIF
        ENDIF
        !Convert read string to value (real or integer)
        IF(code(i)==i_real)THEN
          CALL convert_string_to_real(strlen,comstr,xr(isub,rj),status) 
          IF(status.NE.0)THEN
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub,'colnr',i
            STOP 1
          ENDIF
          rindex(i) = rj
          rj = rj + 1
        ELSEIF(code(i)==i_intg)THEN
          CALL convert_string_to_integer(strlen,comstr,xi(isub,ij),status) 
          IF(status.NE.0)THEN
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub,'colnr',i
            STOP 1
          ENDIF
          iindex(i) = ij
          ij = ij + 1
        ELSE  !String variable
          IF(LEN(TRIM(comstr))<dimstr)THEN
            xs(isub,sj) = comstr
            sindex(i) = sj
            sj = sj + 1
          ELSE
            WRITE(6,*) 'ERROR in ',TRIM(fname),' rownr',isub,'colnr',i,'string too long'
            STOP 1
          ENDIF
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE read_basindata6

  !>\brief Reads the column headings from file and makes the string lower case.
  !---------------------------------------------------------------------------
  SUBROUTINE read_column_headings(funit,maxcol,nstr,str,mcols,status) 

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit                  !<Unit for file
    INTEGER, INTENT(IN)  :: maxcol                 !<Maximum number of data columns
    INTEGER, INTENT(IN)  :: nstr                   !<Number of characters in heading
    CHARACTER(LEN=nstr), INTENT(OUT) :: str(maxcol)  !<Column headings
    INTEGER, INTENT(OUT) :: mcols                  !<Actual number of columns
    INTEGER, INTENT(OUT) :: status                 !<Error status
    
    !Local parameters
    INTEGER,PARAMETER :: linelen = 18000
    
    !Local variables
    INTEGER i,j,io
    INTEGER first,last
    INTEGER tlen
    LOGICAL jtxt,jm1txt
    CHARACTER(LEN=linelen) :: line
    CHARACTER(LEN=20) fmt600

    fmt600 = '(A'//TRIM(int_to_str(linelen))//')'

    status = 0

    !Read content row and calculate number of columns !borde ersattas med count_data_cols (problem med filoppning)
    READ(funit,fmt600,ERR=610,IOSTAT=io) line
    j=0
    DO i = 1,linelen
       IF(line(i:i)==CHAR(32).OR.line(i:i)==CHAR(9).OR.&
            line(i:i)==CHAR(13).OR.line(i:i)==CHAR(10))THEN   !space, tab, CR or LF
       ELSE
          IF(line(i+1:i+1)==CHAR(32).OR.line(i+1:i+1)==CHAR(9).OR.&
               line(i+1:i+1)==CHAR(13).OR.line(i+1:i+1)==CHAR(10)) j=j+1
       ENDIF
    ENDDO
    mcols=j
    IF(mcols>maxcol)THEN
       status = 1
       WRITE(6,*) 'ERROR too many columns in file'
    ENDIF

    !Read content of columns
    i=1   !column
    first=1
    str=''
    DO j=2,linelen-1    !character position
      jtxt=.NOT.(line(j:j)==CHAR(32).OR.line(j:j)==CHAR(9).OR.&
            line(j:j)==CHAR(13).OR.line(j:j)==CHAR(10))
      jm1txt=.NOT.(line(j-1:j-1)==CHAR(32).OR.line(j-1:j-1)==CHAR(9).OR.&
            line(j-1:j-1)==CHAR(13).OR.line(j-1:j-1)==CHAR(10))
      IF(jtxt .AND. (.NOT.(jm1txt)))THEN
        first=j
      ELSEIF(.NOT.(jtxt) .AND. jm1txt)THEN
        last=j-1
        tlen=last-first+1
        IF(tlen>nstr)THEN
          str(i)(1:nstr)=line(first:first+nstr)
        ELSE
          str(i)(1:tlen)=line(first:last)
        ENDIF
        i=i+1
      ENDIF
    ENDDO

    CALL lower_case(str(1:mcols),mcols)   !make sure the headings are lower case letters

    RETURN
    
610 WRITE(6,*) 'ERROR: reading line'
    WRITE(6,*) 'ERROR: io=',io
    status = 1
    RETURN

  END SUBROUTINE read_column_headings

  !>\brief Reads the column headings from line and makes the string lower case.
  !---------------------------------------------------------------------------
  SUBROUTINE read_column_headings2(nline,line,maxcol,nstr,str,mcols,status)

    !Argument declarations
    INTEGER, INTENT(IN)  :: nline                  !<Maximum characters of line
    CHARACTER(LEN=*), INTENT(IN) :: line           !<Line with headings
    INTEGER, INTENT(IN)  :: maxcol                 !<Maximum number of data columns
    INTEGER, INTENT(IN)  :: nstr                   !<Maximum characters of str
    CHARACTER(LEN=*), INTENT(OUT) :: str(maxcol)  !<Column headings
    INTEGER, INTENT(OUT) :: mcols                  !<Actual number of columns
    INTEGER, INTENT(OUT) :: status                 !<Error status
    
    !Local variables
    INTEGER i,j
    INTEGER first,last
    INTEGER tlen
    LOGICAL jtxt,jm1txt

    status = 0

    !Calculate number of columns
    j=0
    DO i = 1,nline
      IF(line(i:i)==CHAR(32).OR.line(i:i)==CHAR(9).OR.&
         line(i:i)==CHAR(13).OR.line(i:i)==CHAR(10))THEN   !space, tab, CR or LF
      ELSE
        IF(line(i+1:i+1)==CHAR(32).OR.line(i+1:i+1)==CHAR(9).OR.&
           line(i+1:i+1)==CHAR(13).OR.line(i+1:i+1)==CHAR(10)) j=j+1
      ENDIF
    ENDDO
    mcols=j
    IF(mcols>maxcol)THEN
      status = 1
      WRITE(6,*) 'ERROR too many columns in file. Max:',maxcol
      RETURN
    ENDIF

    !Read content of columns
    i=1   !column
    first=1
    str=''
    DO j=2,nline-1    !character position
      jtxt=.NOT.(line(j:j)==CHAR(32).OR.line(j:j)==CHAR(9).OR.&
            line(j:j)==CHAR(13).OR.line(j:j)==CHAR(10))
      jm1txt=.NOT.(line(j-1:j-1)==CHAR(32).OR.line(j-1:j-1)==CHAR(9).OR.&
            line(j-1:j-1)==CHAR(13).OR.line(j-1:j-1)==CHAR(10))
      IF(jtxt .AND. (.NOT.(jm1txt)))THEN
        first=j
      ELSEIF(.NOT.(jtxt) .AND. jm1txt)THEN
        last=j-1
        tlen=last-first+1
        IF(tlen>nstr)THEN
          str(i)(1:nstr)=line(first:first+nstr)
        ELSE
          str(i)(1:tlen)=line(first:last)
        ENDIF
        i=i+1
      ENDIF
    ENDDO

    CALL lower_case(str(1:mcols),mcols)   !make sure the headings are lower case letters

    RETURN

  END SUBROUTINE read_column_headings2

  !>\brief Read next string from line 
  !!Ignoring beginning blanks and tabs, remove from line convert to
  !!lower case unless keepc is on String can be with or without '
  !!around
  !-----------------------------------------------------------------------
  SUBROUTINE read_next_codestr_on_line(linelen,strlen,pos,line,str,nostrfound,errstr,keepc) 

    !Argument declarations
    INTEGER, INTENT(IN)                :: linelen    !<Length of line
    INTEGER, INTENT(IN)                :: strlen     !<Length of string
    INTEGER, INTENT(INOUT)             :: pos        !<Current position on line
    CHARACTER(LEN=linelen), INTENT(IN) :: line       !<Line to be read
    CHARACTER(LEN=strlen), INTENT(OUT) :: str        !<Read string
    LOGICAL, INTENT(OUT)               :: nostrfound !<Empty line, may not be error
    CHARACTER(LEN=*), INTENT(IN)       :: errstr     !<Value of str if error
    LOGICAL, OPTIONAL, INTENT(IN)      :: keepc      !<Flag for keeping the capitals
    
    !Local variables
    INTEGER i,j
    LOGICAL found     !are there ' around string?
    LOGICAL lower

    str = ' '
    i = pos
    j = 1
    found = .FALSE.
    nostrfound = .TRUE.
    lower = .TRUE.
    IF(PRESENT(keepc))THEN
      IF(keepc) lower = .FALSE.
    ENDIF

    !Ignore beginning blanks and tabs or equal sign
    DO WHILE ((line(i:i)==CHAR(32)) .OR. (line(i:i)==CHAR(9)) .OR. (line(i:i)==CHAR(61)))
      i = i + 1
      IF (i .GT. linelen) GOTO 4000   !empty line
    ENDDO

    IF(line(i:i) .EQ. CHAR(39))THEN   !find first '
      i = i + 1
      found = .TRUE.
      IF (i .GT. linelen) GOTO 3000
    ENDIF

    IF(found)THEN
      DO WHILE (line(i:i) .NE. CHAR(39))   !read until next '
        IF (j .GT. strlen) GOTO 3000
        str(j:j) = line(i:i)
        i = i + 1
        j = j + 1
        IF (i .GT. linelen) GOTO 3000
      ENDDO
    ELSE
      DO WHILE ((line(i:i) .NE. CHAR(32)) .AND. (line(i:i) .NE. CHAR(9)) &
            .AND. (line(i:i).NE.CHAR(61)))   !read until next blank or tab or =
        IF (j .GT. strlen) GOTO 3000
        str(j:j) = line(i:i)
        i = i + 1
        j = j + 1
        IF (i .GT. linelen) GOTO 3000
      ENDDO
    ENDIF

    i = i + 1                           !remove next character (',blank or tab or =)
    IF (i .GT. linelen) GOTO 3000
    pos = i

    IF(lower) CALL scalar_lower_case(str)
    nostrfound = .FALSE.
    RETURN

    !Error handling
3000 str = ''
    str = errstr
    RETURN

4000 str = ''      !empty line
    RETURN

  END SUBROUTINE read_next_codestr_on_line

  !>\brief Read next date as string from line, ignoring beginning blanks and tabs.
  !!Supports formats yyyy, yyyy-mm, yyyy-mm-dd, yyyy-mm-dd hh:mm, yyyymm, yyyymmdd and yyyymmddhhmm.
  !------------------------------------------------------------------------------------------
  SUBROUTINE read_next_date_on_line(linelen,strlen,pos,line,str,nostrfound,notimefound,errstr) 

    !Argument declarations
    INTEGER, INTENT(IN)                   :: linelen     !<Length of line
    INTEGER, INTENT(IN)                   :: strlen      !<Length of string
    INTEGER, INTENT(INOUT)                :: pos         !<Current position on line
    CHARACTER(LEN=linelen), INTENT(IN)    :: line        !<Line to be read
    CHARACTER(LEN=strlen), INTENT(OUT)    :: str         !<Read string
    LOGICAL, INTENT(OUT)                  :: nostrfound  !<Empty line, may not be error
    LOGICAL, INTENT(OUT)                  :: notimefound !<Only date on line
    CHARACTER(LEN=*), INTENT(IN)          :: errstr      !<Value of str if error
    
    !Local variables
    INTEGER i,j,indx
    CHARACTER(LEN=strlen) :: tempstr

    !Initialise
    str = ' '
    tempstr=''
    i = pos
    j = 1
    nostrfound = .TRUE.
    notimefound = .TRUE.

    !Ignore beginning blanks and tabs or =
    DO WHILE((line(i:i)==CHAR(32)).OR.(line(i:i)==CHAR(9)).OR.(line(i:i)==CHAR(61)))   
      i = i + 1
      IF(i.GT.linelen) GOTO 4000   !empty line
    ENDDO

    !Read date-time from line      
    tempstr(j:j+9) = line(i:i+9)  !yyyy-mm-dd?
    IF(tempstr(5:5)=='-')THEN
      IF(tempstr(8:8)=='-')THEN
        !Normal date format
        i = i + 10
        j = j + 10
        tempstr(j:j) = ' '
        j = j + 1
        !Read possible time from line      
        indx = SCAN(line,':')
        IF(indx.GE.i+2)THEN ! Found a : somewhere
          tempstr(j:j+4) = line(indx-2:indx+2)
          notimefound = .FALSE.
          i = indx + 3   
        ENDIF
      ELSE
        !Montly date: yyyy-mm
        tempstr(8:strlen) = ''
        i = i + 8
      ENDIF
        
    ELSEIF(tempstr(5:5)==CHAR(9).OR.tempstr(5:5)==CHAR(32))THEN
      !Only year; yyyy
      tempstr(5:strlen) = ''
      i = i + 5

    ELSE
      !Integer date
      IF(tempstr(5:5)<=CHAR(57) .AND. tempstr(5:5)>=CHAR(48))THEN   !0-9
        IF(tempstr(7:7)<=CHAR(57) .AND. tempstr(7:7)>=CHAR(48))THEN   !0-9
          IF(tempstr(9:9)<=CHAR(57) .AND. tempstr(9:9)>=CHAR(48))THEN   !0-9
            tempstr(j+10:j+11) = line(i+10:i+11)  !with time
            notimefound = .FALSE.
            i = i + 12
          ELSE
            !without time
            tempstr(9:strlen) = ''
            i = i + 8
          ENDIF
        ELSE
          !Monthly date; yyyymm
          tempstr(7:strlen) = ''
          i = i + 7
        ENDIF
      ELSE
        WRITE(6,*) 'ERROR: read_next_date_on_line'
        GOTO 3000
      ENDIF
    ENDIF

    !Finish subroutine ok
    str = tempstr
    pos = i
    nostrfound = .FALSE.
    RETURN

    !Error handling
3000 str = errstr
    RETURN

4000 str = ''      !empty line
    RETURN

  END SUBROUTINE read_next_date_on_line

  !>Read string from line to next tab and remember the new position on line
  !-------------------------------------------------------------------------
  SUBROUTINE read_next_column(linelen,strlen,line,str,pos,ierr) 

    !Argument declarations
    INTEGER, INTENT(IN)                :: linelen  !<length of line
    INTEGER, INTENT(IN)                :: strlen   !<length of str
    CHARACTER(LEN=linelen), INTENT(IN) :: line     !<line to be read from position pos
    CHARACTER(LEN=strlen), INTENT(OUT) :: str      !<read column
    INTEGER, INTENT(INOUT)             :: pos      !<position on line
    INTEGER, INTENT(OUT)               :: ierr     !<error status
    
    !Local variables
    INTEGER i

    str = ''
    i = 1
    ierr = 0

    !Check for error in pos
    IF (pos .GT. linelen)THEN 
      WRITE(6,*) 'ERROR reading column, position to be read after line end'
      ierr = 1
      RETURN
    ENDIF

    !Read to next tab
    DO WHILE (line(pos:pos).NE.CHAR(9) .AND. line(pos:pos).NE.CHAR(13))   !not tab
      str(i:i) = line(pos:pos)
      i = i + 1
      pos = pos + 1
      IF (i .GT. strlen) EXIT
      IF (pos .GT. linelen) EXIT
    ENDDO

    !Error checking
    IF (i .GT. strlen)THEN 
      !May be error, ok if last column
      ierr = 2
    ENDIF
    IF (pos .GT. linelen)THEN 
      WRITE(6,*) 'ERROR reading column, position to be read after line end',str
      ierr = 3
      RETURN
    ENDIF

    pos = pos + 1                       !move beyond tab

  END SUBROUTINE read_next_column

  !>Read real value from string
  !---------------------------------------------------------------
  SUBROUTINE convert_string_to_real(strlen,str,output,ierr) 

    !Argument declarations
    INTEGER, INTENT(IN)                  :: strlen  !<length of string
    CHARACTER(LEN=strlen), INTENT(INOUT) :: str     !<string to convert
    REAL, INTENT(OUT)                    :: output  !<real value of string
    INTEGER, INTENT(OUT)                 :: ierr    !<error status

    ierr = 0
    READ(str,*,ERR=900,END=901) output
    RETURN

    !Error handling
900 WRITE(6,*) 'ERROR converting string to real: ',TRIM(str)
    ierr = 1
    RETURN
901 WRITE(6,*) 'ERROR (end of string) converting string to real: ',TRIM(str)
    ierr = 1

  END SUBROUTINE convert_string_to_real

  !>Read integer value from string
  !--------------------------------------------------------------
  SUBROUTINE convert_string_to_integer(strlen,str,output,ierr) 

    !Argument declarations
    INTEGER, INTENT(IN)                  :: strlen  !<length of string
    CHARACTER(LEN=strlen), INTENT(INOUT) :: str     !<string to convert
    INTEGER, INTENT(OUT)                 :: output  !<integer value of string
    INTEGER, INTENT(OUT)                 :: ierr    !<error status

    ierr = 0
    READ(str,*,ERR=900,END=901) output
    RETURN

    !Error handling
900 WRITE(6,*) 'ERROR converting string to integer: ',TRIM(str)
    ierr = 1
    RETURN
901 WRITE(6,*) 'ERROR (end of string) converting string to integer: ',TRIM(str)
    ierr = 1

  END SUBROUTINE convert_string_to_integer

  !> Add sequence/ensemble number to filename
  !------------------------------------------------------------
  SUBROUTINE add_number_to_filename(i,fname)

    !Argument declaration
    INTEGER, INTENT(IN) :: i  !<number of file
    CHARACTER(LEN=*), INTENT(INOUT) :: fname    !<file name
    
    !Local variable
    INTEGER l
    CHARACTER(LEN=3) strnr
    CHARACTER(LEN=LEN(fname)) filename1
    
    l=LEN_TRIM(fname)
    filename1 = fname   !Determine first part of filename
    IF(l>4)THEN
      IF(fname(l-3:l)=='.txt')THEN
        filename1 = fname(1:l-4)
      ENDIF
    ENDIF
    fname = ''    !Set filename
    WRITE(strnr,'(I3.3)') i
    fname = TRIM(filename1)//'_'//strnr//'.txt'

  END SUBROUTINE add_number_to_filename
  
  !>Read array values from file in chunks
  !--------------------------------------------------------------------
  SUBROUTINE read_array_from_file(ffunit,rowmax,dim,array)

    INTEGER,INTENT(IN)  :: ffunit       !<file unit
    INTEGER,INTENT(IN)  :: rowmax       !<chunk for reading
    INTEGER,INTENT(IN)  :: dim          !<dimension of array
    REAL,INTENT(OUT)    :: array(dim)   !<array with read values

    !Local variables
    INTEGER  j,jmax

    jmax = dim/rowmax
    DO j = 1,jmax
      READ(ffunit,*) array((j-1)*rowmax+1:j*rowmax)
    ENDDO
    IF(jmax*rowmax<dim) READ(ffunit,*) array(jmax*rowmax+1:dim)

  END SUBROUTINE read_array_from_file

  !>Read array values from file in chunks
  !--------------------------------------------------------------------
  SUBROUTINE read_array_from_file2(ffunit,rowmax,dim,array)

    INTEGER,INTENT(IN)  :: ffunit       !<file unit
    INTEGER,INTENT(IN)  :: rowmax       !<chunk for reading
    INTEGER,INTENT(IN)  :: dim          !<dimension of array
    REAL,INTENT(OUT)    :: array(dim)   !<array with read values

    !Local variables
    INTEGER  j,jmax

    jmax = dim/rowmax
    DO j = 1,jmax
      READ(ffunit) array((j-1)*rowmax+1:j*rowmax)
    ENDDO
    IF(jmax*rowmax<dim) READ(ffunit) array(jmax*rowmax+1:dim)

  END SUBROUTINE read_array_from_file2

  !>Write array values to file in chunks
  !--------------------------------------------------------------------
  SUBROUTINE write_array_to_file2(ffunit,rowmax,dim,array)

    INTEGER,INTENT(IN)  :: ffunit       !<file unit
    INTEGER,INTENT(IN)  :: rowmax       !<chunk for writing
    INTEGER,INTENT(IN)  :: dim          !<dimension of array
    REAL,INTENT(IN)     :: array(dim)   !<array with values to be written

    !Local variables
    INTEGER  i,j,jmax,lastrow

    jmax = dim/rowmax
    DO j = 1,jmax
!      IF(.NOT.(maxval(array((j-1)*rowmax+1:j*rowmax))>0. .OR. minval(array((j-1)*rowmax+1:j*rowmax))<0.)) THEN !test if whole array==0
!        WRITE(ffunit) (0, i=1,rowmax)
!      ELSE
        WRITE(ffunit) (array((j-1)*rowmax+i), i=1,rowmax)
!      ENDIF
    ENDDO
    
    lastrow=dim-jmax*rowmax
    IF(lastrow>0)THEN
!      IF(.NOT.(maxval(array(jmax*rowmax+1:dim))>0. .OR. minval(array(jmax*rowmax+1:dim))<0.)) THEN !test if whole array==0
!        WRITE(ffunit) (0, i=1,lastrow)
!      ELSE
        WRITE(ffunit) (array(jmax*rowmax+i),i=1,lastrow)
!      ENDIF
    ENDIF
    
  END SUBROUTINE write_array_to_file2

  !>Write array values to file in chunks
  !--------------------------------------------------------------------
  SUBROUTINE write_array_to_file(ffunit,rowmax,dim,array)
  
    INTEGER,INTENT(IN)  :: ffunit       !<file unit
    INTEGER,INTENT(IN)  :: rowmax       !<chunk for writing
    INTEGER,INTENT(IN)  :: dim          !<dimension of array
    REAL,INTENT(IN)     :: array(dim)   !<array with values to be written
  
    !Local variables
    INTEGER  i,j,jmax,lastrow
    CHARACTER(12) :: strcols  !number of columns per row
    CHARACTER(50) :: form_es  !write format
  
    WRITE(strcols,*) rowmax   !Integer to character
    jmax = dim/rowmax
    DO j = 1,jmax
      IF(.NOT.(maxval(array((j-1)*rowmax+1:j*rowmax))>0. .OR. minval(array((j-1)*rowmax+1:j*rowmax))<0.)) THEN !test if whole array==0
        form_es = '('//trim(adjustl(strcols))//'(I1'//',A))'
        WRITE(ffunit,form_es) (0, CHAR(9),i=1,rowmax)
      ELSE
        form_es = '('//trim(adjustl(strcols))//'(ES15.8'//',A))'
        WRITE(ffunit,form_es) (array((j-1)*rowmax+i), CHAR(9),i=1,rowmax)
      ENDIF
    ENDDO
    
    lastrow=dim-jmax*rowmax
    IF(lastrow>0)THEN
      WRITE(strcols,*) lastrow
      IF(.NOT.(maxval(array(jmax*rowmax+1:dim))>0. .OR. minval(array(jmax*rowmax+1:dim))<0.)) THEN !test if whole array==0
        form_es = '('//trim(adjustl(strcols))//'(I1'//',A))'
        WRITE(ffunit,form_es) (0, CHAR(9),i=1,lastrow)
      ELSE
        form_es = '('//trim(adjustl(strcols))//'(ES15.8'//',A))'
        WRITE(ffunit,form_es) (array(jmax*rowmax+i), CHAR(9),i=1,lastrow)
      ENDIF
    ENDIF
    
  END SUBROUTINE write_array_to_file
  
  !>Read parameter name and values from line 
  !--------------------------------------------------------------
  SUBROUTINE read_parameterline(line,dim,varstr,values,nvalues)

    USE MODVAR, ONLY : conductwarning

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: line            !<line of parameter file
    INTEGER, INTENT(IN)  :: dim                     !<dimension of parameter type
    CHARACTER(LEN=10), INTENT(OUT) :: varstr        !<parameter name
    REAL, INTENT(OUT)    :: values(dim)             !<parameter values
    INTEGER, INTENT(OUT) :: nvalues                 !<parameter values read from line
    
    !Local variables
    INTEGER i,ibeg,iend
    INTEGER n,linelen

    varstr=''
    linelen = LEN(line)

    !Find the variable name
    i = 1
    DO
      IF(line(i:i)==CHAR(32).OR.line(i:i)==CHAR(39))THEN    !start with first char not ' or  
        i = i + 1
      ELSE
        ibeg = i
        i = i + 1
        EXIT
      ENDIF
      IF(i==linelen) RETURN   !no parameter on this line, end of file
    ENDDO
    DO
      IF(line(i:i)==CHAR(32).OR.line(i:i)==CHAR(39).OR.line(i:i)==CHAR(9))THEN    !end with ',  or tab
        iend = i - 1
        i = i + 1
        EXIT
      ELSE
        i = i + 1
      ENDIF
    ENDDO
    n=iend-ibeg+1
    IF(n>10)THEN
      IF(conductwarning) WRITE(6,*)'WARNING: too many characters for parameter code ',line(ibeg:iend)
      varstr=line(ibeg:ibeg+9)
      RETURN
    ENDIF
    varstr(1:n)=line(ibeg:iend)
    CALL lower_case(varstr(1:n),n)

    !Find the number of values
    nvalues = 0
    DO
      IF((line(i:i)==CHAR(46).OR.(line(i:i)>=CHAR(48).AND.line(i:i)<=CHAR(57))).AND.  &
         (line(i+1:i+1)==CHAR(32).OR.line(i+1:i+1)==CHAR(9).OR.line(i+1:i+1)==CHAR(13)))THEN
        nvalues = nvalues + 1
      ENDIF
      i = i + 1
      IF(i==linelen) EXIT
    ENDDO
    IF(nvalues>dim)THEN
      IF(conductwarning) WRITE(6,*)'WARNING - number of read values reduced for ',varstr(1:n)
      nvalues=dim
    ENDIF

    !Read the values
    READ(line(iend+2:linelen),*,ERR=100) values(1:nvalues)
    RETURN
100 WRITE(6,*) 'ERROR: Reading par.txt on line: ', TRIM(line)
    STOP 1

  END SUBROUTINE read_parameterline

  !>Log simulation progress to log-file
  !------------------------------------------------------------
  SUBROUTINE log_progress(oldyear,newyear)

    !Argument declaration
    INTEGER, INTENT(INOUT) :: oldyear    !<Year id of last timestep
    INTEGER, INTENT(IN)    :: newyear    !<Year id of current timestep
    
    IF(newyear>oldyear)THEN
      WRITE(6,'(A21,I4)') 'Start simulate year: ',newyear
      FLUSH 6
      oldyear = newyear
    ENDIF

  END SUBROUTINE log_progress

  !>Log output information to hyss-file
  !------------------------------------------------------------
  SUBROUTINE print_output_information_to_logfile(funit)

    USE WORLDVAR, ONLY : output, &
                         noutput

    !Argument declaration
    INTEGER, INTENT(IN)    :: funit    !<File unit of log-file

    INTEGER io
  
    WRITE(funit,*)'Output information:'
    DO io=1,noutput
      IF(output(io)%fileformat==1)THEN
        IF(output(io)%narea>=0) WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables are asked to be saved for ',output(io)%narea,' subbasins'
        IF(output(io)%narea==-1) WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables are asked to be saved for all subbasins'
      ELSEIF(output(io)%fileformat==4)THEN
        IF(output(io)%narea>=0) WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables are asked to be saved for ',output(io)%narea,' regions'
        IF(output(io)%narea==-1) WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables are asked to be saved for all regions'
      ELSEIF(output(io)%fileformat==2)THEN
        WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables will be saved as files for mapping'
      ELSEIF(output(io)%fileformat==3)THEN
        WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables will be saved as timeseries output'
      ELSEIF(output(io)%fileformat==5)THEN
        WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' output variables will be saved as timeseries classgroup output'
      ELSEIF(output(io)%fileformat==6)THEN
        IF(output(io)%narea>0) WRITE(funit,'(I5,a,I3,a)')output(io)%nvar,' classgroup output variables are asked to be saved for ',output(io)%narea,' subbasins'
      ENDIF
    ENDDO

  END SUBROUTINE print_output_information_to_logfile

END MODULE READWRITE_ROUTINES
