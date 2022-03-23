MODULE s_PRFLNA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prflna (nCol)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine prints input and output filenames
!
! Author:     C. Urschl
!
! Created:    01-Nov-2000
!
! Changes:    07-Nov-2000 CU: A bug was fixed
!             09-Nov-2000 CU: Increase length of line (131)
!             14-Nov-2000 RD: Optinal parameter nCol (if nCol != 79)
!             11-Jul-2001 DI: Small format corrections (blank lines)
!             23-Apr-2003 AJ: Nullify local pointers
!             27-Feb-2012 RD: Order filename listing
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!
! -------------------------------------------------------------------------

  USE m_bern, ONLY: i4b, keyValueLength, keyNameLength, lineLength, &
                    lfnprt, FileNameLength

  USE s_readkeys
  USE s_alcerr
  USE s_iordup
  IMPLICIT NONE

! List of parameters
! ------------------
  INTEGER(i4b), OPTIONAL                                   :: nCol

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'prflna'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER     :: keyWords
  CHARACTER(LEN=keyValuelength)                            :: value
  CHARACTER(LEN=keyValueLength)                            :: filDescr
  CHARACTER(LEN=keyNameLength)                             :: keyWord
  CHARACTER(LEN=fileNameLength)                            :: filNam
  CHARACTER(LEN=lineLength)                                :: line
  INTEGER(i4b),                  DIMENSION(:), ALLOCATABLE :: idxKeyw
  INTEGER(i4b),                  DIMENSION(:), ALLOCATABLE :: indKeyw
  INTEGER(i4b)                                             :: iCol
  INTEGER(i4b)                                             :: i, j, ivalue
  INTEGER(i4b)                                             :: irc
  INTEGER(i4b)                                             :: iac

  CHARACTER(LEN=keyNameLength), DIMENSION(45) :: keyStart =                 &
  (/ 'CONST                           ','DATUM                           ', &
     'GPSUTC                          ','JPLEPH                          ', &
     'POTCOE                          ','SETIDES                         ', &
     'OTIDES                          ','ALBEMIS                         ', &
     'ALBREFL                         ','SATCRUX                         ', &

     'SATELL                          ','PHASECC                         ', &
     'ANTAZI                          ','RECEIVR                         ', &
     'FRQINFO                         ','NUTMOD                          ', &
     'SUBMOD                          ','POLE                            ', &
     'STDORB                          ','STDOR1                          ', &

     'RPRCOE                          ','TABFIL                          ', &
     'SATCLK                          ','RXCLKIN                         ', &
     'DCBINP                          ','ISBINP                          ', &
     'ABBREV                          ','STAINFO                         ', &
     'STAFILE                         ','COORD                           ', &

     'COORD1                          ','COORD2                          ', &
     'CRDINP                          ','CRDMTR                          ', &
     'VELOS                           ','VELAPR                          ', &
     'VELINP                          ','VELMTR                          ', &
     'VELOCI                          ','ECCENT                          ', &

     'OCNLOAD                         ','ATMLOAD                         ', &
     'STAWGT                          ','TROPEST                         ', &
     'IONOS                           '/)

  CHARACTER(LEN=keyNameLength), DIMENSION(27) :: keyEnde  =                 &
  (/ 'REPOSSUM                        ','SUMOUT                          ', &
     'SUMFIL                          ','SUMACC                          ', &
     'SUMMARY                         ','HISTOGRAM                       ', &
     'SMCINO                          ','SMEINO                          ', &
     'WEKSUM                          ','WKSSUM                          ', &

     'RESID                           ','RESPLOT                         ', &
     'PLOTSKL                         ','PLOTRS                          ', &
     'PLTFIL                          ','RESIDRS                         ', &
     'SYSOUT                          ','SYSERR                          ', &
     'AUXFIL                          ','AUXCOD                          ', &

     'AUXFIL1                         ','AUXFIL2                         ', &
     'AUXPHS                          ','OBCOPY                          ', &
     'SCRATCH                         ','TABAUX                          ', &
     'SESSION_TABLE                   ' /)

  NULLIFY(keyValue)
  NULLIFY(keyWords)

! Get the string of line
! ----------------------
  line=''
  IF (PRESENT(nCol)) THEN
    DO iCol=1,nCol
      line=TRIM(line)//'-'
    ENDDO
  ELSE
    DO iCol=1,79
      line=TRIM(line)//'-'
    ENDDO
  ENDIF

  CALL readKeys('*', keyWords, irc)

! write title lines
! -----------------
  WRITE(lfnprt,'(A)') ' INPUT AND OUTPUT FILENAMES'
  WRITE(lfnprt,'(1X,A,//,1X,A)') line(1:26), TRIM(line)

! find filenames and descriptions from input file
  ivalue = SIZE(keyWords)

  ALLOCATE(idxKeyw(iValue),stat=iac)
  CALL alcerr(iac,'idxKeyw',(/iValue/),srName)

  ALLOCATE(indKeyw(iValue),stat=iac)
  CALL alcerr(iac,'indKeyw',(/iValue/),srName)

  DO i = 1, ivalue
    idxKeyw(i) = 0
    IF (keyWords(i)(1:6) == 'DESCR_') THEN
      idxKeyw(i) = 1000
      keyWord = keyWords(i)(7:)
      DO j = 1,SIZE(keyStart)
        IF ( keyWord == keyStart(j) ) THEN
          idxKeyw(i) = j
          EXIT
        ENDIF
      ENDDO

      DO j = 1,SIZE(keyEnde)
        IF ( keyWord == keyEnde(j) ) THEN
          idxKeyw(i) = j+1000
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDDO

  CALL iordup(idxKeyw,iValue,indKeyw)

  DO j = 1, ivalue
    IF (idxKeyw(indKeyw(j)) == 0) CYCLE
    i = indKeyw(j)

    IF (keyWords(i)(1:6) == 'DESCR_') THEN
      value = keyWords(i)
      CALL readKeys(value, keyValue, irc)
      filDescr = keyValue(1)
      keyWord = value(7:)
      CALL readKeys(keyWord, keyValue, irc)
      IF (keyValue(1) == ' ') THEN
        filNam = ' ---'
      ELSE
        filNam = keyValue(1)
      END IF

! print filenames and descriptions
    WRITE(lfnprt,'(1X,A32,A2,A)')filDescr(1:32),': ',TRIM(filNam)
    END IF
  END DO

  WRITE(lfnprt,'(1X,A,/,/)') TRIM(line)

  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(keyWords,stat=irc)
  DEALLOCATE(idxkeyw,stat=irc)
  DEALLOCATE(indkeyw,stat=irc)

END SUBROUTINE prflna


END MODULE
