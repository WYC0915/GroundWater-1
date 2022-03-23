MODULE s_RPLENVAR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rplEnVar(iStop,string)

! -------------------------------------------------------------------------
!
! Purpose:    Replace Environment Variables
!
!
! Author:     L. Mervart
!
! Created:    08-AUG-2000
! Last mod.:  11-Mar-2010
!
! Changes:    19-Feb-2003  RD: Error handling
!             12-Mar-2003  RD: Keep variable in string if it could not replaced
!             23-Apr-2003  RD: Nullify local pointers
!             19-Nov-2003  RD: Prevent "panic-loop"
!             27-Nov-2003  RD: Ignore unresolved variables
!             11-Mar-2010  SL: ONLY added to USE m_bern
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, keyValueLength, lfnErr
  USE d_inpKey, ONLY: inpKey

  USE s_exitrc
  IMPLICIT NONE

! Variables from parameter list
! -----------------------------
! input:
  INTEGER(i4b)     :: iStop   ! Error handling:
                              ! 0: No warning/error if variable not found
                              ! 1: Warning if variable not found, but continue
                              ! 2: Stop with error if variable not found

! input/output
  CHARACTER(LEN=*) :: string  ! String to replace variables

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength)                              :: varName
  CHARACTER(LEN=keyValueLength)                              :: varValue
  CHARACTER(LEN=keyValueLength)                              :: keyVarName
  CHARACTER(LEN=keyValueLength)                              :: keyVarValue
  CHARACTER(LEN=255)                                         :: strHlp

  INTEGER(i4b), SAVE                                         :: envKey = 0
  INTEGER(i4b)                                               :: iKey
  INTEGER(i4b)                                               :: ios
  INTEGER(i4b)                                               :: i0
  INTEGER(i4b)                                               :: i1
  INTEGER(i4b)                                               :: i2
  INTEGER(i4b)                                               :: ii, jj

! Loop string to fin "$"
! ----------------------
  i0 = 1
  DO WHILE (i0 < LEN_TRIM(string))

! Get index of next "$"
    i1 = INDEX(string(i0:),'$')
    IF (i1 > 0) i1 = i1 + i0-1

! No "$" found
    IF (i1 == 0) EXIT

! Read ENVIRONMENT in the first run
    IF (envKey > 0) THEN
      IF (inpKey%keys(envKey)%name /= 'ENVIRONMENT') envKey = 0
    ENDIF

    IF (envKey == 0) THEN
      DO iKey = 1,inpKey%nKeys
        IF (inpKey%keys(iKey)%name == 'ENVIRONMENT') THEN
          envKey = iKey
          EXIT
        ENDIF
      ENDDO
    END IF

! Get the end of the variable name
    varName = ''

    i2 = i1 + 1
    IF (string(i2:i2) == '{') THEN

      i2 = INDEX(string(i2:),'}')

      IF (i2 == 0) THEN
        i2 = i1 + 1
        IF (iStop == 1) THEN
          WRITE(lfnerr,'(/,A,/)')                                &
               ' ### SR RPLENVAR: "}" is missing in: ' // TRIM(string)
        ELSE IF (iStop == 2) THEN
          WRITE(lfnerr,'(/,A,/)')                                &
               ' *** SR RPLENVAR: "}" is missing in: ' // TRIM(string)
          CALL exitrc(2)
        END IF
      ELSE
        i2 = i2 + i1
        varName = string(i1+2:i2-1)
      END IF
    ELSE
      varName = string(i2:i2)
    ENDIF

! Check for the value of the variable
    IF (varName /= '') THEN
      varValue = ''
      jj = 0
      IF (envKey /= 0) THEN
        DO ii = 1, SIZE(inpKey%keys(envKey)%value)
          keyVarValue = ''
          READ(inpKey%keys(envKey)%value(ii),*,iostat=ios) &
               keyVarName, keyVarValue
          IF (INDEX(keyVarValue,'$') /= 0) CYCLE ! Ignore unresolved variables
          IF (varName == keyVarName) THEN
            varValue = keyVarValue
            jj = ii
            EXIT
          END IF
        END DO
      ENDIF

! Error handling
      IF (jj == 0 .AND. iStop > 0) THEN
        IF (iStop == 1) THEN
          WRITE(lfnerr,'(/,A,/)')                                &
               ' ### SR RPLENVAR: Variable ' // TRIM(varName) // &
                        ' not resolved in: ' // TRIM(string)
        ELSE IF (iStop == 2) THEN
          WRITE(lfnerr,'(/,A,/)')                                &
               ' *** SR RPLENVAR: Variable ' // TRIM(varName) // &
                        ' not resolved in: ' // TRIM(string)
          CALL exitrc(2)
        END IF
      END IF

! Replace variable
      IF (jj /= 0) THEN
        IF (i1 > 1) THEN
          strHlp = string(1:i1-1) // TRIM(varValue) // TRIM(string(i2+1:))
        ELSE
          strHlp = TRIM(varValue) // TRIM(string(i2+1:))
        END IF
        string = strHlp
      ENDIF
    ENDIF

! Search for the next "$" in string
    i0 = i2+1
  END DO

END SUBROUTINE rplEnVar

END MODULE
