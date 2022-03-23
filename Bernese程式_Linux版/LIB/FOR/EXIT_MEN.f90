
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE exit_men(irCode)

! -------------------------------------------------------------------------
! Purpose:    Prints the return code to keyword "MENUAUX_IRCODE"
!
! Author:     R. Dach
!
! Created:    05-Nov-2003
!
! Changes:    17-Nov-2003 RD: Prevent "PANIC LOOP"
!             29-Feb-2012 RD: Unreliable "EOF" reporting by iostat (workaround)
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, lfnloc, &
                      keyvalueLength, program_name
  USE p_menaux, ONLY: inpFileName
  USE s_opnfil
  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  INTEGER(i4b)        ::    irCode          ! Program return code

! output


! Functions
! ---------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER                        :: srName = 'exit_men'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),ALLOCATABLE :: buffer
  CHARACTER(LEN=keyValueLength)                          :: line

  INTEGER(i4b)                                           :: iBlank
  INTEGER(i4b)                                           :: iLine
  INTEGER(i4b)                                           :: nLine
  INTEGER(i4b)                                           :: ii
  INTEGER(i4b)                                           :: ios
  INTEGER(i4b)                                           :: iac


! Write the return code for MENUAUX
! ---------------------------------
  IF (program_Name /= 'MENUAUX') RETURN

! Try to open the input file
! --------------------------
  CALL opnfil(lfnloc, inpFileName, 'OLD', 'FORMATTED',' ', ' ', ios)
  IF (ios /= 0) RETURN

! Get the number of lines
! -----------------------
  nLine = 0
  iBlank = 0
  DO WHILE(ios == 0)
    READ(lfnloc,'(A)',iostat=ios) line

    IF (LEN_TRIM(line) > 0) THEN
      iBlank = 0
    ELSE
      iBlank = iBlank + 1
    ENDIF

    IF (iBlank > 1000) THEN
      nLine = nLine - iBlank + 1
      EXIT
    ENDIF
    IF (ios == 0) nLine = nLine + 1
  ENDDO

! Allocate a buffer
! -----------------
  ALLOCATE(buffer(nLine),stat=iac)
  IF (iac == 0) THEN

! Rewind file, read file into the buffer
! --------------------------------------
    REWIND(lfnloc)

    DO iLine = 1,nLine
      READ(lfnloc,'(A)',iostat=ios) line
      IF (ios == 0) THEN
        IF (index(line,'MENUAUX_IRCODE') /= 1) THEN
          buffer(iLine) = line
        ELSE IF (irCode == 0) THEN
          buffer(iLine) = 'MENUAUX_IRCODE  1  "0"'
        ELSE
          buffer(iLine) = 'MENUAUX_IRCODE  1  "2"'
        ENDIF
      ENDIF
    ENDDO

    CLOSE(lfnloc)

! Write buffer back to the file
! -----------------------------
    CALL opnfil(lfnloc, inpFileName, 'OLD', 'FORMATTED',' ', ' ', ios)
    IF (ios /= 0) THEN
      DEALLOCATE(buffer,stat=iac)
      RETURN
    ENDIF


    DO ii = 1,nLine
      WRITE(lfnloc,'(A)') TRIM(buffer(ii))
    ENDDO


! Close file, deallocate buffer
! -----------------------------
    CLOSE(lfnloc)

    DEALLOCATE(buffer,stat=iac)

! "buffer" could not be allocated
! --------------------------------
  ELSE
    WRITE(lfnerr,'(/,A,/)') &
    ' *** SR EXIT_MEN: Variable "buffer" could not be allocated'
  ENDIF

  RETURN
END SUBROUTINE exit_men
