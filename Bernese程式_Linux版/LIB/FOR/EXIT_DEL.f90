
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE exit_del

! -------------------------------------------------------------------------
! Purpose:    Delete all files listed in DELETE_FILES (see: READINPF)
!
! Author:     R. Dach
!
! Created:    05-Nov-2003
! Last mod.:  17-Nov-2003
!
! Changes:    17-Nov-2003 RD: Prevent "PANIC LOOP"
!
! SRs called: opnfil, inquire
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_inpKey, ONLY: inpKey

  USE s_opnfil
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output


! Functions
! ---------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER                        :: srName = 'exit_del'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength)                      :: filnam

  INTEGER(i4b)                                       :: iDel
  INTEGER(i4b)                                       :: unit
  INTEGER(i4b)                                       :: ios

  LOGICAL                                            :: lExist
  LOGICAL                                            :: lOpen


! No program-name defined
! -----------------------
  IF (LEN_TRIM(program_Name) == 0) RETURN

! It is assumed that an input file exists in all other cases...


! Get the list of keywords to be deleted
! --------------------------------------
  DO iDel = 1,inpKey%nDel


! Path variable was not resolved in OPNFIL
! it is assumed that the file does not esist.
! -------------------------------------------
    IF (INDEX(inpKey%delFil(iDel),'$') /= 0) CYCLE

    filnam = inpKey%delFil(iDel)

! Does the file exist?
! --------------------
    INQUIRE(FILE=filnam,EXIST=lExist,OPENED=lOpen,NUMBER=unit)
    IF (.NOT.lExist) CYCLE

! The file is still opened
! ------------------------
    IF (lOpen) THEN
      CLOSE(unit,status='DELETE')
      CYCLE
    ENDIF

! Try to open with "UNFORMATTED"
! ------------------------------
    CALL opnfil(lfnloc, filnam,'UNKNOWN', 'UNFORMATTED',' ', ' ', ios)

! Try to open with "FORMATTED"
! ----------------------------
    IF (ios /= 0) &
      CALL opnfil(lfnloc, filnam,'UNKNOWN', 'FORMATTED',' ', ' ', ios)

! Delete the file
! ---------------
    IF (ios == 0) CLOSE(lfnloc,STATUS='DELETE')

  ENDDO

  RETURN
END SUBROUTINE exit_del

