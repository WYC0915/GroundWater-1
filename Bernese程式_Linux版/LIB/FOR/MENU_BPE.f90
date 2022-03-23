MODULE s_MENU_BPE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_bpe(keyWord, menuauxinp, output)

! -------------------------------------------------------------------------
! Purpose:    Create list of scripts or list of variables for RUNBPE.INP
!
! Author:     R. Dach
!
! Created:    08-Jul-2002
! Last mod.:  06-Feb-2004
!
! Changes:    13-Dec-2002 RD: New quotes-handling in SR writekey
!             18-May-2003 HU: Nullify pointers
!             06-Feb-2004 RD: Remove the length of the PCF-variables
!
! SR called:  gtflna, rdpcf2, alcerr, writeKey
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_menaux, ONLY: qt
  USE p_bpe2,   ONLY: t_pcf_v2, init_PCF_v2, done_PCF_v2, readPCF, writePCF

  USE s_alcerr
  USE s_writekey
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: keyWord     ! what to do
  CHARACTER(LEN=*)               :: menuauxInp  ! MENUAUX.INP file name
! output:
  TYPE(t_key)                    :: output      ! value: Result to display

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_bpe'

! Local Variables
! ---------------
  TYPE(t_pcf_v2)                :: pcf

  CHARACTER(LEN=keyValueLength) :: pcfFil

  INTEGER(i4b)                  :: iPid
  INTEGER(i4b)                  :: iVar
  INTEGER(i4b)                  :: irc

! Nullify pointers
! ----------------
  CALL init_pcf_v2(pcf)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'BPE_SCRIPT' .AND. keyWord /= 'BPE_VAR' .AND. &
      keyWord /= 'BPE_COPY') RETURN


! To have at least an empty uniline
! ---------------------------------
  IF (keyWord == 'BPE_VAR') THEN

    DEALLOCATE(output%value,stat=irc)

    ALLOCATE(output%value(1),stat=irc)
    CALL alcerr(irc,'output%value',(/ 1 /),srName)

    output%value = qt//qt // ' ' // qt//qt // ' ' // qt//qt

    CALL writekey(menuauxInp,(/output/),1,irc)

  ENDIF

! Get the name of the PCF file
! ----------------------------
  pcfFil = ' '
  CALL gtflna(0,'PCF_FILE',pcfFil,irc)
  IF (irc /= 0) CALL gtflna(0,'S_PCF_FILE',pcfFil,irc)
  IF (irc /= 0 .OR. LEN_TRIM(pcfFil) == 0) RETURN

! Read the PCF-file
! -----------------
  CALL readpcf(pcfFil,pcf)

! Put the results into the output-key
! -----------------------------------
  IF (keyWord == 'BPE_SCRIPT') THEN

    DEALLOCATE(output%value,stat=irc)

    ALLOCATE(output%value(pcf%njob),stat=irc)
    CALL alcerr(irc,'output%value',(/ pcf%njob /),srName)
    output%value = ' '

    DO iPid = 1, pcf%njob
      WRITE(output%value(iPid),'(A,2X,A,2X,A)')   &
            pcf%job(iPid)%pid(1:pcf%mxJob(1)),    &
            pcf%job(iPid)%script(1:pcf%mxJob(2)), &
            pcf%job(iPid)%option(1:pcf%mxJob(3))
    ENDDO

  ELSE IF (keyWord == 'BPE_VAR' .AND. pcf%nVar > 0) THEN

    DEALLOCATE(output%value,stat=irc)

    ALLOCATE(output%value(pcf%nVar),stat=irc)
    CALL alcerr(irc,'output%value',(/ pcf%nVar /),srName)
    output%value = ' '

    DO iVar = 1, pcf%nVar
      WRITE(output%value(iVar),'(A,2X,A,2X,A)') &
            qt // TRIM(pcf%var(iVar)%varnam) // qt, &
            qt // TRIM(pcf%var(iVar)%vardef) // qt, &
            qt // TRIM(pcf%var(iVar)%vardsc) // qt
    ENDDO

! Copy the PCF
  ELSEIF (keyWord == 'BPE_COPY') THEN
    CALL gtflna(1,'PCF_RESULT',pcfFil,irc)
    CALL writePcf(pcfFil,pcf)

  ENDIF

  CALL done_pcf_v2(pcf)

  RETURN
END SUBROUTINE menu_bpe

END MODULE
