MODULE s_GETEST
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE getest(filNam,par,xxx)

! -------------------------------------------------------------------------
! Purpose:    Get estimated value from EST file
!
! Author:     M. Meindl
!
! Created:    15-Dec-2010
!
! Changes:    19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr, lfnloc
  USE d_par,   ONLY: t_par

  USE s_alcerr
  USE s_exitrc
  USE s_opnfil
  USE s_opnerr
  USE f_nextline

  IMPLICIT NONE


! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*)       :: filNam  ! File name
  TYPE(t_par)            :: par     ! Parameter

! output:
  REAL(r8b)              :: xxx     ! Estimated value


! Local parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER                 :: srName = "getest"



! Local Variables
! ---------------
  TYPE(t_par),DIMENSION(:),ALLOCATABLE,SAVE  :: parLst
  TYPE(t_par)                                :: par1

  CHARACTER(LEN=255)                         :: line

  REAL(r8b)                                  :: mjd1, mjd2

  INTEGER(i4b),SAVE                          :: nPar = 0
  INTEGER(i4b)                               :: iPar
  INTEGER(i4b)                               :: irc, iac

  LOGICAL, SAVE                              :: first = .TRUE.



! Some initializations
! --------------------
  xxx  = par%x0


! First call: read file
! ---------------------
  IF (first) THEN
    first = .FALSE.

! Open file
    CALL opnFil(lfnLoc,filNam,'UNKNOWN','FORMATTED',' ',' ',irc)
    CALL opnErr(lfnErr,lfnLoc,irc,filNam,srName)

! Count number of entries
    DO
      line = nextLine(lfnLoc,0)
      IF (line == '') EXIT
      nPar = nPar+1
    ENDDO

! Allocate memory
    ALLOCATE(parLst(nPar),STAT=iac)
    CALL alcErr(iac,'parLst',(/nPar/),srName)

! Read all parameters from file
    REWIND(lfnLoc)
    nPar = 0
    DO
      line = nextLine(lfnLoc,0)
      IF (line == '') EXIT
      nPar = nPar+1
      READ(line,'(A20,2(2X,F15.9),7I12,F17.5)',IOSTAT=irc) &
        parLst(nPar)%name,mjd1,mjd2,parLst(nPar)%locq,parLst(nPar)%x0
      parLst(nPar)%time%mean = (mjd1+mjd2)/2.d0
      parLst(nPar)%time%half = (mjd2-mjd1)/2.d0
      IF (irc /= 0) THEN
        WRITE(lfnErr,'(/,A,/,A,I6,/,2A)')             &
          " *** SR GETEST: Unreadable entry in file", &
          "                Entry number: ",nPar,      &
          "                File name   : ",TRIM(filNam)
        CALL exitRc(2)
      ENDIF
    ENDDO

! Close file
    CLOSE(lfnLoc)
  ENDIF


! Find requested parameter
! ------------------------
  DO iPar=1,nPar
    par1 = parLst(iPar)
    IF (TRIM(par%name) == TRIM(par1%name)                           .AND. &
        ALL(par%locq == par1%locq)                                  .AND. &
        par%time%mean-par%time%half < par1%time%mean+par1%time%half .AND. &
        par%time%mean+par%time%half > par1%time%mean-par1%time%half) THEN
      xxx = par1%x0
      EXIT
    ENDIF
  ENDDO


! The end
! -------
  RETURN
END SUBROUTINE getest
END MODULE s_GETEST
