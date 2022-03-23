MODULE s_ESTSTORE
CONTAINS

! ---------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ---------------------------------------------------------------------

SUBROUTINE eststore(neq)

! ---------------------------------------------------------------------
! Purpose:    Store all estimated parameters
!
! Author:     M. Meindl
!
! Created:    30-Nov-2010
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ---------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_addneq, ONLY: opt, comstat
  USE d_neq,    ONLY: t_neq

  USE s_opnfil
  USE s_opnerr
  USE f_ikf

  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  TYPE(t_neq)                                :: neq  ! Normal equation

! Local Variables
! ---------------
  CHARACTER(LEN=8),PARAMETER                 :: srName  = 'eststore'
  CHARACTER(LEN=5),DIMENSION(30)             :: grepStr =           &
           (/'#CRD ', '#RCO ', '#ORB ', '     ', '#RAO ',           &
             '#TRP ', '     ', '#DCB ', '     ', '#ERP ',           &
             '#SORB', '#SAO ', '     ', '     ', '     ',           &
             '#GCC ', '     ', '#RAP ', '#GIM ', '     ',           &
             '     ', '#GRD ', '#RCK ', '#SCK ', '#SAP ',           &
             '#RGB ', '#HOI ', '     ', '     ', '#GSP ' /)

  INTEGER(i4b)                               :: ios, iPar

  REAL(r8b)                                  :: mjd1, mjd2


! Return, if no output file specified
! -----------------------------------
  IF (opt%paramrs == '') RETURN


! Open output file
! ----------------
  CALL opnfil(lfnloc,opt%paramrs,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,opt%paramrs,'RGBOUT')


! Loop over all parameters
! ------------------------
  DO iPar=1,neq%misc%nPar
    mjd1 = neq%par(iPar)%time%mean-neq%par(iPar)%time%half
    mjd2 = neq%par(iPar)%time%mean+neq%par(iPar)%time%half
    WRITE(lfnloc,'(A20,2(2X,F15.9),7I12,2F17.5,2X,A5)') &
      neq%par(iPar)%name,mjd1,mjd2,neq%par(iPar)%locq, &
      neq%xxx(iPar)+neq%par(iPar)%x0, &
      comstat%rms*SQRT(ABS(neq%aNor(ikf(iPar,iPar)))) ,&
      grepStr(neq%par(iPar)%locq(1))
  ENDDO


! Close output file
! -----------------
  CLOSE(lfnloc)


! The end
! -------
  RETURN

END SUBROUTINE eststore

END MODULE s_ESTSTORE
