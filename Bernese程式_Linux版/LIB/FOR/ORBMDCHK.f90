MODULE s_ORBMDCHK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE orbmdchk(isel, orbdsci,ircode)

! -------------------------------------------------------------------------
! Purpose:    Check orbit model description.
!             orbdsc  = settings from ORBGEN
!             orbdsci = from ELE file
!
! Author:     U. Hugentobler
!
! Created:    14-Jan-2005
! Last mod.:  02-Sep-2008
!
! Changes:    18-Jul-2006 AG: Cycle for OTLOAD
!             16-Feb-2007 HB: Make check for TIDPOT backwards compatible
!             05-Aug-2008 DT: Use orbdsc from P_ORBGEN (remove from list)
!             02-Sep-2008 DT: New format for intervals: INTEGR->INTEG2
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_orbgen,  ONLY: maxomd,orbmdft,t_orbmodel, orbdsc
  USE s_dimtst
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  INTEGER(i4b)                        :: isel        ! =1: Stop on error

! IN/OUT:
  TYPE(t_orbmodel)                    :: orbdsci     ! Model descr., ele

! OUT:
  INTEGER(i4b)                        :: ircode      ! =1: Inconsisteny found
                                                     ! (for isel=1)

! Local Variables
! ---------------
  INTEGER(i4b)                        :: irc
  INTEGER(i4b)                        :: ii,jj
  INTEGER(i4b)                        :: ipos,jpos
  INTEGER(i4b)                        :: nlin1

  ircode=0

! Add defaults to input list
! --------------------------
  nlin1=orbdsci%nlin
  DO ii=1,orbmdft%nlin
    jpos=0
    DO jj=1,orbdsci%nlin
      IF (orbdsci%orbmod(jj)(1:7) == orbmdft%orbmod(ii)(1:7)) THEN
        jpos=jj
        EXIT
      ENDIF
    ENDDO
    IF (jpos == 0) THEN
      nlin1=nlin1+1
      CALL dimtst(1,2,2,'ORBMDCHK','MAXOMD','ORBIT MODEL LINES',' ', &
                  nlin1,maxomd,irc)
      orbdsci%orbmod(nlin1)=orbmdft%orbmod(ii)
    ENDIF
  ENDDO
  orbdsci%nlin=nlin1

! Check consistency
! -----------------
  DO jj=1,orbdsci%nlin

    SELECT CASE (orbdsci%orbmod(jj)(1:7))
    CASE ( 'TITLE :' )
      CYCLE
    CASE ( 'CREATE:' )
      CYCLE
    CASE ( 'INTEGR:' )
      CYCLE
    CASE ( 'INTEG2:' )
      CYCLE
!    CASE ( 'OTLOAD:' )
!      CYCLE
    CASE( 'OTIDES:' )
      IF ( orbdsci%orbmod(jj)(57:57) == ' ') &
        orbdsci%orbmod(jj)(57:64)='DEG   4'
    END SELECT

    ipos=0
    DO ii=1,orbdsc%nlin
      IF (orbdsci%orbmod(jj)(1:7) == orbdsc%orbmod(ii)(1:7)) THEN
        ipos=ii
        EXIT
      ENDIF
    ENDDO
    IF (ipos == 0) THEN
!      WRITE(lfnerr,"(/,' *** SR ORBMDCHK: Unexpected description line found', &
!                  &  /,'                  ',A,/)") TRIM(orbdsci%orbmod(jj))
!      CALL exitrc(2)
      orbdsc%nlin=orbdsc%nlin+1
      CALL dimtst(1,2,2,'ORBMDCHK','MAXOMD','ORBIT MODEL LINES',' ', &
                  orbdsc%nlin,maxomd,irc)
      orbdsc%orbmod(orbdsc%nlin)=orbdsci%orbmod(jj)
      CYCLE
    ENDIF

! Necessary due to switch to solid Earth tide model in file
! ---------------------------------------------------------
! Possibility of ELAS and ANEL for Step 1 corrections
! Before switch: IERS1996, ELASTIC
    IF (orbdsci%orbmod(jj)(1:7) == 'TIDPOT:') THEN
      IF (orbdsci%orbmod(jj) /= orbdsc%orbmod(ipos)) THEN
        IF (orbdsci%orbmod(jj)(9:16) == orbdsc%orbmod(ipos)(9:16).AND.&
             orbdsci%orbmod(jj)(26:80) == orbdsc%orbmod(ipos)(26:80)) THEN
          IF (orbdsci%orbmod(jj)(9:24) == 'IERS1996        '.AND.&
               orbdsc%orbmod(ipos)(9:24) == 'IERS1996    ELAS')&
               orbdsci%orbmod(jj)(9:24) = 'IERS1996    ELAS'
        ENDIF
      ENDIF
    ENDIF


! Difference
      IF (orbdsci%orbmod(jj) /= orbdsc%orbmod(ipos)) THEN

! ..Warning
        IF (isel/=1) THEN
          WRITE(lfnerr,"(/,' ### SR ORBMDCHK: Inconsistent description found',&
                    &  /,'                  Model: ',A,  &
                    &  /,'                  Input: ',A,/)") &
                    TRIM(orbdsc%orbmod(ipos)),TRIM(orbdsci%orbmod(jj))
          ircode=1

! ..Error
        ELSEIF (isel==1) THEN
          WRITE(lfnerr,"(/,' *** SR ORBMDCHK: Inconsistent description found', &
                    &  /,'                  Model: ',A,  &
                    &  /,'                  Input: ',A,/)") &
                    TRIM(orbdsc%orbmod(ipos)),TRIM(orbdsci%orbmod(jj))
          ircode=2
        ENDIF
      ENDIF

  ENDDO

  IF (ircode==2) CALL exitrc(2)

  RETURN

END SUBROUTINE orbmdchk

END MODULE
