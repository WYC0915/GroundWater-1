! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_stdOrbit

! -------------------------------------------------------------------------
! Description: Defines structures and I/O routines for standard orbits
!
! Author:     G.Beutler, L. Mervart
!
! Created:    04-Dec-2006
!
! Changes:    01-Oct-2007 AJ: Use estPar instead of Par in getDxvDp
!             02-Oct-2007 AJ: Read/write estStc
!             03-Dec-2010 HB: Add parameter for SR prtDer
!             20-Sep-2012 RD: Use all modules with ONLY
!             20-Sep-2012 RD: Remove unused modules
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

USE s_stdorbit_t, ONLY: t_stdorb

IMPLICIT NONE

PRIVATE
PUBLIC :: stdOrbit, readStdOrbit, writeStdOrbit,  &
          initSatPos, getSatPos, getDxvDp, getDxvDp2

! Interfaces
! ----------
INTERFACE readStdOrbit
  MODULE PROCEDURE readStdOrbit1, readStdOrbit2
END INTERFACE

INTERFACE writeStdOrbit
  MODULE PROCEDURE writeStdOrbit1, writeStdOrbit2
END INTERFACE

INTERFACE getSatPos
  MODULE PROCEDURE getSatPos1, getSatPos2
END INTERFACE

! Module Variables
! ----------------
TYPE(t_stdOrb), SAVE, TARGET :: stdOrb_
LOGICAL, SAVE                :: fileRead_ = .FALSE.

CONTAINS

! -------------------------------------------------------------------------
! Access to the module variable
! -------------------------------------------------------------------------
FUNCTION stdOrbit()
  TYPE(t_stdOrb), POINTER :: stdOrbit
  stdOrbit => stdOrb_
END FUNCTION

! -------------------------------------------------------------------------
! Read Standard Orbit File
! -------------------------------------------------------------------------
SUBROUTINE readStdOrbit1(orbFileName)
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: orbFileName
  CALL readStdOrbit2(orbFileName, stdOrb_)
  fileRead_ = .TRUE.
END SUBROUTINE

SUBROUTINE readStdOrbit2(orbFileName, stdOrb)
  USE m_bern,  ONLY: i4b, lfnloc, lfnerr
  USE s_stdorbit_t, ONLY: t_stdorb

  USE s_alcerr
  USE s_opnfil
  USE s_opnerr

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  CHARACTER(LEN=*), INTENT(IN)    :: orbFileName
  TYPE(t_stdOrb),   INTENT(INOUT) :: stdOrb

  ! Local Variables
  ! ---------------
  INTEGER(i4b) :: irc, ios, numDesc, iDesc, numVal, iVal, numArcs, iArc, iPar
  INTEGER(i4b) :: numInt, iInt, qq, numSto, iSto, numPar, numEstPar, numEstStc
  INTEGER(i4b) :: version, iEpo, i_Comp,ind_comp, numSto_est, sampl_sto,sampl_typ

  CALL opnfil(lfnloc,orbFileName,'OLD','UNFORMATTED','READONLY',' ',irc)
  CALL opnerr(lfnerr,lfnloc,irc,orbFileName,'readStdOrbit')

  READ(lfnloc) version

  READ(lfnloc) stdorb%title

  READ(lfnloc) numDesc
  IF (numDesc > 0) THEN
    ALLOCATE(stdorb%modDesc(numDesc), STAT=irc)
    CALL alcerr(irc, 'stdorb%modDesc', (/numDesc/), 'readStdOrbit')
    DO iDesc = 1, numDesc
      READ(lfnloc) stdorb%modDesc(iDesc)%name
      READ(lfnloc) numVal
      IF (numVal > 0) THEN
        ALLOCATE(stdorb%modDesc(iDesc)%value(numVal), STAT=irc)
        CALL alcerr(irc, 'stdorb%modDesc%value', (/numVal/), 'readStdOrbit')
        DO iVal = 1, SIZE(stdorb%modDesc(iDesc)%value)
          READ(lfnloc) stdorb%modDesc(iDesc)%value(iVal)
        ENDDO
      ENDIF
    ENDDO
  ENDIF

  READ(lfnloc) numArcs
  IF (numArcs > 0) THEN
    ALLOCATE(stdorb%orbArc(numArcs), STAT=irc)
    CALL alcerr(irc, 'stdorb%orbArc', (/numArcs/), 'readStdOrbit')
    DO iArc = 1, numArcs
      READ(lfnloc) stdorb%orbArc(iArc)%sys
      READ(lfnloc) stdorb%orbArc(iArc)%svn
      READ(lfnloc) stdorb%orbArc(iArc)%man
      READ(lfnloc) stdorb%orbArc(iArc)%tWin%t(1)%day,  &
                   stdorb%orbArc(iArc)%tWin%t(1)%frac, &
                   stdorb%orbArc(iArc)%tWin%t(2)%day,  &
                   stdorb%orbArc(iArc)%tWin%t(2)%frac

      READ(lfnloc) numDesc
      IF (numDesc > 0) THEN
        ALLOCATE(stdorb%orbArc(iArc)%modDesc(numDesc), STAT=irc)
        CALL alcerr(irc, 'orbArc%modDesc', (/numDesc/), 'readStdOrbit')
        DO iDesc = 1, numDesc
          READ(lfnloc) stdorb%orbArc(iArc)%modDesc(iDesc)%name
          READ(lfnloc) numVal
          IF (numVal > 0) THEN
            ALLOCATE(stdorb%orbArc(iArc)%modDesc(iDesc)%value(numVal), STAT=irc)
            CALL alcerr(irc, 'orbArc%modDesc%value', (/numVal/), 'readStdOrbit')
            DO iVal = 1, SIZE(stdorb%orbArc(iArc)%modDesc(iDesc)%value)
              READ(lfnloc) stdorb%orbArc(iArc)%modDesc(iDesc)%value(iVal)
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      READ(lfnloc) numInt
      IF (numInt > 0) THEN
        ALLOCATE(stdorb%orbArc(iArc)%orbInt(numInt), STAT=irc)
        CALL alcerr(irc, 'orbArc%orbInt', (/numInt/), 'readStdOrbit')
        DO iInt = 1, numInt
          READ(lfnloc) stdorb%orbArc(iArc)%orbInt(iInt)%left,   &
                       stdorb%orbArc(iArc)%orbInt(iInt)%right,  &
                       stdorb%orbArc(iArc)%orbInt(iInt)%shadow, &
                       stdorb%orbArc(iArc)%orbInt(iInt)%hh,     &
                       stdorb%orbArc(iArc)%orbInt(iInt)%t0,     &
                       stdorb%orbArc(iArc)%orbInt(iInt)%qq

          qq = stdorb%orbArc(iArc)%orbInt(iInt)%qq

          IF (qq > 0) THEN
            ALLOCATE(stdorb%orbArc(iArc)%orbInt(iInt)%rCoe(3,qq+1), STAT=irc)
            CALL alcerr(irc, 'orbInt%rCoe', (/3,qq+1/), 'readStdOrbit')
            READ(lfnloc) stdorb%orbArc(iArc)%orbInt(iInt)%rCoe

            ALLOCATE(stdorb%orbArc(iArc)%orbInt(iInt)%drdeCoe(3,6,qq+1), STAT=irc)
            CALL alcerr(irc, 'orbInt%drdeCoe', (/3,6,qq+1/), 'readStdOrbit')
            READ(lfnloc) stdorb%orbArc(iArc)%orbInt(iInt)%drdeCoe

            ALLOCATE(stdorb%orbArc(iArc)%orbInt(iInt)%drdsCoe(3,3,qq+1), STAT=irc)
            CALL alcerr(irc, 'orbInt%drdsCoe', (/3,3,qq+1/), 'readStdOrbit')
            READ(lfnloc) stdorb%orbArc(iArc)%orbInt(iInt)%drdsCoe
          ENDIF
        ENDDO

        READ(lfnloc) numSto
        IF (numSto > 0) THEN
          ALLOCATE(stdorb%orbArc(iArc)%stoch(numSto), STAT=irc)
          CALL alcerr(irc, 'orbArc%stoch', (/numSto/), 'readStdOrbit')
          DO iSto = 1, numSto
            READ(lfnloc) stdorb%orbArc(iArc)%stoch(iSto)%typ,    &
                         stdorb%orbArc(iArc)%stoch(iSto)%comp,   &
                         stdorb%orbArc(iArc)%stoch(iSto)%left,   &
                         stdorb%orbArc(iArc)%stoch(iSto)%right,  &
                         stdorb%orbArc(iArc)%stoch(iSto)%value,  &
                         stdorb%orbArc(iArc)%stoch(iSto)%dE,     &
                         stdorb%orbArc(iArc)%stoch(iSto)%dEds
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDIF

  READ(lfnloc) numPar
  IF (numPar > 0) THEN
    ALLOCATE(stdorb%par(numPar), STAT=irc)
    CALL alcerr(irc, 'stdorb%par', (/numPar/), 'readStdOrbit')
    DO iPar = 1, numPar
      READ(lfnloc) stdorb%par(iPar)%locq ,  &
                   stdorb%par(iPar)%name ,  &
                   stdorb%par(iPar)%time ,  &
                   stdorb%par(iPar)%x0   ,  &
                   stdorb%par(iPar)%scale
      stdorb%par(iPar)%type = ""
      stdorb%par(iPar)%omega = 0.0
    ENDDO
  ENDIF
!!! write(lfnprt,*)'*******************************************************************************'
  ! Newly estimated Parameters (optional)
  ! -------------------------------------
  READ(lfnloc, IOSTAT = ios) numEstPar
  numSto_est = 0
  IF (ios == 0 .AND. numEstPar > 0) THEN
    ALLOCATE(stdorb%estPar(numEstpar), STAT=irc)
    CALL alcerr(irc, 'stdorb%estPar', (/numEstpar/), 'readStdOrbit')
    DO iPar = 1, numEstpar
      IF (version >= 2) THEN
        READ(lfnloc) stdorb%estPar(iPar)%locq ,  &
                   stdorb%estPar(iPar)%name ,    &
                   stdorb%estPar(iPar)%time ,    &
                   stdorb%estPar(iPar)%x0   ,    &
                   stdorb%estPar(iPar)%scale,    &
                   stdorb%estPar(iPar)%dx

        ! adjust values of stochastic parameters in array stdorb%orbArc(iArc)%stoch(iSto)%value
        ! *************************************************************************************
        IF(stdorb%estPar(iPar)%locq(1) == 11)THEN
           numSto_est = numSto_est + 1
        ! check whether sampling is different from 1
          IF(numSto_est ==1)THEN
            IF(stdorb%estPar(iPar)%locq(4) == 1)THEN
              sampl_typ = 1
            ELSE
              sampl_typ = 2
            ENDIF
          ENDIF
        ! If sampling is different, initialize stochastic parmaters to zero, update otherwise.
          IF(sampl_typ == 2)THEN
            stdorb%orbArc(1)%stoch(stdorb%estPar(iPar)%locq(4))%value = 0
          ENDIF
        ENDIF
      ELSE
        READ(lfnloc) stdorb%estPar(iPar)%locq ,  &
                   stdorb%estPar(iPar)%name ,    &
                   stdorb%estPar(iPar)%time ,    &
                   stdorb%estPar(iPar)%x0   ,    &
                   stdorb%estPar(iPar)%scale
      ENDIF
    ENDDO

   ! adjust values dE in array stdorb%orbArc(iArc)%stoch(iSto)%dE
   ! ------------------------------------------------------------
   ! sampling of stochastics in estimation process
    IF(numSto_est > 0)THEN
      sampl_sto = numSto/numSto_est
    ELSE
      sampl_sto = 1
    ENDIF

    DO i_Comp = 1,3
      ind_Comp=(i_Comp-1)*SIZE(stdorb%orbArc(1)%stoch)/3
      DO iEpo  = 1,SIZE(stdorb%orbArc(1)%stoch)/3
        IF(mod(iEpo,sampl_sto) ==0)THEN
          IF(iEpo == sampl_sto)THEN
            stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%dE = stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%dEds * &
                                                       stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%value
          ELSE
            stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%dE = stdorb%orbArc(1)%stoch(ind_Comp+iEpo-sampl_sto)%dE + &
                                                       stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%dEds * &
                                                       stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%value
          ENDIF
        ELSE
          IF(iEpo < sampl_sto)THEN
            stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%dE = 0.d0
          ELSE
            stdorb%orbArc(1)%stoch(ind_Comp+iEpo)%dE = stdorb%orbArc(1)%stoch(ind_Comp+(iEpo/sampl_sto)*sampl_sto)%dE
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDIF

  ! Newly estimated Stochastic (optional)
  ! -------------------------------------
  READ(lfnloc, IOSTAT = ios) numEstStc
  IF (ios == 0 .AND. numEstStc > 0) THEN
    ALLOCATE(stdorb%estStc(numEstStc), STAT=irc)
    CALL alcerr(irc, 'stdorb%estStc', (/numEstStc/), 'readStdOrbit')
    DO iSto = 1, numEstStc
      READ(lfnloc) stdorb%estStc(iSto)%typ  ,  &
                   stdorb%estStc(iSto)%comp ,  &
                   stdorb%estStc(iSto)%left ,  &
                   stdorb%estStc(iSto)%right,  &
                   stdorb%estStc(iSto)%value,  &
                   stdorb%estStc(iSto)%dE   ,  &
                   stdorb%estStc(iSto)%dEds
    ENDDO
  ENDIF

  CLOSE(lfnloc)

END SUBROUTINE

! -------------------------------------------------------------------------
! Write Standard Orbit File
! -------------------------------------------------------------------------
SUBROUTINE writeStdOrbit1(orbFileName)
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: orbFileName
  CALL writeStdOrbit2(orbFileName, stdOrb_)
END SUBROUTINE

SUBROUTINE writeStdOrbit2(orbFileName, stdOrb)
  USE m_bern,       ONLY: i4b, lfnloc, lfnerr
  USE s_stdorbit_t, ONLY: t_stdorb

  USE s_opnfil
  USE s_opnerr

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  CHARACTER(LEN=*), INTENT(IN) :: orbFileName
  TYPE(t_stdOrb),   INTENT(INOUT) :: stdOrb

  ! Local Variables
  ! ---------------
  INTEGER(i4b) :: irc, iVal, iDesc, iArc, iInt, iSto, iPar

  CALL opnfil(lfnloc,orbFileName,'UNKNOWN','UNFORMATTED',' ',' ',irc)
  CALL opnerr(lfnerr,lfnloc,irc,orbFileName,'readStdOrbit')

  WRITE(lfnloc) stdOrb%version

  WRITE(lfnloc) stdOrb%title

  IF ( .NOT.ASSOCIATED(stdOrb%modDesc)) THEN
    WRITE(lfnloc) 0
  ELSE
    WRITE(lfnloc) SIZE(stdOrb%modDesc)
    DO iDesc = 1, SIZE(stdOrb%modDesc)
      WRITE(lfnloc) stdOrb%modDesc(iDesc)%name
      WRITE(lfnloc) SIZE(stdOrb%modDesc(iDesc)%value)
      DO iVal = 1, SIZE(stdOrb%modDesc(iDesc)%value)
        WRITE(lfnloc) stdOrb%modDesc(iDesc)%value(iVal)
      ENDDO
    ENDDO
  ENDIF

  WRITE(lfnloc) SIZE(stdOrb%orbArc)
  DO iArc = 1, SIZE(stdOrb%orbArc)
    WRITE(lfnloc) stdOrb%orbArc(iArc)%sys
    WRITE(lfnloc) stdOrb%orbArc(iArc)%svn
    WRITE(lfnloc) stdOrb%orbArc(iArc)%man
    WRITE(lfnloc) stdOrb%orbArc(iArc)%tWin%t(1)%day,  &
                  stdOrb%orbArc(iArc)%tWin%t(1)%frac, &
                  stdOrb%orbArc(iArc)%tWin%t(2)%day,  &
                  stdOrb%orbArc(iArc)%tWin%t(2)%frac

    IF (.NOT. ASSOCIATED(stdOrb%orbArc(iArc)%modDesc)) THEN
      WRITE(lfnloc) 0
    ELSE
      WRITE(lfnloc) SIZE(stdOrb%orbArc(iArc)%modDesc)
      DO iDesc = 1, SIZE(stdOrb%orbArc(iArc)%modDesc)
        WRITE(lfnloc) stdOrb%orbArc(iArc)%modDesc(iDesc)%name
        WRITE(lfnloc) SIZE(stdOrb%orbArc(iArc)%modDesc(iDesc)%value)
        DO iVal = 1, SIZE(stdOrb%orbArc(iArc)%modDesc(iDesc)%value)
          WRITE(lfnloc) stdOrb%orbArc(iArc)%modDesc(iDesc)%value(iVal)
        ENDDO
      ENDDO
    ENDIF

    WRITE(lfnloc) SIZE(stdOrb%orbArc(iArc)%orbInt)
    DO iInt = 1, SIZE(stdOrb%orbArc(iArc)%orbInt)
      WRITE(lfnloc) stdOrb%orbArc(iArc)%orbInt(iInt)%left,   &
                    stdOrb%orbArc(iArc)%orbInt(iInt)%right,  &
                    stdOrb%orbArc(iArc)%orbInt(iInt)%shadow, &
                    stdOrb%orbArc(iArc)%orbInt(iInt)%hh,     &
                    stdOrb%orbArc(iArc)%orbInt(iInt)%t0,     &
                    stdOrb%orbArc(iArc)%orbInt(iInt)%qq
      WRITE(lfnloc) stdOrb%orbArc(iArc)%orbInt(iInt)%rCoe
      WRITE(lfnloc) stdOrb%orbArc(iArc)%orbInt(iInt)%drdeCoe
      WRITE(lfnloc) stdOrb%orbArc(iArc)%orbInt(iInt)%drdsCoe
    ENDDO

    IF (.NOT. ASSOCIATED(stdOrb%orbArc(iArc)%stoch)) THEN
      WRITE(lfnloc) 0
    ELSE
      WRITE(lfnloc) SIZE(stdOrb%orbArc(iArc)%stoch)
      DO iSto = 1, SIZE(stdOrb%orbArc(iArc)%stoch)
        WRITE(lfnloc) stdOrb%orbArc(iArc)%stoch(iSto)%typ,    &
                      stdOrb%orbArc(iArc)%stoch(iSto)%comp,   &
                      stdOrb%orbArc(iArc)%stoch(iSto)%left,   &
                      stdOrb%orbArc(iArc)%stoch(iSto)%right,  &
                      stdOrb%orbArc(iArc)%stoch(iSto)%value,  &
                      stdorb%orbArc(iArc)%stoch(iSto)%dE,     &
                      stdorb%orbArc(iArc)%stoch(iSto)%dEds
      ENDDO
    ENDIF

  ENDDO

  IF (.NOT. ASSOCIATED(stdOrb%par)) THEN
    WRITE(lfnloc) 0
  ELSE
    WRITE(lfnloc) SIZE(stdOrb%par)
    DO iPar = 1, SIZE(stdOrb%par)
      WRITE(lfnloc) stdOrb%par(iPar)%locq ,  &
                    stdOrb%par(iPar)%name ,  &
                    stdOrb%par(iPar)%time ,  &
                    stdOrb%par(iPar)%x0   ,  &
                    stdOrb%par(iPar)%scale
    ENDDO
  ENDIF

  ! Newly estimated Parameters (optional)
  ! -------------------------------------
  IF (ASSOCIATED(stdOrb%estPar)) THEN
    WRITE(lfnloc) SIZE(stdOrb%estPar)
    DO iPar = 1, SIZE(stdOrb%estPar)
      WRITE(lfnloc) stdOrb%estPar(iPar)%locq ,  &
                    stdOrb%estPar(iPar)%name ,  &
                    stdOrb%estPar(iPar)%time ,  &
                    stdOrb%estPar(iPar)%x0   ,  &
                    stdOrb%estPar(iPar)%scale,  &
                    stdOrb%estPar(iPar)%dx
    ENDDO
  ENDIF

  ! Newly estimated Stochastic (optional)
  ! -------------------------------------
  IF (ASSOCIATED(stdOrb%estStc)) THEN
    WRITE(lfnloc) SIZE(stdOrb%estStc)
    DO iSto = 1, SIZE(stdOrb%estStc)
      WRITE(lfnloc) stdOrb%estStc(iSto)%typ  ,  &
                    stdOrb%estStc(iSto)%comp ,  &
                    stdOrb%estStc(iSto)%left ,  &
                    stdOrb%estStc(iSto)%right,  &
                    stdOrb%estStc(iSto)%value,  &
                    stdOrb%estStc(iSto)%dE   ,  &
                    stdOrb%estStc(iSto)%dEds
    ENDDO
  ENDIF

  CLOSE(lfnloc)

END SUBROUTINE

! -------------------------------------------------------------------------
! Initialize Satellite Position
! -------------------------------------------------------------------------
SUBROUTINE initSatPos(satPos, maxDer, nDynPar)
  USE m_bern, ONLY:  i4b
  USE s_stdorbit_t, ONLY: t_satpos
  IMPLICIT NONE

  INTEGER(i4b),   INTENT(IN)    :: maxDer
  INTEGER(i4b),   INTENT(IN)    :: nDynPar
  TYPE(t_satPos), INTENT(INOUT) :: satPos

  IF(ASSOCIATED(satPos%dxvdp))THEN
    DEALLOCATE(satPos%xv)
    DEALLOCATE(satPos%dxvdele)
    DEALLOCATE(satPos%dxvdp)
  ENDIF

  IF (.NOT. ASSOCIATED(satPos%dxvdp) ) THEN
    ALLOCATE(satPos%xv(6,maxDer+1))
    ALLOCATE(satPos%dxvdele(6,6,maxDer+1))
    ALLOCATE(satPos%dxvdp(6,nDynPar))
  ENDIF

END SUBROUTINE

! -------------------------------------------------------------------------
! Get Satellite Position
! -------------------------------------------------------------------------
SUBROUTINE getSatPos1(satPos, orbFileName)
  USE m_bern, ONLY:  lfnerr
  USE s_stdorbit_t, ONLY: t_satpos

  USE s_exitrc
  IMPLICIT NONE

  TYPE(t_satPos), INTENT(INOUT) :: satPos      ! satellite position structure
  CHARACTER(LEN=*), OPTIONAL    :: orbFileName ! name of the orbit file
  LOGICAL, SAVE                 :: first = .TRUE.
  IF (first) THEN
    first = .FALSE.
    IF (.NOT. fileRead_) THEN
      IF (.NOT. PRESENT(orbFileName)) THEN
        WRITE(lfnerr,'(A)') " *** stdorbit: file name requested"
        CALL exitrc(2)
      ENDIF
      CALL readStdOrbit(orbFileName)
    ENDIF
  ENDIF
  CALL getSatPos2(stdOrb_, satPos)
END SUBROUTINE

SUBROUTINE getSatPos2(stdOrb, satPos, irc, skipStoch)
  USE m_bern,   ONLY: i4b, r8b, lfnerr, fileNameLength
  USE m_maxdim, ONLY: maxVar
  USE m_epoch,  ONLY: t_epoch, &
                      OPERATOR(.EpochtoReal.), OPERATOR(+), OPERATOR(-), &
                      isInWin
  USE s_stdorbit_t, ONLY: t_stdOrb, t_satpos, t_orbInt

  USE s_exitrc
  USE s_polevn
  USE s_getorb
  USE s_prtder

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  TYPE(t_stdOrb), INTENT(IN)            :: stdOrb
  TYPE(t_satPos), INTENT(INOUT)         :: satPos ! sat. position structure
  INTEGER(i4b),   INTENT(OUT), OPTIONAL :: irc
  LOGICAL,        INTENT(IN),  OPTIONAL :: skipStoch

  ! Local Variables
  ! ---------------
  LOGICAL                       :: skipStoch_local
  CHARACTER(LEN=fileNameLength) :: chdumm = ' '
  CHARACTER(LEN=8)              :: anltyp
  INTEGER(i4b)                  :: iArc, iInt, iDer, iEle, istc, istc_epo
  INTEGER(i4b)                  :: numSto, ii, icrarc, iorsys, ircode
  INTEGER(i4b)                  :: nvar, nrad
  REAL(r8b)                     :: epsDay, epsSec, sec, tpol
  REAL(r8b), DIMENSION(6)       :: stc_corr
  REAL(r8b)                     :: tMjd, tOsc
  REAL(r8b), DIMENSION(7)       :: ele
  REAL(r8b), DIMENSION(maxVar)  :: rprpar
  TYPE(t_orbInt), POINTER       :: orbInt, orbIntPart
  TYPE(t_epoch)                 :: t0, t0Part

  ! Default return code
  ! -------------------
  IF ( PRESENT(irc) ) THEN
    irc = 0
  ENDIF

  ! Special Case - use old routine GETORB
  ! -------------------------------------
  IF (stdOrb%useOldGetOrb) THEN
    iDer = SIZE(satPos%xv,dim=2) - 1
    tMjd = .EpochtoReal.(satPos%epoch)
    satPos%xv      = 0.0
    satPos%dxvdele = 0.0
    CALL getOrb(satPos%svn, 0, ider, 1, tMjd, icrarc, iorsys, satPos%xv, &
                tOsc, ele, ircode)
    DO iEle = 1,6
      CALL prtder(chdumm, satPos%svn, iEle, 1, 1, tMjd, 1, icrarc, iorsys, nvar, &
                  nrad, satPos%dxvdele(:,iEle,1), ele, rprpar, anltyp, ircode)
    ENDDO
    RETURN
  ENDIF

  ! Find the corresponding arc and interval
  ! ---------------------------------------
  NULLIFY(orbInt)
  epsSec = 0.1
  epsDay = epsSec/86400.0
  Loop_iArc: DO iArc = 1, SIZE(stdOrb%orbArc)
    IF (stdOrb%orbArc(iArc)%sys   == satPos%sys  .AND. &
        stdOrb%orbArc(iArc)%svn   == satPos%svn  .AND. &
        isInWin(satPos%epoch, stdOrb%orbArc(iArc)%tWin, epsDay)) THEN

      sec = (satPos%epoch - stdOrb%orbArc(iArc)%tWin%t(1)) * 86400.0d0
      DO iInt = 1, SIZE(stdOrb%orbArc(iArc)%orbInt)
        IF (sec >= stdOrb%orbArc(iArc)%orbInt(iInt)%left  - epsSec .AND. &
            sec <  stdOrb%orbArc(iArc)%orbInt(iInt)%right + epsSec ) THEN
          orbInt => stdOrb%orbArc(iArc)%orbInt(iInt)
          t0 = stdOrb%orbArc(iArc)%tWin%t(1) + &
               stdOrb%orbArc(iArc)%orbInt(iInt)%t0 / 86400.0
          EXIT Loop_iArc
        ENDIF
      ENDDO

    ENDIF
  ENDDO Loop_iArc

  IF (.NOT.ASSOCIATED(stdOrb%orbArcPart)) THEN
    orbIntPart => orbInt
    t0Part = t0
  ELSE
    NULLIFY(orbIntPart)
    Loop_iArcPart: DO iArc = 1, SIZE(stdOrb%orbArcPart)
      IF (stdOrb%orbArcPart(iArc)%sys   == satPos%sys  .AND. &
          stdOrb%orbArcPart(iArc)%svn   == satPos%svn  .AND. &
          isInWin(satPos%epoch, stdOrb%orbArcPart(iArc)%tWin, epsDay)) THEN

        sec = (satPos%epoch - stdOrb%orbArcPart(iArc)%tWin%t(1)) * 86400.0d0
        DO iInt = 1, SIZE(stdOrb%orbArcPart(iArc)%orbInt)
          IF (sec >= stdOrb%orbArcPart(iArc)%orbInt(iInt)%left  - epsSec .AND. &
              sec <  stdOrb%orbArcPart(iArc)%orbInt(iInt)%right + epsSec ) THEN
            orbIntPart => stdOrb%orbArcPart(iArc)%orbInt(iInt)
            t0Part = stdOrb%orbArcPart(iArc)%tWin%t(1) + &
                     stdOrb%orbArcPart(iArc)%orbInt(iInt)%t0 / 86400.0
            EXIT Loop_iArcPart
          ENDIF
        ENDDO

      ENDIF
    ENDDO Loop_iArcPart
  ENDIF

  IF ( (.NOT. ASSOCIATED(orbInt)) .OR. (.NOT. ASSOCIATED(orbIntPart)) ) THEN
    IF ( PRESENT(irc) ) THEN
      irc = 1
      RETURN
    ELSE
      WRITE(lfnerr,'(A)') " *** stdorbit: orbit not found"
      CALL exitrc(2)
    ENDIF
  ENDIF
  iDer    = SIZE(satPos%xv,dim=2) - 1

  tpol    = (satPos%epoch - t0) * 86400.0
  if(dabs(tpol-dnint(tpol)) < 1.d-6)tpol=dnint(tpol)
  CALL polevn(iDer+1, orbInt%qq, 3, tpol, orbInt%hh, orbInt%rCoe, satPos%xv)

  tpol    = (satPos%epoch - t0Part) * 86400.0
  if(dabs(tpol-dnint(tpol)) < 1.d-6)tpol=dnint(tpol)
  DO iEle = 1, 6
    CALL polevn(iDer+1, orbIntPart%qq, 3, tpol, orbIntPart%hh, &
              orbIntPart%drdeCoe(:,iEle,:), satPos%dxvdele(:,iEle,:))
  ENDDO

  ! Add stochastic part of pos/vel vector
  ! -------------------------------------
  IF (PRESENT(skipStoch)) THEN
    skipStoch_local = skipStoch
  ELSE
    skipStoch_local = .FALSE.
  ENDIF

  IF (.NOT. skipStoch_local) THEN
    IF (ASSOCIATED(stdOrb%orbArc(iarc)%stoch)) THEN
      numSto = SIZE(stdOrb%orbArc(iarc)%stoch)
      ! get correct stochastic interval
      istc_epo = -1
          ii=0
          IF(stdorb%orbArc(iArc)%stoch(1)%typ == 'A')THEN
            ii=1
          ENDIF

      DO iStc = 1, numSto/3 - ii
        IF (satPos%epoch%day+satPos%epoch%frac + epsSec/10/86400 < &
            stdorb%orbArc(iArc)%stoch(iStc)%right) THEN
          istc_epo = iStc-1
          EXIT
        ENDIF
      ENDDO

      IF(istc_epo == -1)THEN
        istc_epo=numsto/3 - ii
      ENDIF

      ! add contribution of stochastic parms to position & velocity vector
      stc_corr=0
      DO ii = 1,3
        DO iEle = 1,6
          IF (istc_epo > 0) THEN
            stc_corr(1:6) = stc_corr(1:6) + &
              stdorb%orbArc(iArc)%stoch(iStc_epo+((ii-1)*numSto/3))%dE(iEle) * &
              satPos%dxvdele(1:6,iEle,1)
          ENDIF
        ENDDO
      ENDDO
      satPos%xv(1:6,1) = satPos%xv(1:6,1) + stc_corr(1:6)
    ENDIF
  ENDIF

END SUBROUTINE

SUBROUTINE getDxvDp(act_sat, par, satPos, StdOrb)

  USE m_bern,      ONLY: i4b, r8b
  USE m_epoch,     ONLY: OPERATOR(.EpochtoReal.)
  USE d_satdynmod, ONLY: m_albedo, m_ap, m_cdrag, m_qdm, m_crad
  USE d_par,       ONLY: t_par
  USE p_gravdet,   ONLY: m_nvar, m_fromto, m_locq, m_iarc, m_beta, &
                         m_numsat, m_prcopt
  USE s_gravField, ONLY: gravField
  USE s_stdorbit_t,ONLY: t_satPos, t_stdOrb

  USE s_ni_dynpartial
  USE s_alcerr
  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  INTEGER(i4b)                                :: act_sat ! satellite number to be treated/integrated
  TYPE(t_satPos),     DIMENSION(:)            :: satPos ! standard otbit position/partials
  TYPE(t_stdOrb),     DIMENSION(:)            :: stdOrb ! standard orbit description
  TYPE(t_par),        DIMENSION(:),   POINTER  :: par    ! estimated parameters

  ! Local Variables
  ! ---------------
  LOGICAL,      SAVE                           :: first = .TRUE.
  INTEGER(i4b), SAVE                           :: iter   ! iteration number
  INTEGER(i4b), SAVE                           :: qvar   ! integration order
  INTEGER(i4b), SAVE                           :: nshad  !
  INTEGER(i4b)                                 :: nPar   ! number of parameters in parameter estimation process
  INTEGER(i4b)                                 :: iPar, kpar ! loop index
  INTEGER(i4b)                                 :: irc, iac ! return code
  INTEGER(i4b)                                 :: sat_number = 0 ! satellite number

  REAL(r8b), SAVE                              :: stpvar ! step size for integration
  REAL(r8b)                                    :: timsav_frac, timsav_day ! save obs epoch
  REAL(r8b), DIMENSION(:,:),   ALLOCATABLE, SAVE :: tshad  ! shadow epochs
  REAL(r8b), DIMENSION(:,:,:), ALLOCATABLE, SAVE :: deledp

  IF (first) THEN
    first = .FALSE.
! copy information into gravdet-structure
! (a) locq for numerical integration, set max. degree for gravity field parms
    m_iarc=1
    nPar = SIZE(par)
    kPar=0
    m_prcopt(4) = 0
    do ipar=1,npar
      IF(par(iPar)%locq(1) == 3 .OR. par(iPar)%locq(1) == 13)THEN
        IF(par(iPar)%locq(1) == 3 .AND. sat_number == 0)THEN
          sat_number = par(iPar)%locq(3)
        ENDIF
  !
        IF((par(iPar)%locq(1) == 3 .AND. par(iPar)%locq(3) == sat_number .AND. par(iPar)%locq(4) > 6)  &
            .OR.                         par(iPar)%locq(1) == 13)THEN
          kPar=kPar+1
          m_locq(1:7,kPar,m_iarc) = par(ipar)%locq(1:7)
          IF(par(ipar)%locq(1) == 13)THEN
            IF(par(ipar)%locq(5) > m_prcopt(4))THEN
              m_prcopt(4) = par(ipar)%locq(5)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO

!  (a.1) number of dynamical parameters (variational equations)
    m_nvar = kPar

! (a.2) satellite number, arc boundaries
    m_numsat = sat_number

! (c) initial parameter values for radiation pressure and drag
    m_albedo = 0
    m_ap     = 0
    m_beta   = 0
    m_cdrag  = 0
    m_qdm    = 0
    m_crad   = 0
    gravField%cApr = 0
    gravField%sApr = 0
    gravField%maxDegree = 0
! (d) miscellaneous parameters
    nshad    =  0   ! currently true (no parms, for which shadow boundaries are important
    iter     =  1   ! iteration number (always 1 in orbdiff)
    qvar     = 20   ! order for numerical quadrature
    stpvar   = 0.05D0 ! stepsize for numerical quadrature (in hours)
! (e) allocate values of intergrals (six per parameter)
    ALLOCATE(deledp(6,m_nvar,2),stat=iac)
    CALL alcerr(iac,'deledp',(/6,m_nvar,2/),'STDORBIT')
! if necessary, allocate shadow boundaries
    if (nshad > 0) THEN
      ALLOCATE( tshad(2,nshad), stat=iac )
      CALL alcerr(iac,'tshad',(/2,nshad/),'STDORBIT')
      tshad = 0.0  ! TODO
    ENDIF

  ENDIF
! set arc boundaries
  m_fromto(1) = .EpochtoReal.(stdorb(act_sat)%orbArc(1)%tWin%t(1))
  m_fromto(2) = .EpochtoReal.(stdorb(act_sat)%orbArc(1)%tWin%t(2))

! perform numerical integration up to time .epochToReal.(satPos%epoch)
! all m_nvar integrals are evaluated, result is stored in array satPos%dxvdp

  timsav_day = satPos(act_sat)%epoch%day
  timsav_frac = satPos(act_sat)%epoch%frac

  CALL ni_dynpartial(act_sat, iter, qvar, stpvar, nshad, tshad, &
                     .epochToReal.(satPos(act_sat)%epoch), deledp(:,:,act_sat), satpos, stdorb)

  satPos(act_sat)%epoch%day = timsav_day
  satPos(act_sat)%epoch%frac= timsav_frac
  CALL getSatPos(stdOrb(act_sat), satPos(act_sat), irc)

  satPos(act_sat)%dxvdp = MATMUL(satPos(act_sat)%dxvdele(:,:,1), deledp(:,:,act_sat))

END SUBROUTINE

! This routine should replace the old sr HILDER
! ---------------------------------------------
SUBROUTINE getDxvDp2(iPar, locq, svn, tBound, tMjd, xNorm, drdp)

  USE m_bern,  ONLY: i4b, r8b
  USE m_epoch, ONLY: OPERATOR(.realToEpoch.)
  USE d_par,   ONLY: t_par, init_techn, add_techn
  USE s_gravField,  ONLY: init_pot, alloc_pot
  USE s_stdorbit_t, ONLY: t_satpos
  USE s_stdOrb2new

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  INTEGER(i4b),                 INTENT(IN)  :: iPar   ! parameter index
  INTEGER(i4b), DIMENSION(:,:), INTENT(IN)  :: locq   ! parameter description
  INTEGER(i4b),                 INTENT(IN)  :: svn    ! satellite number
  REAL(r8b),    DIMENSION(2),   INTENT(IN)  :: tBound ! arc boundaries
  REAL(r8b),                    INTENT(IN)  :: tMjd   ! time of request
  REAL(r8b),                    INTENT(IN)  :: xNorm  ! normalization factor
  REAL(r8b),    DIMENSION(:),   INTENT(OUT) :: drdp   ! partials

  ! Local Variables
  ! ---------------
  LOGICAL, SAVE                               :: first = .TRUE.
  INTEGER(i4b), DIMENSION(:), POINTER, SAVE   :: dynParIndex
  INTEGER(i4b)                                :: nPar, nDynPar, ii
  TYPE(t_satPos), DIMENSION(1), SAVE          :: satPos ! std. orbit position/partials
  TYPE(t_par),    DIMENSION(:), POINTER, SAVE :: par    ! estimated parameters

  IF (first) THEN
    first = .false.
    CALL init_pot
    CALL alloc_pot

    ! Count Number of Dynamic Parameters (Earth Potential Coefficients)
    ! -----------------------------------------------------------------
    nPar = SIZE(locq,2)
    ALLOCATE(dynParIndex(nPar))
    nDynPar = 0
    DO ii = 1, nPar
      IF (locq(1,ii) == 13) THEN
        nDynPar = nDynPar +1
        dynParIndex(ii) = nDynPar
      ENDIF
    ENDDO
    ALLOCATE(par(nDynPar))
    CALL initSatPos(satPos(1), 1, nDynPar)

    ! Prepare the Parameter Description Structures
    ! --------------------------------------------
    nDynPar = 0
    DO ii = 1, nPar
      IF (locq(1,ii) == 13) THEN
        nDynPar = nDynPar +1
        par(nDynPar)%locq        = locq(:,ii)
        par(nDynPar)%name        = ""
        par(nDynPar)%time%mean   = 0.0
        par(nDynPar)%time%half   = 0.0
        par(nDynPar)%x0          = 0.0
        par(nDynPar)%scale       = xNorm
        CALL init_techn(par(nDynPar))
        CALL add_techn(par(nDynPar), gnss=1, gps=1, glo=1, slr=1)
        par(nDynPar)%obstim%t(nDynPar) = 0.0
        par(nDynPar)%obstim%t(2) = 0.0
        par(nDynPar)%type        = ""
        par(nDynPar)%omega       = 0.0
      ENDIF
    ENDDO
  ENDIF

  ! Satellite Position Structure (output of getSatPos)
  ! --------------------------------------------------
  satPos(1)%sys   = 'G'
  satPos(1)%svn   = svn
  satPos(1)%epoch = .realToEpoch.(tMjd)

  CALL getDxvDp(1, par, satPos, StdOrb2New)

  drdp = satPos(1)%dxvdp(1:3, dynParIndex(iPar)) / xnorm;

END SUBROUTINE

END MODULE
