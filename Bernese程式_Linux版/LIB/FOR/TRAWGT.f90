MODULE s_TRAWGT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE trawgt(neq,iPar,ipart,ifil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine applies N, E, and Up constraints on
!             GNSS-specific station translation parameters
!
! Author:     M. Meindl
!
! Created:    03-Dez-2010
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE f_ikf
  USE s_err3d
  USE f_gtweight
  USE f_istobeel
  USE s_xyzell
  IMPLICIT NONE


! List of parameters
! ------------------
  TYPE(t_neq)                        :: neq
  INTEGER(i4b)                       :: iPar
  INTEGER(i4b)                       :: ipart
  INTEGER(i4b)                       :: ifil


! Local variables
! ---------------
  INTEGER(i4b)                                 :: ik
  INTEGER(i4b),DIMENSION(3)                    :: indPar
  INTEGER(i4b)                                 :: ii, jj, iSta

  REAL(r8b),DIMENSION(3,3)                     :: wgtNeu, wgtXyz
  REAL(r8b),DIMENSION(3)                       :: xyz, neu
  REAL(r8b)                                    :: radius, cosPhi



! Initialize some variables
! -------------------------
  wgtNeu = 0.d0


! Check for complete parameter set
! --------------------------------
  IF (neq%par(iPar+1)%name    /= neq%par(iPar)%name    .OR.      &
      neq%par(iPar+1)%locq(4) /= neq%par(iPar)%locq(4) .OR.      &
      neq%par(iPar+1)%locq(1) /= 30                    .OR.      &
      neq%par(iPar+1)%locq(3) /= 2                     .OR.      &
      neq%par(iPar+2)%name    /= neq%par(iPar)%name    .OR.      &
      neq%par(iPar+2)%locq(4) /= neq%par(iPar)%locq(4) .OR.      &
      neq%par(iPar+2)%locq(1) /= 30                    .OR.      &
      neq%par(iPar+2)%locq(3) /= 3                          ) THEN

    WRITE(lfnerr,'(/,A,/,A,/,2A,/,A,I2,/)')                                   &
     ' ### SR TRAWGT: Incomplete set of GNSS-specific station translations.', &
     '                No constraints applied.',                               &
     '                Station name: ',TRIM(neq%par(iPar)%name),               &
     '                GNSS        : ',neq%par(iPar)%locq(4)
    RETURN
  ENDIF


! Get N, E, and U coordinate of corresponding station
! ---------------------------------------------------
  iSta = 0
  DO ii=1,neq%misc%nPar
    IF (neq%par(ii)%locq(1) == 1 .AND. &
        neq%par(ii)%locq(3) == 1 .AND. &
        neq%par(ii)%name    == neq%par(iPar)%name) THEN
      iSta = ii
      EXIT
    ENDIF
  ENDDO

! Check for complete coordinate set
  IF (iSta == 0) THEN
    WRITE(lfnerr,'(/,A,/,A,/,2A,/)')                                        &
     ' ### SR TRAWGT: No corresponding station in normal equation system.', &
     '                No constraints applied.',                             &
     '                Station name: ',TRIM(neq%par(iPar)%name)
    RETURN

  ELSEIF (neq%par(iSta+1)%name    /= neq%par(iSta)%name .OR. &
          neq%par(iSta+1)%locq(1) /= 1                  .OR. &
          neq%par(iSta+1)%locq(3) /= 2                  .OR. &
          neq%par(iSta+1)%locq(4) == 2                  .OR. &
          neq%par(iSta+2)%name    /= neq%par(iSta)%name .OR. &
          neq%par(iSta+2)%locq(1) /= 1                  .OR. &
          neq%par(iSta+2)%locq(3) /= 3                  .OR. &
          neq%par(iSta+2)%locq(4) == 2                      ) THEN

    WRITE(lfnerr,'(/,A,/,A,/,2A,/)')                              &
     ' ### SR TRAWGT: Incomplete station coordinate set.',        &
     '                No constraints applied.',                   &
     '                Station name: ',TRIM(neq%par(iPar)%name)
    RETURN
  ENDIF


! North, East, Up coordinates
! ---------------------------
  xyz(1) = neq%par(iSta)%x0
  xyz(2) = neq%par(iSta+1)%x0
  xyz(3) = neq%par(iSta+2)%x0

  CALL xyzell(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,               &
              datum%scEll,xyz,neu)

  radius = SQRT(xyz(1)**2+xyz(2)**2+xyz(3)**2)
  cosPhi = COS(neu(1))
  IF (cosPhi==0.d0) cosPhi = 1


! Get weights
! -----------
  wgtNeu(1,1) = gtweight(neq%par(ipar),'A')/radius**2
  wgtNeu(2,2) = gtweight(neq%par(ipar+1),'A')/(radius*cosPhi)**2
  wgtNeu(3,3) = gtweight(neq%par(ipar+2),'A')
  IF (wgtNeu(1,1) > 0d0) THEN
    neq%misc%npseu = neq%misc%npseu + 1
    IF (isToBeEl(neq%par(ipar),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1
  ENDIF
  IF (wgtNeu(2,2) > 0d0) THEN
    neq%misc%npseu = neq%misc%npseu + 1
    IF (isToBeEl(neq%par(ipar+1),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1
  ENDIF
  IF (wgtNeu(3,3) > 0d0) THEN
    neq%misc%npseu = neq%misc%npseu + 1
    IF (isToBeEl(neq%par(ipar+2),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1
  ENDIF


! Create constraint matrix
! ------------------------
  CALL err3d(neu(1),neu(2),neu(3),datum%aEll,datum%bEll,1,wgtXyz,wgtNeu)


! Add weights to normal equation
! ------------------------------
  indPar = (/iPar,iPar+1,iPar+2/)
  DO ii=1,3
    DO jj=ii,3
      IF (wgtXyz(ii,jj)==0.d0) CYCLE
      ik = ikf(indPar(ii),indPar(jj))
      neq%aNor(ik) = neq%aNor(ik)+wgtXyz(ii,jj)
    END DO
  END DO


! The end
! -------
  RETURN
END SUBROUTINE trawgt


END MODULE s_TRAWGT
