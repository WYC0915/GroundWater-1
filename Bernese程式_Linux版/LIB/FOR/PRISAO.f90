MODULE s_PRISAO
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prisao(parSao)

! -------------------------------------------------------------------------
! Purpose:    Prints a priori information of satellite antenna offsets
!             (from neq) into ADDNEQ2 output file
!
! Author:     C. Urschl
!
! Created:    10-Dec-2002
!
! Changes:    23-Dec-2003 RS: Print gnroff instead of grpoff
!             04-Oct-2006 AG: Print groups for sigma
!             16-Jul-2008 RD: no message if a satellite missing in a NEQ
!             06-May-2009 RD: General revision (considering multi-year sol.)
!             17-May-2011 LM: Correct call of LISTI4
!             05-Mar-2012 RD: Use listi4 as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lineLength
  USE p_addneq, ONLY: t_parSao,maxoff,maxsat
  USE f_listi4
  USE s_dimtst
  USE s_iordup
  USE s_clrflg
  USE s_setflg
  USE f_tstflg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_parSao), DIMENSION(:), POINTER :: parSao ! Sat.ant.offset inp. opt.

! input/output:

! output:


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=1), DIMENSION(2), PARAMETER :: yesno = (/ 'N','Y' /)

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)                 :: line
  CHARACTER(LEN=1)                          :: grpMsg

  INTEGER(i4b)                              :: nOff
  INTEGER(i4b), DIMENSION(maxoff)           :: grpLst
  INTEGER(i4b), DIMENSION(maxoff)           :: idx
  INTEGER(i4b), DIMENSION(maxsat)           :: idxSa1,idxSa2
  INTEGER(i4b)                              :: iGrp,jGrp,fGrp
  INTEGER(i4b)                              :: irc
  INTEGER(i4b)                              :: iSat
  INTEGER(i4b)                              :: iFil
  INTEGER(i4b)                              :: ii
  INTEGER(i4b)                              :: diff
  INTEGER(i4b)                              :: from


  nOff   = 0
  grpLst = 0
  DO iFil = 1, SIZE(parSao)
    DO iGrp = 1,parSao(iFil)%nanoff
      jGrp = listi4(1,maxOff,grpLst,parSao(iFil)%gnroff(iGrp),nOff)
      IF (jGrp == 0) THEN
        CALL DIMTST(1,2,2,'PRISAO','MAXOFF','SATELLITE ANTENNE OFFSETS', &
                    'Defined in module "P_ADDNEQ.f90"',nOff,maxoff,irc)
      ENDIF
    ENDDO
  ENDDO

  IF (nOff > 0) THEN

! Print title for satellite antenna offset listing
! ------------------------------------------------
    WRITE(lfnprt,'(3(/,A))')                                   &
      '        Component',                                     &
      ' Group   X  Y  Z      Files      #Sat Satellite numbers', &
      ' -----------------------------------------------'//     &
      '------------------------------------------------'//     &
      '------------------------------------'
    CALL iordup(grpLst,nOff,idx)

    DO iGrp = 1,nOff
      CALL setflg(grpMsg,0)
      CALL setflg(grpMsg,1)

      from = 0
      DO iFil = 1,SIZE(parSao)
        DO jGrp = 1,parSao(iFil)%nanoff
          IF ( parSao(iFil)%gnroff(jGrp) /= grpLst(idx(iGrp)) ) CYCLE
          diff = 1
          IF (from /= 0) THEN
            diff = 0
            IF ( parSao(from)%nsaoff(fGrp) /= parSao(iFil)%nsaoff(jGrp) ) THEN
              IF (parSao(from)%nsaoff(fGrp) /= 0 .AND. &
                  parSao(iFil)%nsaoff(jGrp) /= 0) THEN
                IF ( tstflg(grpmsg,0) ) THEN
                  WRITE (lfnerr,'(2(/,A),I4,A,/,A,/)')                                &
                    '### SR PRISAO: The number of satellites in satellite',         &
                    '               antenna offset group ',grpLst(idx(iGrp)),       &
                                                                    ' differs!',    &
                    '               This could cause some stacking problems.'
                  CALL clrflg(grpmsg,0)
                ENDIF
                diff = 1
              ENDIF
            ELSE
              CALL iordup(parSao(iFil)%satoff(:,jGrp), &
                          parSao(iFil)%nsaoff(jGrp),idxsa2)
              DO iSat = 1,parSao(from)%nsaoff(fGrp)
                IF (parSao(from)%satoff(idxsa1(iSat),fGrp) /=                    &
                    parSao(iFil)%satoff(idxsa2(iSat),jGrp)) THEN
                  IF ( tstflg(grpmsg,1) ) THEN
                    WRITE (lfnerr,'(2(/,A),I4,A,/,A,/)')                               &
                      '### SR PRISAO: There are different satellites in satellite',  &
                      '               antenna offset group ', grpLst(idx(iGrp)), '!',&
                      '               This could cause some stacking problems.'
                    CALL clrflg(grpmsg,1)
                  ENDIF
                  diff = 1
                ENDIF
              ENDDO
            ENDIF
          ENDIF

          IF ( from == 0 .OR. diff /= 0) THEN
            IF (from /= 0 ) WRITE(lfnprt,'(A)') TRIM(line)
            from = iFil
            fGrp = jGrp
            CALL iordup(parSao(iFil)%satoff(:,jGrp), &
                        parSao(iFil)%nsaoff(jGrp),idxsa1)

            WRITE(line,'(I4,5X,3(A1,2X),I4,A,I4,2X,I4,1X,60I4)')   &
              grpLst(idx(iGrp)),                                &
              (yesno(parSao(iFil)%santoff(jGrp,ii)+1),ii=1,3),  &
              iFil,' to ',iFil,parSao(iFil)%nsaoff(jGrp),       &
              (parSao(iFil)%satoff(idxSa1(ii),jGrp),            &
                    ii=1,parSao(iFil)%nsaoff(jGrp))
          ELSE
            WRITE(line(27:30),'(I4)') iFil
          ENDIF

        ENDDO
      ENDDO
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDDO
    WRITE(lfnprt,'(/)')

  ENDIF
END SUBROUTINE prisao

END MODULE

