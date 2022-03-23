MODULE s_PRISAP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prisap(parSap)

! -------------------------------------------------------------------------
! Purpose:    Prints a priori information of satellite antenna patterns
!             (from neq) into ADDNEQ2 output file
!
! Author:     C. Urschl, R. Schmid
!
! Created:    23-Dec-2003
! Last mod.:  17-May-2011
!
! Changes:    04-Oct-2006 AG: number of azimuth corrected
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
  USE p_addneq, ONLY: t_parSap,maxoff,maxsat
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
  TYPE(t_parSap), DIMENSION(:), POINTER :: parSap ! Sat.ant.pattern inp. opt.

! input/output:

! output:


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)                 :: line
  CHARACTER(LEN=1)                          :: grpMsg

  INTEGER(i4b)                              :: nSpv
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




  nSpv   = 0
  grpLst = 0
  DO iFil = 1, SIZE(parSap)
    DO iGrp = 1,parSap(iFil)%nanspv
      jGrp = listi4(1,maxOff,grpLst,parSap(iFil)%gnrspv(iGrp),nSpv)
      IF (jGrp == 0) THEN
        CALL DIMTST(1,2,2,'PRISAP','MAXOFF','SATELLITE ANTENNE PATTERN', &
                    'Defined in module "P_ADDNEQ.f90"',nSpv,maxoff,irc)
      ENDIF
    ENDDO
  ENDDO

  IF (nSpv > 0) THEN

! Print title for satellite antenna offset listing
! ------------------------------------------------
    WRITE(lfnprt,'(3(/,A))')                                         &
          '        # Points',                                        &
          ' Group   El. Az.        Files   #Sat Satellite numbers',  &
          ' -----------------------------------------------'//       &
          '------------------------------------------------'//       &
          '------------------------------------'
    CALL iordup(grpLst,nSpv,idx)

    DO iGrp = 1,nSpv
      CALL setflg(grpMsg,0)
      CALL setflg(grpMsg,1)

      from = 0
      DO iFil = 1,SIZE(parSap)
        DO jGrp = 1,parSap(iFil)%nanspv
          IF ( parSap(iFil)%gnrspv(jGrp) /= grpLst(idx(iGrp)) ) CYCLE
          diff = 1
          IF (from /= 0) THEN
            diff = 0
            IF ( parSap(from)%nsaspv(fGrp) /= parSap(iFil)%nsaspv(jGrp) ) THEN
              IF (parSap(from)%nsaspv(fGrp) /= 0 .AND. &
                  parSap(iFil)%nsaspv(jGrp) /= 0) THEN
                IF ( tstflg(grpmsg,0) ) THEN
                  WRITE (lfnerr,'(2(/,A),I4,A,/,A,/)')                                &
                    '### SR PRISAP: The number of satellites in satellite',         &
                    '               antenna pattern group ',grpLst(idx(iGrp)),       &
                                                                    ' differs!',    &
                    '               This could cause some stacking problems.'
                  CALL clrflg(grpmsg,0)
                ENDIF
                diff = 1
              ENDIF
            ELSE
              CALL iordup(parSap(iFil)%satspv(:,jGrp), &
                          parSap(iFil)%nsaspv(jGrp),idxsa2)
              DO iSat = 1,parSap(from)%nsaspv(fGrp)
                IF (parSap(from)%satspv(idxsa1(iSat),fGrp) /=                    &
                    parSap(iFil)%satspv(idxsa2(iSat),jGrp)) THEN
                  IF ( tstflg(grpmsg,1) ) THEN
                    WRITE (lfnerr,'(2(/,A),I4,A,/,A,/)')                               &
                      '### SR PRISAP: There are different satellites in satellite',  &
                      '               antenna pattern group ', grpLst(idx(iGrp)), '!',&
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
            CALL iordup(parSap(iFil)%satspv(:,jGrp), &
                        parSap(iFil)%nsaspv(jGrp),idxsa1)

            WRITE(line,'(I4,5X,I3,1X,I3,2X,I4,A,I4,2X,I4,1X,60I4)')   &
              grpLst(idx(iGrp)),                                &
              parSap(iFil)%nptspv(1,jGrp),                      &
              parSap(iFil)%nptspv(2,jGrp)-1,iFil,' to ',iFil,   &
              parSap(iFil)%nsaspv(jGrp),                        &
              (parSap(iFil)%satspv(idxSa1(ii),jGrp),            &
                    ii=1,parSap(iFil)%nsaspv(jGrp))
          ELSE
            WRITE(line(27:30),'(I4)') iFil
          ENDIF

        ENDDO
      ENDDO
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDDO
    WRITE(lfnprt,'(/)')

  ENDIF
END SUBROUTINE prisap

END MODULE

