MODULE s_GPHECM2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE GPHECM2(ANTTYP,IANT,STRSIN,pcvMod,TYPE,ADOPTED,INDIVID)

! -------------------------------------------------------------------------
! PURPOSE    :  Read model name (e.g."IGS_01"), PCV model and model type
!               gives info if antenna adopted or not
!
! PARAMETERS :
!         IN :  ANTTYP : Antenna name              CH*20
!               IANT   : Antenna number             I*6
!        OUT :  STRSIN : Model name                CH*10
!               pcvMod : Antenna calibration type  CH*1
!                        E = elevation dependent
!                        F = full
!               TYPE   : Antenna model             CH*1
!                        R = relativ
!                        A = absolute
!               ADOPTED: Return 1 if antenna is
!                        adopted from NONE          I*1
!
! SR CALLED  :  RDAPHC, GTFLNA
!
! REMARKS    :  Complete redesign of GPHECM.f
!
! AUTHOR     :  A.GAEDE
!
! VERSION    :  5.1
!
! CREATED    :  23-MAR-2006           LAST MODIFIED : 05-Dez-2006
!
! CHANGES    :  24-Apr-2006 AG: SAVE attribute for variables added
!                               PRESENT function for ADOPTED
!                               IANTEN test for AOAD/M_T added
!               05-Dez-2006 AG: INDIVID added
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      1996      UNIVERSITY OF BERN
!                    SWITZERLAND
!
! -------------------------------------------------------------------------

  USE m_bern
  USE s_rdaphc
  USE s_gtflna
  IMPLICIT NONE
!
! DECLARATIONS INSTEAD OF IMPLICIT
! --------------------------------
  INTEGER(i4b)      :: IOSTAT, IRC, IANT, IELV, IAZI, IFRQ
  INTEGER(i4b)      :: MAXAZI, MAXELV, MAXRCV, IRCV
  INTEGER(i4b),SAVE :: NRCV
  LOGICAL,SAVE      :: first =.TRUE.
!
  PARAMETER (MAXRCV=500,MAXELV=19,MAXAZI=73)
!
  CHARACTER(LEN=80)      ::  TITLE,FILINFO
  CHARACTER(LEN=32)      ::  FILPHC
  CHARACTER(LEN=20)      ::  RECTPI(MAXRCV)
  CHARACTER(LEN=20),SAVE ::  ANTTPI(MAXRCV)
  CHARACTER(LEN=20),SAVE ::  METHOD(MAXRCV)
  CHARACTER(LEN=10)      ::  atDATE(MAXRCV)
  CHARACTER(LEN=10),SAVE ::  SINEX(MAXRCV)
!
  REAL(r8b),SAVE         ::  ANTPCV(MAXELV,MAXAZI,2,MAXRCV)
  REAL(r8b)              ::  ANTOFF(3,2,MAXRCV)
!
  INTEGER(i4b)           ::  NFRANT(MAXRCV),MAXZEN(MAXRCV)
  INTEGER(i4b),SAVE      ::  MODTYP(MAXRCV),NPCV(2,MAXRCV),IANTEN(2,MAXRCV)
  INTEGER(i4b),OPTIONAL  ::  ADOPTED
  INTEGER(i4b),OPTIONAL  ::  INDIVID
!
  CHARACTER(LEN=20)      ::  ANTTYP
  CHARACTER(LEN=10)      ::  STRSIN
  CHARACTER(LEN=1)       ::  pcvMod, TYPE

!
! INITIALISATION
! --------------
  STRSIN='          '
  IF(PRESENT(ADOPTED)) ADOPTED=0
  IF(PRESENT(ADOPTED)) INDIVID=0
!
! OPEN BERNESE PHASE FILE AT FIRST CALL
! ----------------------------------------------------------------
  IF (first) THEN
    first = .FALSE.
    CALL GTFLNA(0,'PHASECC',FILPHC,IRC)
    IF(IRC.NE.0) RETURN
!
    CALL RDAPHC(MAXRCV,MAXELV,MAXAZI,FILPHC,TITLE ,NRCV  , &
                RECTPI,ANTTPI,IANTEN,NFRANT,ANTOFF,MODTYP, &
                SINEX ,NPCV  ,ANTPCV,MAXZEN,METHOD,atDATE, &
                FILINFO)
  ENDIF
!      CALL OPNFIL(LFNLOC,FILPHS,'OLD','FORMATTED',   &
!                  'READONLY',' ',IOSTAT)
!      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPHS,'GPHECM2')
!
! READ MODEL NAME (E.G. MODEL NAME: IGS_01), PCV model and model type
! -------------------------------------------------------------------
  DO IRCV=1,NRCV
    IF (ANTTYP == ANTTPI(IRCV) .AND. IANTEN(1,IRCV) <= IANT .AND. &
         IANTEN(2,IRCV) >= IANT) THEN
      STRSIN=SINEX(IRCV)
      IF (MODTYP(IRCV) == 1) THEN
        IF (NPCV(2,IRCV) == 1) THEN
          pcvMod = 'E'
        ELSE
          pcvMod = 'F'
        ENDIF
      ELSE
        pcvMod = '?'
      ENDIF
      IF (PRESENT(ADOPTED) .AND. METHOD(IRCV)(1:7)=='ADOPTED') ADOPTED=1
      IF (PRESENT(INDIVID) .AND. IANTEN(2,IRCV) /= 999999 ) INDIVID=1
      EXIT
    ENDIF
  ENDDO
  DO IRCV=1,NRCV
    IF ((ANTTPI(IRCV) =='AOAD/M_T        NONE' .OR.  &
         ANTTPI(IRCV) =='AOAD/M_T            ').AND. &
         IANTEN(2,IRCV)==999999) THEN
      TYPE='R'
      IF (MODTYP(IRCV) /= 0) THEN
        DO IELV=1,NPCV(1,IRCV)
          DO IAZI=1,NPCV(2,IRCV)
            DO IFRQ=1,2
              IF(ABS(ANTPCV(IELV,IAZI,IFRQ,IRCV))>1.D-6)THEN
                TYPE='A'
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      EXIT
    ENDIF
  ENDDO
!
  RETURN
END SUBROUTINE GPHECM2

END MODULE s_GPHECM2
