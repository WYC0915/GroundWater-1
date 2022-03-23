MODULE s_RXGLST
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxglst(nfil,filnam,intgra,opt)

! -------------------------------------------------------------------------
!
! Purpose:    gets a list of RINEX files corresponding to some conditions
!
! Author:     R. Dach
!
! Created:    20-Nov-2000
! Last mod.:  17-Jun-2011
!
! Changes:    21-Dec-2001  HU: Use m_bern, other modules with ONLY
!             27-Feb-2003  RD: Replace path variables before writing file lists
!             16-Feb-2004  RD: Disable the options with value "0"
!             17-Jun-2011  LP: Typo in error message corrected
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_rnxgra, ONLY: t_rnxgra_opt, maxfil,maxsat,maxchr

  USE s_opnfil
  USE s_opnerr
  USE s_rplenvar
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                                :: nFil    ! # of RINEX files
  CHARACTER(LEN=fileNameLength),  &
               DIMENSION(MAXFIL)              :: FILNAM  ! names of RINEX files
  INTEGER(i4b),DIMENSION(MAXFIL,MAXSAT,MAXCHR):: INTGRA  ! array with obs.
  TYPE(t_rnxgra_opt)                          :: opt     ! input options

! output

! Used functions
! --------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  LOGICAL,     DIMENSION(MAXFIL)              :: inList  ! OK-Flag

  INTEGER(i4b),DIMENSION(MAXFIL)              :: NOBS    ! # obs per station
  INTEGER(i4b),DIMENSION(MAXFIL)              :: NBAD    ! # of bad obs per station
  INTEGER(i4b)                                :: mFil    ! # files in "OK-list"
  INTEGER(i4b)                                :: jFil    ! Index in file list
  INTEGER(i4b)                                :: minObs  ! helps for maxsta cond.
  INTEGER(i4b)                                :: ObsEpo  ! helps for minobs cond.
  INTEGER(i4b)                                :: BadEpo  ! helps for badobs cond.
  INTEGER(i4b)                                :: iFil    ! counter files
  INTEGER(i4b)                                :: iSat    ! counter satellites
  INTEGER(i4b)                                :: iChr    ! counter characters
  INTEGER(i4b)                                :: ios     ! iostatus

  CHARACTER(LEN=255)                          :: filnm2  ! Long file name

  mFil=0
  DO iFil=1,nFil
!
! Count the number of observations per station
! --------------------------------------------
    inList(iFil)=.TRUE.
    nOBS(iFil)=0
    nBad(iFil)=0
    DO iChr=1,MAXCHR
      BadEpo=0
      ObsEpo=0
      DO iSat=1,MAXSAT
        IF (intGra(iFil,iSat,iChr) == +1) ObsEpo=ObsEpo+1
        IF (intGra(iFil,iSat,iChr) == -1) BadEpo=BadEpo+1
      ENDDO
      IF (ObsEpo > 0)  nOBS(iFil)=nOBS(iFil)+ObsEpo
      IF (opt%badobs > 0 .AND. ObsEpo <= opt%badobs) &
        nBad(iFil)=nBad(iFil)+1
    ENDDO
!
! Check the min. number of obs.
! -----------------------------
   IF (nObs(iFil) < opt%minObs) THEN
      inList(iFil)=.False.
      WRITE(lfnerr,'(/,A,/,16X,A,A,2(/,16X,A,I5),/)')                     &
                 ' ### SR RXGLST: TOO FEW OBSERVATIONS IN FILE',          &
                         'FILE NAME               : ',TRIM(filnam(iFil)), &
                         'NUMBER OF OBSERVATIONS  : ',nobs(iFIl),         &
                         'MIN. OBSERV. REQUESTED  : ',opt%minobs
!
! Check the number of bad epochs
! ------------------------------
    ELSE IF (nBad(iFil) > opt%maxBad .AND. opt%maxBad > 0) THEN
      inList(iFil)=.False.
      WRITE(lfnerr,'(/,A,/,16X,A,A,3(/,16X,A,I5),/)')                     &
                 ' ### SR RXGLST: TOO MANY EPOCHS WITH FEW OBSERVATIONS', &
                         'FILE NAME               : ',TRIM(filnam(iFil)), &
                         'NUMBER OF EPOCHS        : ',nbad(iFIl),         &
                         'WITH LESS THAN OBSERV.  : ',opt%badobs+1,       &
                         'MAX. # OF EPOCHS ALLOWED: ',opt%maxBad
!
! Count the number of good files
! ------------------------------
    ELSE
      mFil=mFil+1
!      WRITE(*,*) 'OK : ',TRIM(filnam(iFil)),nobs(iFIl),nbad(iFil),mFil
    ENDIF

  ENDDO
!
! Select only maxfil
! ------------------
  DO WHILE (mFil > opt%maxsta .AND. opt%maxsta /= 0)
    minObs=999999
    jFil=-1
    DO iFil=1,nFil
      IF (inList(iFil) .AND. nObs(iFil) < minObs) THEN
        jFil=iFil
        minOBs=nObs(iFIl)
      ENDIF
    ENDDO
    inList(jFil)=.False.
      WRITE(lfnerr,'(/,A,/,16X,A,A,2(/,16X,A,I5),/)')                   &
               ' ### SR RXGLST: TOO MANY FILES IN LIST',                &
                       'FILE NAME               : ',TRIM(filnam(jFil)), &
                       'NUMBER OF OBSERVATIONS  : ',nobs(jFIl),         &
                       'MAX # OF FILES ALLOWED  : ',opt%mAXSTA
    mFil=mFil-1
  ENDDO
!
! Write file names into the lists
! -------------------------------
  IF (LEN_TRIM(opt%lstfil) > 0) THEN
    CALL OPNFIL(LFN001,opt%lstfil,'UNKNOWN','FORMATTED',' ',' ',IOS)
    CALL OPNERR(LFNERR,LFN001,IOS,opt%lstfil,'RXGLST')
  ENDIF
!
  IF (LEN_TRIM(opt%delfil) > 0) THEN
    CALL OPNFIL(LFN002,opt%delfil,'UNKNOWN','FORMATTED',' ',' ',IOS)
    CALL OPNERR(LFNERR,LFN002,IOS,opt%lstfil,'RXGLST')
  ENDIF
!
  DO iFil=1,nFil
    IF (inList(iFil)) THEN
      IF (LEN_TRIM(opt%lstfil) > 0) THEN
        filnm2=FILNAM(iFil)
        CALL rplenvar(1,filnm2)
        WRITE(lfn001,'(A)') TRIM(FILNM2)
      ENDIF
    ELSE
      IF (LEN_TRIM(opt%delfil) > 0) THEN
        filnm2=FILNAM(iFil)
        CALL rplenvar(1,filnm2)
        WRITE(lfn002,'(A)') TRIM(FILNM2)
      ENDIF
    ENDIF
  ENDDO
!
  IF (LEN_TRIM(opt%lstfil) > 0) CLOSE(LFN001)
  IF (LEN_TRIM(opt%delfil) > 0) CLOSE(LFN002)
!
  END SUBROUTINE rxglst

END MODULE
