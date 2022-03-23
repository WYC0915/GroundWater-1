MODULE s_CHKANT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chkant(isasys, nftot,  nfreq,  ndiff,                  &
                  rectyp, anttyp, ianten, nsatel, satnum,         &
                  nanrao, antrao, numrao, sigrao, prnrao, nfrrao, &
                  nancal, antcal, numcal, prncal, nfrcal, nptcal, sigcal)


! -------------------------------------------------------------------------
! Purpose:    Check receiver antenna lists whether they are really observed
!
! Author:     R. Dach
!
! Created:    26-Nov-2010
!
! Changes:    13-Jan-2011 RD: single system bugfix (w/o GPS)
!             28-Mar-2012 RD: Use SVN2CHR as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b

  USE s_svn2chr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                      :: isasys ! satellite system selection
                                              ! = 0: ALL
                                              ! = 1: GPS
                                              ! = 2: GLONASS
                                              ! = 3: Galileo
                                              ! = 4: GPS+GLONASS
                                              ! = 5: GPS+Galileo
                                              ! = 6: GLONASS+Galileo
  INTEGER(i4b)                      :: nftot  ! total number of files
  INTEGER(i4b),      DIMENSION(:)   :: nfreq  ! Number of frequencies per file
  INTEGER(i4b),      DIMENSION(:)   :: ndiff  ! Difference level of the obs.
                                              ! files (0: zero/1: baseline)
  CHARACTER(LEN=20), DIMENSION(:,:) :: rectyp ! receiver type
  CHARACTER(LEN=20), DIMENSION(:,:) :: anttyp ! antenna type
  INTEGER(i4b),      DIMENSION(:,:) :: ianten ! antenna number
  INTEGER(i4b),      DIMENSION(:)   :: nsatel ! number of satellites per file
  INTEGER(i4b),      DIMENSION(:,:) :: satnum ! satellite numbers per file


! input/output:
  INTEGER(i4b)                      :: nanrao ! number of receiver
                                              ! antenna offsets
  CHARACTER(LEN=20), DIMENSION(2,*) :: antrao ! receiver and antenna
                                              ! name for request i
  INTEGER(i4b),      DIMENSION(2,*) :: numrao ! antenna numbers
                                              ! "from - to" for request i
  INTEGER(i4b),      DIMENSION(*)   :: prnrao ! satellite system (see g_svnsys)
  INTEGER(i4b),      DIMENSION(*)   :: nfrrao ! frequency for
                                              ! receiver ant. offset request i
  REAL(r8b),         DIMENSION(2,*) :: sigrao ! a priori sigmas in meters
                                              ! j=1: horizontal components
                                              ! j=2: vertical component


  INTEGER(i4b)                      :: nancal ! number of receiver antenna phase
                                              ! center requests
  CHARACTER(LEN=20), DIMENSION(2,*) :: antcal ! receiver and antenna
                                              ! name for request i
  INTEGER(i4b),      DIMENSION(2,*) :: numcal ! antenna numbers
                                              ! "from - to" for request i
  INTEGER(i4b),      DIMENSION(*)   :: prncal ! satellite system (index in g_svnsys)
  INTEGER(i4b),      DIMENSION(*)   :: nfrcal ! frequency for phase center req. i
  INTEGER(i4b),      DIMENSION(2,*) :: nptcal ! # of points to be estimated in
                                              ! elevation (j=1) and azimuth (j=2)
                                              ! (1) > 0 ... linear model
                                              ! (1) < 0 ... spherical harmonics
  REAL(r8b),         DIMENSION(*)   :: sigcal ! a priori sigmas in meters

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),  PARAMETER      :: srName = 'chkant'


! Local Variables
! ---------------
  CHARACTER(LEN=1)                  :: svnchr

  INTEGER(i4b)                      :: inum
  INTEGER(i4b)                      :: irao,jrao
  INTEGER(i4b)                      :: irap,jrap
  INTEGER(i4b)                      :: ifil,ista,isat


! Init some variables
! -------------------

! PART 1: Loop the receiver antenna offset requests
! -------------------------------------------------
  DO irao=1,nanrao

    jrao = 0

! Loop the observation files
! --------------------------
    DO ifil=1,nftot

! Check the number of frequencies
! -------------------------------
      IF (nfrrao(irao) > 1 .AND. nfreq(ifil) == 1) CYCLE

! Check the equipment
! -------------------
      DO ista=1,ndiff(ifil)+1
        IF (antrao(1,irao) /= ' ' .AND. &
            antrao(1,irao) /= rectyp(ista,ifil)) CYCLE
        IF (antrao(2,irao) /= anttyp(ista,ifil)) CYCLE
        IF (numrao(1,irao) >  ianten(ista,ifil)) CYCLE
        IF (numrao(2,irao) <  ianten(ista,ifil)) CYCLE
        jRao = 1
      ENDDO
      IF (jRao == 0) CYCLE
      jRao = 0

! Check the satellites
! --------------------
      IF (prnrao(irao) == 10) jrao = 1

      DO isat=1,nsatel(ifil)

        CALL svn2chr(satnum(isat,ifil),inum,svnchr)

        IF ( svnchr == 'G' .AND. &
            (isasys == 2 .OR. isasys == 3 .OR. isasys == 6)) CYCLE
        IF ( svnchr == 'R' .AND. &
            (isasys == 1 .OR. isasys == 3 .OR. isasys == 5)) CYCLE
        IF ( svnchr == 'E' .AND. &
            (isasys == 1 .OR. isasys == 2 .OR. isasys == 4)) CYCLE
        IF ( svnchr == 'S' .AND. isasys /= 0) CYCLE

        IF (INT(satnum(isat,ifil)/100) /= prnrao(irao)) CYCLE

        jrao = 1
        EXIT
      ENDDO
      IF (jrao == 1) EXIT
    ENDDO ! End of file loop

! No observations found for the request
! -------------------------------------
    IF (jrao == 0) antrao(1:2,irao) = ''
  ENDDO

! Shrink the list of requests
! ---------------------------
  jrao = 0
  DO irao = 1,nanrao
    IF ( LEN_TRIM(antrao(2,irao)) == 0 ) CYCLE
    jrao = jrao + 1
    IF ( jrao == irao ) CYCLE

    antrao(1:2,jrao) = antrao(1:2,irao)
    numrao(1:2,jrao) = numrao(1:2,irao)
    sigrao(1:2,jrao) = sigrao(1:2,irao)
    prnrao(jrao)     = prnrao(irao)
    nfrrao(jrao)     = nfrrao(irao)
  ENDDO
  nanrao = jrao




! PART 2: Loop the receiver antenna pattern requests
! --------------------------------------------------
  DO irap=1,nancal

    jrap = 0

! Loop the observation files
! --------------------------
    DO ifil=1,nftot

! Check the number of frequencies
! -------------------------------
      IF (nfrcal(irap) > 1 .AND. nfreq(ifil) == 1) CYCLE

! Check the equipment
! -------------------
      DO ista=1,ndiff(ifil)+1
        IF (antcal(1,irap) /= ' ' .AND. &
            antcal(1,irap) /= rectyp(ista,ifil)) CYCLE
        IF (antcal(2,irap) /= anttyp(ista,ifil)) CYCLE
        IF (numcal(1,irap) >  ianten(ista,ifil)) CYCLE
        IF (numcal(2,irap) <  ianten(ista,ifil)) CYCLE
      ENDDO

! Check the satellites
! --------------------
      IF (prncal(irap) == 10) jrap = 1

      DO isat=1,nsatel(ifil)

        CALL svn2chr(satnum(isat,ifil),inum,svnchr)
        IF ( svnchr == 'G' .AND. &
            (isasys == 2 .OR. isasys == 3 .OR. isasys == 6)) CYCLE
        IF ( svnchr == 'R' .AND. &
            (isasys == 1 .OR. isasys == 3 .OR. isasys == 5)) CYCLE
        IF ( svnchr == 'E' .AND. &
            (isasys == 1 .OR. isasys == 2 .OR. isasys == 4)) CYCLE
        IF ( svnchr == 'S' .AND. isasys /= 0) CYCLE

        IF (INT(satnum(isat,ifil)/100) /= prncal(irap)) CYCLE

        jrap = 1
        EXIT
      ENDDO
      IF (jrap == 1) EXIT
    ENDDO ! End of file loop

! No observations found for the request
! -------------------------------------
    IF (jrap == 0) antcal(1:2,irap) = ''
  ENDDO

! Shrink the list of requests
! ---------------------------
  jrap = 0
  DO irap = 1,nancal
    IF ( LEN_TRIM(antcal(2,irap)) == 0 ) CYCLE
    jrap = jrap + 1
    IF ( jrap == irap ) CYCLE

    antcal(1:2,jrap) = antcal(1:2,irap)
    numcal(1:2,jrap) = numcal(1:2,irap)
    nptcal(1:2,jrap) = nptcal(1:2,irap)
    sigcal(jrap)     = sigcal(irap)
    prncal(jrap)     = prncal(irap)
    nfrcal(jrap)     = nfrcal(irap)
  ENDDO
  nancal = jrap

  RETURN
END SUBROUTINE chkant

END MODULE
