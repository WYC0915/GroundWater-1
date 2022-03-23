MODULE s_RNXAMB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rnxamb(icsflg,nSat,svnMea,                     &
    timref,iDeltT,nEpoch,nFreq,nwlsat,wlfsat,wlfrnx,ambnep, &
    numAmb,ambSat,ambiep,ambigu,ambcls,ambwlf)

! -------------------------------------------------------------------------
! Purpose:    Initialize ambiguities for RXOBV3
!
! Parameters:
!     in: icsflg : Cycle Slips from file?           i4b
!         nSat   : # of satellites                  i4b
!         svnMea : Satellite numbers                i4b(:,:)
!         timref : Reference epoch (MJD)            r8b
!         iDeltT : Sampling rate                    i4b
!         nEpoch : number of epochs                 i4b
!         nFreq  : Number of frequencies            i4b
!         nwlsat : # of element in list nwlsat      i4b
!         wlfsat : Sat.dept. wave length fact.      i4b(:,:)
!         wlfrnx : Wave length fact. from RINEX     i4b(:)
!         ambnep : amb. next valid phase epoch      r8b(maxAmb)
!    out: numAmb : # of ambig. in file              i4b
!         ambSat : satellite numb. for ambig.       i4b(:)
!         ambiep : epoch numb. for ambig.           i4b(:)
!         ambigu : Value of the ambig.              r8b(:,:)
!         ambcls : cluster for the ambig.           i4b(:,:)
!         ambwlf : wave length factor for ambig.    i4b(:,:)
!
!
! Author:     R. Dach
!
! Created:    22-DEC-00
!
! Changes:    08-nov-01 rd: no phase observations in file
!             15-Jul-10 EO: Handling of stations problems
!
! SR used:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

!*
!
  USE m_bern
  USE m_maxdim, ONLY: maxsat,maxamb
  IMPLICIT NONE
!
! Global Parameters
! -----------------
  INTEGER(i4b)                     :: icsflg ! Cycle Slips from file?
  INTEGER(i4b)                     :: nSat   ! # of satellites
  INTEGER(i4b), DIMENSION(maxsat,2):: svnMea ! svnMea(K,I),K=1,2,..,NSAT, I=1,2
                                             ! svn numbers for all satellites in
                                             ! current file (CODE: I=1, PHASE: I=2)
  REAL(r8b)                        :: timref ! Reference epoch (MJD)
  INTEGER(i4b)                     :: iDeltT ! Sampling rate
  INTEGER(i4b)                     :: nEpoch ! number of epochs
  INTEGER(i4b)                     :: nFreq  ! Number of frequencies
  INTEGER(i4b)                     :: nwlsat ! # of element in list nwlsat
  INTEGER(i4b), DIMENSION(:,:)     :: wlfsat ! Sat.dept. wave length fact.
  INTEGER(i4b), DIMENSION(:)       :: wlfrnx ! Wave length fact. from RINEX
  REAL(r8b)   , DIMENSION(maxamb)  :: ambnep ! Value of the ambig.

  INTEGER(i4b)                     :: numAmb ! # of ambig. in file
  INTEGER(i4b), DIMENSION(:)       :: ambSat ! satellite numb. for ambig.
  INTEGER(i4b), DIMENSION(:)       :: ambiep ! epoch numb. for ambig.
  REAL(r8b)   , DIMENSION(:,:)     :: ambigu ! Value of the ambig.
  INTEGER(i4b), DIMENSION(:,:)     :: ambcls ! cluster for the ambig.
  INTEGER(i4b), DIMENSION(:,:)     :: ambwlf ! wave length factor for ambig.

!************************************************************************

! Local Variables
! ---------------
  INTEGER(i4b)                          :: iAmb   ! Counter for ambiguities
  INTEGER(i4b)                          :: jAmb   ! Counter for ambiguities
  INTEGER(i4b)                          :: kAmb   ! Counter for ambiguities
  INTEGER(i4b)                          :: iFreq  ! Counter for Frequ.
  INTEGER(i4b)                          :: jFreq  ! Counter for Frequ.
  INTEGER(i4b)                          :: iSat   ! Counter for satellites
  INTEGER(i4b)                          :: jSat   ! Counter for satellites
!
  REAL(r8b)                             :: epoch  ! epoch for ambig. (MJD)
!


! No phase observations in file
! -----------------------------
  IF (nSat == 0) THEN
    numAmb = 0
    RETURN
  ENDIF

! Set epochs of the Ambiguities
! -----------------------------
  IF (icsflg == 0) numAmb=nSat
!
  DO iAmb=1,numAmb
    IF (icsflg == 0) THEN
      ambSat(iAmb)=svnMea(iAmb,2)
      ambiep(iAmb)=1
    ELSE
      IF (ambnep(iamb) == 0.0D0) THEN
        ambiep(iAmb) = 0
      ELSE
        epoch = ambnep(iAmb)
        ambiep(iAmb)=IDNINT(86400.D0*(epoch-timref)/iDeltT)+1
        IF (ambiep(iAmb) < 1) ambiep(iAmb)=1
        IF (ambiep(iAmb) > nEpoch) ambiep(iAmb)=0
      ENDIF
    ENDIF
  ENDDO
!
! Remove Ambiguities Behind the Obs. Win.
! ---------------------------------------
  iAmb=numAmb
  DO WHILE (iAmb >= 1)
    IF (ambiep(iAmb) == 0) THEN
      numAmb=numAmb-1
      DO jAmb=iAmb,numAmb
        ambSat(jAmb)=ambSat(jAmb+1)
        ambiep(jAmb)=ambiep(jAmb+1)
      ENDDO
    ENDIF
    iAmb=iAmb-1
  ENDDO
!
! Remove Ambiguities Before the Obs. Win.
! ---------------------------------------
  iAmb=1
  DO WHILE (iAmb < numAmb)
    IF (ambiep(iAmb) == 1) THEN
      jAmb=iAmb+1
      DO WHILE (jAmb <= numAmb)
        IF (ambiep(jAmb) == 1 .AND. ambSat(iAmb) == ambSat(jAmb)) THEN
          numAmb=numAmb-1
          DO kAmb=jAmb,numAmb
            ambSat(kAmb)=ambSat(kAmb+1)
            ambiep(kAmb)=ambiep(kAmb+1)
          ENDDO
        ENDIF
        jAmb=jAmb+1
      ENDDO
    ENDIF
    iAmb=iAmb+1
  ENDDO
!
! Remove Ambiguities for Satellites Without Obs.
! ----------------------------------------------
  iAmb=1
  DO WHILE (iAmb < numAmb)
    jAmb=0
    DO iSat=1,nSat
      IF (svnMea(isat,2) == ambSat(iAmb)) jAmb=iAmb
    ENDDO
    IF (jAmb == 0) THEN
      numAmb=numAmb-1
      DO jAmb=iAmb,numAmb
        ambSat(jAmb)=ambSat(jAmb+1)
        ambiep(jAmb)=ambiep(jAmb+1)
      ENDDO
    ENDIF
    iAmb=iAmb+1
  ENDDO
!
! Initialize Ambiguity Array
! --------------------------
  DO iAmb=1,numAmb
    DO iFreq=1,(nFreq-1)*2+1             ! either L1 or L1,L2,L5
      ambigu(iAmb,iFreq)=0.D0
      ambcls(iAmb,iFreq)=iAmb
    ENDDO
!
    jSat=0
    DO iSat=1,nwlsat
      IF (ambSat(iAmb) == wlfsat(3,iSat)) jSat=iSat
    ENDDO
!
    IF (jSat == 0) THEN
      DO jFreq=1,nFreq
        ambwlf(iAmb,jFreq)=wlfrnx(jFreq)
      ENDDO
    ELSE
      DO jFreq=1,nFreq
        ambwlf(iAmb,jFreq)=wlfsat(jFreq,jSat)
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END SUBROUTINE

END MODULE
