MODULE s_GTSTANUM
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtStaNum(nAllSta, allStaNum, allStaName,              &
                    keyList, keyNameSta, keyNameFil, keyNameFlg, &
                    nsta, stanum, stanam, nWeight, weight)

! -------------------------------------------------------------------------
! Purpose:    This subroutine resolves the station numbers selected in
!             a standard way (the first keyword is a station selection list
!             and the second keyword is a name of file containing a list
!             of stations). Optionally an array of weights is read, too.
!
! Author:     L. Mervart
!
! Created:    26-Jul-2000
!
! Changes:    01-Jun-2001 RD: The fix file must not have a constrain
!                             (generate the same behaviour like the old menu)
!                             An empty line stops reading the fix file
!             28-Jun-2001 RD: Handle FIRST, LAST, ALL, MANUAL, FROM_FILE
!                             Use SR getfixst to read the station list
!             04-Jul-2001 RD: Give back a list of station names (ADDNEQ2)
!             28-Aug-2001 RD: Use SR readstsg for reading the station list
!             29-Aug-2001 RD: Special keyword-values for rdiclk
!             08-Oct-2002 RD: FIRST/LAST from alphabetic order
!                             Add WITH_FLAG option
!             11-Dec-2002 RD: Bugfix for "LAST"
!             03-Feb-2003 RD: weight is a pointer now
!             23-Apr-2003 CU: Nullify local pointers
!             16-May-2003 CU: Initialize structure
!             04-Nov-2003 HB: Declare allStaName with (:)
!             21-Jan-2004 RD: Add "ALL_SATELLITES"
!             19-Feb-2004 HU: Warning message modified
!             27-Nov-2009 RD: Read flags also from a "lineedit"
!             23-Aug-2011 SL/RD: return if no usable observations
!             23-Aug-2011 SL: use m_bern with ONLY
!             28-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength, &
                      staNameLength, lfnErr
  USE d_stalst, ONLY: t_stalist, init_stalist

  USE s_alcerr
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  USE s_readstsg
  USE s_maxtst
  USE s_getco3
  USE s_splarg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                    , INTENT(IN)    :: nAllSta
  INTEGER(i4b)    , DIMENSION(*)  , INTENT(IN)    :: allStaNum
  CHARACTER(LEN=*), DIMENSION(:)  , INTENT(IN)    :: allStaName
  CHARACTER(LEN=*)                , INTENT(IN)    :: keyList
  CHARACTER(LEN=*)                , INTENT(IN)    :: keyNameSta
  CHARACTER(LEN=*)                , INTENT(IN)    :: keyNameFil
  CHARACTER(LEN=*)                , INTENT(IN)    :: keyNameFlg

! output
  INTEGER(i4b)                    , INTENT(OUT)   :: nsta
  INTEGER(i4b)    , DIMENSION(:)  , INTENT(OUT)   :: stanum
  CHARACTER(LEN=*), DIMENSION(:)  , POINTER       :: stanam

! input/output
  INTEGER(i4b)                                    :: nWeight
  REAL(r8b)       , DIMENSION(:,:), POINTER       :: weight

! Functions
! ---------

! Local Parameters
! ----------------

! Local Types
! -----------

! Local Variables
! ---------------
  TYPE(t_staList)                                          :: staList

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER     :: argv
  CHARACTER(LEN=fileNameLength)                            :: listFile
  CHARACTER(LEN=fileNameLength)                            :: crdFil
  CHARACTER(LEN=staNameLength),  DIMENSION(:), POINTER     :: stName
  CHARACTER(LEN=1),              DIMENSION(:), ALLOCATABLE :: flgLst

  INTEGER(i4b)                                             :: iSta,jSta
  INTEGER(i4b)                                             :: mSta
  INTEGER(i4b)                                             :: nFlg
  INTEGER(i4b)                                             :: ii, jj
  INTEGER(i4b)                                             :: irc
  INTEGER(i4b)                                             :: ios

  REAL(r8b)       ,               DIMENSION(:)  , POINTER  :: rHlp

! Init some variables
! -------------------
  nSta = 0

  NULLIFY(keyValue)
  NULLIFY(stName)
  NULLIFY(rHlp)
  NULLIFY(argv)

  IF(nAllSta == 0) THEN
    WRITE(lfnerr,'(/,A,/,18X,A,A,/)') &
    ' ### SR GTSTANUM: No observations available for the station(s) ', &
                      'specified by keyword ',keyNameSta
    RETURN
  ENDIF

! Copy the default sigma
! ----------------------
  IF (nWeight > 0) THEN
    ALLOCATE(rHlp(nWeight),stat=irc)
    CALL alcerr(irc, 'rHlp', (/nWeight/), 'gtstanum')

    CALL maxtst(0,'gtstanum','nWeight',SIZE(weight,1),nWeight,irc)
    IF (irc /= 0) CALL exitrc(2)

    rHlp(1:nWeight) = weight(1:nWeight,1)
    weight = 0d0
  ENDIF

! Read the list keyword
! ---------------------
  CALL readKeys(keyList, keyValue, irc)

! First station was selected
! --------------------------
  IF ( irc == 0 .AND. &
       ( keyValue(1) == 'FIRST' .OR. &
         (keyList == 'REFCLOCK' .AND. keyValue(1) == 'FIRST_STATION'))) THEN
    nSta         = 1
    staNum(nSta) = allStaNum(1)
    staNam(nSta) = allStaName(1)
    IF (nWeight > 0) weight(1:nWeight,1) = rHlp(1:nWeight)

    DO iSta = 2,nAllSta
      IF (staNam(nSta) > allStaName(iSta)) THEN
        staNum(nSta) = allStaNum(iSta)
        staNam(nSta) = allStaName(iSta)
        IF (nWeight > 0) THEN
          weight=0d0
          weight(1:nWeight,nSta) = rHlp(1:nWeight)
        ENDIF
      ENDIF
    ENDDO

! Last station was selected
! -------------------------
  ELSE IF ( irc == 0 .AND. &
       ( keyValue(1) == 'LAST' .OR. &
         (keyList == 'REFCLOCK' .AND. keyValue(1) == 'LAST_STATION'))) THEN
    nSta         = 1
    staNum(nSta) = allStaNum(1)
    staNam(nSta) = allStaName(1)
    IF (nWeight > 0) weight(1:nWeight,1) = rHlp(1:nWeight)

    DO iSta = 2,nAllSta
      IF (staNam(nSta) < allStaName(iSta)) THEN
        staNum(nSta) = allStaNum(iSta)
        staNam(nSta) = allStaName(iSta)
        IF (nWeight > 0) THEN
          weight=0d0
          weight(1:nWeight,nSta) = rHlp(1:nWeight)
        ENDIF
      ENDIF
    ENDDO

! All stations were selected
! --------------------------
  ELSE IF ( irc == 0 .AND. &
       ( keyValue(1) == 'ALL' .OR. &
         (keyList == 'REFCLOCK'  .AND. keyValue(1) == 'ALL_STATIONS'  ) .OR. &
         (keyList == 'REFCLOCK'  .AND. keyValue(1) == 'ALL_SATELLITES') .OR. &
         (keyList == 'REFCLOCKS' .AND. keyValue(1) == 'ALL_SATELLITES'))) THEN

    nSta           = nAllSta
    staNum(1:nSta) = allStaNum(1:nAllSta)
    staNam(1:nSta) = allStaName(1:nAllSta)

    IF (nWeight > 0) THEN
      DO ii = 1, nSta
        weight(1:nWeight,ii) = rHlp(1:nWeight)
      ENDDO
    ENDIF

! Manually selected station list
! ------------------------------
  ELSE IF (irc == 0 .AND. keyValue(1) == 'MANUAL') THEN
    nsta = 0
    CALL readKeys(keyNameSta, keyValue, irc)
    DO ii = 1, SIZE(keyValue)
      DO jj = 1, nAllSta
        IF (allStaName(jj) == keyValue(ii)) THEN
          nsta = nsta + 1
          stanum(nsta) = allStaNum(jj)
          stanam(nsta) = allStaName(jj)
          IF (nWeight > 0) weight(1:nWeight,nSta) = rHlp(1:nWeight)
          EXIT
        ENDIF
      ENDDO
    ENDDO

! Station list is taken from a file
! ---------------------------------
  ELSE IF (irc == 0 .AND. keyValue(1) == 'FROM_FILE') THEN
    nsta = 0
    staList%nSta = 0

! Read the file with the station list
    CALL gtflna(0,keyNameFil,listFile,irc)
    IF (irc == 0 .AND. LEN_TRIM(listFile) > 0) THEN
      CALL init_stalist(staList)
      CALL readstsg(listFile, nWeight, staList)

! Generate the station list
      DO ii = 1, staList%nSta
        DO jj = 1, nAllSta
          IF (allStaName(jj) == staList%staNam(ii)) THEN
            nsta = nsta + 1
            stanum(nsta) = allStaNum(jj)
            stanam(nsta) = allStaName(jj)
            IF (nWeight > 0) &
              weight(1:nWeight,nSta) = staList%sigma(1:nWeight,ii)
            EXIT
          ENDIF ! station found in list
        ENDDO
      ENDDO
      DEALLOCATE(staList%stanam, stat=ios)
      DEALLOCATE(staList%sigma, stat=ios)
    ENDIF ! file with station list found

! Station with a flag
! -------------------
  ELSE IF (irc == 0 .AND. keyValue(1) == 'WITH_FLAG') THEN

    nSta = 0

! Flag list is not valid there
! ----------------------------
    IF (LEN_TRIM(keyNameFlg) == 0) THEN
      WRITE(lfnerr,'(/,A,/,18X,A,/)')                                 &
      ' *** SR GTSTANUM: No flags available for option "WITH_FLAG" ', &
      'in keyword ' // TRIM(keyList)
      CALL exitrc(2)
    ENDIF

! Read the list of flags
! ----------------------
    CALL readKeys(keyNameFlg, keyValue, irc)
    IF (irc == 0) THEN

      ! Generate the list of flags
      nFlg = SIZE(keyValue)

      ! Flag list is an uniline
      IF (nFlg > 1) THEN

        ALLOCATE(flgLst(nFlg),stat=irc)
        CALL alcerr(irc,'flgLst',(/nFlg/),'gtStaNum')

        DO ii = 1,nFlg
          flgLst(ii) = keyValue(ii)(1:1)
        ENDDO

      ! All flags in a lineedit
      ELSE IF (nFlg == 1 .AND. LEN_TRIM(keyValue(1)) > 0) THEN

        CALL splarg(keyValue(1),argv)
        nFlg = SIZE(argv)

        ALLOCATE(flgLst(nFlg),stat=irc)
        CALL alcerr(irc,'flgLst',(/nFlg/),'gtStaNum')

        DO ii = 1,nFlg
          flgLst(ii) = argv(ii)(1:1)
        ENDDO

        DEALLOCATE(argv)

      ! One blank flied selects all coordinates
      ELSE IF (nFlg == 1 .AND. LEN_TRIM(keyValue(1)) == 0) THEN
        nFlg = 1

        ALLOCATE(flgLst(nFlg),stat=irc)
        CALL alcerr(irc,'flgLst',(/nFlg/),'gtStaNum')

        flgLst(1) = '@'
      ENDIF


      ! Get the coordinate/velocity file
      IF (keyList == 'FIXVEL' .OR. &
          keyList == 'SIGVEL' .OR. &
          keyList == 'FREEVEL') THEN
        CALL gtflna(1,'VELAPR',crdFil,irc)
      ELSE
        CALL gtflna(1,'COORD', crdFil,irc)
      ENDIF
    ENDIF

! Read all stations with this flag(s) from the coordinate file
! ------------------------------------------------------------
    IF (irc == 0) THEN

      CALL getco3(crdFil,nFlg,flgLst,mSta,stName)

      DO iSta = 1,nAllSta
        DO jSta = 1,mSta

          IF (allStaName(iSta) == stName(jSta)) THEN
            nSta = nSta+1
            stanum(nSta) = allStaNum(iSta)
            stanam(nSta) = allStaName(iSta)
            IF (nWeight > 0) weight(1:nWeight,nSta) = rHlp(1:nWeight)
            EXIT
          ENDIF

        ENDDO
      ENDDO

    ENDIF

    DEALLOCATE(flgLst,stat=irc)
    DEALLOCATE(stName,stat=irc)

  ENDIF ! possible entries for the selection

  IF (nWeight > 0) DEALLOCATE(rHlp, stat=irc)
  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE gtStaNum

END MODULE
