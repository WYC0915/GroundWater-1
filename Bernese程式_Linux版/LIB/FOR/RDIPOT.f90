MODULE s_RDIPOT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdipot(maxpot, nPot, potTyp, sigpot)

! -------------------------------------------------------------------------
! Purpose:    Reads the gravity field input options for GPSEST
!
! Author:     R. Dach
!
! Created:    03-Jul-2001
! Last mod.:  23-Apr-2003
!
! Changes:    01-Apr-2003  HU: Comment in DIMTST adapted
!             23-Apr-2003  RD: Nullify local pointers
!
! SR used:    dimtst, exitrc, readKeys
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_dimtst
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b) :: maxpot ! maximum number of potential parms

! output:
  INTEGER(i4b)                 :: npot   ! number of parms of earth's pot.
  INTEGER(i4b), DIMENSION(3,*) :: pottyp ! char of pot parms
  REAL(r8b),    DIMENSION(*)   :: sigpot

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(3)           :: hlpStr

  INTEGER(i4b)                  :: degree, order  ! max. degree/order
  INTEGER(i4b)                  :: mPot
  INTEGER(i4b)                  :: irc, ios
  INTEGER(i4b)                  :: ii, jj

  REAL(r8b)                     :: sigma          ! default sigma

! Init the variables
! ------------------
  nPot                 = 0
  pottyp(1:3,1:maxpot) = 0
  sigpot(1:maxpot)     = 0.0

  NULLIFY(keyValue)

! Default input type
! ------------------
  CALL readKeys('GRAVITY_D', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN

! Read degree
! -----------
    CALL readKeys('POTDEG',keyValue,irc)
    IF (irc == 0) READ(keyValue(1),*,iostat=ios) degree
    IF (irc /= 0 .OR. ios /= 0 .OR. degree < 1) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                       &
      ' *** SR RDIPOT: Wrong specification of the max. ' // &
                                             'degree of development', &
                      'for gravity field terms (default setup)',      &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

! Read order
! ----------
    CALL readKeys('POTORD',keyValue,irc)
    IF (irc == 0) READ(keyValue(1),*,iostat=ios) order
    IF (irc /= 0 .OR. ios /= 0 .OR. order < 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                       &
      ' *** SR RDIPOT: Wrong specification of the max. ' // &
                                             'order of development',  &
                      'for gravity field terms (default setup)',      &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

! Read the apriori sigma
! ----------------------
    sigma = 0d0
    CALL readKeys('POTSIG',keyValue,irc)
    IF (irc == 0) READ(keyValue(1),*,iostat=ios) sigma
    IF ( irc /= 0                                  .OR. &
        (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
         sigma < 0d0 )                               THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                       &
      ' *** SR RDIPOT: Wrong specification of the sigma of the ' //   &
                                                          'parameter',&
                      'for gravity field terms (default setup)',      &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

! Check maximum dimension
! -----------------------
    CALL dimtst(1,1,2,'rdipot','maxpot',                          &
                'gravity field parameters',                       &
                'Parameter is defined in module "P_GPSEST.f90".', &
                degree*order,maxpot,irc)

! Generate the requests
! ---------------------
    DO ii = 2, degree
      DO jj = 0, order
        IF (jj > ii) EXIT
! cosine term
        nPot = nPot + 1
        potTyp(1,nPot) = 1
        potTyp(2,nPot) = ii
        potTyp(3,nPot) = jj
        sigPot(nPot)   = sigma
! sine term
        IF (jj > 0) THEN
          nPot = nPot + 1
          potTyp(1,nPot) = 2
          potTyp(2,nPot) = ii
          potTyp(3,nPot) = jj
          sigPot(nPot)   = sigma
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! Manual input type
! -----------------
  CALL readKeys('GRAVITY_M', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN

! Read the parameter spec. string
! -------------------------------
    CALL readKeys('POTSTR', keyValue, irc)
    IF (irc == 0) THEN

! Check maximum dimension
! -----------------------
      CALL dimtst(1,1,2,'rdipot','maxpot',                          &
                  'gravity field',                                  &
                  'Parameter is defined in module "P_GPSEST.f90".', &
                  SIZE(keyValue)*2,maxpot,irc)

! Extract the information
! -----------------------
      mPot = 0
      DO ii = 1, SIZE(keyValue)
        READ(keyValue(ii),*,iostat=ios) (hlpStr(jj), jj=1,3)

        IF (ios == 0) THEN
          mPot = mPot + 1
          nPot = nPot + 1
          pottyp(1,npot) = 1

! Read degree
! -----------
          READ(hlpStr(1),*,iostat=ios) pottyp(2,npot)
          IF (ios /= 0 .OR. potTyp(2,nPot) < 1) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I6,/,16X,A,A,/)')            &
            ' *** SR RDIPOT: Wrong specification of the ' // &
                                                   'degree of development', &
                            'for gravity field terms (manual setup)',       &
                            'Number of entry:  ',ii,                        &
                            'Specified value:  ',TRIM(hlpStr(1))
            IF (mPot > 0) mPot = mPot - 1
          ENDIF

! Read order
! ----------
          READ(hlpStr(2),*,iostat=ios) pottyp(3,npot)
          IF (ios /= 0 .OR. potTyp(3,nPot) < 0 .OR. &
            potTyp(3,nPot) > potTyp(2,nPot)) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I6,/,16X,A,A,/)')            &
            ' *** SR RDIPOT: Wrong specification of the ' // &
                                                   'order of development',  &
                            'for gravity field terms (manual setup)',       &
                            'Number of entry:  ',ii,                        &
                            'Specified value:  ',TRIM(hlpStr(2))
            IF (mPot > 0) mPot = mPot - 1
          ENDIF

! Read the apriori sigma
! ----------------------
          READ(hlpStr(3),*,iostat=ios) sigPot(nPot)
          IF ((ios /= 0 .AND. LEN_TRIM(hlpStr(3)) > 0) .OR. &
              sigPot(nPot) < 0d0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I6,/,16X,A,A,/)')            &
            ' *** SR RDIPOT: Wrong specification of the sigma of the ' //   &
                                                                'parameter',&
                            'for gravity field terms (manual setup)',       &
                            'Number of entry:  ',ii,                        &
                            'Specified value:  ',TRIM(hlpStr(3))
            IF (mPot > 0) mPot = mPot - 1
          ENDIF

! Set also the sine term
! ----------------------
          IF (potTyp(3,nPot) /= 0) THEN
            nPot = nPot + 1
            potTyp(1,nPot) = 2
            potTyp(2:3,nPot) = potTyp(2:3,nPot-1)
            sigPot(nPot) = sigPot(nPot-1)
          ENDIF
        ENDIF  ! Request was readable
      ENDDO  ! Loop all requests

! Stop if invalid requests found
      IF (mPot /= SIZE(keyValue)) CALL exitrc(2)

    ENDIF  ! Request was readable
  ENDIF  ! Manual selection of the requests

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdipot

END MODULE
