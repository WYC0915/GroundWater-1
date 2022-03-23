MODULE s_RDIALB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdialb(maxalb, maxsat, nAllSat, allSatNum, &
                  nalb, albtyp, sigalb, nalbgr, nsaalb, satalb)

! -------------------------------------------------------------------------
! Purpose:    Reads the albedo input options for GPSEST
!
! Author:     R. Dach
!
! Created:    29-Jun-2001
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
  INTEGER(i4b)                 :: maxalb    ! maximum number of albedo groups
  INTEGER(i4b)                 :: maxsat    ! max. number of satellites
  INTEGER(i4b)                 :: nAllSat   ! number of all satellites
  INTEGER(i4b), DIMENSION(*)   :: allSatNum ! satellite numbers

! output:
  INTEGER(i4b)                 :: nalb      ! number of albedo parameters/group
  INTEGER(i4b), DIMENSION(*)   :: albtyp    ! parameter type (1, 2, or 3)
  REAL(r8b),    DIMENSION(*)   :: sigalb    ! a priori sigmas for alb par types
  INTEGER(i4b)                 :: nalbgr    ! number of albedo groups
  INTEGER(i4b), DIMENSION(*)   :: nsaalb    ! number of satellites per group
  INTEGER(i4b),                 &
         DIMENSION(maxsat,*)   :: satalb    ! satalb(j,i),j=1,..,nsaalb(i),
                                            ! i=1,..,nalbgr
                                            ! satellite numbers of each
                                            ! albedo group

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! Albedo-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3), PARAMETER :: albKeyw = &
           (/ 'ALB1', 'ALB2', 'ALB3' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(17)          :: hlpStr

  INTEGER(i4b)                  :: satNum
  INTEGER(i4b)                  :: irc, ios
  INTEGER(i4b)                  :: iGrp, iSat
  INTEGER(i4b)                  :: ii, jj

! Init the variables
! ------------------
  nalb            = 0
  nalbgr          = 0

  NULLIFY(keyValue)

! Get Types to estimate and apriori sigmas
! ----------------------------------------
  DO ii = 1, SIZE(albKeyw)
    CALL readKeys(albKeyw(ii), keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') THEN
      nalb         = nalb + 1
      albtyp(nalb) = ii
      CALL readKeys('SIG'//TRIM(albKeyw(ii)), keyValue, irc)
      IF (irc == 0) READ(keyValue(1),*, iostat = ios) sigalb(nalb)
      IF ( irc /= 0                                     .OR. &
           (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0)   .OR. &
           sigalb(nalb) < 0d0  )                          THEN
        WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                       &
        ' *** SR RDIALB: Wrong apriori sigma for the Earth ' // &
                                            'albedo radiation',&
                        'Specified Value: ',TRIM(keyValue(1))
        CALL exitrc(2)
      ENDIF
    ENDIF
  ENDDO

! Read the satellite groups
! -------------------------
  CALL readKeys('ALBSATSTR', keyValue, irc)

  DO ii = 1, SIZE(keyValue)
    READ(keyValue(ii), *, iostat=ios) (hlpStr(jj), jj=1,17)

    READ(hlpStr(1),*,iostat=ios) iGrp
    IF (ios == 0 .AND. iGrp > nalbgr) nalbgr = iGrp
  ENDDO

! Check maximum dimension
! -----------------------
  CALL dimtst(1,1,2,'rdialb','maxalb',                          &
              'albedo satellite groups',                        &
              'Parameter is defined in module "P_GPSEST.f90".', &
              nalbgr,maxalb,irc)

  DO ii = 1, SIZE(keyValue)
    READ(keyValue(ii),*,iostat=ios) (hlpStr(jj), jj=1,17)

    READ(hlpStr(1),*,iostat=ios) iGrp

    IF (ios == 0) THEN
! Get the list of satellites per group
      DO jj = 1, 16

! An empty string found
        IF (LEN_TRIM(hlpStr(jj+1)) == 0) CYCLE

! Read the satellite number
        READ(hlpStr(jj+1),*,iostat=ios) satNum
        IF (ios /= 0) CYCLE

! Is the satellite in the observation files?
        DO iSat = 1, nAllSat
          IF (satNum == allSatNum(iSat)) THEN

! Check the size of the group variable
            nsaalb(iGrp) = nsaalb(iGrp) + 1
            IF (nsaalb(igrp) > maxsat) THEN
              WRITE(lfnerr,'(/,A,3(/,16X,A,I5),/,16X,A,/)')                  &
               ' *** SR RDIALB: Too many satellites per group selected',     &
                        'Groups for earth albedo rediation:   ',igrp,        &
                        'Number of requests:                  ',nsaalb(igrp),&
                        'Maximum number allowed:              ',maxsat,      &
                        'Increase maxsat or reduce the num. of requests.'
              CALL exitrc(2)
            ENDIF

! Put the satellite number into the list
            satalb( nsaalb(iGrp), iGrp ) = satNum
            EXIT
          ENDIF
        ENDDO
      ENDDO

    ENDIF
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdialb

END MODULE
