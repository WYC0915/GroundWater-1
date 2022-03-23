! --------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! --------------------------------------------------------------------------

MODULE d_rcvr

! --------------------------------------------------------------------------
! Purpose:   This module reads and queries the receiver file
!
! Author:    M. Meindl
!
! Created:   25-May-2012
!
! Changes:   __-___-____ __:
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! --------------------------------------------------------------------------

! Modules
  USE m_bern, ONLY: i4b,r8b,fileNameLength,longLineLength,lfnLoc,lfnErr
  USE s_exitrc

! No implicit
  IMPLICIT NONE

! Save variables
  SAVE

! Access rights
  PRIVATE
  PUBLIC :: rcvObs,maxTypr


! =========================================================================
! Global variable definitions
! =========================================================================


! =========================================================================
! Parameter definitions
! =========================================================================
  CHARACTER(LEN=6),PARAMETER                  :: modNam  = 'D_RCVR'
  INTEGER(i4b)    ,PARAMETER                  :: maxTypr = 12


! =========================================================================
! Type definitions
! =========================================================================

! t_receiver
! ----------
  TYPE t_receiver
    CHARACTER(LEN=20)                          :: name = ""
    INTEGER(i4b)                               :: nSys = 0
    CHARACTER(LEN=3),DIMENSION(:)  ,POINTER    :: sys
    CHARACTER(LEN=3),DIMENSION(:,:),POINTER    :: L1
    CHARACTER(LEN=3),DIMENSION(:,:),POINTER    :: L2
    CHARACTER(LEN=3),DIMENSION(:,:),POINTER    :: C1
    CHARACTER(LEN=3),DIMENSION(:,:),POINTER    :: C2
  END TYPE t_receiver


! Module variable declarations
! ----------------------------
  TYPE(t_receiver),DIMENSION(:),POINTER       :: rcvLst
  CHARACTER(LEN=fileNameLength)               :: rcvFil
  REAL(r8b)                                   :: fmtVer
  INTEGER(i4b)                                :: iDef  = 0
  LOGICAL                                     :: first = .TRUE.



! =========================================================================
! Subroutine definitions
! =========================================================================

  CONTAINS

! --------------------------------------------------------------------------
  SUBROUTINE defRcv
! --------------------------------------------------------------------------
! Purpose:   Reads the observation selection list
!
! Author:    M. Meindl
!
! Created:   25-May-2012
!
! Changes:   06-Jun-2012 LP : Use LISTC1 and SPLSTR as module; use helpvariables
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! --------------------------------------------------------------------------

! Used Modules
    USE s_alcErr
    USE s_gtflna
    USE s_opnErr
    USE s_opnFil
    USE s_splstr
    USE f_listc1

! No implicit
    IMPLICIT NONE


! Parameter declaration
! ---------------------
    CHARACTER(LEN=6),PARAMETER        :: srName = 'DEFRCV'


! Variable declaration
! --------------------
    CHARACTER(LEN=longLineLength)              :: line
    CHARACTER(LEN=3),DIMENSION(maxTypr+2)      :: subStr

    INTEGER(i4b)                               :: nRcv = 0
    INTEGER(i4b)                               :: nStr, nTyp, nHlp
    INTEGER(i4b)                               :: ii, idx, dmy, helpint
    INTEGER(i4b)                               :: irc, iac, ioStat


! First call
! ----------
    first = .FALSE.


! Open receiver file
! ------------------
    CALL gtflna(1,'OBSSEL',rcvFil,irc)
    CALL opnfil(lfnLoc,rcvFil,'OLD','FORMATTED','READONLY',' ',ioStat)
    CALL opnerr(lfnErr,lfnLoc,ioStat,rcvFil,srName)


! Get format version, number of receivers, number of entries
! ----------------------------------------------------------
    DO
      READ(lfnLoc,"(A)") line
      IF (line(1:15) == "Format version:") READ(line(16:21),'(F6.2)') fmtVer
      IF (line(1:20) == "********************") EXIT
    END DO

    nTyp = 0
    nHlp = 0
    DO
      READ(lfnLoc,"(A)",ioStat=ioStat) line
      IF (ioStat<0) EXIT
      IF (TRIM(line(1:20)) /= "") THEN
        IF (nHlp > nTyp) nTyp = nHlp
        nHlp = 0
        nRcv = nRcv+1
      ENDIF
      IF (LEN_TRIM(line) > 0) nHlp = nHlp+1
    ENDDO
    IF (nHlp > nTyp) nTyp = nHlp

    REWIND(lfnLoc)
    DO
      READ(lfnLoc,"(A)") line
      IF (line(1:20) == "********************") EXIT
    END DO

! No receivers found
    IF (nRcv == 0) THEN
      WRITE(lfnErr,'(2(/,A),/)')                    &
            ' *** SR DEFRCV: No receivers found',   &
            '                File: ' // rcvFil
      CLOSE(lfnLoc)
      CALL exitrc(2)
    ENDIF


! Allocate some memory
! --------------------
    ALLOCATE(rcvLst(nRcv),stat=iac)
    CALL alcErr(iac,'rcvLst',(/nRcv/),srName)

    DO ii=1,nRcv
      ALLOCATE(rcvLst(ii)%sys(nTyp),stat=iac)
      CALL alcErr(iac,'rcvLst%sys',(/nTyp/),srName)
      ALLOCATE(rcvLst(ii)%L1(nTyp,maxTypr),stat=iac)
      CALL alcErr(iac,'rcvLst%L1',(/nTyp,maxTypr/),srName)
      ALLOCATE(rcvLst(ii)%L2(nTyp,maxTypr),stat=iac)
      CALL alcErr(iac,'rcvLst%L2',(/nTyp,maxTypr/),srName)
      ALLOCATE(rcvLst(ii)%C1(nTyp,maxTypr),stat=iac)
      CALL alcErr(iac,'rcvLst%C1',(/nTyp,maxTypr/),srName)
      ALLOCATE(rcvLst(ii)%C2(nTyp,maxTypr),stat=iac)
      CALL alcErr(iac,'rcvLst%C2',(/nTyp,maxTypr/),srName)
      rcvLst(ii)%sys = ""
      rcvLst(ii)%L1  = ""
      rcvLst(ii)%L2  = ""
      rcvLst(ii)%C1  = ""
      rcvLst(ii)%C2  = ""
    ENDDO


! Read all receivers
! ------------------
    nRcv = 0
    DO
      READ(lfnLoc,"(A)",ioStat=ioStat) line
      IF (ioStat<0) EXIT
      IF (LEN_TRIM(line) == 0) CYCLE

! New receiver type
      IF (LEN_TRIM(line(1:20)) /= 0) THEN
        nRcv = nRcv+1
        rcvLst(nRcv)%name = TRIM(line(1:20))
        IF (TRIM(rcvLst(nRcv)%name) == 'DEFAULT') iDef = nRcv
      ENDIF

! Read observation types
      helpint = maxTypr+2
      CALL splStr(line(23:80),helpint,' ',nStr,subStr,irc)

! Update system list
      idx = listc1(1,3,nTyp,rcvLst(nRcv)%sys,substr(1),rcvLst(nRcv)%nSys)

! Store observation types
      nHlp = 0
      DO ii=3,nStr
        IF (LEN_TRIM(subStr(ii)) == 2) &
          subStr(ii) = subStr(2)(1:1)//TRIM(subStr(ii))

        IF (LEN_TRIM(subStr(ii)) == 0) EXIT
        IF (TRIM(subStr(2)) == "L1") &
             dmy = listC1(1,3,maxTypr,rcvLst(nRcv)%L1(idx,:),subStr(ii),nHlp)
        IF (TRIM(subStr(2)) == "L2") &
             dmy = listC1(1,3,maxTypr,rcvLst(nRcv)%L2(idx,:),subStr(ii),nHlp)
        IF (TRIM(subStr(2)) == "C1") &
             dmy = listC1(1,3,maxTypr,rcvLst(nRcv)%C1(idx,:),subStr(ii),nHlp)
        IF (TRIM(subStr(2)) == "C2") &
             dmy = listC1(1,3,maxTypr,rcvLst(nRcv)%C2(idx,:),subStr(ii),nHlp)
      ENDDO

    ENDDO
    CLOSE(lfnLoc)


! End of subroutine
! -----------------
    RETURN
  END SUBROUTINE defRcv


! --------------------------------------------------------------------------
  SUBROUTINE rcvObs(rec,prn,L1,L2,C1,C2,recout)
! --------------------------------------------------------------------------
! Purpose:   Returns the list of observation types for a receiver/PRN
!
! Author:    M. Meindl
!
! Created:   25-May-2012
!
! Changes:   06-Jun-2012 LP: Use svn2chr as module now
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! --------------------------------------------------------------------------

! Used Modules
    USE s_svn2chr

! No implicit
    IMPLICIT NONE

! Parameter declaration
! ---------------------
    CHARACTER(LEN=6),PARAMETER    :: srName = 'RCVOBS'


! Argument declaration
! --------------------
    CHARACTER(LEN=*)                   :: rec     ! Receiver name
    INTEGER(i4b)                       :: prn     ! PRN
    CHARACTER(LEN=20),OPTIONAL         :: recout  ! Name of selected receiver
    CHARACTER(LEN=3),DIMENSION(maxTypr):: L1      ! L1
    CHARACTER(LEN=3),DIMENSION(maxTypr):: L2      ! L2
    CHARACTER(LEN=3),DIMENSION(maxTypr):: C1      ! C1
    CHARACTER(LEN=3),DIMENSION(maxTypr):: C2      ! C2


! Local Variables
! ---------------
    CHARACTER(LEN=3)          :: sys
    CHARACTER(LEN=1)          :: svnChr
    INTEGER(i4b)              :: svnMod
    INTEGER(i4b)              :: iRec, iSys
    INTEGER(i4b)              :: ii, idx
    LOGICAL                   :: found


! First call
! ----------
    IF (first) CALL defRcv


! Initialization of all variables
! -------------------------------
    L1 = ''
    L2 = ''
    C1 = ''
    C2 = ''


! Find receiver
! -------------

! Single receiver
    found = .false.
    iRec  = 0
    DO ii=1,SIZE(rcvLst)
      idx = INDEX(rcvLst(ii)%name,'*')-1
      IF (idx >= 0) THEN
        found = ( rcvLst(ii)%name(1:idx) == rec(1:idx) )
      ELSE
        found = ( TRIM(rcvLst(ii)%name) == TRIM(rec) )
      ENDIF

      IF (found) THEN
        iRec = ii
        EXIT
      ENDIF
    ENDDO

! Default receiver
    IF (.NOT. found) iRec = iDef

! Receiver not found
    IF (iRec == 0) THEN
      WRITE(lfnErr,'(3(/,A),/)')                          &
            ' *** SR RCVOBS: Receiver/default not found', &
            '                Receiver: ' // TRIM(rec),    &
            '                File    : ' // rcvFil
      CALL exitrc(2)
    ENDIF


! Find satellite
! --------------
    CALL svn2chr(prn,svnmod,svnchr)
    WRITE(sys,'(A1,I2.2)') svnchr,svnmod

! Single satellite
    iSys = 0
    DO ii=1,rcvLst(iRec)%nSys
      IF (rcvLst(iRec)%sys(ii) /= sys) CYCLE
      iSys = ii
      EXIT
    ENDDO

! System
    IF (iSys == 0) THEN
      DO ii=1,rcvLst(iRec)%nSys
        IF (TRIM(rcvLst(iRec)%sys(ii)) /= sys(1:1)) CYCLE
        iSys = ii
        EXIT
      ENDDO
    ENDIF

! Satellite not found
    IF (iSys == 0) THEN
      WRITE(lfnErr,'(5(/,A),/)')                                         &
!        ' *** SR RCVOBS: Satellite/system not found',                   &
        ' ### SR RCVOBS: Satellite/system not found',                    &
        '                Receiver name    : ' // TRIM(rec),              &
        '                Selected receiver: ' // TRIM(rcvLst(iRec)%name),&
        '                PRN              : ' // sys,                    &
        '                File             : ' // rcvFil
!      CALL exitrc(2)
    ENDIF


! Fill output arrays
! ------------------
    L1 = rcvLst(iRec)%L1(iSys,:)
    L2 = rcvLst(iRec)%L2(iSys,:)
    C1 = rcvLst(iRec)%C1(iSys,:)
    C2 = rcvLst(iRec)%C2(iSys,:)

    IF (PRESENT(recout)) WRITE(recout,'(A)')TRIM(rcvLst(iRec)%name)

! End of subroutine
! -----------------
    RETURN
  END SUBROUTINE rcvObs




END MODULE d_rcvr
