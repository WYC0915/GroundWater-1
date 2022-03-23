MODULE s_SELSTA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE selsta(filename, stacruxin, stacruxout, flag)

! -----------------------------------------------------------------------------
! Purpose:    This subroutine filters station information of the selected
!             stations from stacruxin to stacruxout. The selection text file
!             'filename' contains only the 4-character-IDs of the selected
!             stations
!
! Author:     D. Lenhart
!
! Created:    19-Feb-2003
!
! Changes:    02-May-2003 PS: Initialize Remark and Description with space
!                             Added flag
!             21-May-2003 DL: Removed call to LFNUM.inc
!             30-Aug-2003 HU: Error output adapted, filename has open length
!             02-Sep-2003 RD: Use Bernese formatted station selection file
!             01-Oct-2010 SL: use m_bern with ONLY, flag (INTEGER->CHARACTER)
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ----------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnPrt
  USE d_stacrx, ONLY: t_stacrux
  USE d_stalst, ONLY: t_staList, init_staList

  USE s_alcerr
  USE s_readstsg
  IMPLICIT NONE

! List of parameters
! ------------------
  CHARACTER(LEN=*)   ::  filename
  CHARACTER(LEN=3)   ::  flag
  TYPE(t_stacrux)    ::  stacruxin, stacruxout


! Local Prameters
! ---------------
  CHARACTER(LEN=6), PARAMETER          :: srName = 'selsta'

  INTEGER(i4b),     PARAMETER          :: nChr   = 14 ! Test only 14 characters
                                                      ! of station names

! Local Variables
! ---------------
  TYPE(t_staList)                      :: staList

  INTEGER(i4b)                         :: iInfo, jInfo
  INTEGER(i4b)                         :: nInfo
  INTEGER(i4b)                         :: iSta
  INTEGER(i4b), DIMENSION(:), POINTER  :: pos
  INTEGER(i4b)                         :: iac

  LOGICAL                              :: prtHead
  LOGICAL                              :: found


! Nullify pointers
! ----------------
  NULLIFY(pos)

! Read stetion selection list
! ---------------------------
  CALL init_staList(staList)

  CALL readstsg(filename,0,staList)

! Allocate memory for position list
! ---------------------------------
  ALLOCATE(pos(stacruxin%ninfo),stat=iac)
  CALL alcerr(iac,'pos',(/stacruxin%ninfo/),srName)


! Find position of selected stations and size of output
! -----------------------------------------------------
  nInfo = 0
  DO iInfo = 1,staCruxIn%nInfo
    DO iSta = 1,staList%nSta
      IF (staList%stanam(iSta)(1:nChr) ==       &
          stacruxin%stainfo(iInfo)%stanam(1:nChr)) THEN
        nInfo = nInfo+1
        pos(nInfo) = iInfo
        EXIT
      ENDIF
    ENDDO
  ENDDO

! Report stations not in SINEX file
! ---------------------------------
  prtHead = .FALSE.
  DO iSta = 1,staList%nSta
    found = .FALSE.
    DO iInfo = 1,staCruxIn%nInfo
      IF (staList%stanam(iSta)(1:nChr) ==       &
          stacruxin%stainfo(iInfo)%stanam(1:nChr)) THEN
        found = .TRUE.
        EXIT
      ENDIF
    ENDDO
    IF (.NOT. found) THEN
      IF (.NOT. prtHead) THEN
        WRITE(lfnprt,'(A)') 'Stations not in SINEX-File:'
        prtHead = .TRUE.
      ENDIF
      WRITE(lfnprt,'(A)') TRIM(staList%stanam(ista))
    ENDIF
  ENDDO

  DEALLOCATE(staList%staNam,stat=iac)

! Allocate memory for stacruxout
! ------------------------------
  ALLOCATE(stacruxout%stainfo(nInfo),stat=iac)
  CALL alcerr(iac,'stacruxout%stainfo',(/nInfo/),srName)

! Copy info into stacruxout
! -------------------------
  DO jInfo = 1,nInfo
    iInfo = pos(jInfo)
    stacruxout%stainfo(jInfo) = stacruxin%stainfo(iInfo)
    stacruxout%stainfo(jInfo)%flg=flag
  END DO
  stacruxout%ninfo = nInfo

  DEALLOCATE(pos)

  RETURN
END SUBROUTINE selsta

END MODULE
