MODULE s_MENU_REF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_ref(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of all clocks from clock rinex files
!             in CCRNXC (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    04-Jul-2001
!
! Changes:    05-Sep-2001 HU: Interface for rdcrxh added
!             29-Apr-2002 RD: Use keywords from MENUAUX.INP
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize and deallocate structure
!             03-May-2004 HB: Remove '0' for GPS satellite numbers, e.g.,
!                             '03' => ' 3'
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, lfnloc, t_key, &
                      fileNameLength, staNameLength
  USE d_clkrnx, ONLY: t_clkhead,init_clkhead
  USE s_gtfile2
  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_rdcrxh
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord         ! what to do

! output:
  TYPE(t_key)                            :: output   ! name = keyWord, if OK
                                                     ! value: Result to display

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER    :: srName = 'menu_ref'

! Local Variables
! ---------------
  TYPE(t_clkhead)                :: clkhead

  CHARACTER(LEN=fileNameLength),  &
       DIMENSION(:,:),POINTER    :: filList
  CHARACTER(LEN=staNameLength),   &
       DIMENSION(:), ALLOCATABLE :: staList
  CHARACTER(LEN=staNameLength),   &
       DIMENSION(:), ALLOCATABLE :: hlpList

  INTEGER(i4b),                   &
      DIMENSION(:),ALLOCATABLE   :: staIdx ! Index for sorted stations
  INTEGER(i4b)                   :: numFil
  INTEGER(i4b)                   :: numSta
  INTEGER(i4b)                   :: ii, jj, kk
  INTEGER(i4b)                   :: ios, irc, iac

  LOGICAL                        :: sorted

  NULLIFY(filList)
  CALL init_clkHead(clkHead)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'SELREF_STA' .AND. keyWord /= 'SELREF_SAT') RETURN

  CALL gtfile2('RCLKINP',1,numFil,filList)
  IF (numFil == 0) RETURN

  numSta=0
  DO ii=1,numFil
!
! read clock rinex header
! -----------------------
    CALL opnfil(lfnloc, filList(1,ii), 'OLD', 'FORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,filList(1,ii),srName)

    clkHead%tFirst = 0d0
    CALL rdcrxh(lfnloc,lfnerr,clkhead,irc)
    CLOSE(lfnloc)

    IF (irc==0) THEN

! Allocate memory in the first call
! ---------------------------------
      IF (ii == 1) THEN
        ALLOCATE(staList(clkHead%nSta+clkHead%nSat), stat=iac)
        CALL alcerr(iac, 'staList', (/clkHead%nSta+clkHead%nSat/), srName)
      ENDIF
!
! Build a list of stations
! ------------------------
      IF (keyWord == 'SELREF_STA') THEN
        jjLoop1: DO jj=1,clkHead%nSta
          DO kk=1,numSta
            IF (staList(kk) == clkHead%ClkName(jj)) THEN
              CYCLE jjLoop1
            ENDIF
          ENDDO

! Extent the list of stations
! ---------------------------
          IF (SIZE(staList) == numSta) THEN
            ALLOCATE(hlpList(numSta), stat=iac)
            CALL alcerr(iac, 'hlpList', (/numSta/), srName)

            hlpList(1:numSta) = staList(1:numSta)

            DEALLOCATE(staList, stat=iac)
            ALLOCATE(staList(numSta+clkHead%nSta+clkHead%nSat), stat=iac)
            CALL alcerr(iac, 'staList', &
                 (/numSta+clkHead%nSta+clkHead%nSat/), srName)

            staList(1:numSta) = hlpList(1:numSta)

            DEALLOCATE(hlpList, stat=iac)
          ENDIF

! Put the new clocks into the list
! --------------------------------
          numSta=numSta+1
          staList(numSta) = TRIM(clkHead%ClkName(jj))
        ENDDO jjLoop1
!
! Build a list of satellites
! --------------------------
      ELSE IF (keyWord == 'SELREF_SAT') THEN
        DO jj=1,clkHead%nSat
          IF (clkHead%ClkName(clkHead%nSta+jj)(1:1) == 'G') THEN
            clkHead%ClkName(clkHead%nSta+jj)(1:1) = ' '
            IF (clkHead%ClkName(clkHead%nSta+jj)(2:2) == '0') &
              clkHead%ClkName(clkHead%nSta+jj)(2:2) = ' '
          ENDIF
          IF (clkHead%ClkName(clkHead%nSta+jj)(1:1) == 'R')  &
            clkHead%ClkName(clkHead%nSta+jj)(1:1) = '1'
        ENDDO
        jjLoop2: DO jj=1,clkHead%nSat
          DO kk=1,numSta
            IF (staList(kk) == TRIM(clkHead%ClkName(clkHead%nSta+jj))) THEN
              CYCLE jjLoop2
            ENDIF
          ENDDO

! Extent the list of stations
! ---------------------------
          IF (SIZE(staList) == numSta) THEN
            ALLOCATE(hlpList(numSta), stat=iac)
            CALL alcerr(iac, 'hlpList', (/numSta/), srName)

            hlpList(1:numSta) = staList(1:numSta)

            DEALLOCATE(staList, stat=iac)
            ALLOCATE(staList(numSta+clkHead%nSta+clkHead%nSat), stat=iac)
            CALL alcerr(iac, 'staList', &
                 (/numSta+clkHead%nSta+clkHead%nSat/), srName)

            staList(1:numSta) = hlpList(1:numSta)

            DEALLOCATE(hlpList, stat=iac)
          ENDIF

! Put the new clocks into the list
! --------------------------------
          numSta=numSta+1
          staList(numSta) = TRIM(clkHead%ClkName(clkHead%nSta+jj))
        ENDDO jjLoop2
      ENDIF
    ENDIF
  ENDDO

  DEALLOCATE(filList,stat=iac)

  IF (numSta == 0) RETURN

! Sort clock names
! ----------------
  ALLOCATE(staIdx(numSta),stat=iac)
  CALL alcerr(iac,'staIdx',(/numSta/),srName)

  staIdx = (/ (ii, ii=1,numSta) /)

  sorted = .TRUE.
  DO WHILE (sorted)
    sorted = .FALSE.
    DO jj = 1,numSta-1
      IF (staList(staIdx(jj)) > staList(staIdx(jj+1))) THEN
        ii           = staIdx(jj)
        staIdx(jj)   = staIdx(jj+1)
        staIdx(jj+1) = ii

        sorted = .TRUE.
      ENDIF
    ENDDO
  ENDDO

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(numSta),stat=irc)
  CALL alcerr(irc,'output%value',(/ numSta /),srName)
  output%value = ' '

  DO ii=1,numSta
    output%value(ii) = TRIM(staList(staIdx(ii)))
  ENDDO

  DEALLOCATE(staIdx, stat=iac)
  DEALLOCATE(staList,stat=iac)
  DEALLOCATE(filList,stat=iac)
  DEALLOCATE(clkhead%comment, stat=iac)
  DEALLOCATE(clkhead%DatTyp, stat=iac)
  DEALLOCATE(clkhead%clkName, stat=iac)
  DEALLOCATE(clkhead%staCoord, stat=iac)
  DO ii=1,clkHead%numRef
    DEALLOCATE(clkHead%Ref(ii)%clk,stat=iac)
  ENDDO
  DEALLOCATE(clkhead%ref, stat=iac)


  RETURN
END SUBROUTINE menu_ref

END MODULE
