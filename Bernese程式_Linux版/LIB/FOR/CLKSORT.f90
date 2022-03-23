MODULE s_CLKSORT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE clkSort(iTask,ClkHead,Index)

! -------------------------------------------------------------------------
! Purpose:    Sorts the clock for the clock rinex output (Index is used!)
!
! Parameters:
!         in: iTask   : ==0: list of sta/sat clocks is sorted           i4b
!                       > 0: corresponding reference list is sorted
!             ClkHead : Information for rinex file header            t_clkhead
!        out: Index   : Index for sorted clocks                         i4b(*)
!
!
! Author:     R. Dach
!
! Created:    15-May-2001
! Last mod.:  07-Feb-2005
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             23-Apr-2003 CU: Nullify local pointers
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!
! SR used:    alcerr
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead

  USE s_alcerr
  IMPLICIT NONE
!
! Variables for the parameter list
! --------------------------------
  INTEGER(i4b)      :: iTask          ! > 0: sort reference clocks
                                      ! ==0: sort sta/sat clock list
  TYPE(t_clkhead)   :: clkhead        ! Clock rinex header
  INTEGER(i4b), DIMENSION(:), POINTER :: Index ! Index to order clocks
!
! Local variables
! ---------------
  INTEGER(i4b), DIMENSION(:), POINTER :: RefIdx ! Index for reference clocks
  LOGICAL           :: isOK           ! List is ordered
  INTEGER(i4b)      :: iRef,jRef      ! Counter for ref clocks
  INTEGER(i4b)      :: iSta           ! Counter for sta clocks
  INTEGER(i4b)      :: iSat           ! Counter for sat clocks
  INTEGER(i4b)      :: iClk           ! Counter for clocks
  INTEGER(i4b)      :: IdxHlp         ! Used for sort the entries
  INTEGER(i4b)      :: ios            ! IO status
  INTEGER(i4b)      :: numRef
!
  NULLIFY(RefIdx)
!
! Deallocate the Index array
! --------------------------
  DEALLOCATE(Index,stat=ios)
!
! Order the reference clocks of record jRef
! -----------------------------------------
  IF (iTask > 0) THEN
    jRef=iTask
!
! Init Index-array
! ----------------
!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
    numRef = ClkHead%ref(jRef)%nRef
    ALLOCATE(Index(numRef), stat=ios)
    CALL alcerr(ios,'Index',(/numRef/),'clksort')
#else
    ALLOCATE(Index(ClkHead%ref(jRef)%nRef), stat=ios)
    CALL alcerr(ios,'Index',(/ClkHead%ref(jRef)%nRef/),'clksort')
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Index=(/(IdxHlp,IdxHlp=1,ClkHead%ref(jRef)%nRef)/)
!
! Generate the sort order for the reference clocks
! ------------------------------------------------
    isOK = .FALSE.
    DO WHILE (.NOT. isOK)
      isOK = .TRUE.
      DO iRef=1,ClkHead%ref(jRef)%nRef-1
        IF (ClkHead%ref(jRef)%clk(Index(iRef))%name > &
                            ClkHead%ref(jRef)%clk(Index(iRef+1))%name) THEN
          isOK=.FALSE.
          IdxHlp=Index(iRef)
          Index(iRef)=Index(iRef+1)
          Index(iRef+1)=IdxHlp
        ENDIF
      ENDDO
    ENDDO
  ELSE IF (iTask == 0) THEN
!
! Allocate and init reference clock index
! ---------------------------------------
    ALLOCATE(refIdx(ClkHead%nSta+ClkHead%nSat), stat=ios)
    CALL alcerr(ios,'refIdx',(/ClkHead%nSta+ClkHead%nSat/),'clksort')
    refIdx = ClkHead%numRef+1
!
    iRef=ClkHead%numRef
    DO WHILE (iRef > 0)
      DO jRef=1,ClkHead%Ref(iRef)%nRef
        DO iClk=1,ClkHead%nSta+ClkHead%nSat
          IF (ClkHead%Ref(iRef)%clk(jRef)%name == clkHead%clkName(iClk)) &
            RefIdx(iClk) = iRef
        ENDDO
      ENDDO
      iRef=iRef-1
    ENDDO
!
! Allocate and init Index
! -----------------------
    ALLOCATE(Index(ClkHead%nSta+ClkHead%nSat), stat=ios)
    CALL alcerr(ios,'Index',(/ClkHead%nSta+ClkHead%nSat/),'clksort')
!
    Index=(/(IdxHlp,IdxHlp=1,ClkHead%nSta+ClkHead%nSat)/)
!
! Order station clocks
! --------------------
    isOK = .FALSE.
    DO WHILE (.NOT. isOK)
      isOK = .TRUE.
      DO iSta=1,ClkHead%nSta-1
        IF ((refIdx(Index(iSta)) == refIdx(Index(iSta+1)) .AND. &
             ClkHead%clkName(Index(iSta)) > &
                          ClkHead%clkName(Index(iSta+1))) .OR. &
            (refIdx(Index(iSta)) > refIdx(Index(iSta+1))))    THEN
          isOK=.FALSE.
          IdxHlp=Index(iSta)
          Index(iSta)=Index(iSta+1)
          Index(iSta+1)=IdxHlp
        ENDIF
      ENDDO
    ENDDO
!
! Order satellite clocks
! ----------------------
    isOK = .FALSE.
    DO WHILE (.NOT. isOK)
      isOK = .TRUE.
      DO iSat=ClkHead%nSta+1,ClkHead%nSta+ClkHead%nSat-1
        IF ((refIdx(Index(iSat)) == refIdx(Index(iSat+1)) .AND. &
             ClkHead%clkName(Index(iSat)) > &
                          ClkHead%clkName(Index(iSat+1))) .OR. &
            (refIdx(Index(iSat)) > refIdx(Index(iSat+1))))    THEN
          isOK=.FALSE.
          IdxHlp=Index(iSat)
          Index(iSat)=Index(iSat+1)
          Index(iSat+1)=IdxHlp
        ENDIF
      ENDDO
    ENDDO
!
    DEALLOCATE(refIdx,stat=ios)
  ENDIF
!
  RETURN
  END SUBROUTINE clkSort

END MODULE
