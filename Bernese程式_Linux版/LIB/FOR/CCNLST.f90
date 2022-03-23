MODULE s_CCNLST
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccnlst(CCopt, CCFil, InClkHead, OutClkHead, irCode)

! -------------------------------------------------------------------------
! Purpose:    Compiles the list of data types, stations, and satellites
!             for the combined clock output file
!
! Parameters:
!        in : CCopt     : CCRNXC input options                      t_ccrnxc_opt
!             CCFil     : Names of the clock rinex files            t_ccrnxc_fil
!             InClkHead : Header from all input files               t_clkhead(*)
!       out : OutClkHead: Output Header with the new lists          t_clkhead
!             irCode    : return code                               i4b
!                         (0: OK, 1: reference station not found)
!
!
! Author:     R. Dach
!
! Created:    18-Aug-2000
! Last mod.:  19-Jul-2010
!
! Changes:    14-Feb-2001 RD: Use ALCERR
!             14-May-2001 RD: Improved structure od reference clock array
!             06-Jun-2001 RD: Enable sat as reference clocks
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Jan-2004 RD: Use m_bern, ONLY for modules
!             24-Nov-2006 AG: timsys, pgmnam, dcbstr, pcvstr added to ClkHead
!             19-Jul-2010 SL: tab characters removed
!
! SR used:    alcerr
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_ccrnxc_fil
  USE s_alcerr
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  TYPE(t_ccrnxc_opt)             :: CCopt         ! ccrnxc input options
  TYPE(t_ccrnxc_fil)             :: CCFil         ! clock rinex file names
  TYPE(t_clkhead), DIMENSION(:), POINTER          &
                                 :: InClkHead     ! Headers of the input files
  TYPE(t_clkhead)                :: OutClkHead    ! New output header
  INTEGER(i4b)                   :: irCode        ! Return code from the sr
!
! Local variables
! ---------------
  TYPE(t_clkhead)                :: HlpClkHead    ! A help variable
!
!
  INTEGER(i4b)                   :: iFil       ! Counts the file in list
  INTEGER(i4b)                   :: iSta       ! Counts the stations in list
  INTEGER(i4b)                   :: jSta       ! Counts the stations in list
  INTEGER(i4b)                   :: iClk       ! Counts in the clock list
  INTEGER(i4b)                   :: nClk       ! Number of clocks in list
  INTEGER(i4b)                   :: iSat       ! Counts the satellites in list
  INTEGER(i4b)                   :: jSat       ! Counts the satellites in list
  INTEGER(i4b)                   :: iTyp       ! Counts the data types in list
  INTEGER(i4b)                   :: jTyp       ! Counts the data types in list
  INTEGER(i4b)                   :: iRef       ! Counts the reference clocks in list
  INTEGER(i4b)                   :: kRef       ! Counts the reference clocks in list
  INTEGER(i4b)                   :: ios        ! io status
!
  LOGICAL                        :: NewSta       ! Insert station in new list?
  LOGICAL                        :: NewSat       ! Insert satellite in new list?
  LOGICAL                        :: NewTyp       ! Insert data type in new list?
  LOGICAL                        :: RefFound     ! Reference Clock found in list and valid
!
! Initialization
! --------------
  OutClkHead%NumTyp=0
  ALLOCATE(OutClkHead%DatTyp(2),stat=ios)
  CALL alcerr(ios,'OutClkHead%DatTyp',(/2/),'ccnlst')
!
  OutClkHead%nSta=0
  OutClkHead%nSat=0
  ALLOCATE(OutClkHead%ClkName(10),stat=ios)
  CALL alcerr(ios,'OutClkHead%ClkName',(/10/),'ccnlst')
  ALLOCATE(OutClkHead%StaCoord(3,10),stat=ios)
  CALL alcerr(ios,'OutClkHead%StaCoord',(/3,10/),'ccnlst')
!
! Take the reference clock definition from first input file
! ---------------------------------------------------------
  OutClkHead%numRef=InClkHead(1)%numRef
!
  ALLOCATE(OutClkHead%Ref(OutClkHead%numRef), stat=ios)
  CALL alcerr(ios,'OutClkHead%Ref',(/OutClkHead%numRef/),'ccnlst')
  OutClkHead%Ref=InClkHead(1)%Ref
!
! Loop over all input files
! -------------------------
  DO iFil=1,SIZE(InClkHead)
!
! New list of data types
! ----------------------
    TypLoop: DO iTyp=1,InClkHead(iFil)%NumTyp
      NewTyp=.TRUE.
      NewTypLoop: DO jTyp=1,OutClkHead%NumTyp
        IF (InClkHead(iFil)%DatTyp(iTyp)==OutClkHead%DatTyp(jTyp)) THEN
          NewTyp=.FALSE.
          EXIT NewTypLoop
        ENDIF
      ENDDO NewTypLoop
      IF (NewTyp) THEN
        OutClkHead%NumTyp=OutClkHead%NumTyp+1
!
! The "DatTyp" array has to be extented
! -------------------------------------
        IF (OutClkHead%NumTyp>SIZE(OutClkHead%DatTyp)) THEN
          ALLOCATE(HlpClkHead%DatTyp(OutClkHead%NumTyp-1),stat=ios)
          CALL alcerr(ios,'HlpClkHead%DatTyp',(/OutClkHead%NumTyp-1/),'ccnlst')
          HlpClkHead%DatTyp(:)=OutClkHead%DatTyp(:)
          DEALLOCATE(OutClkHead%DatTyp)
!
          ALLOCATE(OutClkHead%DatTyp(OutClkHead%NumTyp+1),stat=ios)
          CALL alcerr(ios,'OutClkHead%DatTyp',(/OutClkHead%NumTyp+1/),'ccnlst')
          OutClkHead%DatTyp(1:OutClkHead%NumTyp-1) = &
                       HlpClkHead%DatTyp(1:OutClkHead%NumTyp-1)
          DEALLOCATE(HlpClkHead%DatTyp)
        ENDIF
!
! Add the new value to the list
! -----------------------------
        OutClkHead%DatTyp(OutClkHead%NumTyp)=InClkHead(iFil)%DatTyp(iTyp)
      ENDIF
    ENDDO TypLoop
  ENDDO
!
! New list of stations
! --------------------
  DO iFil=1,SIZE(InClkHead)
    StaLoop: DO iSta=1,InClkHead(iFil)%nSta
      NewSta=.TRUE.
      NewStaLoop: DO jSta=1,OutClkHead%nSta
        IF ((InClkHead(iFil)%ClkName(iSta) == OutClkHead%ClkName(jSta))) THEN
          NewSta=.FALSE.
          EXIT NewStaLoop
        ENDIF
      ENDDO NewStaLoop
      IF (NewSta) THEN
        OutClkHead%nSta=OutClkHead%nSta+1
!
! The station arrays have to be extented
! --------------------------------------
        IF (OutClkHead%nSta>SIZE(OutClkHead%ClkName)) THEN
          ALLOCATE(HlpClkHead%ClkName(OutClkHead%nSta-1),stat=ios)
          CALL alcerr(ios,'HlpClkHead%ClkName',(/OutClkHead%nSta/),'ccnlst')
          ALLOCATE(HlpClkHead%StaCoord(3,OutClkHead%nSta-1),stat=ios)
          CALL alcerr(ios,'HlpClkHead%StaCoord',(/3,OutClkHead%nSta/),'ccnlst')
          HlpClkHead%ClkName(:) = OutClkHead%ClkName(:)
          HlpClkHead%StaCoord(:,:) = OutClkHead%StaCoord(:,:)
          DEALLOCATE(OutClkHead%ClkName)
          DEALLOCATE(OutClkHead%StaCoord)
!
          ALLOCATE(OutClkHead%ClkName(OutClkHead%nSta+9),stat=ios)
          CALL alcerr(ios,'OutClkHead%ClkName',(/OutClkHead%nSta+9/),'ccnlst')
          ALLOCATE(OutClkHead%StaCoord(3,OutClkHead%nSta+9),stat=ios)
          CALL alcerr(ios,'OutClkHead%StaCoord',(/3,OutClkHead%nSta+9/),&
                                                                      'ccnlst')
          OutClkHead%ClkName(1:OutClkHead%nSta-1)       = &
                                   HlpClkHead%ClkName(1:OutClkHead%nSta-1)
          OutClkHead%StaCoord(1:3,1:OutClkHead%nSta-1) = &
                                   HlpClkHead%StaCoord(1:3,1:OutClkHead%nSta-1)
          DEALLOCATE(HlpClkHead%ClkName)
          DEALLOCATE(HlpClkHead%StaCoord)
        ENDIF
!
! Add the new values to the lists
! -------------------------------
        OutClkHead%ClkName(OutClkHead%nSta)=InClkHead(iFil)%ClkName(iSta)
        OutClkHead%StaCoord(:,OutClkHead%nSta)=InClkHead(iFil)%StaCoord(:,iSta)
      ENDIF
    ENDDO StaLoop
  ENDDO

!
! New list of satellites
! ----------------------
  DO iFil=1,SIZE(InClkHead)
    SatLoop: DO iSat=InClkHead(iFil)%nSta+1, &
                     InClkHead(iFil)%nSta+InClkHead(iFil)%nSat
      NewSat=.TRUE.
      NewSatLoop: DO jSat=OutClkHead%nSta+1, &
                          OutClkHead%nSta+OutClkHead%nSat
        IF (InClkHead(iFil)%ClkName(iSat)==OutClkHead%ClkName(jSat)) THEN
          NewSat=.FALSE.
          EXIT NewSatLoop
        ENDIF
      ENDDO NewSatLoop
      IF (NewSat) THEN
        OutClkHead%nSat=OutClkHead%nSat+1
!
! The "SatNum" array has to be extented
! -------------------------------------
        IF (OutClkHead%nSat+OutClkHead%nSta>SIZE(OutClkHead%ClkName)) THEN
          nClk=OutClkHead%nSat+OutClkHead%nSta-1
          ALLOCATE(HlpClkHead%ClkName(nClk),stat=ios)
          CALL alcerr(ios,'HlpClkHead%ClkName',(/nClk/),'ccnlst')
          HlpClkHead%ClkName(:)=OutClkHead%ClkName(:)
          DEALLOCATE(OutClkHead%ClkName)
!
          ALLOCATE(OutClkHead%ClkName(nClk+4),stat=ios)
          CALL alcerr(ios,'OutClkHead%Clkname',(/nClk+4/),'ccnlst')
          OutClkHead%ClkName(1:nClk) = HlpClkHead%Clkname(1:nClk)
          DEALLOCATE(HlpClkHead%Clkname)
        ENDIF
!
! Add the new value to the list
! -----------------------------
        OutClkHead%ClkName(OutClkHead%nSta+OutClkHead%nSat) = &
                                   InClkHead(iFil)%ClkName(iSat)
      ENDIF
    ENDDO SatLoop
!
! Get the index for the reference stations of every file
! ------------------------------------------------------
    DO kRef=1,InClkHead(iFil)%numRef
      RefLoop: DO iRef=1,InClkHead(iFil)%Ref(kRef)%nRef
        RefFound=.FALSE.
        DO iSta=1,InClkHead(iFil)%nSta+InClkHead(iFil)%nSat
          IF ((InClkHead(iFil)%Ref(kRef)%clk(iRef)%name  == &
                                    InClkHead(iFil)%ClkName(iSta)))   THEN
            RefFound=.TRUE.
            EXIT
          ENDIF
        ENDDO
        IF (RefFound) THEN
          InClkHead(iFil)%Ref(kRef)%Clk(iRef)%Idx=iSta
        ELSE
          InClkHead(iFil)%Ref(kRef)%Clk(iRef)%Idx = -1
          IF (CCopt%refFil) THEN
            WRITE(lfnerr,'(/,A,/,A,A,/,A,A,A,A,//)')                           &
            ' *** SR CCNLST: Reference station was not found in station list:',&
            '                FILE   : ',TRIM(CCfil%ClkFilNam(iFil)),           &
            '                STATION: ',                                       &
                                 TRIM(InClkHead(iFil)%Ref(kref)%clk(iRef)%name)
            irCode=1
          ENDIF
        ENDIF
      ENDDO RefLoop
    ENDDO
  ENDDO
!
! Make some arrays empty behind the data
! --------------------------------------
  DO iClk=OutClkHead%nSta+OutClkHead%nSat+1,SIZE(OutClkHead%ClkName)
    OutClkHead%Clkname(iClk)=''
  ENDDO
  DO iTyp=OutClkHead%NumTyp+1,SIZE(OutClkHead%DatTyp)
    OutClkHead%DatTyp(iTyp)='  '
  ENDDO
!
! Put all other information into the new header record
! ----------------------------------------------------
  OutClkHead%ProgNam    = 'CCRNXC V'//PGMVER
  OutClkHead%LeapSec    = InClkHead(1)%LeapSec
  OutClkHead%timsys     = InClkHead(1)%timsys
  OutClkHead%pgmnam     = InClkHead(1)%pgmnam
  OutClkHead%dcbStr     = InClkHead(1)%dcbStr
  OutClkHead%pcvStr     = InClkHead(1)%pcvStr
  OutClkHead%TRFName    = InClkHead(1)%TRFName
!
  RETURN
  END SUBROUTINE

END MODULE
