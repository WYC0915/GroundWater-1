MODULE s_WTCRXH
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wtcrxh(lfnclk,lfnmsg,ClkHead,irCode)

! -------------------------------------------------------------------------
! Purpose:    writes the entire header information of a rinex clock file
!             (based on the f77 sr R2WTCH)
!
! Parameters:
!         in: lfnclk  : Logical file number of clock rinex file         i4b
!             lfnmsg  : Logical file number for error messages          i4b
!        out: ClkHead : Information for rinex file header            t_clkhead
!             irCode  : return code                                     i4b
!                         0: ok
!
!
! Author:     R. Dach
!
! Created:    09-Aug-2000
! Last mod.:  24-Nov-2006
!
! Changes:    18-Feb-2001  RD: More than 1 reference clock set allowed
!             14-May-2001  RD: Improved structure od reference clock array
!             15-May-2001  RD: Order clocks in the output
!             21-May-2001  RD: Modified writing coordinates
!             22-May-2001  RD: Special case if no sat/sta clocks to write
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             25-Sep-2002  HU: Remove i_astlib
!             11-Feb-2003  SS: rxType set to "CLOCK DATA" (instead of "C")
!             23-Apr-2003  RD: Nullify local pointers
!             08-Aug-2005  HB: Use new SR TIMST2 (module)
!             24-Nov-2006  AG: timsys, pgmnam, dcbstr, pcvstr added to ClkHead
!
! SR used:    jmt, radgms,clksort
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead

  USE f_djul
  USE s_clksort
  USE s_timst2
  IMPLICIT NONE
!
! Variables for the parameter list
! --------------------------------
  INTEGER(i4b)      :: lfnclk         ! Logical file number of clock rinex file
  INTEGER(i4b)      :: lfnmsg         ! Logical file number for error messages
  TYPE(t_clkhead)   :: ClkHead        ! Clock rinex header
  INTEGER(i4b)      :: irCode         ! Return code of this SR
!
! Local variables
! ---------------
  CHARACTER(len=shortlinelength) :: line       ! A line string
  CHARACTER(len=60) :: string         ! Help string for content of header
  CHARACTER(len=19) :: epostr         ! Epoch string
  CHARACTER(LEN=1)  :: sysstr         ! SYSTEM CHARACTER 'G','R','E','S'
!
  REAL(r8b)         :: secs, sece     ! Start/End date of file (sec.)
!
  INTEGER(i4b), DIMENSION(:), POINTER :: Index ! Index to order clocks
!
  INTEGER(i4b)      :: I1,I2          ! Computes the position in a string
  INTEGER(i4b)      :: iCom           ! Counts the comment lines
  INTEGER(i4b)      :: iTyp           ! Counts the data types
  INTEGER(i4b)      :: iRef           ! Counts reference clocks
  INTEGER(i4b)      :: jRef           ! Counts reference clocks
  INTEGER(i4b)      :: iSta           ! Counts number of stations
  INTEGER(i4b)      :: iCrd           ! Counts coordinates for writing
  INTEGER(i4b)      :: iSat           ! Counts number of satellites
  INTEGER(i4b)      :: iyyys,mms,ids  ! Start date of file (year, month, day)
  INTEGER(i4b)      :: ihs,ims        ! Start date of file (hour, min.)
  INTEGER(i4b)      :: iyyye,mme,ide  ! End date of file (year, month, day)
  INTEGER(i4b)      :: ihe,ime        ! End date of file (hour, min.)
  INTEGER(i4b)      :: iHead          ! Header record type
  INTEGER(i4b)      :: ios            ! IO status
!
! RINEX FILE TYPE
! ---------------
  CHARACTER(len= 10), PARAMETER :: rxType = 'CLOCK DATA'
!
! MAXIMUM RINEX FORMAT VERSION
! ----------------------------
  REAL(r8b)        , PARAMETER :: rxVers = 2.00D0
!
! HEADER RECORDS
! --------------
  CHARACTER(len=20), DIMENSION(20) :: HEADRC  = &
  (/'RINEX VERSION / TYPE', 'PGM / RUN BY / DATE ', 'COMMENT             ', &
    'LEAP SECONDS        ', '# / TYPES OF DATA   ', 'STATION NAME / NUM  ', &
    'STATION CLK REF     ', 'ANALYSIS CENTER     ', '# OF CLK REF        ', &
    'ANALYSIS CLK REF    ', '# OF SOLN STA / TRF ', 'SOLN STA NAME / NUM ', &
    '# OF SOLN SATS      ', 'PRN LIST            ', 'SYS / # / OBS TYPES ', &
    'TIME SYSTEM ID      ', 'SYS / DCBS APPLIED  ', 'SYS / PCVS APPLIED  ', &
    '                    ', 'END OF HEADER       '/)
!
! WRITE FIRST LINE
! ----------------
  iHead=1
  WRITE(lfnclk,'(F9.2,11X,A10,30X,A20)') RXVers,RXType,HeadRC(iHead)
!
! PGM / RUN BY / DATE
! -------------------
  iHead=2
  WRITE(lfnclk,'(A20,A20,A20,A20)')                                      &
        ClkHead%ProgNam,ClkHead%RunBy,ClkHead%CrDate,HeadRC(iHead)
!
! COMMENT LINES
! -------------
  ihead=3
  DO iCom=1,ClkHead%nComment
    WRITE(lfnclk,'(A60,A20)') ClkHead%Comment(iCom),HeadRc(iHead)
  ENDDO
!
! TIME SYSTEM ID LINES
! --------------------
  ihead=16

  WRITE(lfnclk,'(3X,A3,54X,A20)') ClkHead%timsys,HeadRc(iHead)
!
! LEAP SECONDS
! ------------
  IF (ClkHead%LeapSec /= 0) THEN
    iHead=4
    WRITE(lfnclk,'(I6,54X,A20)') ClkHead%LeapSec,HeadRC(iHead)
  ENDIF
!
! Sort the clocks
! ---------------
  NULLIFY(INDEX)
  CALL clksort(0,clkhead,Index)
!
! SYS / PCVS APPLIED LINES
! ------------------------
  ihead=18
  sysstr=' '
  DO iSat=1,ClkHead%nSat
    IF (ClkHead%ClkName(Index(ClkHead%nSta+isat))(1:1) /= sysstr) THEN
      sysstr = ClkHead%ClkName(Index(ClkHead%nSta+isat))(1:1)
      WRITE(lfnclk,'(A1,1X,A17,1X,A40,A20)') sysstr,ClkHead%pgmnam, &
           ClkHead%pcvstr,HeadRc(iHead)
    ENDIF
  ENDDO
!
! SYS / DCBS APPLIED LINES
! ------------------------
  ihead=17
  sysstr=' '
  DO iSat=1,ClkHead%nSat
    IF (ClkHead%ClkName(Index(ClkHead%nSta+isat))(1:1) /= sysstr) THEN
      sysstr = ClkHead%ClkName(Index(ClkHead%nSta+isat))(1:1)
      WRITE(lfnclk,'(A1,1X,A17,1X,A40,A20)') sysstr,ClkHead%pgmnam, &
           ClkHead%dcbStr,HeadRc(iHead)
    ENDIF
  ENDDO
!
! # / TYPES OF DATA
! -----------------
  iHead=5
  WRITE(string,'(I6)') ClkHead%NumTyp
  DO iTyp=1,ClkHead%NumTyp
    I1=6*iTyp+1
    I2=6*(iTyp+1)
    WRITE(string(I1:I2),'(4X,A2)') ClkHead%DatTyp(iTyp)
  ENDDO
  WRITE(lfnclk,'(A60,A20)') string,HeadRC(iHead)
!
! STATION NAME / NUM (not supported yet)
! ------------------
!  iHead=6
!  WRITE(lfnclk,'(A4,1X,A20,35X,A20)')                                   &
!        ClkHead%ClkName,ClkHead%ClkDomes,HeadRC(iHead)
!
! STATION CLK REF (not supported yet)
! ---------------
!  iHead=7
!  WRITE(lfnclk,'(A60,A20)') ClkHead%CalName,HeadRc(iHead)
!
! ANALYSIS CENTER
! ---------------
  iHead=8
  WRITE(lfnclk,'(A3,2X,A55,A20)') ClkHead%AC,ClkHead%ACName,HeadRC(iHead)
!
! # OF CLK REF
! ------------
  DO jRef=1,ClkHead%numRef
    IF (ClkHead%ref(jRef)%nRef > 0) THEN
      iHead=9
      IF (ClkHead%numRef == 1 .OR. &
          ClkHead%ref(jRef)%refWin%t(1) + ClkHead%ref(jRef)%refWin%t(2) == 0d0) THEN
        WRITE(lfnclk,'(I6,54X,A20)') ClkHead%ref(jRef)%nRef,HeadRC(iHead)
      ELSE
        CALL timst2(2,1, &
             ClkHead%TFirst+ClkHead%ref(jRef)%refWin%t(1)/86400d0,epostr)
        READ(epostr,*) iyyys, mms, ids, ihs, ims
        secs=(ClkHead%TFirst+ClkHead%ref(jRef)%refWin%t(1)/86400d0 - &
               djul(iyyys, mms, DBLE(ids+(ihs+ims/60d0)/24d0)))*86400d0
!
        CALL timst2(2,1, &
             ClkHead%TFirst+ClkHead%ref(jRef)%refWin%t(2)/86400d0,epostr)
        READ(epostr,*) iyyye, mme, ide, ihe, ime
        sece=(ClkHead%TFirst+ClkHead%ref(jRef)%refWin%t(2)/86400d0 - &
               djul(iyyye, mme, DBLE(ide+(ihe+ime/60d0)/24d0)))*86400d0
!
        WRITE(lfnclk,'(I6,1X,I4,4I3,F10.6,1X,I4,4I3,F10.6,A20)')          &
                      ClkHead%ref(jRef)%nRef,iyyys,mms,ids,ihs,ims,secs,  &
                      iyyye,mme,ide,ihe,ime,sece,HeadRC(iHead)
      ENDIF
!
! ANALYSIS CLK REF
! ----------------
      iHead=10
      NULLIFY(index)
      CALL clkSort(jRef,ClkHead,Index)
      DO iRef=1,ClkHead%ref(jRef)%nRef
        string=ClkHead%ref(jRef)%clk(Index(iRef))%Name//'         '
        WRITE(lfnclk,'(A25,15X,E19.12,1X,A20)')                       &
              string(1:25), ClkHead%ref(jRef)%clk(Index(iRef))%Sigma, &
              HeadRC(iHead)
      ENDDO
      DEALLOCATE(Index, stat=ios)
    ENDIF
  ENDDO
!
! Sort the clocks
! ---------------
  NULLIFY(INDEX)
  CALL clksort(0,clkhead,Index)
!
! # OF SOLN STA / TRF
! -------------------
  IF (ClkHead%nSta >= 0) THEN
    iHead=11
    WRITE(lfnclk,'(I6,4X,A50,A20)')                                 &
          ClkHead%nSta,ClkHead%TRFName,HeadRC(iHead)
!
! SOLN STA NAME / NUM (+ CRD (cm))
! --------------------------------
    iHead=12
    DO iSta=1,ClkHead%nSta
      string=ClkHead%ClkName(Index(iSta))
      line=''
      WRITE(line,'(A25)') string(1:25)
!
      DO iCrd=1,3
        i1=iCrd*12+13
        i2=iCrd*12+25
        WRITE(line(i1:i2),'(F13.0)') ClkHead%StaCoord(iCrd,Index(iSta))*1D3
      ENDDO
!
      WRITE(lfnclk,'(A60,A20)') line(1:60),HeadRC(iHead)
    ENDDO
  ENDIF
!
! # OF SOLN SATS
! --------------
  IF (ClkHead%nSat >= 0) THEN
    iHead=13
    WRITE(lfnclk,'(I6,54X,A20)') ClkHead%nSat,HeadRC(iHead)
!
! PRN LIST
! --------
    iHead=14
    DO iSat=1,ClkHead%nSat+14
      I1=MOD((iSat-1)*4+1, 60)
      I2=MOD( iSat*4     , 60)
      IF (I2 == 0) I2 = 60
      IF (isat > ClkHead%nSat) THEN
        WRITE(string(I1:I2),'(A4)')         '    '
      ELSE
        WRITE(string(I1:I2),'(A3,A1)') &
              Clkhead%ClkName(Index(ClkHead%nSta+isat))(1:3),' '
      ENDIF
!
      IF (MOD(iSat,15) == 0)                                             &
        WRITE(lfnclk,'(A60,A20)')string,HeadRC(iHead)
    ENDDO
  ENDIF
!
! END OF HEADER
! -------------
  iHead=20
  WRITE(lfnclk,'(60X,A20)') HeadRC(iHead)
!
  DEALLOCATE(Index,stat=ios)
!
  irCode=0
!
  RETURN
  END SUBROUTINE

END MODULE
