MODULE s_GETPOT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE GETPOT(NPOTMX,TMJD,MPOL,NTERM,TITLE,GM,AE,CPOT,SPOT,IPOT,IZTID)

! -------------------------------------------------------------------------
! Purpose:    INPUT OF POTENTIAL COEFFICIENTS UP TO ORDER NPOTMX
!             FROM INPUT DATASET (LFNLOC)
!
! Author:     H. Bock
!
! Remark:     F90-Version of old SR GETPOT.f
!  AUTHOR     :  W. GURTNER
!
!  CREATED    :  87/11/30 08:11
!
!  CHANGES    :  11-JAN-1993 : USE OF SR "OPNFIL" TO OPEN FILES
!                25-JUN-1996 : TS: ALLOW TWO FORMATS (GEMT3 and JGM3)
!                11-JUL-1996 : TS: ACCOUNT FOR C02 DRIFT IN JGM3
!                21-SEP-1999 : TS: ALLOW FOR GRIM5 FORMAT
!                03-MAY-2000 : HB: INCLUDE I:MAXPOT,CHANGES INDEXING OF
!                                  CPOT AND SPOT (ADD THE FIRST THREE
!                                  TERMS WHICH ARE =0.D0)
!                22-JUN-2000 : HB: ALLOW FOR EGM96 FORMAT
!                30-Jan-2002 : HB: ALLOW FOR TEG4 and EIGEN1S FORMAT,
!                                  REALLY ACTIVATE C20 DRIFT IN JGM3
!                01-Oct-2002 : HB: add zero tide for EGM96 and EIGEN1S models
!                21-DEC-2002 : DS: EIGEN-2 introduced
!                08-MAR-2003 : HU: TITLE AS CHR80 STRING
!                13-MAR-2003 : DS: GRIM5-S1 and GRIM5-C1 introduced
!                13-MAR-2003 : DS: INITIALIZE CPOT(1)=1.D0
!                13-MAR-2003 : DS: DO NOT ADD INITIALIZED CPOT
!                                  WHEN READING EIGEN1S AND EIGEN2
!                13-MAR-2003 : DS: TUM GRAVITY MODEL INTRODUCED
!                11-JUN-2003 : HU: ACTIVATE DOTJ2 CORRECTION
!                14-JUL-2003 : HU: EGM96: PERMANENT TIDE CORRECTION CHANGED
!                                  FROM -4.201D-9 TO -4.173D-9 (IERS2000)
!                18-AUG-2003 : RD: CLOSE FILE
!                19-AUG-2003 : DS: CSR GRACE MODELS GGM01S and GGM01C introduced
!                19-AUG-2003 : DS: GFZ GRACE MODEL EIGEN-G1S introduced
!                22-FEB-2004 : DS: MERGE GPSWS AND LXWS VERSIONS
!                06-JAN-2004 : HU: DO NOT HANDLE PERMANENT TIDE, PARAM IZTID
!
! Created:    19-Oct-2004
! Last mod.:  31-May-2011
!
! Changes:    21-Dec-2004 HU: All EIGEN models: read ref epochs, handle
!                             CPOT(1) differently, dot also for SPOT
!             14-Apr-2005 HU: Use interface to alcerr
!             02-Jun-2006 HB: Put 'model_name' of GOCE-HPF-formatted files
!                             in TITLE-string => orbit model for STD-file
!                             Handle comment lines in GOCE-HPF-format
!             07-Aug-2006 HB: Some layout modifications for error messages
!                             Move application of C02 drift to the end
!             26-Aug-2006 HU: Compute mean pole, replace C21, S21
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             15-Jul-2009 DT: Correct time-dependent coeff. if HPF format
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             06-May-2011 HB: MEANPOL correction also for IERS2010 conventions
!             31-May-2011 HB: Coefficients for IERS2010 conventional model
!                             (based on EGM2008) added
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnerr, &
                      shortLineLength, fileNameLength, lineLength, timStrgLength
  USE m_maxdim, ONLY: MAXPOT

  USE s_exitrc
  USE f_djul
  USE s_opnfil
  USE s_alcerr
  USE s_gtflna
  USE s_opnerr
  USE s_meanpol
  USE s_st2tim
  USE f_nextline
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b) :: nPotMx ! MAXIMUM ORDER OF COEFFICIENTS ALLOWED
  REAL(r8b)    :: tMjd   ! TIME OF REQUEST (BECAUSE OF C02 DOT)
  INTEGER(i4b) :: mpol   ! 2: COMPUTE MEAN POLE ACCORDING TO IERS2003

! output:
  INTEGER(i4b) :: nTerm                   ! ACTUAL MAXIMUM ORDER OF INPUT COEFF.
  CHARACTER(LEN=shortlineLength) :: title ! Title

  REAL(r8b),DIMENSION(*) :: CPOT          ! ZONAL    COEFFICIENT C(N,M)
  REAL(r8b),DIMENSION(*) :: SPOT          ! TESSERAL COEFFICIENT C(N,M)
                                          !  WITH: I=N*(N+1)/2+1+M

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'GETPOT'

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)  :: filpot
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=shortlineLength) :: dummy2
  CHARACTER(LEN=shortLineLength) :: gravFil
  CHARACTER(LEN=shortLineLength) :: modNam
  CHARACTER(LEN=shortLineLength) :: tidSys
  CHARACTER(LEN=timStrgLength)   :: timStr
  CHARACTER(LEN=shortLineLength) :: dummy
  CHARACTER(LEN=10)              :: error
  CHARACTER(LEN=8)               :: TIM
  CHARACTER(LEN=6)               :: RECKEY

  REAL(r8b),DIMENSION(:),ALLOCATABLE :: csTim,sDot,cDot
  REAL(r8b)                          :: time
  REAL(r8b)                          :: DC02,DC03,DC04,DC20,DS20,DC21,DS21
  REAL(r8b)                          :: C,S
  REAL(r8b)                          :: tmjd0, flat, omega, refDat
  REAL(r8b)                          :: drftc, drfts
  REAL(r8b)                          :: gm, ae
  REAL(r8b)                          :: xp, yp

  INTEGER(i4b) :: iostat, irc, iac
  INTEGER(i4b) :: i,n,m
  INTEGER(i4b) :: iMax,iPot,izTid,iYear, imm, iDay
  INTEGER(i4b) :: maxDeg

! OPEN FILE WITH EARTH POTENTIAL COEFFICIENTS
! -------------------------------------------
  CALL GTFLNA(1,'POTCOE ',FILPOT,IRC)
  CALL OPNFIL(LFNLOC,FILPOT,'OLD',' ','READONLY',' ',IOSTAT)
  CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPOT,'GETPOT')

  IF(NPOTMX > MAXPOT) THEN
    WRITE(LFNERR,'(/,A,/,16X,A,/,16X,A,I4,/,16X,A,I4,/)') &
         ' *** SR GETPOT: ORDER/DEGREE OF EARTH ',&
                         'POTENTIAL TERMS IS TOO LARGE',&
                         'REQUESTED ORDER/DEGREE   >=',NPOTMX,&
                         'MAX. ORDER OF POTENTIAL   :',MAXPOT
    CALL EXITRC(2)
  ENDIF

! INITIALIZATION
! --------------
  IMAX=(NPOTMX+1)*(NPOTMX+2)/2
  DO I=1,IMAX
    CPOT(I)=0.D0
    SPOT(I)=0.D0
  ENDDO
  NTERM=0
  IPOT=-99
  IZTID=-99

! SET CPOT(1)
  CPOT(1)=1.D0

! READ TITLE LINE
! ---------------
  READ(LFNLOC,'(A80)') TITLE

! CHECK FORMAT
! ------------
! IZTID: 0: TIDE FREE   (C02 = -0.484165e-3)
!        1: ZERO TIDE   (C02 = -0.484169e-3)
  IF (TITLE(3:5) == 'GEM') THEN
    IPOT=1
    IZTID=0
  ELSEIF (TITLE(3:5) == 'JGM') THEN
    IPOT=2
    IZTID=1
  ELSEIF (TITLE(3:5) == 'GRI') THEN
    IPOT=3
    IZTID=0
  ELSEIF (TITLE(3:5) == 'EGM') THEN
    IPOT=4
    IZTID=0
  ELSEIF (TITLE(3:5) == 'TEG') THEN
    IPOT=5
    IZTID=1
  ELSEIF (TITLE(3:5) == 'EIG') THEN
    IPOT=6
    IZTID=0
  ELSEIF (TITLE(3:5) == 'GGM') THEN
    IPOT=7
    IZTID=1
  ELSEIF (TITLE(3:5) == 'TUM' .OR. TITLE(3:5) == 'MGM') THEN
    IPOT=8
    IZTID=0 ! ok?

! 0. IPOT=0 --> New GOCE-HPF gravity field format
! -----------------------------------------------
! Read header of gravity field file
! ---------------------------------
  ELSE
    IF (TITLE(1:12) == 'product_type') THEN
      IPOT=0
      READ(TITLE,*)dummy,dummy2
      gravFil = ADJUSTL(dummy2)
      IF (gravFil /= 'gravity_field') THEN
        WRITE(lfnErr,'(A,A,/,16X,A,/,16X,A,A,/)')&
             '*** SR GETPOT: The file: ',TRIM(filpot),&
                            'contains no gravity field information',&
                            'product type: ',TRIM(gravFil)
        CALL exitrc(2)
      ENDIF
    ENDIF
    DO
      line = nextline(lfnloc,1)
      IF (line(1:11) == 'end_of_head') EXIT
      IF (line       == '')            EXIT
      IF (line(1:12) == 'product_type') THEN
        IPOT=0
        IF (IZTID==-99) IZTID = -1
        READ(line,*)dummy,dummy2
        gravFil = ADJUSTL(dummy2)
        IF (gravFil /= 'gravity_field') THEN
          WRITE(lfnErr,'(A,A,/,16X,A,/,16X,A,A,/)')&
               '*** SR GETPOT: The file: ',TRIM(filpot),&
                              'contains no gravity field information',&
                              'product type: ',TRIM(gravFil)
          CALL exitrc(2)
        ELSE
          CYCLE
        ENDIF
      ENDIF
      IF (line(1:9) == 'modelname') THEN
        READ(line,*)dummy,dummy2
        modNam = ADJUSTL(dummy2)
        title  = ' '
        TITLE(3:18)=modnam

! GRIMC7 - test file
! ******************
        IF (modNam(1:4) == 'GRIM') izTid = 0
! ******************
        CYCLE
      ENDIF
      IF (line(1:22) == 'earth_gravity_constant') THEN
        READ(line,*)dummy2,GM
        CYCLE
      ENDIF
      IF (line(1:6) == 'radius') THEN
        READ(line,*)dummy2,AE
        CYCLE
      ENDIF
      IF (line(1:10) == 'max_degree') THEN
        READ(line,*)dummy2,maxDeg
        CYCLE
      ENDIF
      IF (line(1:6) == 'errors') THEN
        READ(line,*)dummy2,error
        CYCLE
      ENDIF
      IF (line(1:4) == 'norm') THEN
        CYCLE
      ENDIF
      IF (line(1:11) == 'tide_system') THEN
        READ(line(12:255),*)dummy2
        tidSys = ADJUSTL(dummy2)
        IF (tidSys == 'tide_free') THEN
          izTid = 0
        ELSEIF (tidSys == 'zero_tide') THEN
          izTid = 1
        ENDIF
        CYCLE
      ENDIF
    ENDDO

    IF (IPOT < 0.OR.line=='') THEN
      WRITE(LFNERR,'(A,/,16X,A,A32,/,16X,A,A20,/)')&
           ' *** SR GETPOT: Unknown Earth gravity file format',&
                           'file  : ',FILPOT,&
                           'title : ',TITLE(1:20)
      CALL EXITRC(2)
    ENDIF

    IF (izTid < 0) THEN
      WRITE(lfnerr,'(A,/,16X,A,/,16X,A)')&
           ' *** SR GETPOT: Unknown tide system for the ',&
                           'Earth gravity model in file: ',&
                            TRIM(FILPOT)
      CALL EXITRC(2)
    ENDIF

! Allocate csTim, cDot, sDot
! --------------------------
    ALLOCATE(csTim(iMax),stat=iac)
    CALL alcerr(iac,'csTim',(/iMax/),srName)
    ALLOCATE(cDot(iMax),stat=iac)
    CALL alcerr(iac,'cDot',(/iMax/),srName)
    ALLOCATE(sDot(iMax),stat=iac)
    CALL alcerr(iac,'sDot',(/iMax/),srName)
    cDot(:)=0.D0
    sDot(:)=0.D0
    csTim(:)=0.D0

! Read coefficients
! -----------------
    DO
      line = nextline(lfnloc,1)
      IF (line      == '' ) EXIT
      IF (line(1:4) == 'gfc ') THEN
        READ(line,*)dummy,N,M,C,S
        I=N*(N+1)/2+1+M
      ENDIF
      IF (line(1:4) == 'gfct') THEN
        IF (error == 'no') THEN
          READ(line,*)dummy,N,M,C,S,TIM
        ELSEIF (error == 'calibrated' .OR. error == 'formal') THEN
          READ(line,*)dummy,N,M,C,S,dummy,dummy,TIM
        ENDIF
        I=N*(N+1)/2+1+M
        timStr = TIM(1:4)//'-'//TIM(5:6)//'-'//TIM(7:8)//' 00:00:00'
        CALL ST2TIM(1,1,timStr,time)
      ENDIF
      IF (line(1:3) == 'dot') THEN
        READ(line,*)dummy,N,M,C,S
        I=N*(N+1)/2+1+M
      ENDIF
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      IF(N == 0.AND.M == 0) CYCLE
      NTERM=MAX0(N,M,NTERM)
!!!      I=N*(N+1)/2+1+M
      IF (line(1:3) == 'dot') THEN
        cDot(I)=C
        sDot(I)=S
      ELSE
        CPOT(I)=C
        SPOT(I)=S
      ENDIF
      IF (line(1:4) == 'gfct') csTim(I)=time
    ENDDO
    DO I = 1,iMax
      IF (cDot(I) /= 0.D0 ) THEN

        IF ( csTim(I) == 0.D0 ) THEN
          WRITE(lfnerr, '(A,/,16X,A,/,2(16X,A,I4,/))') &
                ' *** SR GETPOT: Reference epoch not given for',             &
                                'time-dependent gravity field coefficient.', &
                                'Degree: ', N,                               &
                                'Order:  ', M
          CALL EXITRC(2)
        ENDIF

        CPOT(I) = CPOT(I) + cDot(I)*(tMjd - csTim(I))/365.25

      ENDIF

      IF (sDot(I) /= 0.D0 ) THEN

        IF ( csTim(I) == 0.D0 ) THEN
          WRITE(lfnerr, '(A,/,16X,A,/,2(16X,A,I4,/))') &
                ' *** SR GETPOT: Reference epoch not given for',             &
                                'time-dependent gravity field coefficient.', &
                                'Degree: ', N,                               &
                                'Order:  ', M
          CALL EXITRC(2)
        ENDIF

        SPOT(I) = SPOT(I) + sDot(I)*(tMjd - csTim(I))/365.25
      ENDIF
    ENDDO
    DEALLOCATE(csTim,stat=iac)
    DEALLOCATE(cDot,stat=iac)
    DEALLOCATE(sDot,stat=iac)
    GOTO 900
  ENDIF

! 1. IPOT=1 --> GEM-T3N
! ---------------------
  IF (IPOT == 1) THEN

! READ GM AND AE
    READ(LFNLOC,'(19X,F9.2,4X,F8.3)')GM,AE

! SET GM AND AE (SINCE THEY ARE ZERO IN THE GEMT3. FILE)
! ------------------------------------------------------
    GM=398600.4415*1.D9
    AE=6378.137*1.D3

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(6X,2I2,2D15.8)',END=900) N,M,C,S
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      IF(N == 0.AND.M == 0) EXIT
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      CPOT(I)=C
      SPOT(I)=S
    ENDDO

! 1. IPOT=2 --> JGM-3
! -------------------
  ELSEIF (IPOT == 2) THEN

! READ GM AND AE
! --------------
    READ(LFNLOC,'(/,20X,2E20.10,/)')GM,AE

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(6X,2I2,2D21.14)',END=900) N,M,C,S
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      IF(N == 0.AND.M == 0) EXIT
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      CPOT(I)=C
      SPOT(I)=S
    ENDDO

! 1. IPOT=3 --> GRIM5
! -------------------
  ELSEIF (IPOT == 3) THEN
    READ(LFNLOC,'(//,4E20.14,/,16X,F8.2,//)')AE,FLAT,GM,OMEGA,REFDAT
    READ(LFNLOC,'(9X,E21.14)')DC02,DC03,DC04
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(2I3,3X,2D21.14)',END=900) N,M,C,S
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      IF(N == 0 .AND. M == 0 .AND. C == 0.D0 .AND. S == 0.D0) EXIT

! APPLY C02, C03, C04 DRIFTS IN CASE OF GRIM5
! -------------------------------------------
      IF (N == 2 .AND. M == 0) C=C+DC02*(TMJD-TMJD0)/365.25D0
      IF (N == 3 .AND. M == 0) C=C+DC03*(TMJD-TMJD0)/365.25D0
      IF (N == 4 .AND. M == 0) C=C+DC04*(TMJD-TMJD0)/365.25D0
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      CPOT(I)=C
      SPOT(I)=S
    ENDDO

! 1. IPOT=4 --> EGM96
! -------------------
  ELSEIF (IPOT == 4) THEN
    READ(LFNLOC,'(//,F9.2,6X,E15.3/,16X,F8.2,//)')AE,GM,REFDAT
    READ(LFNLOC,'(11X,E17.10,1X,E19.12)')DC20,DS20,DC21,DS21
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(2I4,2E20.12)',END=900) N,M,C,S
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      IF(N == 0.AND.M == 0) EXIT

! APPLY C20, C21, S21 DRIFTS
! --------------------------
      IF (N == 2 .AND. M == 0) THEN
        C = C + DC20*(TMJD-TMJD0)/365.25D0
      ENDIF
      IF (N == 2 .AND. M == 1) THEN
        C=C+DC21*(TMJD-TMJD0)/365.25D0
        S=S+DS21*(TMJD-TMJD0)/365.25D0
      ENDIF
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      CPOT(I)=C
      SPOT(I)=S
    ENDDO

! 1. IPOT=5 or 8 --> CSR Models: TEG4, GGM01C, GGM01S
! ---------------------------------------------------
  ELSEIF (IPOT == 5 .OR. IPOT == 7) THEN

! READ GM AND AE, REFERENCE EPOCH
! -------------------------------
    READ(LFNLOC,'(/,20X,3E20.10,/)')GM,AE,REFDAT

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(6X,2I3,2D21.14)',END=850) N,M,C,S
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      IF(N == 0.AND.M == 0) EXIT
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      CPOT(I)=C
      SPOT(I)=S
    ENDDO

! APPLY TIME VARIABLE PART FOR C20 AND C21, S21
! ---------------------------------------------
850 CONTINUE
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)+REFDAT-IYEAR

    CPOT(4)=CPOT(4)+1.162755D-11*(TMJD-TMJD0)/365.25D0

    IF (TITLE(3:7).EQ.'TEG-4') THEN
      CPOT(5)=-2.20D-10
      SPOT(5)=14.51D-10
    END IF
    CPOT(5)=CPOT(5)-0.337000D-11*(TMJD-51544.D0)/365.25D0
    SPOT(5)=SPOT(5)+1.606000D-11*(TMJD-51544.D0)/365.25D0

! 1. IPOT=6 --> EIGEN1S
! ---------------------
  ELSEIF (IPOT == 6) THEN
! READ GM AND AE
! --------------
    IF (TITLE(3:9)  ==  'EIGEN1S') THEN
      READ(LFNLOC,'(/,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,13(/))') &
           GM,AE,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:9)  ==  'EIGEN-2') THEN
      READ(LFNLOC,'(///,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,16(/))') &
           GM,AE,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:9)  ==  'EIGEN-3p') THEN
      READ(LFNLOC,'(///,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,16(/))') &
           GM,AE,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:12) ==  'EIGEN-G01S' .OR. &
            TITLE(3:16) ==  'EIGEN-GRACE01S') THEN
      READ(LFNLOC,'(////,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,/)') &
           GM,AE,IYEAR,IMM,IDAY
    ELSE
      WRITE(LFNERR,'(A,/,16X,A,A32,/,16X,A,A20,/)') &
           ' *** SR GETPOT: Unknown Earth gravity file format',&
                           'file  : ',FILPOT,&
                           'title : ',TITLE(1:20)
      CALL EXITRC(2)
    ENDIF
    TMJD0=DJUL(IYEAR,IMM,DBLE(IDAY))

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(A6,2I5,2(1X,1E18.12))',END=900) RECKEY,N,M,C,S

! APPLY ZERO TIDE FOR C20 OF EIGEN1S (-4.201D-9,k=0.30190)
! --------------------------------------------------------
      IF(N > NPOTMX.OR.M > NPOTMX) CYCLE
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      IF(RECKEY(1:6)=='GRCOEF') THEN
        IF (I > 1) THEN
          CPOT(I)=CPOT(I)+C
          SPOT(I)=SPOT(I)+S
        ELSE
          CPOT(I)=C
        ENDIF
      ELSEIF(RECKEY(1:6)=='GRDOTA') THEN
        DRFTC=C*(TMJD-TMJD0)/365.25D0
        DRFTS=S*(TMJD-TMJD0)/365.25D0
        IF (I > 1) THEN
          CPOT(I)=CPOT(I)+DRFTC
          SPOT(I)=SPOT(I)+DRFTS
        ENDIF
      ENDIF
    ENDDO

!
! 1. IPOT=8 --> IAPG MODELS
! -------------------------
  ELSEIF (IPOT.EQ.8) THEN

! READ GM AND AE
! --------------
    IF (TITLE(3:5) .EQ. 'MGM' .OR. TITLE(3:5) .EQ. 'TUM') THEN
      READ(LFNLOC,'(/,9X,2D25.16,D16.8)') GM,AE,REFDAT
    ELSE
      WRITE(LFNERR,'(A,/,16X,A,A32,/,16X,A,A20,/)') &
            ' *** SR GETPOT: Unknown Earth gravity file format',&
                            'file  : ',FILPOT,&
                            'title : ',TITLE(1:20)
      CALL EXITRC(2)
    END IF
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)
!
! INPUT OF COEFFICIENTS
! ---------------------
    DO
      READ(LFNLOC,'(A1,2X,2I4,2D25.16)',END=900) RECKEY,N,M,C,S
      IF(N > NPOTMX .OR. M > NPOTMX) CYCLE
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      IF(RECKEY(1:1)=='C') THEN
        IF (I > 1) THEN
          CPOT(I)=CPOT(I)+C
          SPOT(I)=SPOT(I)+S
        ELSE
          CPOT(I)=C
        ENDIF
      ELSEIF(RECKEY(1:1)=='D') THEN
        DRFTC=C*(TMJD-TMJD0)/365.25D0
        DRFTS=S*(TMJD-TMJD0)/365.25D0
        IF (I > 1) THEN
          CPOT(I)=CPOT(I)+DRFTC
          SPOT(I)=SPOT(I)+DRFTS
        ENDIF
      ENDIF
    ENDDO

  ENDIF

900 CONTINUE

! APPLY C02 DRIFT IN CASE OF JGM3 AND TEG4
! J02 DRIFT = 2.6*10**-11 1/YEAR (REF TIME 1-1-1986 = MJD 46431.0)
! Activated on June 11, 2003
! -----------------------------------------------------------------
  IF (IPOT == 2.OR.IPOT == 5) THEN
    DC02=(2.6D-11/DSQRT(5.0D0))*(TMJD-46431.0D0)/365.25D0
    CPOT(4)=CPOT(4)+DC02
  ENDIF

! IERS 2010 conventional geopotential model
! -----------------------------------------
  IF (title(3:9) == 'EGM2008') THEN
! C20
    CPOT(4)=-0.48416531D-3
    DC02=11.6D-12*(TMJD-51544.D0)/365.25D0
    CPOT(4)=CPOT(4)+DC02
! C30
    CPOT(7)=0.9571612D-6
    DC03= 4.9D-12*(TMJD-51544.D0)/365.25D0
    CPOT(7)=CPOT(7)+DC03
! C40
    CPOT(11)=0.5399659D-6
    DC04= 4.7D-12*(TMJD-51544.D0)/365.25D0
    CPOT(11)=CPOT(11)+DC04
  ENDIF

! COMPUTE MEAN POLE ACCORDING TO IERS2003/IERS2010
! IERS 2010 TN No.36, Ch. 6.1, Eq.(6.5)
! ------------------------------------------------
  IF (MPOL.GE.2) THEN
    CALL MEANPOL(MPOL,TMJD,XP,YP)
    CPOT(5)= DSQRT(3D0)*XP*CPOT(4) - XP*CPOT(6) + YP*SPOT(6)
    SPOT(5)=-DSQRT(3D0)*YP*CPOT(4) - YP*CPOT(6) - XP*SPOT(6)
  ENDIF

! CLOSE FILE
! ----------
  CLOSE(LFNLOC)

  RETURN
  END SUBROUTINE GETPOT


END MODULE
