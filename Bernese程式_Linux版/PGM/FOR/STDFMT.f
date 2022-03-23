C*
      PROGRAM STDFMT
CC
CC NAME       :  STDFMT
CC
CC PURPOSE    :  TRANSFORM UNFORMATTED STAND. ORBIT AND RAD. PRESS.
CC               FILES INTO FORMATTED FILES.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.GU
CC
CC CREATED    :  91/01/25 12:37
CC
CC CHANGES    :  05-SEP-91 : ??: USE OPNFIL TO OPEN STDFIL AND RPRFIL
CC               12-NOV-92 : EB: READ COORDINATE SYSTEM J2000.0
CC               24-DEC-92 : ??: USE OPNFIL FOR ALL FILES
CC               22-MAR-93 : ??: ALL VERSIONS OF STD- AND RPR-FILES
CC                               CORRECTLY READ AND WRITTEN
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               26-AUG-98 : LM: NEW FORMAT
CC               16-AUG-99 : JJ: RM UNUSED VARS RCOEFF, TOTREC, RPRPAR
CC               22-JAN-00 : HU: BACKSPACE REMOVED, NO LONGER SUPPORTED
CC               06-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               06-AUG-03 : HU: NEW STD FORMAT
CC               25-AUG-03 : CU: CHANGE FORMAT STRING
CC               27-AUG-03 : HU: WRITE OLD FORMAT OPTIONALLY
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               13-AUG-04 : CU: USE GTFILE2 INSTEAD OF STDFMTFL
CC               04-APR-05 : AJ: ALLOW FOR NEW RPR-FORMAT
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON
CC               04-AUG-08 : DT: USE MAXINT FROM M_MAXDIM
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL CHANGED
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT
CC               27-APR-12 : RD: NULLIFY POINTERS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1991     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, keyValueLength, fileNameLength,
     1                    lfnPrt, lfnErr, lfnRes, lfnOrb, lfnRpr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: MAXSAT, MAXINT
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_gtfile2
      USE s_opnfil
      USE s_prflna
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_readkeys
      USE s_exitrc
      USE s_ckoptc
      USE s_opnsys
      USE s_defcon
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , IARC    , IDEG    , IFIL    , IFMT    ,
     1          II      , IINT    , IOSTAT  , IQ      , ISAT    ,
     2          IVAR    , K       , KK      , LL      ,
     3          MAXQ1   , NARC    , NINT    , NLIN    , NRCRPR  ,
     4          NRCSTD  , NSAT    , NVAR    , LFORM   , NSTC    ,
     5          ISTC    , IFRC    , KRCTYP  , NSTCEP  , INTSTC
C
      REAL*8    ELEPRESS, H       , HSEC    , SCALPA  , T0      ,
     1          TA      , TB      , TBHLP   , TOSC1   , ZERON   ,
     2          TIMSTC  , PARSTC
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXQ1=20)
C      PARAMETER (MAXFIL=400,MAXINT=1000,MAXQ1=20)
C
C MAXFIL: MAXIMUM NUMBER OF FILES
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXINT: MAXIMUM NUMBER OF INTEGRATION INTERVALS
C MAXQ1 : MAXIMUM NUMBER FOR POLYNOMIAL DEGREE + 1 (NUM. INTEGR.)
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=keyValueLength),DIMENSION(:)  ,POINTER :: keyValue
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: orbfil
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: rprfil
      CHARACTER*80 LINE
      CHARACTER*2  LINE1
      CHARACTER*1  SOURCE(10)
      character*8  anltyp
      integer*4    locq(6)
      real*8       zcoen(3)
C
      REAL*8       TBOUND(MAXINT+1),ELESAT(7),COEFF(3,MAXSAT)
C
      INTEGER*4    NAVNUM(MAXSAT)
      INTEGER*4    IFORM,IRC,IRCODE
      INTEGER(i4b) :: norbfil, nrprfil
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C LOGICAL FILE NUMBERS AND FILE NAMES
C -----------------------------------
      IRCODE=0
      NULLIFY(keyValue)
      NULLIFY(orbfil)
      NULLIFY(rprfil)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C PRINT TITLE
C -----------
      CALL pritit('STDFMT',
     1            'Convert STD and RPR files (binary to ASCII)')
      CALL prflna
C
C GET OPTIONS
C -----------
      CALL readKeys('OUTFORM', keyValue, irc)
      CALL ckoptc(1,'OUTFORM', keyValue,(/'OLD','NEW'/), 'pg stdfmt',
     1            'output format', irc, irCode, maxVal=1,result1=iform)
C
      IF (IFORM.EQ.1) THEN
        WRITE(lfnerr,"(/,' ### PG STDFMT: Warning, you are writing ',
     1                                   'old STD ASCII format',/)")
      ENDIF
C
C GET INPUT STANDARD ORBIT AND RAD. PRESS. FILE NAMES
C ----------------------------------------------------
      CALL gtfile2('STDFIL',2,norbfil,orbfil)
      CALL gtfile2('RPRFIL',2,nrprfil,rprfil)
C
C TRANSFORMATION OF STANDARD ORBIT FILES
C --------------------------------------
C WRITE TITLE LINES FOR FILE NAME OUTPUT
      IF (norbfil > 0) WRITE(LFNPRT,1)
1     FORMAT(' File  Binary standard orbit files',7X,
     1               'ASCII standard orbit files        #Rec',
     2       /,' ----  ',32('-'),2X,32('-'),2X,'-----')
C
C LOOP OVER ALL FILES TO BE FORMATTED
C -----------------------------------
      DO IFIL = 1, NORBFIL
C
C OPEN STANDARD ORBIT FILE AND FORMATTED OUTPUT FILE
C --------------------------------------------------
        CALL OPNFIL(LFNORB,ORBFIL(1,IFIL),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,ORBFIL(1,IFIL),'STDFMT')
C
        CALL OPNFIL(LFNRES,ORBFIL(2,IFIL),'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,ORBFIL(2,IFIL),'STDFMT')
C
C READ NUMBER OF ARCS
        READ(LFNORB) NARC
        IF (IFORM.GT.1.OR.NARC.GT.0) WRITE(LFNRES,300) NARC
300     FORMAT(2I4)
C
C READ FORMAT NUMBER AND ORBIT DESCRIPTION
        IF (NARC.LT.0) THEN
          READ(LFNORB) IFMT,NARC
          IF (IFORM.GT.1) THEN
            WRITE(LFNRES,300) IFMT,NARC
          ELSE
            WRITE(LFNRES,300) NARC
          ENDIF
          READ(LFNORB) NLIN
          IF (IFORM.GT.1) WRITE(LFNRES,300) NLIN
          DO I=1,NLIN
            READ(LFNORB) LINE
            IF (IFORM.GT.1) WRITE(LFNRES,"(A)") LINE
          ENDDO
        ENDIF
C
        NRCSTD=1
C
C READ OSCULATION EPOCH AND INTERVAL BOUNDARIES
        DO 500 IARC=1,NARC
          READ(LFNORB) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT),
     1                 (SOURCE(I),I=1,10)
          WRITE(LFNRES,301) NSAT,NINT,IQ
          WRITE(LFNRES,302) (NAVNUM(I),I=1,NSAT)
          WRITE(LFNRES,303) (SOURCE(I),I=1,10)
301       FORMAT(3I7)
302       FORMAT(24I3)
303       FORMAT(10A1)
C
C CHECK MAXIMUM NUMBER OF SATELLITES
          IF(NSAT.GT.MAXSAT) THEN
            WRITE(LFNERR,1003) NSAT,MAXSAT
1003        FORMAT(/,' *** PG STDFMT: TOO MANY SATELLITES',/,
     1                           16X,'NUMBER OF SATELLITES:',I3,/,
     2                           16X,'MAXIMUM NUMBER      :',I3,/)
            CALL EXITRC(2)
          END IF
C
C CHECK MAXIMUM NUMBER OF PARTIAL INTERVALS
          IF(NINT.GT.MAXINT) THEN
            WRITE(LFNERR,1004) NINT,MAXINT
1004        FORMAT(/,' *** PG STDFMT: TOO MANY INTEGR. INTERVALS',/,
     1                           16X,'NUMBER OF INTERVALS:',I4,/,
     2                           16X,'MAXIMUM NUMBER     :',I4,/)
            CALL EXITRC(2)
          END IF
          IF(IQ.GT.MAXQ1) THEN
            WRITE(LFNERR,1005)IQ,MAXQ1
1005        FORMAT(/,' *** PG STDFMT: POLYNOMIAL DEGREE TOO HIGH',/,
     1                           16X,'POLYNOMIAL DEGREE:',I3,/,
     2                           16X,'MAX. POLY. DEGREE:',I3,/)
            CALL EXITRC(2)
          END IF
C
C READ OSCULATING EPOCH AND INTERVAL BOUNDARIES
          READ(LFNORB) TOSC1,TBOUND(1)
          IF(TBOUND(1).EQ.0.D0 .OR. TBOUND(1).EQ.2.D0) THEN
            WRITE(LFNRES,*) TOSC1,TBOUND(1)
            DO 51 I=1,NINT+1
              READ(LFNORB) TBOUND(I)
              WRITE(LFNRES,*) TBOUND(I)
51          CONTINUE
          ELSE
C           BACKSPACE LFNORB
C           READ(LFNORB) TOSC1,(TBOUND(I),I=1,NINT+1)
            WRITE(LFNERR,904)
904         FORMAT(/,' *** PG STDFMT: OLD BINARY FORMAT NO ',
     1                               'LONGER SUPPORTED',/,
     1                           16X,'REQUIRES "BACKSPACE"',/)
            CALL EXITRC(2)
C           ZERO=0.D0
C           WRITE(LFNRES,*) TOSC1,ZERO
C           DO 52 I=1,NINT+1
C             READ(LFNORB) TBOUND(I)
C             WRITE(LFNRES,*) TBOUND(I)
C52         CONTINUE
          END IF
C
C READ SET OF OSCULATING ELEMENTS
          DO 10 ISAT=1,NSAT
            READ(LFNORB) (ELESAT(K),K=1,7)
            DO 5 K=1,7
              WRITE(LFNRES, *) ELESAT(K)
5           CONTINUE
10        CONTINUE
C
C LOOP OVER ALL PARTIAL INTERVALS
C -------------------------------
          DO 200 IINT=1,NINT
C
C READ SET OF POLYNOMIAL COEFFICIENTS
            READ(LFNORB) T0,H
            WRITE(LFNRES,*) T0,H
            DO 30 I=1,IQ+1
              READ(LFNORB) ((COEFF(K,ISAT),K=1,3),ISAT=1,NSAT)
              DO 20 ISAT=1,NSAT
                WRITE(LFNRES,*) (COEFF(K,ISAT),K=1,3)
20            CONTINUE
30          CONTINUE
C
200       CONTINUE
C
C NUMBER OF RECORDS IN ARC
        NRCSTD=NRCSTD +4 +NINT+1 +7*NSAT +NINT*(1+(IQ+1)*NSAT)
C
500     CONTINUE
C CLOSE FILES
C -----------
        CLOSE(UNIT=LFNORB)
        CLOSE(UNIT=LFNRES)
C
C WRITE INPUT AND OUTPUT FILE NAMES AND NUMBER OF RECORDS
C -------------------------------------------------------
        WRITE(LFNPRT,2) IFIL,ORBFIL(1,IFIL),ORBFIL(2,IFIL),NRCSTD
2       FORMAT(I5,2X,A32,2X,A32,I7)
C
      ENDDO
C
C TRANSFORMATION OF RAD.PRESS.COEFFICIENT FILES
C ---------------------------------------------
C WRITE TITLE LINES FOR FILE NAME OUTPUT
      IF (nrprfil > 0) WRITE(LFNPRT,3)
3     FORMAT(/,' Binary radiation pressure files',3X,
     1           'ASCII radiation pressure files    #Rec',
     2       /,' ----  ',32('-'),2X,32('-'),2X,'-----')
C
C LOOP OVER ALL FILES TO BE FORMATTED
C -----------------------------------
      DO IFIL = 1, NRPRFIL
C
C OPEN RAD.PRESS. INPUT AND OUTPUT FILE
C -------------------------------------
        CALL OPNFIL(LFNRPR,RPRFIL(1,IFIL),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRPR,IOSTAT,RPRFIL(1,IFIL),'STDFMT')
C
        CALL OPNFIL(LFNRES,RPRFIL(2,IFIL),'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,RPRFIL(2,IFIL),'STDFMT')
C READ FORMAT
        READ(LFNRPR) LINE1
        IF(LINE1.EQ.'#P') THEN
          WRITE(LFNRES,299) LINE1
299              FORMAT(1A2)
          LFORM=1
        ELSE
          LFORM=0
          REWIND LFNRPR
        END IF
C READ NUMBER OF ARCS
        READ(LFNRPR) NARC
        WRITE(LFNRES,300) NARC
C
        NRCRPR=1
C
C READ FIRST AND LAST OBSERVATION TIMES FOR ALL ARCS
C --------------------------------------------------
        DO 1500 IARC=1,NARC
          READ(LFNRPR) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT),
     1                 (SOURCE(I),I=1,10)
          WRITE(LFNRES,301) NSAT,NINT,IQ
          WRITE(LFNRES,302) (NAVNUM(I),I=1,NSAT)
          WRITE(LFNRES,303) (SOURCE(I),I=1,10)
          IF(NSAT.GT.MAXSAT) THEN
            WRITE(LFNERR,2003) NSAT,MAXSAT
2003        FORMAT(/,' *** PG STDFMT: TOO MANY SATELLITES',/,
     1                           16X,'NUMBER OF SATELLITES:',I3,/,
     2                           16X,'MAXIMUM NUMBER      :',I3,/)
            CALL EXITRC(2)
          ENDIF
          IF(NINT.GT.MAXINT) THEN
            WRITE(LFNERR,2004) NINT,MAXINT
2004        FORMAT(/,' *** PG STDFMT: TOO MANY INTEGR. INTERVALS',/,
     1                           16X,'NUMBER OF INTERVALS:',I3,/,
     2                           16X,'MAXIMUM NUMBER     :',I3,/)
            CALL EXITRC(2)
          ENDIF
          IF(IQ.GT.MAXQ1) THEN
            WRITE(LFNERR,2005) IQ,MAXQ1
2005        FORMAT(/,' *** PG STDFMT: POLYNOMIAL DEGREE TOO HIGH',/,
     1                           16X,'POLYNOMIAL DEGREE:',I3,/,
     2                           16X,'MAX. POLY. DEGREE:',I3,/)
            CALL EXITRC(2)
          ENDIF
C*******************************************************************
          read(lfnrpr) ta, tb, zeron
          write(lfnres,'(3(e22.15,1x))') ta, tb, zeron

          read(lfnrpr) nvar, anltyp
          write(lfnres,'(i22, 1x, a8)') nvar, anltyp

          do ii = 1, nint
            read(lfnrpr) tbhlp
            write(lfnres,'(e22.15)') tbhlp
          end do

          read(lfnrpr) tbhlp
          write(lfnres,'(e22.15)') tbhlp

          do ii = 1, nsat
            do kk = 1, nvar
              read(lfnrpr) elepress, scalpa, (locq(ll),ll=1,6)
              write(lfnres,'(e22.15,1x,e22.15,6(I5,1x))')
     &               elepress, scalpa, (locq(ll),ll=1,6)
            end do
          end do

C READ & WRITE STOCH. PULSES
          IF(LFORM.EQ.1) THEN
            DO 47 ISAT=1,NSAT
             READ(LFNRPR) NSTC
              WRITE(LFNRES,'(I4)') NSTC
              DO 46 ISTC=1,NSTC
                IFRC=0
                DO 45
                  IFRC=IFRC+1
                  READ(LFNRPR) KRCTYP,NSTCEP,INTSTC,TIMSTC,PARSTC
                  WRITE(LFNRES,'(I2,1x,I1,1x,I4,1x,e22.15,1x,e22.15)')
     1                         KRCTYP,NSTCEP,INTSTC,TIMSTC,PARSTC
                  IF(IFRC.EQ.NSTCEP) GOTO 46
45              CONTINUE
46            CONTINUE
47          CONTINUE
          END IF

          do iint = 1, nint
            read(lfnrpr) tbhlp, hsec
            write(lfnres,'(e22.15,1x,e22.15)') tbhlp, hsec

            do isat = 1, nsat
              do ivar = 1, nvar
                do ideg = 1, iq+1
                  read(lfnrpr) (zcoen(kk), kk=1,3)
                  write(lfnres,'(3(e22.15,1x))') (zcoen(kk), kk=1,3)
                end do
              end do
            end do

          end do
C*******************************************************************
C
          NRCRPR=NRCRPR +4 +NINT+1 +NSAT +NINT*(1+3*(IQ+1)*NSAT)
C
1500    CONTINUE
C
C CLOSE FILES
C -----------
        CLOSE(UNIT=LFNRPR)
        CLOSE(UNIT=LFNRES)
C
C WRITE INPUT AND OUTPUT FILE NAMES AND NUMBER OF RECORDS
C -------------------------------------------------------
        WRITE(LFNPRT,4) IFIL,RPRFIL(1,IFIL),RPRFIL(2,IFIL),NRCRPR
4       FORMAT(I5,2X,A32,2X,A32,I7)
C
      ENDDO
C
      CALL EXITRC(0)
      END
