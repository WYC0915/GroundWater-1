      MODULE s_GETPOL2
      CONTAINS

C*
      SUBROUTINE GETPOL2(TREQ,ISUBFL,T,X,Y,UT1UTC,GPSUTC,DEPS,DPSI,
     1                  POLTYP)
CC
CC NAME       :  GETPOL2
CC
CC PURPOSE    :  GIVEN THE TIME TREQ, THE VALUES
CC               XPOLE, YPOLE, UT1-UTC, GPS TIME - UTC
CC               ARE EXTRACTED FOR  TIMES T(1), T(2) FROM
CC               THE POLE FILE.
CC               T(2) IS THE LARGEST TABULAR TIME
CC               SMALLER THEN TREQ, T(3) THE SMALLEST TABULAR
CC               TIME LARGER THEN TREQ.
CC               IF T(2)-T(1) > 20, AN ERROR MESSAGE IS PRINTED,
CC               PROCESSING IS STOPPED. IF A LEAP SECOND IS DE-
CC               TECTED IN GPS TIME - UTC, THE VALUES UT1-UTC AND
CC               GPS TIME - UTC ARE CORRECTED FOR T(2).
CC
CC PARAMETERS :
CC         IN :  TREQ   : TIME OF REQUEST                       R*8
CC               ISUBFL : 0: DO NOT PERFORM CONSISTENCY CHECK   I*4
CC                        1: PERFORM CONSISTENCY CHECK
CC        OUT :  T(I),I=1,4: TABULAR TIMES                      R*8
CC               X(I),Y(I): POLE POSITIONS AT T(I)              R*8
CC               UT1UTC(I),I=1,4 : UT1-UTC AT TIME T(I) (DAYS)  R*8
CC               GPSUTC(I),I=1,4 : GPS TIME - UTC (DAYS)        R*8
CC               DEPS(I),I=1,4   : NUTATION OFFSET IN OBLIQUITY R*8
CC               DPSI(I),I=1,4   : NUTATION OFFSET IN LONGITUDE R*8
CC               POLTYP(I): I=1 NUTATION MODEL                  I*4(*)
CC                              1=NO, 2=OBSERVED, 3=HERRING
CC                          I=2 SUBDAILY POLE MODEL
CC                              1=NO, 2=RAY
CC
CC REMARKS    :  UNITS ARE RADIANS FOR ANGLES AND DAYS FOR TIME
CC
CC               *** CAUTION AT START AND END OF FILE ***
CC
CC AUTHOR     :  U.HUGENTOBLER
CC
CC CREATED    :  21-JAN-06
CC
CC CHANGES    :  21-JAN-06 HU: ADAPTED FROM GETPOL.F
CC               03-JUL-09 SL: TIMSTR(1,.,.) CALL CORRECTED
CC               26-MAR-12 RD: USE TIMSTR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, lfnloc, lfnerr
      USE s_rdpolh
      USE s_timstr
      USE s_rdpoli
      USE s_opnfil
      USE s_subpol
      USE s_exitrc
      USE s_gtflna
      USE s_opnerr
      USE f_nopolfil
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER(i4b), SAVE :: nBuf
      INTEGER*4 I     , IBUF  , IEND  , IFIRST, IFORM , IOSTAT, IRC   ,
     1          IREAD , MAXBUF
C
      REAL*8    GPSINP, TEST  , TINP  , TREQ
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXBUF=200)
C
      CHARACTER*80 DUMMYC
      CHARACTER*36 TSTRNG
      CHARACTER*32 FILPOL
      CHARACTER*17 THLP
      CHARACTER*3  REM
      CHARACTER*16 NUTNAM
      CHARACTER*16 SUBNAM
      CHARACTER*16 SUBNA2

C SUBNAM is the name of the model specified in the input panel
C SUBNA2 is the name of the model in the ERP file

      REAL*8       T(4),X(4),Y(4),UT1UTC(4),GPSUTC(4),DEPS(4),DPSI(4)
      REAL*8       TBUF(MAXBUF),XBUF(MAXBUF),YBUF(MAXBUF)
      REAL*8       UT1BUF(MAXBUF),GPSBUF(MAXBUF)
      REAL*8       DEPSBF(MAXBUF),DPSIBF(MAXBUF)
      REAL*8       POLCOO(5),RMSPOL(5)
      REAL*8       ERPSUB(3),ERPSUR(3)
C
      INTEGER*4    POLTYP(*),TYPTMP(2),ISUBFL
C
      DATA IFIRST /1/

C
C CHECK WHETHER THE POLE FILE HAS TO READ
C ---------------------------------------
      IF (IFIRST.EQ.1) THEN
        CALL GTFLNA(1,'POLE   ',FILPOL,IRC)
        IF (NOPOLFIL(FILPOL)) THEN
          IREAD=-1
        ELSE
          IREAD=1
        ENDIF
        IFIRST=0
      ELSEIF (TREQ.LT.TBUF(2) .OR. TREQ.GE.TBUF(NBUF-1)) THEN
        IREAD=1
      ELSE
        IREAD=0
      ENDIF
C
C NO POLE FILE
C ------------
      IF (IREAD.EQ.-1) THEN
        T  = (/-1D20,0D0,1D20,2D20/)
        X  = 0D0
        Y  = 0D0
        UT1UTC = 0D0
        GPSUTC = 0D0
        DEPS   = 0D0
        DPSI   = 0D0
        POLTYP(1:2) = 0
        RETURN
      ENDIF
C
C READ POLE FILE INTO BUFFER
C --------------------------
      IF (IREAD.EQ.1) THEN
C
C OPEN POLE FILE
        CALL GTFLNA(1,'POLE   ',FILPOL,IRC)
        CALL OPNFIL(LFNLOC,FILPOL,'OLD',' ','READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPOL,'GETPOL2')
C
C READ HEADER AND FIRST PARAMETER SET OF POLE FILE
        CALL RDPOLH(LFNLOC,1,DUMMYC,TYPTMP,IFORM,IEND,NUTNAM,SUBNAM)
        CALL RDPOLI(LFNLOC,TINP,POLCOO,GPSINP,REM,RMSPOL,IFORM,IEND)
        IF (IEND.GT.0) GOTO 910
        TBUF(4)    =TINP
        XBUF(1:4)  =POLCOO(1)
        YBUF(1:4)  =POLCOO(2)
        UT1BUF(1:4)=POLCOO(3)
        DEPSBF(1:4)=POLCOO(4)
        DPSIBF(1:4)=POLCOO(5)
        GPSBUF(1:4)=GPSINP
C
C ALLOW FOR 1 MINUTE INTERVAL BEFORE FIRST POLE RECORD
        TBUF(1)=TBUF(4)-1.5D0/1440.D0
        TBUF(2)=TBUF(4)-1.0D0/1440.D0
        TBUF(3)=TBUF(4)-0.5D0/1440.D0
C
        NBUF=4
C
C CHECK IF REQUESTED TIME "TREQ" IS BEFORE FIRST POLE RECORD
        IF (TREQ.LT.TBUF(1)) GOTO 910
C
C READ NEXT RECORDS
        DO 100 I=1,100000
          CALL RDPOLI(LFNLOC,TINP,POLCOO,GPSINP,REM,
     1                RMSPOL,IFORM,IEND)
          IF (IEND.GT.0) THEN
C
C ALLOW FOR 1 MINUTE INTERVAL AFTER LAST POLE RECORD
            IF (NBUF.LT.MAXBUF) THEN
              TBUF(NBUF+1)=TBUF(NBUF)
              XBUF(NBUF+1)=XBUF(NBUF)
              YBUF(NBUF+1)=YBUF(NBUF)
              UT1BUF(NBUF+1)=UT1BUF(NBUF)
              GPSBUF(NBUF+1)=GPSBUF(NBUF)
              DEPSBF(NBUF+1)=DEPSBF(NBUF)
              DPSIBF(NBUF+1)=DPSIBF(NBUF)
              NBUF=NBUF+1
            ELSEIF (TREQ.GT.TBUF(3)) THEN
              DO IBUF=2,NBUF
                TBUF(IBUF-1)  =TBUF(IBUF)
                XBUF(IBUF-1)  =XBUF(IBUF)
                YBUF(IBUF-1)  =YBUF(IBUF)
                UT1BUF(IBUF-1)=UT1BUF(IBUF)
                GPSBUF(IBUF-1)=GPSBUF(IBUF)
                DEPSBF(IBUF-1)=DEPSBF(IBUF)
                DPSIBF(IBUF-1)=DPSIBF(IBUF)
              ENDDO
              NBUF=NBUF-1
C
              TBUF(NBUF+1)=TBUF(NBUF)
              XBUF(NBUF+1)=XBUF(NBUF)
              YBUF(NBUF+1)=YBUF(NBUF)
              UT1BUF(NBUF+1)=UT1BUF(NBUF)
              GPSBUF(NBUF+1)=GPSBUF(NBUF)
              DEPSBF(NBUF+1)=DEPSBF(NBUF)
              DPSIBF(NBUF+1)=DPSIBF(NBUF)
              NBUF=NBUF+1
            ENDIF
C
C CHECK IF REQUESTED TIME "TREQ" IS AFTER LAST POLE RECORD
            IF (TREQ.GE.TBUF(NBUF)) GOTO 910
            GOTO 200
          ENDIF
C
C CHECK IF BUFFER HAS BEEN COMPLETELY FILLED (TREQ IN THE FIRST BIN)
          IF (NBUF.EQ.MAXBUF. AND. TREQ.LE.TBUF(3)) THEN
            GOTO 200
C
C RE-STACK POLE RECORD (REMOVE FIRST ELEMENT AND ADD LAST)
          ELSEIF (NBUF.EQ.MAXBUF) THEN
            DO IBUF=2,NBUF
              TBUF(IBUF-1)  =TBUF(IBUF)
              XBUF(IBUF-1)  =XBUF(IBUF)
              YBUF(IBUF-1)  =YBUF(IBUF)
              UT1BUF(IBUF-1)=UT1BUF(IBUF)
              GPSBUF(IBUF-1)=GPSBUF(IBUF)
              DEPSBF(IBUF-1)=DEPSBF(IBUF)
              DPSIBF(IBUF-1)=DPSIBF(IBUF)
            ENDDO
            NBUF=NBUF-1
          ENDIF
C
C SAVE LATEST POLE RECORD IN BUFFER
          NBUF=NBUF+1
          TBUF(NBUF)  =TINP
          XBUF(NBUF)  =POLCOO(1)
          YBUF(NBUF)  =POLCOO(2)
          UT1BUF(NBUF)=POLCOO(3)
          DEPSBF(NBUF)=POLCOO(4)
          DPSIBF(NBUF)=POLCOO(5)
          GPSBUF(NBUF)=GPSINP
C
100     CONTINUE
C
C BUFFER UPDATED
200     CONTINUE
        CLOSE(UNIT=LFNLOC)
C
      ENDIF

C CHECK IF NAME OF SUBDAILY POLE MODEL IN ERP FILE IS CONSISTENT
C WITH CHOICE IN INPUT PANEL, IF NOT => WARNING
C ----------------------------------------------------------------

      IF(IREAD.EQ.1.AND.ISUBFL.EQ.1) THEN
        CALL SUBPOL(0.0D0,SUBNA2,ERPSUB,ERPSUR)

        IF(SUBNAM.NE.SUBNA2) THEN
          WRITE(LFNERR,375) SUBNAM,SUBNA2
375        FORMAT(/,' ### SR GETPOL2: DIFFERENT SUBDAILY POLE MODELS ',/,
     1           16X,'ERP FILE     : ',A,/,
     2           16X,'INPUT PANEL  : ',A,/)
        ENDIF
      ENDIF
C
C SEARCH BUFFER FOR CORRECT INTERVAL OF POLE VALUES
C -------------------------------------------------
      DO IBUF=2,NBUF-2
        IF (TREQ.GE.TBUF(IBUF) .AND. TREQ.LT.TBUF(IBUF+1)) THEN
C
C RETURN VALUES IN PROPER UNITS
          DO I=1,4
            T(I)=TBUF(IBUF+I-2)
            X(I)=XBUF(IBUF+I-2)/206264.806D0
            Y(I)=YBUF(IBUF+I-2)/206264.806D0
            UT1UTC(I)=UT1BUF(IBUF+I-2)/86400.D0
            GPSUTC(I)=GPSBUF(IBUF+I-2)/86400.D0
            DEPS(I)=DEPSBF(IBUF+I-2)/206264.806D0
            DPSI(I)=DPSIBF(IBUF+I-2)/206264.806D0
          ENDDO
          POLTYP(1:2)=TYPTMP(1:2)
C
C WARNING IF EPOCHS FURTHER APART THAN 10 DAYS
          IF (DABS(T(2)-T(1)).GT.10.D0) THEN
            CALL TIMSTR(2,T,TSTRNG)
            WRITE(LFNERR,920) FILPOL,TSTRNG
920         FORMAT(/,' ### SR GETPOL2: POLE VALUES FURTHER APART THAN',
     1               ' 10 DAYS',/,
     2               16X,'INTERPOLATION ERRORS MAY RESULT',
     3               16X,'POLE FILE: ',A,/,
     4               16X,'EPOCHS   : ',A,/)
          ENDIF

C
C LOOK FOR LEAP SECOND
          TEST=DNINT((GPSUTC(2)-GPSUTC(1))*86400.D0)
cc          IF (TEST.NE.0) THEN
cc            CALL TIMSTR(2,T,TSTRNG)
cc            IF (DABS(TEST).GT.1.D0) THEN
cc              WRITE(LFNERR,140) TEST,TSTRNG
cc140           FORMAT(/,' *** SR GETPOL2: JUMP IN GPS-UTC',/,
cc     1                             16X,'JUMP IN SEC        :',F10.0,/,
cc     2                             16X,'EPOCHS IN POLE FILE: ',A,/)
cc              CALL EXITRC(2)
cc            ELSE
cc              WRITE(LFNERR,150) TSTRNG
cc150           FORMAT(/,' ### SR GETPOL2: LEAP SECOND DETECTED',/,
cc     1                             16X,'EPOCHS IN POLE FILE: ',A,/)
cc              GPSUTC(2)=GPSUTC(2)-TEST/86400.D0
cc              UT1UTC(2)=UT1UTC(2)-TEST/86400.D0
cc            ENDIF
cc          ENDIF
C
C CONSISTENCY BETWEEN UT1-UTC AND GPS-UTC
          IF (DABS(UT1UTC(2)-UT1UTC(1)).GT.0.5) THEN
            CALL TIMSTR(2,T,TSTRNG)
            WRITE(LFNERR,151) TSTRNG
151         FORMAT(/,' *** SR GETPOL2: INCONSISTENCY BETWEEN UT1-UTC',
     1               ' AND GPS-UTC',/,
     2             16X,'PROBABLY LEAP SECOND PROBLEM',/,
     3             16X,'EPOCHS IN POLE FILE: ',A,/)
            CALL EXITRC(2)
          ENDIF
C
C RECORDS FOUND IN BUFFER AND SET
          GOTO 999
        ENDIF
      ENDDO
C
C NO SUITABLE RECORDS FOUND: UNEXPECTED ERROR
C -------------------------------------------
      WRITE(LFNERR,905) FILPOL
905   FORMAT(/,' *** SR GETPOL: UNEXPECTED ERROR READING POLE FILE',/,
     1         16X,'NO SUITABLE INTERVAL FOUND IN BUFFER',/,
     2         16X,'POLE FILE: ',A,/)
      CALL EXITRC(2)
C
C ERROR READING POLE FILE, STOP:
910   CALL TIMSTR(1,(/TREQ/),THLP)
      WRITE(LFNERR,115) THLP,FILPOL
115   FORMAT(/,' *** SR GETPOL: NO SUITABLE EPOCHS IN POLE FILE',/,
     1                     16X,'EPOCH    : ',A,/,
     2                     16X,'POLE FILE: ',A,/)
      CALL EXITRC(2)
C
C END, RETURN
999   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
