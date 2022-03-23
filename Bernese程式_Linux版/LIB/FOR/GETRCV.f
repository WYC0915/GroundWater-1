      MODULE s_GETRCV
      CONTAINS

C*
      SUBROUTINE GETRCV(RECTYP,NFREQ,ICODE,IWLFAC,ICLASS,ISYST)
CC
CC NAME       :  GETRCV
CC
CC PURPOSE    :  GET RECEIVER SPECIFIC INFORMATION FROM (NEW) RECEIVER
CC               INFORMATION FILE
CC
CC PARAMETERS :
CC         IN :  RECTYP : RECEIVER TYPE                       CH*(*)
CC        OUT :  NFREQ  : NUMBER OF FREQUENCIES               I*4
CC               ICODE(I): TYPE OF CODE DATA ON FREQ I        I*4(2)
CC                          =1: P-CODE: P1 or P2
CC                          =2: C/A-CODE: C1 or C2
CC                          =3: X2=C1+(P2-P1)
CC                          =0: NO CODE
CC               IWLFAC(I): I=1,NFREQ: WAVELENGTH FACTOR FOR  I*4(2)
CC                         FREQUENCY I
CC               ICLASS : RECEIVER CLASS                      I*4
CC                        =0: SINGLE-FREQUENCY RECEIVER
CC                        =1: P1/P2
CC                        =2: C1/X2
CC                        =3: C1/P2
CC               ISYST  : TRACKED SATELLITE SYSTEMS           I*4
CC                        =-1: GPS+GLONASS+GALILEO (ALL)
CC                        = 0: GPS
CC                        = 1: GPS+GLONASS
CC                        = 2: GPS+GALILEO
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/08/19 08:15
CC
CC CHANGES    :  28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-DEC-01 : MM: Use new RECEIVER-file format
CC               07-MAY-02 : SS: DCB UPDATE
CC               06-AUG-02 : SS: X2 INSTEAD OF C2
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR(*)
CC                               FILENAMES CHR(*)
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-MAY-07 : SS: MAXRCV FROM 50 TO 100
CC               21-JUN-07 : SS: RETURN SET OF TRACKED SATELLITE SYSTEMS
CC               04-AUG-08 : DT: CHECK FOR SLR
CC               07-JUN-11 : HB: MAXRCV 100 => 120
CC               25-JUL-11 : LP: MAXRCV 120 => 200
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE d_stacrx, ONLY: MTypeSLR
      USE s_opnfil
      USE s_exitrc
      USE s_gtflna
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLASS, IFIRST, IFRQ  , IOSTAT, IRC   , IRCV  , LREC  ,
     1          NFREQ , NRCV  , MAXRCV, ISYST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMUM NUMBER OF RECEIVER TYPES
C --------------------------------
      PARAMETER (MAXRCV=200)
C
      CHARACTER*(fileNameLength80) RCVFIL
      CHARACTER*(*) RECTYP
      CHARACTER*20 RCVTYP(MAXRCV)
      CHARACTER*4  SYST
      CHARACTER    CODTYP(2)*1,STRING*80
      INTEGER*4    NFRRCV(MAXRCV),RCVCOD(2,MAXRCV),ICLRCV(MAXRCV)
      INTEGER*4    IWLFAC(2),ICODE(2),IWLRCV(2,MAXRCV),ISYRCV(MAXRCV)
      INTEGER*4    NUMCODE(2), NUMFREQ(2), IERR
C
C
      DATA IFIRST/1/
C
C Check for SLR "receiver"
C ------------------------
      IF(RECTYP(1:20).EQ.MTypeSLR) THEN
        NFREQ=1
        ICODE=0
        IWLFAC=1
        ICLASS=0
        ISYST=-1

        RETURN
      END IF
C
C READ ALL RECEIVER INFORMATION FROM FILE
C ---------------------------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        IERR=0
C
C OPEN RECEIVER INFORMATION FILE
        CALL GTFLNA(1,'RECEIVR',RCVFIL,IRC)
        CALL OPNFIL(LFNLOC,RCVFIL,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,RCVFIL,'GETRCV')
C
C SKIP TITLE LINES
        READ(LFNLOC,2)
2       FORMAT(/////)
C
C READ ALL RECEIVER TYPES
        IRCV=1
10      READ(LFNLOC,11,END=20) STRING
11      FORMAT(A)
C
C CHECK FOR BLANK LINE
        IF(STRING.EQ.' ') GOTO 20
C
        READ(STRING,3) RCVTYP(IRCV),NFRRCV(IRCV),
     1                 CODTYP(1),NUMCODE(1),NUMFREQ(1),IWLRCV(1,IRCV),
     2                 SYST
3       FORMAT(A20,3X,I1,5X,A1,I1,5X,I1,6X,I1,5X,A4)
C
C READ SECOND LINE
          IF(NFRRCV(IRCV).GT.1) THEN
            READ(LFNLOC,4)CODTYP(2),NUMCODE(2),NUMFREQ(2),IWLRCV(2,IRCV)
4           FORMAT(29X,A1,I1,5X,I1,6X,I1)
          END IF
C
C READ BLANK LINE
        READ(LFNLOC,'( )')
C
C CHECK NUMBER OF FREQUENCIES
        IF (NFRRCV(IRCV).NE.1 .AND. NFRRCV(IRCV).NE.2) THEN
          WRITE(LFNERR,902) RCVTYP(IRCV),NFRRCV(IRCV)
902       FORMAT(/,' *** SR GETRCV: WRONG NUMBER OF FREQUENCIES',/,
     1                         16X,'RECEIVER-TYPE          : ',A,/,
     2                         16X,'NUMBER OF FREQUENCIES  : ',I3,/,
     3                         16X,'ALLOWED NUMBER OF FREQ.: 1 OR 2',/)
          CALL EXITRC(2)
        ENDIF
C
C CHECK WAVELENGTH FACTOR
        IF (IWLRCV(1,IRCV).NE.1) THEN
          WRITE(LFNERR,903) RCVTYP(IRCV),IWLRCV(1,IRCV)
903       FORMAT(/,' *** SR GETRCV: WRONG WAVELENGTH FACTOR ON L1',/,
     1                         16X,'RECEIVER-TYPE      : ',A,/,
     2                         16X,'SPECIFIED WL FACTOR: ',I3,/,
     3                         16X,'ALLOWED WL FACTOR  :   1',/)
          CALL EXITRC(2)
        ENDIF
        IF (NFRRCV(IRCV).EQ.2) THEN
          IF (IWLRCV(2,IRCV).NE.1 .AND. IWLRCV(2,IRCV).NE.2) THEN
            WRITE(LFNERR,904) RCVTYP(IRCV),IWLRCV(2,IRCV)
904         FORMAT(/,' *** SR GETRCV: WRONG WAVELENGTH FACTOR ON L2',/,
     1                           16X,'RECEIVER-TYPE      : ',A,/,
     2                           16X,'SPECIFIED WL FACTOR: ',I3,/,
     3                           16X,'ALLOWED WL FACTOR  : 1 OR 2',/)
            CALL EXITRC(2)
          ENDIF
        ENDIF
C
C CHECK FREQUENCY NUMBER, CODE NUMBER AND CODE TYPE
        DO 70 IFRQ=1,NFRRCV(IRCV)
          IF (NUMFREQ(IFRQ).NE.IFRQ) THEN
            WRITE(LFNERR,905) IFRQ,RCVTYP(IRCV),NUMFREQ(IFRQ),IFRQ
905         FORMAT(/,' *** SR GETRCV: WRONG VALUE FOR FREQUENCY ',I1/,
     1                           16X,'RECEIVER-TYPE  : ',A,/,
     2                           16X,'SPECIFIED VALUE: L',I1,/,
     3                           16X,'EXPECTED VALUE : L',I1,/)
            CALL EXITRC(2)
          ENDIF
C
          IF (CODTYP(IFRQ).NE.'P' .AND.
     1        CODTYP(IFRQ).NE.'C' .AND.
     2        CODTYP(IFRQ).NE.'X') THEN
            WRITE(LFNERR,906) IFRQ,RCVTYP(IRCV),CODTYP(IFRQ)
906         FORMAT(/,' *** SR GETRCV: WRONG CODE TYPE ON L',I1/,
     1                           16X,'RECEIVER-TYPE  : ',A,/,
     2                           16X,'SPECIFIED VALUE: ',A1,/,
     3                           16X,'EXPECTED VALUE : P OR C OR X',/)
            CALL EXITRC(2)
          ENDIF
C
          IF (NUMCODE(IFRQ).NE.NUMFREQ(IFRQ)) THEN
            WRITE(LFNERR,907) IFRQ,RCVTYP(IRCV),CODTYP(IFRQ),
     1                        NUMCODE(IFRQ),CODTYP(IFRQ),IFRQ
907         FORMAT(/,' *** SR GETRCV: WRONG CODE NUMBER ON L',I1/,
     1                           16X,'RECEIVER-TYPE  : ',A,/,
     2                           16X,'SPECIFIED VALUE: ',A1,I1/,
     3                           16X,'EXPECTED VALUE : ',A1,I1,/)
            CALL EXITRC(2)
          ENDIF
70      CONTINUE
C
C CONVERT CODE TYPE
        DO 30 IFRQ=1,NFRRCV(IRCV)
          IF(CODTYP(IFRQ).EQ.'P') THEN
            RCVCOD(IFRQ,IRCV)=1
          ELSEIF(CODTYP(IFRQ).EQ.'C') THEN
            RCVCOD(IFRQ,IRCV)=2
          ELSEIF(CODTYP(IFRQ).EQ.'X') THEN
            RCVCOD(IFRQ,IRCV)=3
          ELSE
            RCVCOD(IFRQ,IRCV)=0
          ENDIF
30      CONTINUE
C
C DEFINE RECEIVER CLASS
        IF (NFRRCV(IRCV).EQ.1) THEN
          ICLRCV(IRCV)=0
        ELSE
          IF (RCVCOD(1,IRCV).EQ.1 .AND. RCVCOD(2,IRCV).EQ.1) THEN
            ICLRCV(IRCV)=1
          ELSEIF (RCVCOD(1,IRCV).EQ.2 .AND. RCVCOD(2,IRCV).EQ.3) THEN
            ICLRCV(IRCV)=2
          ELSEIF (RCVCOD(1,IRCV).EQ.2 .AND. RCVCOD(2,IRCV).EQ.1) THEN
            ICLRCV(IRCV)=3
          ELSE
            WRITE(LFNERR,908) RCVTYP(IRCV),CODTYP(1),CODTYP(2)
908         FORMAT(/,' *** SR GETRCV: UNEXPECTED RECEIVER CLASS',/,
     1         16X,'RECEIVER TYPE   : ',A,/,
     2         16X,'CODE OBSERVABLES: ',A1,'1/',A1,'2',/)
            CALL EXITRC(2)
          ENDIF
        ENDIF
C
C RETURN SET OF TRACKED SATELLITE SYSTEMS
        IF (SYST(1:3).EQ.'GRE') THEN
          ISYRCV(IRCV)=-1
        ELSEIF (SYST(1:2).EQ.'GR') THEN
          ISYRCV(IRCV)=1
        ELSEIF (SYST(1:2).EQ.'GE' .OR. SYST(1:3).EQ.'G E') THEN
          ISYRCV(IRCV)=2
        ELSEIF (SYST(1:1).EQ.'G') THEN
          ISYRCV(IRCV)=0
        ELSE
          WRITE(LFNERR,909) RCVTYP(IRCV),SYST
909       FORMAT(/,' *** SR GETRCV: UNKNOWN SET OF SATELLITE SYSTEMS',/,
     1       16X,'RECEIVER TYPE    : ',A,/,
     2       16X,'SATELLITE SYSTEMS: ',A4,/)
          CALL EXITRC(2)
        ENDIF
C
C READ NEXT RECEIVER TYPE
        IRCV=IRCV+1
        IF(IRCV.GT.MAXRCV) THEN
          WRITE(LFNERR,901) IRCV,MAXRCV
901       FORMAT(/,' *** SR GETRCV: TOO MANY RECEIVER TYPES',/,
     1                         16X,'NUMBER OF RECEIVER TYPES >=',I3,/,
     2                         16X,'MAX. NUMBER OF REC.TYPES : ',I3,/)
          CALL EXITRC(2)
        ENDIF
        GOTO 10
C
C NUMBER OF RECEIVER TYPES
20      NRCV=IRCV-1
        CLOSE(UNIT=LFNLOC)
      ENDIF
C
C FIND REQUESTED RECEIVER TYPE IN RECEIVER TYPE LIST "RCVTYP"
C -----------------------------------------------------------
      LREC=20
      IF (RECTYP(17:20)=='????') LREC=16
      DO 50 IRCV=1,NRCV
        IF(RCVTYP(IRCV)(1:LREC).EQ.RECTYP(1:LREC)) GOTO 60
50    CONTINUE
C
C RECEIVER TYPE NOT FOUND
      WRITE(LFNERR,51) TRIM(RECTYP)
51    FORMAT(/,' *** SR GETRCV: RECEIVER TYPE NOT FOUND',/,
     1                     16X,'RECEIVER TYPE: ',A,/)
      CALL EXITRC(2)
C
C COPY RECEIVER INFO
C ------------------
60    NFREQ=NFRRCV(IRCV)
      DO 80 IFRQ=1,NFREQ
        ICODE (IFRQ)=RCVCOD(IFRQ,IRCV)
        IWLFAC(IFRQ)=IWLRCV(IFRQ,IRCV)
80    CONTINUE
      ICLASS=ICLRCV(IRCV)
      ISYST =ISYRCV(IRCV)
C
      RETURN
      END SUBROUTINE

      END MODULE
