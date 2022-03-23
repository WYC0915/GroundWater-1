      MODULE s_WTSKEL
      CONTAINS

C*
      SUBROUTINE WTSKEL(SKLKEY,SKLFIL,FIELDS,IFLAG,LFNOUT)
CC
CC NAME       :  WTSKEL
CC
CC PURPOSE    :  READ SECTION WITH THE KEY "SKLKEY" FROM SKELETON
CC               FILE, FILL VARIABLES INTO SKELETON, AND WRITE
CC               RECORDS TO OUTPUT FILE (LFNOUT).
CC               SECTIONS IN THE SKELETON FILE START WITH A LINE
CC               CONTAINING ")XXX" IN COLUMNS 1 TO 4,
CC               WHERE "XXX" IS A KEY CHARACTER STRING OF 3 CHARCATERS
CC
CC PARAMETERS :
CC         IN :  SKLKEY : PART OF SKELETON TO BE USED         CH*3
CC               SKLFIL : FILENAME OF SKELETON FILE           CH*(*)
CC               FIELDS : VECTOR WITH INPUT FIELDS            CH*(*)(*)
CC               IFLAG  : FLAGS FOR EACH FIELD                 I*4(*)
CC               LFNOUT : LOGICAL FILE NUMBER OF OUTPUT FILE   I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER, M. ROTHACHER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  12-JUL-92
CC
CC CHANGES    :  29-OCT-93 : SF: UPDATE HEADER INFORMATION
CC               10-AUG-94 : MR: CALL EXITRC
CC               20-MAY-03 : HU: OPEN FILENAME LENGTH
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      USE f_lengt0
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , IFIELD, IOSTAT, IREC  , J     , JFIELD,
     1          K     , KFIELD, LACT  , LDECL , LFIELD,
     2          LFNOUT, LSTR  , LTEXT , NFIELD
C
      CHARACTER     FIELDS(*)*(*)
      CHARACTER*132 STR132
      CHARACTER*(*) SKLFIL
      CHARACTER*3   SKLKEY
C
      INTEGER*4     IFLAG(*)
      INTEGER*4     K1(66),K2(66)
C
C
C OPEN SKELETON FILE
C ------------------
      CALL OPNFIL(LFNLOC,SKLFIL,'OLD','FORMATTED','READONLY',
     1            ' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,SKLFIL,'WTSKEL')
C
C FIND CORRECT SKELETON PART
C --------------------------
      DO 10 IREC=1,100000
        READ(LFNLOC,11,END=20) STR132
11      FORMAT(A)
        IF (STR132(1:1).EQ.')'.AND.STR132(2:4).EQ.SKLKEY) GOTO 30
10    CONTINUE
C
C SECTION WITH KEY "SKLKEY" NOT FOUND
20    WRITE(LFNERR,901) SKLKEY,TRIM(SKLFIL)
901   FORMAT(/,' *** SR WTSKEL: SECTION NOT FOUND IN SKELETON FILE',/,
     1                     16X,'SECTION KEY  : ',A3,/,
     2                     16X,'SKELETON FILE: ',A,/)
      CALL EXITRC(2)
C
30    CONTINUE
C
C LOOP OVER ALL RECORDS OF SECTION "SKLKEY"
C -----------------------------------------
      JFIELD=0
1000  READ(LFNLOC,11,END=900) STR132
C
C END OF SECTION
      IF (STR132(1:1).EQ.')') GOTO 900
C
      LSTR=LENGT0(STR132)
      NFIELD=0
      I1=1
C
C  LOOK FOR %-FIELD
60    DO 50 I=I1,LSTR
        IF(STR132(I:I).EQ.'%') GOTO 70
50    CONTINUE
C
      GOTO 110
C
70    NFIELD=NFIELD+1
      K1(NFIELD)=I
      DO 80 I=K1(NFIELD)+1,LSTR
        IF(STR132(I:I).NE.'%') GOTO 90
80    CONTINUE
90    K2(NFIELD)=I-1
      I1=I+1
      GOTO 60
C
C  COPY FROM GIVEN FIELDS INTO OUTPUT STRING
110   IF(NFIELD.NE.0) THEN
        DO 100 KFIELD=1,IFLAG(JFIELD+1)
          DO 120 IFIELD=1,NFIELD
            JFIELD=JFIELD+1
            J=0
            LTEXT=LENGT0(FIELDS(JFIELD))
            LFIELD=K2(IFIELD)-K1(IFIELD)+1
            IF(LTEXT.GT.LFIELD) THEN
              WRITE(LFNERR,902) JFIELD,LFIELD,LTEXT,
     1                          FIELDS(JFIELD)(1:LTEXT),SKLKEY,
     2                          SKLFIL(1:LENGT1(SKLFIL))
902           FORMAT(/,' *** SR WTSKEL: FORMAT OVERFLOW IN SKELETON',
     1                 ' FILE',/,
     2                             16X,'FIELD NUMBER :',I4,/,
     3                             16X,'FIELD LENGTH :',I4,/,
     4                             16X,'TEXT  LENGTH :',I4,/,
     5                             16X,'TEXT         : ',A,/,
     6                             16X,'SECTION KEY  : ',A,/,
     7                             16X,'SKELETON FILE: ',A,/)
              CALL EXITRC(2)
            END IF
            LDECL=LEN(FIELDS(JFIELD))
            IF(LDECL.LT.LFIELD) THEN
              WRITE(LFNERR,903) JFIELD,LFIELD,LDECL,
     1                          FIELDS(JFIELD)(1:LTEXT),SKLKEY,
     2                          SKLFIL(1:LENGT1(SKLFIL))
903           FORMAT(/,' *** SR WTSKEL: TEXT DECLARATION TOO SMALL',/,
     1                             16X,'FIELD NUMBER :',I4,/,
     2                             16X,'FIELD LENGTH :',I4,/,
     3                             16X,'TEXT DECLAR. :',I4,/,
     4                             16X,'TEXT         : ',A,/,
     5                             16X,'SECTION KEY  : ',A,/,
     6                             16X,'SKELETON FILE: ',A,/)
              CALL EXITRC(2)
            END IF
            DO 130 K=K1(IFIELD),K2(IFIELD)
              J=J+1
              STR132(K:K)=FIELDS(JFIELD)(J:J)
130         CONTINUE
120       CONTINUE
          LACT=LENGT0(STR132)
          IF(LACT.NE.0) THEN
            WRITE(LFNOUT,121) STR132(1:LACT)
121         FORMAT(A)
          ELSE
            WRITE(LFNOUT,*)
          END IF
100     CONTINUE
      ELSE
        IF(LSTR.NE.0) THEN
          WRITE(LFNOUT,121) STR132(1:LSTR)
        ELSE
          WRITE(LFNOUT,*)
        END IF
      END IF
      GOTO 1000
C
900   CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
