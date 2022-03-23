      MODULE s_RDAPHC
      CONTAINS

C*
      SUBROUTINE RDAPHC(MAXRCV,MAXELV,MAXAZI,FILPHC,TITLE ,NRCV  ,
     1                  RECTYP,ANTTYP,IANTEN,NFRANT,ANTOFF,MODTYP,
     2                  SINEX ,NPCV  ,ANTPCV,MAXZEN,METHOD,atDATE,
     3                  FILINFO)
CC
CC NAME       :  RDAPHC
CC
CC PURPOSE    :  READ RECEIVER AND SATELLITE ANTENNA PHASE CENTER
CC               INFORMATION INCLUDING ELEVATION DEPENDENT CORRECTIONS
CC               AND ELEVATION/AZIMUTH COEFFICIENTS.
CC
CC PARAMETERS :
CC        OUT :  MAXRCV : MAXIMUM NUMBER OF RECEIVER/ANTENNA  I*4
CC                        PAIRS RESP. OF ANTENNAS
CC               MAXELV : MAXIMUM NUMBER OF ELEVATION-DEP.    I*4
CC                        CORRECTION VALUES
CC               MAXAZI : MAXIMUM NUMBER OF AZIMUTH VALUES    I*4
CC               FILPHC : ANTENNA PHASE CENTER FILE NAME      CH*(*)
CC               TITLE  : TITLE OF PHASE CENTER FILE          CH*80
CC               NRCV   : NUMBER OF RECEIVER/ANTENNA PAIRS    I*4
CC                        RESP. OF ANTENNAS
CC               RECTYP(I),I=1,..,NRCV: RECEIVER TYPES        CH*20
CC               ANTTYP(I),I=1,..,NRCV: ANTENNA TYPES         CH*20
CC               IANTEN(2,I),I=1,..,NRCV: ANTENNA NUMBERS     I*4
CC                        FROM - TO
CC               NFRANT(I),I=1,..,NRCV: NUMBER OF FREQUENCIES I*4
CC               ANTOFF(3,2,I),I=1,..,NRCV: ANTENNA OFFSETS   R*8
CC                        3 COMP. FOR L1 AND L2 IN METERS
CC               MODTYP(I),I=1,..,NRCV: MODEL TYPE FOR        I*4
CC                        AZIMUTH/ELEVATION COEFFICIENTS
CC                        =-1: GRID POINTS (OLD FORMAT,
CC                             WITH EXACTLY 19 VALUES, ONLY
CC                             ELEVATION-DEPENDENT
CC                        = 0: NO PHASE CENTER VARIATIONS
CC                        = 1: GRID POINTS
CC                        = 2: OLD SHPERICAL HARMONICS (NOT
CC                             NORMALIZED !)
CC                        = 3: NEW SPHERICAL HARMONICS
CC                             (NORMALIZED)
CC                        = 4: NORMALIZED SPHERICAL HARMONICS
CC                             (USING UPPER HEMISPHERE ONLY)
CC               SINEX(I),I=1,...,NRCV: SINEX CODE            CHR*10
CC               NPCV(2,I),I=1,..,NRCV: NUMBER OF ELEVATION   I*4
CC                        AND AZIMUTH VALUES OR COEFFICIENTS
CC                        MODTYP=1    : NUMBER OF GRID POINTS
CC                        MODTYP=2,3,4: DEGREE AND ORDER OF
CC                                      SPHERICAL HARMONICS
CC               ANTPCV(K,J,L,I),K=1,..,MAXELV,J=1,..,MAXAZI,
CC                        L=1,2,I=1,..,NRCV: GRID VALUES (M)  R*8
CC                        OR COEFF. FOR ELEV./AZIMUTH-DEP.
CC                        MODELS
CC               MAXZEN(I),I=1,..,NRCV: MAXIMUM ZENITH/NADIR  I*4
CC                        ANGLE (DEG), FOR WHICH PHASE CENTER
CC                        VARIATION INFORMATION IS AVAILABLE
CC               METHOD(I),I=1,..,NRCV: METHOD OF DETERMINATION CHR*20
CC               atDATE(I),I=1,..,NRCV: DATE OF DETERMINATION  CHR*10
CC               FILINFO: FILENAME OF SOURCE ANTEX FILE               CHR*80
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  28-DEC-95
CC
CC CHANGES    :  13-MAY-98 : MR: REMOVE "DFLOAT"
CC               11-AUG-03 : RS: RECTYP CAN BE BLANK, READ ALSO SAT.
CC                               ANTENNA INFORMATION, ADD MAXZEN
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20,
CC                               FILENAMES CHR(*)
CC               10-SEP-03 : HU: MERGED
CC               24-OCT-03 : RS: USE REAL IN NPCV CALCULATION
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-NOV-05 : AG: READING OF SINEX, METHOD, ATDATE AND
CC                               FILINFO ADDED
CC               15-MAR-06 : AG: REALLY READING OF METHOD AND ATDATE
CC               24-MAR-06 : AG: INITIALSATION CONCERNING ZERO PATTERNS
CC                               OF NPCV, SINEX, METHOD AND ATDATE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAZI  , IELV  , IFREQ1, IFREQ2, IFRMAP, IFRMAT, IFROM ,
     1          IFRQ  , IOSTAT, IRCV  , ITO   , J     , K     , MAXAZI,
     2          MAXELV, MAXEV2, MAXRCV, MODMAP, NAZI  , NELV  , NRCV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMUM DIMENSIONS
C ------------------
C
C MAXRCV: MAXIMUM NUMBER OF RECEIVER/ANTENNA PAIRS
C MAXELV: MAXIMUM NUMBER OF DIFFERENT ELEVATION ANGLES
C         (19 ALLOWS FOR ELEVATION INCREMENTS DOWN TO 5 DEG)
C MAXAZI: MAXIMUM NUMBER OF DIFFERENT AZIMUTH ANGLES
C         (73 ALLOWS FOR AZIMUTH INCREMENTS DOWN TO 5 DEG)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*80  TITLE,FILINFO
      CHARACTER*(*) FILPHC
      CHARACTER*20  RECTYP(MAXRCV),ANTTYP(MAXRCV),METHOD(MAXRCV)
      CHARACTER*10  SINEX(MAXRCV),atDATE(MAXRCV)
C
      REAL*8        ANTOFF(3,2,MAXRCV),ANTPCV(MAXELV,MAXAZI,2,MAXRCV)
C
      INTEGER*4     IANTEN(2,MAXRCV),NFRANT(MAXRCV),MODTYP(MAXRCV)
      INTEGER*4     NPCV(2,MAXRCV),MAXZEN(MAXRCV)
C
C LOCAL DECLARATIONS
C ------------------
      PARAMETER (MAXEV2=19)
C
      CHARACTER*150 STRNG1,STRNG2
      CHARACTER*20  RECMAP,ANTMAP
C
      INTEGER*4     ELVECC(MAXEV2,2)
C
C
C INITIALIZE ARRAY "ANTPCV", "NPCV" AND "MAXZEN"
C ----------------------------------------------
      DO IRCV=1,MAXRCV
        DO IFRQ=1,2
          DO IAZI=1,MAXAZI
            DO IELV=1,MAXELV
              ANTPCV(IELV,IAZI,IFRQ,IRCV)=0.D0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      DO IRCV=1,MAXRCV
        NPCV(1,IRCV)=MAXELV
        NPCV(2,IRCV)=1
      ENDDO
C
      DO IRCV=1,MAXRCV
        MAXZEN(IRCV)=90
        SINEX='          '
        METHOD='                   '
        atDATE='          '
      ENDDO
C
C READ ALL ANTENNA INFORMATION FROM FILE
C --------------------------------------
C
C OPEN ANTENNA PHASE CENTER INFORMATION FILE
      CALL OPNFIL(LFNLOC,FILPHC,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPHC,'RDAPHC')
C
C READ TITLE LINES
      READ(LFNLOC,3) TITLE,FILINFO
3     FORMAT(A80,/,A80,/////)
C
C LOOP OVER ALL RECEIVER/ANTENNA PAIRS RESP. OVER ALL ANTENNAS
C ------------------------------------------------------------
      DO IRCV=1,100000
        READ(LFNLOC,11,END=20) STRNG1
11      FORMAT(A)
C
C CHECK FOR BLANK LINE
        IF(STRNG1.EQ.' ') GOTO 20
C
C TOO MANY RECEIVER/ANTENNA PAIRS RESP. ANTENNAS
        IF (IRCV.GT.MAXRCV) THEN
          WRITE(LFNERR,52) IRCV,MAXRCV
52        FORMAT(/,' *** SR RDAPHC: TOO MANY RECEIVER/ANTENNA ',
     1             'PAIRS RESP. ANTENNAS',
     1           /,16X,'NUMBER OF REC./ANT. PAIRS >=',I4,
     3           /,16X,'MAXIMUM NUMBER ALLOWED     :',I4,
     4           /,16X,'INCREASE "MAXRCV" IN SR CALLING RDAPHC',/)
          CALL EXITRC(2)
        ENDIF
C
        READ(LFNLOC,11,END=20) STRNG2
        READ(STRNG1,4) RECTYP(IRCV),IANTEN(1,IRCV),IANTEN(2,IRCV),
     1                 IFREQ1,(ANTOFF(K,1,IRCV),K=1,3),IFRMAT,
     2                 (ELVECC(J,1),J=1,MAXEV2)
4       FORMAT(A20,2X,I6,1X,I6,3X,I1,2X,3(1X,F7.4),3X,I1,3X,19I3)
        READ(STRNG2,5) ANTTYP(IRCV),
     1                 IFREQ2,(ANTOFF(K,2,IRCV),K=1,3),
     2                 (ELVECC(J,2),J=1,MAXEV2)
5       FORMAT(A20,15X,        3X,I1,2X,3(1X,F7.4),7X,      19I3)
C
        IF(ANTTYP(IRCV).EQ.' ') THEN
          ANTTYP(IRCV)=RECTYP(IRCV)
          RECTYP(IRCV)=' '
        ENDIF
C
        IF(IFREQ1.NE.1) THEN
          WRITE(LFNERR,6) STRNG1,STRNG2
6         FORMAT(/,' *** SR RDAPHC: ERROR IN THE PHASE CENTER ',
     1             'ECCENTR.FILE',
     2           /,            16X,'ON THE LINES',
     3           /,1X,A79,/,1X,A79,/)
          CALL EXITRC(2)
        END IF
        IF(IFREQ2.NE.2.AND.IFREQ2.NE.0) THEN
          WRITE(LFNERR,6) STRNG1,STRNG2
          CALL EXITRC(2)
        END IF
        IF(IFREQ2.EQ.0) THEN
          NFRANT(IRCV)=1
        ELSE
          NFRANT(IRCV)=2
        ENDIF
C
C READ BLANK LINE
        READ(LFNLOC,'( )')
C
C REFORMAT ELEVATION DEPENDENT CORRECTIONS
        IF (IFRMAT.EQ.0) THEN
          MODTYP(IRCV)=0
        ELSEIF (IFRMAT.EQ.1) THEN
          MODTYP(IRCV)=-1
          NPCV(1,IRCV)=MAXEV2
          NPCV(2,IRCV)=1
          DO IFRQ=1,NFRANT(IRCV)
            DO IELV=1,NPCV(1,IRCV)
              ANTPCV(IELV,1,IFRQ,IRCV)=ELVECC(IELV,IFRQ)/1000.D0
            ENDDO
          ENDDO
        ELSEIF (IFRMAT.EQ.2) THEN
          MODTYP(IRCV)=-2
        ELSE
          WRITE(LFNERR,53) IFRMAT,ANTTYP(IRCV),RECTYP(IRCV)
53        FORMAT(/,' *** SR RDAPHC: INVALID FORMAT FOUND',/,
     1             '                FORMAT NUMBER:',I2,/,
     2             '                ANTENNA  TYPE: ',A,/,
     3             '                RECEIVER TYPE: ',A,/)
          CALL EXITRC(2)
        ENDIF
C
C READ NEXT RECEIVER TYPE
      ENDDO
C
C NUMBER OF RECEIVER TYPES
20    NRCV=IRCV-1
C
C READ AZIMUTH/ELEVATION MAPS
C ---------------------------
      DO IRCV=1,NRCV
        IF (MODTYP(IRCV) .EQ. -2) THEN
C
C SEARCH FOR FIRST MAP
C
32        READ(LFNLOC,11,END=40) STRNG1
          IF (STRNG1(1:24) .NE. 'RECEIVER TYPE        ANT' .AND.
     1        STRNG1(1:24) .NE. 'ANTENNA TYPE         DUM') GOTO 32
C
C SKIP A HEADER LINE
C
          READ(LFNLOC,'( )',END=40)
C
C GET THE RECEIVER TYPE, ANTENNA TYPE, "FROM TO", THE NUMBER OF
C ELEVATION COLUMNS AND AZIMUTH ROWS
C
          IF (RECTYP(IRCV).EQ.' ') THEN
            READ(LFNLOC,12) ANTMAP,RECMAP,IFROM,ITO,MODMAP,NELV,NAZI,
     1                MAXZEN(IRCV),SINEX(IRCV),METHOD(IRCV),atDATE(IRCV)
          ELSE
            READ(LFNLOC,13) RECMAP,ANTMAP,IFROM,ITO,MODMAP,
     1                   NELV,NAZI,SINEX(IRCV),METHOD(IRCV),atDATE(IRCV)
          ENDIF
12        FORMAT(A20,1X,A20,1X,I6,1X,I6,2X,I3,2X,3(2X,I3),2X,A10,1X,A20,
     1           1X,A10)
13        FORMAT(A20,1X,A20,1X,I6,1X,I6,2X,I3,2X,2(2X,I3),2X,A10,1X,A20,
     1           1X,A10)
C
C MAKE SURE THIS IS THE RIGHT MAP (CORRECT REC/ANT PAIR AND NUMBERS)
C
          IF ((RECTYP(IRCV)   .NE. RECMAP) .OR.
     1        (ANTTYP(IRCV)   .NE. ANTMAP) .OR.
     2        (IANTEN(1,IRCV) .NE. IFROM)  .OR.
     3        (IANTEN(2,IRCV) .NE. ITO)) THEN
            WRITE(LFNERR,8)
     1          ANTTYP(IRCV),RECTYP(IRCV),
     2          IANTEN(1,IRCV),IANTEN(2,IRCV),
     3          ANTMAP,RECMAP,IFROM,ITO
8           FORMAT(
     1        /,' *** SR RDAPHC: ERROR IN THE PHASE CENTER ',
     2          'ECCENTR.FILE',
     3        /,'                MAP PARAMETERS DO NOT MATCH',
     4        /,'                LOOKING FOR:',A20,1X,A20,1X,I6,1X,I6,
     5        /,'                FOUND      :',A20,1X,A20,1X,I6,1X,I6)
            CALL EXITRC(2)
          END IF
C
          MODTYP(IRCV)=MODMAP
C
C VALIDATE THE NUMBER OF ELEVATION COLUMNS AND AZIMUTH ROWS
C
          IF (MODTYP(IRCV).EQ.1) THEN
            IF (NELV.EQ.90) THEN
              NPCV(1,IRCV)=1
            ELSE
              NPCV(1,IRCV) = NINT(DBLE(MAXZEN(IRCV))/DBLE(NELV)) + 1
            ENDIF
            IF (NAZI.EQ.360) THEN
              NPCV(2,IRCV)=1
            ELSE
              NPCV(2,IRCV) = IDNINT(360.D0/DBLE(NAZI)) + 1
            ENDIF
          ELSE
            NPCV(1,IRCV) = NELV
            NPCV(2,IRCV) = 2*NAZI+1
          END IF
C
          IF (NPCV(1,IRCV) .GT. MAXELV) THEN
            WRITE(LFNERR,7) 'ELEVATION',NPCV(1,IRCV),MAXELV,
     1                      ANTMAP,RECMAP,IFROM,ITO
7           FORMAT(/,' *** SR RDAPHC: ERROR IN THE PHASE CENTER ',
     1               'ECCENTR.FILE',/,
     2                           16X,'TOO MANY ',A,' INTERVALS',/,
     3                           16X,'NUMBER OF INTERVALS:',I3,/,
     4                           16X,'MAX. NUMBER OF INT.:',I3,/,
     5                           16X,'ANTENNA  TYPE      : ',A,/,
     6                           16X,'RECEIVER TYPE      : ',A,/,
     7                           16X,'ANTENNA  NRS       :',2I6,/)
            CALL EXITRC(2)
          END IF
          IF (NPCV(2,IRCV) .GT. MAXAZI) THEN
            WRITE(LFNERR,7) 'AZIMUTH',NPCV(2,IRCV),MAXAZI,
     1                      ANTMAP,RECMAP,IFROM,ITO
          END IF
C
C SKIP ANOTHER 2 HEADER LINES
C
          READ(LFNLOC,'(/)')
C
C READ IN MAP
C ??? ZEN ELEVATION VALUES GO FROM 90 TO 0 IN MAP BUT WE STORE
C ??? THEM FROM 0 TO 90 IN ANTPCV
C
          DO IAZI=1,NPCV(2,IRCV)
            DO IFRQ=1,NFRANT(IRCV)
              READ(LFNLOC,'(1X,I1,4X,19(F7.2))',END=40) IFRMAP,
     1          (ANTPCV(IELV,IAZI,IFRQ,IRCV),IELV=1,NPCV(1,IRCV))
              IF (IFRMAP.NE.IFRQ) THEN
                WRITE(LFNERR,71) ANTMAP,RECMAP,IFROM,ITO
71              FORMAT(/,' *** SR RDAPHC: ERROR IN THE PHASE CENTER ',
     1                   'ECCENTR.FILE',/,
     2                           16X,'INVALID FREQUENCY INDICATION ',
     3                               'IN PHASE CENTER MAP',/,
     4                           16X,'ANTENNA  TYPE      : ',A,/,
     5                           16X,'RECEIVER TYPE      : ',A,/,
     6                           16X,'ANTENNA  NRS       :',2I6,/)
                CALL EXITRC(2)
              END IF
              DO IELV=1,NPCV(1,IRCV)
                ANTPCV(IELV,IAZI,IFRQ,IRCV)=
     1                         ANTPCV(IELV,IAZI,IFRQ,IRCV)/1000.D0
              END DO
            END DO
          END DO
        END IF
      ENDDO
      GOTO 50
C
40    CONTINUE
      WRITE(LFNERR,9) ANTTYP(IRCV),RECTYP(IRCV)
9     FORMAT(/,' *** SR RDAPHC: ERROR IN THE PHASE CENTER ',
     1         'ECCENTR.FILE',
     2       /,'                END OF FILE WHILE READING MAP',
     3       /,'                FOR ANTENNA/RECEIVER:',
     4       /,'               ',A20,'/',A20)
      CALL EXITRC(2)
C
50    CONTINUE
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
