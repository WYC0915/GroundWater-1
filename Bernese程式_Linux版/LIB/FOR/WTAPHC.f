      MODULE s_WTAPHC
      CONTAINS

C*
      SUBROUTINE WTAPHC(MAXRCV,MAXELV,MAXAZI,FILPHC,TITLE ,NRCV  ,
     1                  RECTYP,ANTTYP,IANTEN,NFRANT,ANTOFF,MODTYP,
     2                  SINEX ,NPCV  ,ANTPCV,MAXZEN,METHOD,atDATE,
     3                  FILINFO)
CC
CC NAME       :  WTAPHC
CC
CC PURPOSE    :  WRITE RECEIVER AND SATELLITE ANTENNA PHASE CENTER
CC               INFORMATION INCLUDING ELEVATION DEPENDENT CORRECTIONS
CC               AND ELEVATION/AZIMUTH COEFFICIENTS.
CC
CC PARAMETERS :
CC        IN  :  MAXRCV : MAXIMUM NUMBER OF RECEIVER/ANTENNA  I*4
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
CC               ANTPCV(K,J,2,I),K=1,..,MAXELV,J=1,..,MAXAZI,
CC                        I=1,..,NRCV: GRID VALUES (METERS)   R*8
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
CC CREATED    :  07-JAN-96
CC
CC CHANGES    :  04-SEP-01 : SS: APC-PARAMETER REPRESENTATION TYP 4
CC               18-FEB-03 : LM: USE BACKSLASH FROM M_BERN
CC               20-MAY-03 : HU: OPEN FILENAME LENGTH
CC               11-AUG-03 : RS: DON'T WRITE RECTYP, ADD MAXZEN
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               09-SEP-03 : HU: MERGED
CC               24-OCT-03 : RS: WRITE RECTYP, IF NOT BLANK
CC               24-NOV-03 : RS: SPHERIC. HARMONICS: WRITE MAXZEN=90
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-NOV-05 : AG: WRITING OF SINEX, METHOD, atDATE and
CC                               FILINFO ADDED
CC               15-MAR-06 : AG: WRITE FORMAT OF ANTTYP CORRECTED
CC               29-JUN-06 : AG: HEADER LINE FOR ELV/AZI GRID CORRECTED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAZI  , ICH1  , ICH2  , IDAZI , IDELV , IELV  , IFREQ1,
     1          IFREQ2, IFRMAT, IFRQ  , II    , IORD  , IOSTAT, IPRINT,
     2          IRCV  , K     , MAXAZI, MAXELV, MAXRCV, NAZI  ,
     3          NELV  , NRCV
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
      CHARACTER*150 STRING(2)
C
C
C OPEN ANTENNA PHASE CENTER FILE
C ------------------------------
      CALL OPNFIL(LFNLOC,FILPHC,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPHC,'WTAPHC')
C
C WRITE TITLE LINES
C -----------------
      WRITE(LFNLOC,3) TITLE,FILINFO
3     FORMAT(A80,/,80('-'),/,A80,/,
     1       'ANTENNA TYPE           ANTENNA S/N  FREQ  PHASE CENTER ',
     2       'OFFSETS (M)              ZENITH DEPENDENCE OF PHASE CEN',
     3       'TER (MM)',/,
     4       '                       FROM    TO    L*    NORTH    EAS',
     5       'T     UP    FMT   00 05 10 15 20 25 30 35 40 45 50 55 6',
     6       '0 65 70 75 80 85 90',/,
     7       '********************  ****** ******   *   **.**** **.**',
     8       '** **.****   *    ** ** ** ** ** ** ** ** ** ** ** ** *',
     9       '* ** ** ** ** ** **',/)
C
C LOOP OVER ALL RECEIVER/ANTENNA PAIRS RESP. OVER ALL ANTENNAS: OFFSETS
C ---------------------------------------------------------------------
      DO IRCV=1,NRCV
C
C FORMAT TYPE
        IF (MODTYP(IRCV).EQ.0) THEN
          IFRMAT=0
        ELSEIF (MODTYP(IRCV).EQ.-1) THEN
          IFRMAT=1
          IF (NPCV(1,IRCV).NE.19 .AND. NPCV(2,IRCV).NE.1) THEN
            WRITE(LFNERR,901) ANTTYP(IRCV),RECTYP(IRCV),
     1                        (IANTEN(II,IRCV),II=1,2)
901         FORMAT(/,' ### SR WTAPHC: FORMAT TYPE 1 (WRITE ELEV.-DEP.',
     1               ' CORRECTIONS ON THE ',
     2             /,16X,'SAME LINE AS THE OFFSETS) CAN ONLY BE USED',
     3               'IF',
     4             /,16X,' - THERE IS NO AZIMUTH DEPENDENCE AND',
     5             /,16X,' - THE ELEVATION INTERVAL IS 5 DEGREES',
     6             /,16X,'FORMAT TYPE 1 CHANGED TO FORMAT TYPE 2',
     6             /,16X,'ANTENNA TYPE  : ',A,
     7             /,16X,'RECEIVER TYPE : ',A,
     8             /,16X,'ANTENNA NRS   :',2I7,/)
            IFRMAT=2
            MODTYP(IRCV)=1
          ENDIF
        ELSEIF (MODTYP(IRCV).GE.1 .AND. MODTYP(IRCV).LE.4) THEN
          IFRMAT=2
        ELSE
          WRITE(LFNERR,902) MODTYP(IRCV),ANTTYP(IRCV),RECTYP(IRCV),
     1                      (IANTEN(II,IRCV),II=1,2)
902       FORMAT(/,' *** SR WTAPHC: INVALID ANTENNA PHASE CENTER MODEL',
     1             ' TYPE',
     2           /,16X,'MODEL TYPE    :',I7,
     3           /,16X,'ANTENNA TYPE  : ',A,
     4           /,16X,'RECEIVER TYPE : ',A,
     5           /,16X,'ANTENNA NRS   :',2I7,/)
          CALL EXITRC(2)
        ENDIF
C
C WRITE OFFSETS TO STRING
        IFREQ1=1
        IF (RECTYP(IRCV).EQ.' ') THEN
          WRITE(STRING(1),11) ANTTYP(IRCV),IANTEN(1,IRCV),
     1                        IANTEN(2,IRCV),IFREQ1,
     2                        (ANTOFF(K,1,IRCV),K=1,3),IFRMAT
        ELSE
          WRITE(STRING(1),11) RECTYP(IRCV),IANTEN(1,IRCV),
     1                        IANTEN(2,IRCV),IFREQ1,
     2                        (ANTOFF(K,1,IRCV),K=1,3),IFRMAT
        ENDIF
11      FORMAT(A20,2X,I6,1X,I6,3X,I1,2X,3(1X,F7.4),3X,I1)
        IF (NFRANT(IRCV).EQ.1) THEN
          WRITE(STRING(2),12)
12        FORMAT(20X)
        ELSEIF (NFRANT(IRCV).EQ.2) THEN
          IFREQ2=2
          IF (RECTYP(IRCV).EQ.' ') THEN
            WRITE(STRING(2),13) IFREQ2,(ANTOFF(K,2,IRCV),K=1,3)
13          FORMAT(20X,2X,6X,1X,6X,3X,I1,2X,3(1X,F7.4))
          ELSE
            WRITE(STRING(2),130) ANTTYP(IRCV),IFREQ2,
     1                           (ANTOFF(K,2,IRCV),K=1,3)
130         FORMAT(A20,2X,6X,1X,6X,3X,I1,2X,3(1X,F7.4))
          ENDIF
        ELSE
          WRITE(LFNERR,903) NFRANT(IRCV),ANTTYP(IRCV),RECTYP(IRCV),
     1                      (IANTEN(II,IRCV),II=1,2)
903       FORMAT(/,' *** SR WTAPHC: INVALID NUMBER OF FREQUENCIES IN',
     1             ' PHASE CENTER MODEL',
     2           /,16X,'NUMBER OF FREQUENCIES:',I7,
     3           /,16X,'ANTENNA TYPE         : ',A,
     4           /,16X,'RECEIVER TYPE        : ',A,
     5           /,16X,'ANTENNA NUMBERS      :',2I7,/)
          CALL EXITRC(2)
        ENDIF
C
C WRITE FORMAT 1 (ELEVATION-DEP. VARIATIONS) TO STRING
        IF (IFRMAT.EQ.1) THEN
          DO IFRQ=1,NFRANT(IRCV)
            WRITE(STRING(IFRQ)(73:150),'(19I3)')
     1        (IDNINT(ANTPCV(IELV,1,IFRQ,IRCV)*1000.D0),
     2         IELV=1,NPCV(1,IRCV))
          ENDDO
        ENDIF
C
C WRITE OFFSET STRINGS
        WRITE(LFNLOC,14) STRING(1)(1:LENGT1(STRING(1))),
     1                   STRING(2)(1:LENGT1(STRING(2)))
14      FORMAT(A,/,A,/)
C
C NEXT RECEIVER/ANTENNA PAIR RESP. NEXT ANTENNA
      ENDDO
C
C WRITE FORMAT EXPLANATIONS
C -------------------------
      WRITE(LFNLOC,15)
15    FORMAT(/,'FORMAT INDICATOR:',
     1       /,'  FMT=0 :  ONLY PHASE CENTER OFFSETS ARE USED',
     2       /,'  FMT=1 :  ZENITH-DEPENDENT CORRECTIONS GIVEN TO THE ',
     3         'RIGHT OF THE OFFSET',
     4       /,'           VALUES ARE USED',
     5       /,'  FMT=2 :  PHASE CENTER MAPS OR SPHERICAL HARMONICS A',
     6         'RE USED (ZENITH RESP.',
     7       /,'           NADIR/AZIMUTH-DEPENDENT)',/,
     8       /,'ANTENNA PHASE CENTER OFFSETS MEASURED FROM ANTENNA R',
     9         'EFERENCE POINT (ARP)',
     1       /,'TO THE MEAN L1/L2 PHASE CENTER.',/)
C
C WRITE ELEVATION/AZIMUTH DEPENDENT VARIATIONS (FORMAT 2)
C -------------------------------------------------------
      IPRINT=1
      DO IRCV=1,NRCV
        IF (MODTYP(IRCV).GT.0) THEN
C
C GENERAL TITLE
          IF (IPRINT.EQ.1) THEN
            IPRINT=0
            WRITE(LFNLOC,16)
16          FORMAT(//,'PHASE CENTER MAPS AND/OR COEFFICIENTS OF ',
     1                'SPHERICAL HARMONICS IN MILLIMETERS:',/,75('-'),
     2             //,'TYPE 1 :  ELEVATION (RESP. NADIR)/AZIMUTH GRID',
     3              /,'TYPE 2 :  SPHERICAL HARMONICS COEFFICIENTS ',
     4                '(UNNORMALIZED)',
     5              /,'TYPE 3 :  SPHERICAL HARMONICS COEFFICIENTS ',
     6                '(NORMALIZED)',
     7              /,'TYPE 4 :  SPHERICAL HARMONICS COEFFICIENTS, ',
     8                'UPPER HEMISPHERE ONLY (NORMALIZED)',
     9             //,'D(Z)   :  ZENITH (RESP. NADIR) TABULAR ',
     1                'INTERVAL (DEGREES)',
     2              /,'D(A)   :  AZIMUTH TABULAR INTERVAL (DEGREES)',
     3              /,'N(Z)   :  DEGREE OF SPHERICAL HARMONICS DEVE',
     4                'LOPMENT',
     5              /,'M(A)   :  ORDER  OF SPHERICAL HARMONICS DEVE',
     6                'LOPMENT',
     7              /,'M(Z)   :  MAXIMUM ZENITH (RESP. NADIR) ANGLE')
          ENDIF
C
C TITLE LINES
          IF (MODTYP(IRCV).EQ.1) THEN
            IF (NPCV(1,IRCV).GT.1) THEN
              IDELV=NINT(DBLE(MAXZEN(IRCV))/DBLE(NPCV(1,IRCV)-1))
            ELSE
              IDELV=MAXZEN(IRCV)
            ENDIF
            IF (NPCV(2,IRCV).GT.1) THEN
              IDAZI=IDNINT(360.D0/DBLE(NPCV(2,IRCV)-1))
            ELSE
              IDAZI=360
            ENDIF
            IF (RECTYP(IRCV).EQ.' ') THEN
              WRITE(LFNLOC,17) ANTTYP(IRCV),
     1                         (IANTEN(II,IRCV),II=1,2),MODTYP(IRCV),
     2                         IDELV,IDAZI,MAXZEN(IRCV),SINEX(IRCV),
     3                         METHOD(IRCV),atDATE(IRCV),BACKSLASH,
     4                         (IDELV*(II-1),II=1,NPCV(1,IRCV))
17          FORMAT(/,'ANTENNA TYPE         DUMMY                FROM',
     1               '   TO      TYP   D(Z) D(A) M(Z)  SINEX      ',
     2               'METHOD               DATE',
     3             /,'******************** ******************** ****',
     4               '** ******  ***    ***  ***  ***  ********** ',
     5               '******************** **********',
     6             /,A20,1X,20X,1X,I6,1X,I6,2X,I3,2X,3(2X,I3),2X,A10,
     7               1X,A20,1X,A10,/,
     8             /,'    A',A1,'Z',I6,100I7)
            ELSE
              WRITE(LFNLOC,170) RECTYP(IRCV),ANTTYP(IRCV),
     1                          (IANTEN(II,IRCV),II=1,2),MODTYP(IRCV),
     2                          IDELV,IDAZI,MAXZEN(IRCV),SINEX(IRCV),
     3                          METHOD(IRCV),atDATE(IRCV),BACKSLASH,
     4                          (IDELV*(II-1),II=1,NPCV(1,IRCV))
170         FORMAT(/,'ANTENNA TYPE         DUMMY                FROM',
     1               '   TO      TYP   D(Z) D(A) M(Z)  SINEX      ',
     2               'METHOD               DATE',
     3             /,'******************** ******************** ****',
     4               '** ******  ***    ***  ***  ***  ********** ',
     5               '******************** **********',
     6             /,A20,1X,A20,1X,I6,1X,I6,2X,I3,2X,3(2X,I3),2X,A10,
     7               1X,A20,1X,A10,/,
     8             /,'    A',A1,'Z',I6,100I7)
            ENDIF
          ELSE
            NELV=NPCV(1,IRCV)
            NAZI=(NPCV(2,IRCV)-1)/2
            WRITE(LFNLOC,18) ANTTYP(IRCV),
     1                       (IANTEN(II,IRCV),II=1,2),MODTYP(IRCV),
     2                       NELV,NAZI,MAXZEN(IRCV),SINEX(IRCV),
     3                       METHOD(IRCV),atDATE(IRCV),BACKSLASH,
     4                       (IDELV*(II-1),II=1,NPCV(1,IRCV))
18          FORMAT(/,'ANTENNA TYPE         DUMMY                FROM',
     1               '   TO      TYP   N(Z) M(A) M(Z)  SINEX      ',
     2               'METHOD               DATE',
     3             /,'******************** ******************** ****',
     4               '** ******  ***    ***  ***  ***  ********** ',
     5               '******************** **********',
     6             /,A20,1X,20X,1X,I6,1X,I6,2X,I3,2X,3(2X,I3),2X,A10,
     7               1X,A20,1X,A10,/,
     8             /,'    A',A1,'Z',I6,100I7)
          ENDIF
C
C GRID VALUES OR COEFFICIENTS
          DO IAZI=1,NPCV(2,IRCV)
            DO IFRQ=1,NFRANT(IRCV)
              IF (MODTYP(IRCV).EQ.1) THEN
                WRITE(LFNLOC,19) IFRQ,IDAZI*(IAZI-1),
     1                           (ANTPCV(IELV,IAZI,IFRQ,IRCV)*1000.D0,
     2                            IELV=1,NPCV(1,IRCV))
19              FORMAT('L',I1,I4,100F7.2)
              ELSE
                STRING(1)=' '
                IORD=IAZI-(NPCV(2,IRCV)+1)/2
                DO IELV=IABS(IORD),NPCV(1,IRCV)
                  ICH1=(IELV-1)*7+1
                  ICH2=ICH1+6
                  WRITE(STRING(1)(ICH1:ICH2),'(F7.2)')
     1                            ANTPCV(IELV,IAZI,IFRQ,IRCV)*1000.D0
                ENDDO
                WRITE(LFNLOC,20) IFRQ,IORD,
     1                           STRING(1)(1:LENGT1(STRING(1)))
20              FORMAT('L',I1,I4,A)
              ENDIF
            ENDDO
          ENDDO
C
        ENDIF
      ENDDO
C
C ADD BLANK LINES
      IF (IPRINT.EQ.0) WRITE(LFNLOC,'(//)')
C
C CLOSE ANTENNA PHASE CENTER OUTPUT FILE
C --------------------------------------
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
