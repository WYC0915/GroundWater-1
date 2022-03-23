      MODULE s_PRIARC
      CONTAINS

C*
      SUBROUTINE PRIARC(TITLES,NARC,NUMSAT,NAVNUM,TBOUND,SOURCE,
     1                  INDEX)
CC
CC NAME       :  PRIARC
CC
CC PURPOSE    :  PRINT ARC CHARACTERISTICS
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NARC   : NUMBER OF ARCS                      I*4
CC               NUMSAT(I),I=1,..,NARC: NUMBER OF SATELLITES  I*4
CC                        IN ARC I
CC               NAVNUM(J),J=1,..: SATELLITE NUMBERS FOR ALL  I*4
CC                        ARCS
CC               TBOUND(2,I),I=1,..,NARC: TIME BOUNDARIES OF  R*8
CC                        ARC I
CC               SOURCE(10,I),I=1,..,NARC: SOURCE OF ARC I    CH*1
CC      LOCAL :  INDEX(I),I=1,..,MAXSAT: INDCEX FOR ORDERING  I*4
CC                        OF SATELLITES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 18:32
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               21-DEC-93 : MR: ORDER OF SATELLITES
CC               28-DEC-93 : MR: CHANGE FORMAT DUE TO NUMBER OF SAT.
CC               12-JAN-94 : MR: WRONG INDEX ISAT0
CC               23-JUN-98 : MR: ALLOW FOR SEVERAL LINES OF SATELLITES
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt
      USE s_iordup
      USE s_timstr
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IREC  , ISAT0 , NARC  , NSAPRT,
     1          NSAREC, NSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*36  TSTRNG
      CHARACTER*1   SOURCE(10,*)
C
      REAL*8        TBOUND(2,*)
C
      INTEGER*4     NUMSAT(*),NAVNUM(*),INDEX(*)
C
C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                TITLES(2)(1:LENGT1(TITLES(2)))
1     FORMAT(//,A,/,A,/,' ',131('-'),//)
C
      WRITE(LFNPRT,"(
     1     ' 5. SATELLITE ORBITS'
     2  ,/,' -------------------'
     3  ,/,' '
     4  ,/,' ARC CHARACTERISTICS:'
     5  ,/,' -------------------'
     6  ,/,' '
     7  ,/,' ARC  START OF ARC       END OF ARC         SOURCE  '
     7    ,'    #SAT SATELLITES'
     8  ,/,1X,131('-')
     9  ,/,1X)")
C
C ARC CHARACTERISTICS
C -------------------
      ISAT0=0
      DO 20 IARC=1,NARC
C
C DATE FOR START AND END OF ARC
        CALL TIMSTR(2,TBOUND(1,IARC),TSTRNG)
C
        CALL IORDUP(NAVNUM(ISAT0+1),NUMSAT(IARC),INDEX)
C
        NSAREC=18
        NSAT=0
        DO IREC=1,100
          IF (NUMSAT(IARC).GT.NSAT) THEN
            IF (NUMSAT(IARC)-NSAT.GT.NSAREC) THEN
              NSAPRT=NSAREC
            ELSE
              NSAPRT=NUMSAT(IARC)-NSAT
            ENDIF
            IF (IREC.EQ.1) THEN
              WRITE(LFNPRT,3) IARC,TSTRNG,
     1                        (SOURCE(I,IARC),I=1,10),NUMSAT(IARC),
     2                        (NAVNUM(ISAT0+INDEX(I)),I=1,NSAPRT)
3             FORMAT(I4,2X,A36,2X,10A1,I5,1X,18I4)
            ELSE
              WRITE(LFNPRT,4) (NAVNUM(ISAT0+INDEX(I)),
     1                        I=NSAT+1,NSAT+NSAPRT)
4             FORMAT(60X,18I4)
            ENDIF
            NSAT=NSAT+NSAREC
            IF (NSAT.GT.NUMSAT(IARC)) GOTO 15
          ENDIF
        ENDDO
C
15      CONTINUE
        ISAT0=ISAT0+NUMSAT(IARC)
20    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
