      MODULE s_JESTAT
      CONTAINS
C*
      SUBROUTINE JESTAT(JED,LIST,PV,NUT,*)
CC
CC NAME       :  JESTAT
CC
CC PURPOSE    :  THIS SUBROUTINE READS AND INTERPOLATES THE JPL
CC               PLANETARY EPHEMERIS FILE.
CC
CC PARAMETERS :
CC         IN :  JED(I),I=1,2: DP 2-WORD JULIAN EPHEMERIS EPOCH   R*8
CC                        AT WHICH INTERPOLATION IS WANTED. ANY
CC                        COMBINATION OF JED(1)+JED(2) WHICH
CC                        FALLS WITHIN THE TIME SPAN ON THE FILE
CC                        IS A PERMISSIBLE EPOCH.
CC                        A. FOR EASE IN PROGRAMMING, THE USER
CC                           MAY PUT THE ENTIRE EPOCH IN JED(1)
CC                           AND SET JED(2)=0.
CC                        B. FOR MAXIMUM INTERPOLATION ACCURACY,
CC                           SET JED(1) = THE MOST RECENT MIDNIGHT
CC                           AT OR BEFORE INTERPOLATION EPOCH AND
CC                           SET JED(2) = FRACTIONAL PART OF A DAY
CC                           ELAPSED BETWEEN JED(1) AND EPOCH.
CC                        C. AS AN ALTERNATIVE, IT MAY PROVE
CC                           CONVENIENT TO SET JED(1) = SOME FIXED
CC                           EPOCH, SUCH AS START OF INTEGRATION,
CC                           AND JED(2) = ELAPSED INTERVAL BETWEEN
CC                           THEN AND EPOCH.
CC               LIST(I),I=1,12: ARRAY SPECIFYING WHAT INTERPOL-  I*4
CC                        ATION IS WANTED FOR EACH OF THE BODIES
CC                        ON THE FILE.
CC                        LIST(I)=0, NO INTERPOLATION FOR BODY I
CC                               =1, POSITION ONLY
CC                               =2, POSITION AND VELOCITY
CC                        THE DESIGNATION OF THE ASTRONOMICAL
CC                        BODIES BY I IS:
CC                           I = 1: MERCURY
CC                             = 2: VENUS
CC                             = 3: EARTH-MOON BARYCENTER
CC                             = 4: MARS
CC                             = 5: JUPITER
CC                             = 6: SATURN
CC                             = 7: URANUS
CC                             = 8: NEPTUNE
CC                             = 9: PLUTO
CC                             =10: GEOCENTRIC MOON
CC                             =11: NUTATIONS IN LONGITUDE AND
CC                                  OBLIQUITY
CC                             =12: LUNAR LIBRATIONS (IF ON FILE)
CC         OUT:  PV(K,I),K=1,6;I=1,11: ARRAY THAT WILL CONTAIN    R*8
CC                        REQUESTED INTERPOLATED QUANTITIES.
CC                        THE BODY SPECIFIED BY LIST(I) WILL
CC                        HAVE ITS STATE IN THE ARRAY STARTING
CC                        AT PV(1,I). (ON ANY GIVEN CALL, ONLY
CC                        THOSE WORDS IN 'PV' WHICH ARE
CC                        AFFECTED BY THE FIRST 10 'LIST'
CC                        ENTRIES (AND BY LIST(12) IF LIBRA-
CC                        TIONS ARE ON THE FILE) ARE SET.
CC                        THE REST OF THE 'PV' ARRAY IS
CC                        UNTOUCHED.)  THE ORDER OF COMPONENTS
CC                        STARTING IN PV(1,I) IS: X,Y,Z,
CC                        DX,DY,DZ.
CC                        ALL OUTPUT VECTORS ARE REFERENCED TO
CC                        THE EARTH MEAN EQUATOR AND EQUINOX
CC                        OF EPOCH. THE MOON STATE IS ALWAYS
CC                        GEOCENTRIC; THE OTHER NINE STATES
CC                        ARE EITHER HELIOCENTRIC OR SOLAR-
CC                        SYSTEM BARYCENTRIC, DEPENDING ON THE
CC                        SETTING OF COMMON FLAGS (SEE BELOW).
CC                        LUNAR LIBRATIONS, IF ON FILE, ARE
CC                        PUT INTO PV(K,11) IF LIST(12) IS 1
CC                        OR 2.
CC               NUT(K),K=1,4: ARRAY THAT WILL CONTAIN NUTATIONS  R*8
CC                        AND RATES, DEPENDING ON THE SETTING
CC                        OF LIST(11). THE ORDER OF QUANTITIES
CC                        IN NUT IS:
CC                          D PSI  (NUTATION IN LONGITUDE)
CC                          D EPSILON (NUTATION IN OBLIQUITY)
CC                          D PSI DOT
CC                          D EPSILON DOT
CC               *      : STATEMENT # FOR ERROR RETURN, IN CASE
CC                        OF EPOCH OUT OF RANGE OR I/O ERRORS.
CC
CC REMARKS    :  COMMON AREA STCOMM:
CC               KM     LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE
CC                      OUTPUT STATES. KM = .TRUE. , KM AND KM/SEC
CC                                        = .FALSE., AU AND AU/DAY
CC                      DEFAULT VALUE = .FALSE.  (KM DETERMINES
CC                      TIME UNIT FOR NUTATIONS AND LIBRATIONS.
CC                      ANGLE UNIT IS ALWAYS RADIANS.)
CC               BARY   LOGICAL FLAG DEFINING OUTPUT CENTER. ONLY
CC                      THE 9 PLANETS ARE AFFECTED.
CC                      BARY = .TRUE.  => CENTER IS SOLAR-SYSTEM
CC                                        BARYCENTER
CC                           = .FALSE. => CENTER IS SUN
CC                      DEFAULT VALUE = .FALSE.
CC
CC               PVSUN  DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC
CC                      POSITION AND VELOCITY OF THE SUN.
CC
CC AUTHOR     :  JPL
CC
CC VERSION    :  3.4
CC
CC CREATED    :  22-OCT-92
CC
CC CHANGES    :  28-SEP-95 : JJ: DECLARE FIRST AS L*4 INSTEAD OF L*1
CC CHANGES    :  28-SEP-95 : JJ: DECLARE KM AS L*4 INSTEAD OF L*1
CC CHANGES    :  28-SEP-95 : JJ: DECLARE BARY AS L*4 INSTEAD OF L*1
CC               17-FEB-03 : LM: USE M_COMJPL
CC               27-FEB-03 : HU: M_COMJPL RENAMED TO D_COMJPL
CC               14-MAY-03 : HU: PVSUN ARRAY DECLARATION CORRECTED
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               27-SEP-11 : SL: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  NONE
CC      1992
CC
C*
      USE m_bern,   ONLY: lfnEph
      USE d_comjpl, ONLY: ksize
      USE s_jeintp
      USE s_jeopen
      USE s_jesplt
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I , J , K , NR
C
      REAL*8    JED(2),PV(6,11),NUT(4)
      INTEGER*4 LIST(12)
C
      REAL*8    T(2),AUFAC,JD(4),S
C
      LOGICAL*4 FIRST
C
C     COMMON AREA FOR CHARACTER DATA IN RECORD 1
C
      CHARACTER*6 TTL(14,3)
      CHARACTER*6 CNAM(400)
      COMMON/CHRHDR/TTL,CNAM
C
C     COMMON AREA FOR CONSTANTS AND POINTERS IN RECORD 1
C
      REAL*8    SS(3),CVAL(400),AU,EMRAT
      INTEGER*4 NCON,L(3,12),DENUM,LPT(3)
      COMMON/EPHHDR/SS,CVAL,AU,EMRAT,NCON,L,DENUM,LPT
C
      LOGICAL*4 KM,BARY
      REAL*8    PVSUN(6)
      COMMON/STCOMM/PVSUN,KM,BARY
C
      INTEGER*4 NRL
C
C
      INTEGER*4 IB(KSIZE)
      REAL*8    BUF(KSIZE/2)
      EQUIVALENCE(IB,BUF)
C
C     DATA STATEMENTS
C
      DATA AUFAC/1.D0/
      DATA FIRST/.TRUE./
      DATA NRL/0/
C
C     ENTRY POINT -- 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL JEOPEN(.TRUE.)
        IF(KM) THEN
          T(2)=SS(3)*86400.D0
        ELSE
          T(2)=SS(3)
          AUFAC=1.D0/AU
        ENDIF
      ENDIF
C
C     MAIN ENTRY POINT -- CHECK EPOCH AND READ RIGHT RECORD
C
      S=JED(1)-.5D0
      CALL JESPLT(S,JD(1))
      CALL JESPLT(JED(2),JD(3))
      JD(1)=JD(1)+JD(3)+.5D0
      JD(2)=JD(2)+JD(4)
      CALL JESPLT(JD(2),JD(3))
      JD(1)=JD(1)+JD(3)
C
C     ERROR RETURN OF EPOCH OUT OF RANGE
C
      IF(JD(1).LT.SS(1) .OR. JD(1)+JD(4).GT.SS(2)) RETURN 1
C
C     CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL
C
      NR=IDINT((JD(1)-SS(1))/SS(3))+3
      IF(JD(1).EQ.SS(2)) NR=NR-1
      T(1)=((JD(1)-(DBLE(NR-3)*SS(3)+SS(1)))+JD(4))/SS(3)
C
C     READ CORRECT RECORD IF NOT IN CORE
C
      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(UNIT=LFNEPH,REC=NR,ERR=98)(IB(K),K=1,KSIZE)
      ENDIF
C
C     INTERPOLATE SSBARY SUN
C
      CALL JEINTP(BUF(L(1,11)),T,L(2,11),3,L(3,11),2,PVSUN)
      DO 6 I=1,6
        PVSUN(I)=PVSUN(I)*AUFAC
    6 CONTINUE
C
C     CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED
C
      DO 3 I=1,10
        IF(LIST(I).GT.0) THEN
          CALL JEINTP(BUF(L(1,I)),T,L(2,I),3,L(3,I),LIST(I),PV(1,I))
          DO 4 J=1,LIST(I)*3
            IF(I.LE.9 .AND. .NOT.BARY) THEN
              PV(J,I)=PV(J,I)*AUFAC-PVSUN(J)
            ELSE
              PV(J,I)=PV(J,I)*AUFAC
            ENDIF
    4     CONTINUE
        ENDIF
    3 CONTINUE
C
C     DO NUTATIONS IF REQUESTED (AND IF ON FILE)
C
      IF(LIST(11).GT.0 .AND. L(2,12).GT.0)
     * CALL JEINTP(BUF(L(1,12)),T,L(2,12),2,L(3,12),LIST(11),NUT)
C
C     GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
C
      IF(LPT(2).GT.0 .AND. LIST(12).GT.0)
     * CALL JEINTP(BUF(LPT(1)),T,LPT(2),3,LPT(3),LIST(12),PV(1,11))
C
C     THAT'S ALL
C
      RETURN
C
C     ERROR EXIT
C
   98 RETURN 1
C
      END SUBROUTINE

      END MODULE
