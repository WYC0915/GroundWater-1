      MODULE s_STOECL
      CONTAINS

C*
      SUBROUTINE STOECL(FILORB,NSAT,NAVNUM,TB1,TB2,
     1                  NSASTC,NUMSTC,NSTDAY,SIGSTC,
     2                  NSPEC,NUMSPC,TIMSPC,SIGSPC)
CC
CC NAME       :  STOECL
CC
CC PURPOSE    :  DEFINE STOCHASTIC REQUESTS FOR ECLIPSING SATELLITES
CC               TYPE 97 ... PULSES 45 MINUTES AFTER SHADOW EXIT
CC               TYPE 98 ... NORMAL STOCHASTIC REQUEST
CC               TYPE 99 ... ALL SATELLITES
CC
CC PARAMETERS :
CC         IN :  FILORB : FILE NAME OF STD FILE               CH*32
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               NAVNUM : SATELLITE NUMBERS                   I*4(*)
CC               TB1    : START OF ARC (MJD)                  R*8
CC               TB2    : END OF ARC (MJD)                    R*8
CC     IN/OUT :  NSASTC : NUMBER OF SATS WITH STOCH ORBITS    I*4
CC               NUMSTC : CORRESPONDING SAT NUMBERS           I*4(*)
CC               NSTDAY : NUMBER OF STOCH EPOCHS/DAY FOR EACH I*4(*)
CC                        SATELLITE
CC               SIGSTC : A PRIORI SIGMAS FOR STOCH REQUESTS  R*8(*,*)
CC               NSPEC  : NUMBER OF SPECIAL STOCH. REQUESTS   I*4
CC               NUMSPC : CORRESPONDING SAT NUMBERS           I*4(*)
CC               TIMSPC : TIMES FOR THESE REQUESTS            R*8(*)
CC               SIGSPC : CORRESPONSING A PRIORI SIGMAS       R*8(*,*)
CC
CC REMARKS    :
CC
CC AUTHOR     :  L. MERVART
CC
CC VERSION    :  3.6  (JUNE 95)
CC
CC CREATED    :  2-JUN-95
CC
CC CHANGES    :   6-JUN-95 : LM: CALL GETORF
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_getorf
      USE s_shdbnd
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I97   , I9798 , I98   , I99   , IANT  , ICRARC,
     1          IDER  , IECL  , IFLECL, IORSYS, IRCODE, IREM  , ISAT  ,
     2          ISPR  , ISTOP , J     , MXCSAT, MXCSTP, NDAY9 , NDAY99,
     3          NHELP , NSASTC, NSAT  , NSPEC
C
      REAL*8    RADK  , SIG991, SIG992, SIG993, SIG9_1, SIG9_2, SIG9_3,
     1          TB1   , TB2   , TOSC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON/MCMSTP/MXCSTP,MXNSTP
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      CHARACTER*32 FILORB
      CHARACTER*6  MXNSAT,MXNSTP
C
      INTEGER*4    NAVNUM(*)
      INTEGER*4    NUMSTC(*),NSTDAY(*),NUMSPC(*)
C
      REAL*8       SIGSTC(3,*),SIGSPC(3,*),TIMSPC(*)
      REAL*8       XXX(6),ELE(7),HT0(2),TSHAD(2)
C
C
C EXPAND THE REQUEST "99" WITH THE LOWEST PRIORITY
C ------------------------------------------------
      I99=0
      NHELP=NSASTC
      DO 15 I=1,NHELP
        IF (NUMSTC(I) .EQ. 99) THEN
C
          I99    = I
          NDAY99 = NSTDAY(I99)
          SIG991 = SIGSTC(1,I99)
          SIG992 = SIGSTC(2,I99)
          SIG993 = SIGSTC(3,I99)
C
          NSASTC=NSASTC-1
          DO 25 J=I99,NSASTC
            NUMSTC(J)=NUMSTC(J+1)
            NSTDAY(J)=NSTDAY(J+1)
            SIGSTC(1,J)=SIGSTC(1,J+1)
            SIGSTC(2,J)=SIGSTC(2,J+1)
            SIGSTC(3,J)=SIGSTC(3,J+1)
25        CONTINUE
C
        END IF
15    CONTINUE
C
      IF (I99 .NE. 0) THEN
        DO 35 ISAT=1,NSAT
          DO 45 I=1,NSASTC
            IF (NAVNUM(ISAT).EQ.NUMSTC(I)) GOTO 35
45        CONTINUE
          NSASTC=NSASTC+1
          IF (NSASTC.GT.MXCSAT) THEN
            WRITE(LFNERR,1002) NSASTC,MXNSAT
            CALL EXITRC(2)
          END IF
          NUMSTC(NSASTC)   = NAVNUM(ISAT)
          NSTDAY(NSASTC)   = NDAY99
          SIGSTC(1,NSASTC) = SIG991
          SIGSTC(2,NSASTC) = SIG992
          SIGSTC(3,NSASTC) = SIG993
35      CONTINUE
      END IF
C
C REQUSTS "97" AND "98" (ECLIPSING SATELLITES)
C --------------------------------------------
      I97=0
      I98=0
      I9798=0
C
C ARE THERE STOCHASTIC REQUESTS FOR ECLIPSING SATELLITES ?
C --------------------------------------------------------
      DO 10 I=1,NSASTC
        IF (NUMSTC(I).EQ.97 .OR. NUMSTC(I).EQ.98) THEN
          I9798  = I
          NDAY9  = NSTDAY(I)
          SIG9_1 = SIGSTC(1,I)
          SIG9_2 = SIGSTC(2,I)
          SIG9_3 = SIGSTC(3,I)
        ENDIF
        IF (NUMSTC(I).EQ.97) I97=I
        IF (NUMSTC(I).EQ.98) I98=I
10    CONTINUE
      IF (I97.EQ.0 .AND. I98.EQ.0) RETURN
C
C TEST, IF OPTIONS ARE CHOSEN IN A CORRECT WAY
C --------------------------------------------
      IF (I97.NE.0 .AND. I98.NE.0) THEN
        WRITE(LFNERR,1000)
1000    FORMAT(/,' *** SR STOECL: BAD OPTIONS FOR STOCHASTIC ORBITS',/,
     1                 16X,'IT IS NOT ALLOWED TO COMBINE ',/,
     2                 16X,'THE OPTIONS 97 AND 98',/)
        CALL EXITRC(2)
      ENDIF
C
C REMOVE THE "SATELLITE" 97 OR 98 FROM ARRAYS NUMSTC,NSTDAY,SIGSTC
C ----------------------------------------------------------------
      NSASTC = NSASTC - 1
      DO 20 I=I9798,NSASTC
        NUMSTC(I)=NUMSTC(I+1)
        NSTDAY(I)=NSTDAY(I+1)
        SIGSTC(1,I)=SIGSTC(1,I+1)
        SIGSTC(2,I)=SIGSTC(2,I+1)
        SIGSTC(3,I)=SIGSTC(3,I+1)
20    CONTINUE

      IANT=0
      IDER=1
      ISTOP=0
C
C LOOP OVER ALL SATELLITES
C ------------------------
      DO 100 ISAT=1,NSAT
        IFLECL = 0
C
C ECLIPSING SATELLITE ?
        CALL GETORF(FILORB,NAVNUM(ISAT),IANT,IDER,ISTOP,TB1,ICRARC,
     1              IORSYS,XXX,TOSC,ELE,IRCODE)
        IECL = 0
        HT0(1)=23200.D0
        HT0(2)=TB1
        RADK=1.25D0
C
C LOOP OVER ALL ORBIT PERIODS
C ---------------------------
60      CONTINUE
          TSHAD(1)=0.D0
          TSHAD(2)=0.D0
          CALL SHDBND(NAVNUM(ISAT),HT0,TOSC,ELE,IORSYS,
     1                TSHAD,RADK)
          IF (DABS(TSHAD(2)).GT.1.D-6) THEN
            IFLECL = IFLECL + 1
          ELSE
            IFLECL = 0
          END IF
C
          IF (IFLECL .EQ. 1) THEN
C
C CHECK THE PRIORITY OF THE REQUEST
C ---------------------------------
            ISPR=0
40          IF (ISPR .LT. NSASTC) THEN
              ISPR=ISPR+1
C
              IF (NUMSTC(ISPR) .EQ. NAVNUM(ISAT)) THEN
                IF (I9798 .LE. ISPR) THEN
C REQUEST 97 OR 98 HAS A HIGHER PRIORITY, REMOVE THE OTHER REQUEST
                  NSASTC=NSASTC-1
                  DO 50 IREM=ISPR,NSASTC
                    NUMSTC(IREM)=NUMSTC(IREM+1)
                    NSTDAY(IREM)=NSTDAY(IREM+1)
                    SIGSTC(1,IREM)=SIGSTC(1,IREM+1)
                    SIGSTC(2,IREM)=SIGSTC(2,IREM+1)
                    SIGSTC(3,IREM)=SIGSTC(3,IREM+1)
50                CONTINUE
                  ISPR=ISPR-1
                ELSE
C REQUEST 97 OR 98 HAS A LOWER PRIORITY, CONTINUE WITH THE NEXT SATELLITE
                  GOTO 100
                END IF
              END IF
C
              GOTO 40
            END IF
          END IF
C
          IF (IFLECL .NE. 0) THEN
C
C HANDLE SPECIAL STOCHASTIC REQUEST
C ---------------------------------
            IF (I97.NE.0) THEN
              NSPEC=NSPEC+1
              IF (NSPEC.GT.MXCSTP) THEN
                WRITE(LFNERR,1001) NSPEC,MXCSTP
1001            FORMAT(/,' *** SR STOECL: TOO MANY STOCH. ORBIT ',
     1                 'PARAMETERS (SPECIAL REQUESTS)',/,
     2                 16X,'NUMBER OF REQUESTS:',I3,/,
     3                 16X,'MAXIMUM NUMBER    :',I3,/)
                CALL EXITRC(2)
              END IF
              NUMSPC(NSPEC)=NAVNUM(ISAT)
C
C SET STOCHASTIC PULSES 45 MINUTES AFTER SHADOW EXIT
              TIMSPC(NSPEC)=TSHAD(2)+0.03125
              IF (TIMSPC(NSPEC).GE.TB2) TIMSPC(NSPEC)=TB2
C
C SET SIGMAS EQUAL TO NORMAL STOCHASTIC REQUESTS
              SIGSPC(1,NSPEC)=SIG9_1
              SIGSPC(2,NSPEC)=SIG9_2
              SIGSPC(3,NSPEC)=SIG9_3
            ENDIF
          END IF
C
          IF (IFLECL .EQ. 1) THEN
C
C HANDLE NORMAL STOCHASTIC REQUEST
C --------------------------------
            IF (I98.NE.0) THEN
              NSASTC=NSASTC+1
              IF (NSASTC.GT.MXCSAT) THEN
                WRITE(LFNERR,1002) NSASTC,MXNSAT
1002            FORMAT(/,' *** SR STOECL: TOO MANY STOCH. ORBIT ',
     1                 'PARAMETERS (NORMAL REQUESTS)',/,
     2                 16X,'NUMBER OF REQUESTS:',I3,/,
     3                 16X,'MAXIMUM NUMBER    :',I3,/)
                CALL EXITRC(2)
              END IF
              NUMSTC(NSASTC)=NAVNUM(ISAT)
              NSTDAY(NSASTC)=NDAY9
              SIGSTC(1,NSASTC)=SIG9_1
              SIGSTC(2,NSASTC)=SIG9_2
              SIGSTC(3,NSASTC)=SIG9_3
              GOTO 100
            ENDIF
C
          END IF
C
C END OF LOOP OVER ALL ORBIT PERIODS
C
        IF (HT0(2)+0.5D0 .LT. TB2) THEN
          HT0(2) = HT0(2)+0.5D0
          GOTO 60
        END IF
C
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
