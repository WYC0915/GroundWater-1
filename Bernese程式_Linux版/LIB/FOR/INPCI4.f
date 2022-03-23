      MODULE f_inpci4
      CONTAINS
C*
      FUNCTION INPCI4(LUN,NA,NMENU,STRINP,IA,CHMENU,NRMENU,IRCODE)
CC
CC NAME        :  INPCI4
CC
CC PURPOSE     :  INPUT OF A LINE OF NUMERIC (INT*4) VALUES OR MENU
CC                PARAMETERS
CC
CC                THE SUBROUTINE IS INTENDED TO BE USED E.G. FOR THE
CC                FOLLOWING TYPE OF INPUT:
CC                - THE PROGRAM ASKS FOR 1 OR MORE NUMERIC VALUES
CC                - INSTEAD OF NUMERIC VALUES THE USER MAY INPUT
CC                  MENU PARAMETERS, E.G. =2.3.4
CC                                        =X
CC                                        =T      ETC.
CC                - IF THE PROGRAM ASKS E.G. FOR 3 NUMERIC VALUES
CC                  THE USER CAN INPUT 0, 1, 2, OR 3 VALUES.
CC                  MISSING VALUES WILL KEEP THE PREVIOUSLY GIVEN
CC                  CORRESPONDING VALUES IN THE VECTOR IA.
CC                  E.G.  GIVEN ON ENTRY OF THE FUNCTION:
CC                                          IA(1)=1,IA(2)=2,IA(3)=3,ETC.
CC                  USER INPUT: 9       --> IA(1)=9,IA(2)=2,IA(3)=3,ETC.
CC                             <RETURN> --> IA(1)=1,IA(2)=2,IA(3)=3,ETC.
CC                              8,,9    --> IA(1)=8,IA(2)=2,IA(3)=9,ETC.
CC
CC                - NUMERIC MENU PARAMETERS ARE STORED IN VECTOR NRMENU
CC                  IF NMENU MENU PARAMETERS ARE EXPECTED AND ONLY
CC                  N PARAMETERS ARE INPUT, NRMENU(N+1),NRMENU(N+2),...,
CC                  NRMENU(NMENU) ARE SET TO ZERO.
CC                - A CHARACTER MENU PARAMETER IS STORED IN CHARACTER
CC                  CHMENU (BLANK IF NO CHARACTER PARAMETER HAS BEEN
CC                  INPUT) IN ORDER TO ACCEPT A CHARACTER PARAMETER,
CC                  NMENU HAS TO BE PUT TO AT LEAST 1.
CC                - THE FUNCTION VALUE GIVES THE ACTUAL NUMBER OF INPUT
CC                  CHARACTERS (VALUES IN A, NUMERIC MENU PARAMETERS
CC                  IN NRMENU, CHARACTER MENU PARAMETER IN CH)
CC                - IF THE LOGICAL UNIT NUMBER IS SET TO A NEGATIVE
CC                  VALUE,THE INPUT LINE IS TAKEN FROM STRING 'STRINP'
CC                  UP TO CHARACTER NUMBER ABS(LUN)
CC
CC PARAMETERS :
CC         IN :  LUN    : LOGICAL UNIT NUMBER FOR INPUT           I*4
CC               NA     : NUMBER OF EXPECTED NUMERIC VALUES       I*4
CC               NMENU  : NUMBER OF EXPECTED MENU PARAMETERS      I*4
CC               STRINP : STRING WITH INPUT PARAMETERS (LUN<0)   CH*(*)
CC        OUT :  IA     : NUMERIC VALUES                          I*4(.)
CC                        (MAY BE DEFINED EXTERNALLY)
CC               CHMENU : CHARACTER MENU PARAMETER               CH*(*)
CC                        (BLANK IF NO CHARACTER INPUT)
CC               NRMENU : NUMERIC MENU PARAMETERS                 I*4(.)
CC                        (MAY BE DEFINED EXTERNALLY)
CC               IRCODE : RETURN CODE                             I*4
CC                        0: OK
CC                        1: END OF FILE ON CHANNEL LUN
CC                        2: INPUT ERROR
CC                        3: NUMERIC MENU PARAMETERS
CC                        4: '=X'
CC                        5: '=Q'
CC                        6: '=?', ? ANY CHAR. EXCEPT X,Q
CC                        7: INPCI4=0
CC                        8: INPCI4<NA
CC
CC               INPCI4 : ACTUAL NUMBER OF VALUES
CC                        (NUMERIC OR NUMERIC MENU)
CC
CC REMARKS    :  NOT MORE THAN 256 CHARACTERS INPUT
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/03/18 10:00
CC
CC CHANGES    :  25-JUN-98 : MR: MAXIMUM STRING LENGTH 80 --> 256
CC               03-SEP-98 : ??: READ(*,...) IF LUN=5 OR LUN=0
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-MAR-12 : RD: USE INPCI4 AS MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
C
C  GLOBAL PARAMETERS
      USE s_upperc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IEND  , IG    , INPCI4, IPOS  , IZ    , LSTRNG,
     1          N     , NGUT
C
      CHARACTER    CHMENU*1,STRINP*(*)
      INTEGER*4    LUN,NA,NMENU,NRMENU(*),IRCODE
      INTEGER*4    IA(*)
C
C  LOCAL PARAMETERS
      CHARACTER      Z
      CHARACTER*256  STRING
      INTEGER*4      VG,G
C
      LSTRNG=LEN(STRING)
      IRCODE=0
      INPCI4=0
C
      IEND=0
      IPOS=0
      N=0
      STRING=' '
      CHMENU=' '
C
C  READ STRING
      IF(LUN.LT.0) THEN
        STRING=STRINP(1:IABS(LUN))
      ELSE
        IF(LUN.EQ.5.OR.LUN.EQ.0) THEN
          READ(*,1,END=10) STRING
        ELSE
          READ(LUN,1,END=10) STRING
        END IF
1       FORMAT(A)
        GOTO 20
10      IRCODE=1
        GOTO 999
      END IF
C
C  BEGIN (LEADING BLANKS, LEADING CHARACTER)
C
20    IPOS=IPOS+1
C
C  END OF STRING
      IF(IPOS.GT.LSTRNG) GOTO 210
C
      Z=STRING(IPOS:IPOS)
C
C  SKIP LEADING BLANKS
      IF(Z.EQ.' ') GOTO 20
C
C  DETECT  '='-CHARACTER
      IF(NMENU.GT.0.AND.Z.EQ.'=') GOTO 410
C
      IPOS=IPOS-1
C
C  DECODE NUMBERS
C
      DO 40 I=1,NA
C
        VG=1
        G=0
        NGUT=0
C
50      IPOS=IPOS+1
C
        IF(IPOS.GT.LSTRNG) GOTO 210
        Z=STRING(IPOS:IPOS)
C
C  SKIP LEADING BLANKS
        IF(Z.EQ.' ') GOTO 50
C
C  MINUS-SIGN
        IF(Z.EQ.'-'.AND.VG.EQ.1) THEN
          VG=-1
          GOTO 50
        END IF
C
C  PLUS-SIGN
        IF(Z.EQ.'+') THEN
          VG=+1
          GOTO 50
        END IF
C
        IPOS=IPOS-1
C
60      IPOS=IPOS+1
C  BEYOND LAST CHARACTER IN STRING?
        IF(IPOS.GT.LSTRNG) THEN
          IEND=1
          GOTO 110
        END IF
C
        Z=STRING(IPOS:IPOS)
C
C  BLANK OR KOMMA: END OF CURRENT NUMBER
        IF(Z.EQ.' '.OR.Z.EQ.',') GOTO 110
C
C  MANTISSE
C
C  DECODE DIGIT
        READ(Z,11,ERR=920) IZ
11      FORMAT(I1)
C
C  NUMBER OF VALID CHARACTERS
        NGUT=NGUT+1
C
C  ADJUST MANTISSE
        G=10*G+IZ
C
        GOTO 60
C
C  END OF CURRENT NUMBER, ADD SIGN
110     N=N+1
        IF(NGUT.NE.0) THEN
          IA(N)=VG*G
        END IF
220     IF(IEND.EQ.1) GOTO 210
C
C  NEXT NUMBER
40    CONTINUE
C
C  ALL NUMBERS INPUT
210   INPCI4=N
      IF(INPCI4.EQ.0) THEN
        IRCODE=7
      ELSEIF(INPCI4.LT.NA) THEN
        IRCODE=8
      ELSE
        IRCODE=0
      END IF
      GOTO 999
C
C  CHECK INPUT WITH '=' (MENU INPUT)
C
410   N=0
      DO 420 I=1,NMENU
        IG=0
        NGUT=0
400     IPOS=IPOS+1
        IF(IPOS.GT.LSTRNG) GOTO 500
C
        Z=STRING(IPOS:IPOS)
C
C  DETECT FIRST ALPHABETIC CHARACTER
        CALL UPPERC(Z)
        IF(Z.GE.'A'.AND.Z.LE.'Z') THEN
          CHMENU=Z
          INPCI4=1
          IF(CHMENU.EQ.'X') THEN
            IRCODE=4
          ELSEIF(CHMENU.EQ.'Q') THEN
            IRCODE=5
          ELSE
            IRCODE=6
          END IF
          GOTO 999
        END IF
C
C  PERIOD (DELIMITER BETWEEN MENU NUMBERS)
C
        IF(Z.EQ.'.') GOTO 510
C
C  END
        IF(Z.EQ.' ') GOTO 500
C
C  MANTISSE
C
        READ(Z,11,ERR=920) IZ
        IG=10*IG+IZ
        NGUT=NGUT+1
C
        GOTO 400
C
500     IEND=1
C
C  END OF CURRENT NUMBER
C
510     IF(NGUT.NE.0) THEN
          N=N+1
          NRMENU(N)=IG
        ELSE
          IF(IEND.EQ.0) GOTO 920
        END IF
C
        IF(IEND.EQ.1) GOTO 600
420   CONTINUE
C
600   INPCI4=N
      DO 610 I=N+1,NMENU
        NRMENU(I)=0
610   CONTINUE
      IRCODE=3
      GOTO 999
C
920   IRCODE=2
      GOTO 999
C
999   RETURN
      END FUNCTION

      END MODULE
