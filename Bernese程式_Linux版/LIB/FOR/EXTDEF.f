      MODULE s_EXTDEF
      CONTAINS

C*
      SUBROUTINE EXTDEF(FILNAM,NARC,TMJD,NSAT,SVN,RMSSAT,NECL,SVNECL,
     1                  ECLTIM,NRARC,IRC)
CC
CC NAME       :  EXTDEF
CC
CC PURPOSE    :  READ DEFSTD OUTPUT FILES
CC
CC PARAMETER  :
CC        IN  : FILNAM  : NAME OF THE OUTPUT FILE (INCL EXT)  CH*(*)
CC            : NARC    : WHICH ARC TO EXTRACT                I*4
CC        OUT : TMJD    : START AND END TIME OF ARC           R*8(*)
CC              NSAT    : NUMBER OF SATELLITES                I*4
CC              SVN     : SATELLITE NUMBERS                   I*4(*)
CC              RMSSAT  : RMS OF SATELLITE (DEFSTD)           R*8(*)
CC              NECL    : NUMBER OF ECLIPSING SATELLITES      I*4
CC              SVNECL  : NUMBERS OF THE ECLIPSING SATELLITES I*4(*)
CC              ECLTIM  : TIME OF LONGEST ECLIPS (PER SAT SEC)I*4(*)
CC              NRARC   : NUMBER OF ARCS IN OUTPUT FILE       I*4
CC              IRC     : NOT ALL INFO FOUND                  I*4
CC
CC REMARKS    :  RESTRICTED TO ONLY ONE ARC
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.3
CC
CC CREATED    :  23-OCT-92
CC
CC CHANGES    :  04-JUN-96 : MR: REMOVE INCLUDE "LFNUM"
CC               08-JUL-96 : SF: DIRECT FORMT: X --> 1X
CC               07-NOV-00 : CU: REPLACE OPEN --> OPNFIL
CC               30-OCT-01 : DI: READ SVN/SVNECL WITH I3
CC               22-JAN-04 : HB: USE SR OPNERR
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               12-AUG-10 : SS: 'EXTAMB' REPLACED BY 'EXTDEF'
CC               10-AUG-10 : RD: USE TIMST2 INSTEAD OF TIMSTR
CC               21-SEP-10 : RD: ST2TIM CAN BE USED AS A MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      USE s_st2tim
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , I2    , I3    , IARC  , IMIN  , IOSTAT, IPOS  ,
     1          IRC   , ISEC  , ITER  , NARC  , NECL  , NITER , NRARC ,
     2          NSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATIONS
C ------------
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER   FILNAM*(*)
      INTEGER*4   SVN(*),SVNECL(*),ECLTIM(*)
      REAL*8      RMSSAT(*),TMJD(*)
C
C INTERNAL DECLARATIONS
C ---------------------
      CHARACTER*150 LINE
      CHARACTER*40  TSTRNG
      LOGICAL       GO_ON
C
C OPEN THE OUTPUT FILE
C --------------------
      CALL OPNFIL(LFNLOC,FILNAM,'OLD','FORMATTED',
     1              ' ',' ',IOSTAT)
c      OPEN(UNIT=LFNLOC,FILE=FILNAM,STATUS='OLD',
c     1       IOSTAT=IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'EXTDEF')
!!      IF (IOSTAT.NE.0)THEN
!!        WRITE(*,*)'ERROR OPENING OUTPUT FILE'
!!        GOTO 999
!!      END IF
C
C READ THE OUTPUT FILE
C --------------------
C
C FIND THE NUMBER OF ITERATIONS
C -----------------------------
      DO 65 I1=1,100000
        READ(LFNLOC,'(A)',END=900) LINE
        IF(LINE(2:21).EQ.'NUMBER OF ITERATIONS') THEN
          READ(LINE,'(52X,I3)')NITER
          GOTO 66
        ENDIF
65    CONTINUE
C
C GET NUMBER OF ARCS AND THE START AND END TIME OF THE SPECIFIED ARC (NARC)
C -------------------------------------------------------------------------
66    CONTINUE
      DO 70 I1=1,100000
        READ(LFNLOC,'(A)',END=900) LINE
        IF(LINE(2:16).EQ.'ARC  START TIME') THEN
          READ(LFNLOC,'(A)',END=900) LINE
          READ(LFNLOC,'(A)',END=900) LINE
          READ(LFNLOC,'(A)',END=900) LINE
          GO_ON=.TRUE.
          DO 67 WHILE(GO_ON)
            READ(LINE,'(1X,I3)')IARC
            IF (IARC.EQ.0) THEN
              GO_ON=.FALSE.
            ELSE
              NRARC=IARC
            ENDIF
            IF (IARC.EQ.NARC) THEN
              READ(LINE(7:46),'(A)') TSTRNG
              CALL ST2TIM(1,2,TSTRNG,TMJD)
            ENDIF
            READ(LFNLOC,'(A)',END=900) LINE
67        CONTINUE
          GOTO 75
        END IF
70    CONTINUE
75    CONTINUE
C
C TOTAL NUMBER OF SATELLITES
C
      DO 10 I1=1,100000
        READ(LFNLOC,'(A)',END=900) LINE
        IF(LINE(2:31).EQ.'TOTAL NUMBER OF SATELLITES   :') THEN
          READ(LINE(33:35),*) NSAT
          GOTO 15
        END IF
10    CONTINUE
15    CONTINUE
C
C FIND THE RIGHT ARC AND ITERATION
C --------------------------------
      DO 22 I1=1,100000
        READ(LFNLOC,'(A)',END=900) LINE
        IF (LINE(2:30).EQ.'RMS ERRORS AND MAX. RESIDUALS') THEN
          READ(LINE,'(44X,I3,30X,I3)')IARC,ITER
          IF ((IARC.EQ.NARC).AND.(ITER.EQ.NITER)) GOTO 23
        ENDIF
22    CONTINUE
C
C RMS OF SATELLITES
C -----------------
23    CONTINUE
      DO 20 I1=1,100000
        READ(LFNLOC,'(A)',END=900) LINE
        IF (LINE(2:12).EQ.'SAT    #POS') THEN
          READ(LFNLOC,'(A)',END=900) LINE
          DO 30 I2=1,NSAT
            READ(LFNLOC,'(A)')LINE
            READ(LINE,2,ERR=20) SVN(I2),IPOS,RMSSAT(I2)
2           FORMAT(1X,I3,4X,I4,3X,F6.2)
30        CONTINUE
        ELSE IF (LINE(2:11).EQ.'SAT   #POS') THEN
          READ(LFNLOC,'(A)',END=900) LINE
          READ(LFNLOC,'(A)',END=900) LINE
          DO 31 I2=1,NSAT
            READ(LFNLOC,'(A)')LINE
            READ(LINE,3,ERR=20) SVN(I2),IPOS,RMSSAT(I2)
3           FORMAT(1X,I3,3X,I4,3X,F6.2)
31        CONTINUE
        END IF
C
C NUMBER OF ECLIPSING SATELLITES
C
        IF (LINE(2:32).EQ.'NUMBER OF ECLIPSING SATELLITES:') THEN
          READ(LINE(34:35),*) NECL
          DO 40 I2=1,4
            READ(LFNLOC,'(A)',END=900) LINE
40        CONTINUE
C
C SVN OF ECLIPSING SATELLITES AND DURATION OF ECLIPS
C
          DO 50 I2=1,NECL
            ECLTIM(I2)=0
            DO 60 I3=1,1000
              READ(LFNLOC,'(A)',END=900) LINE
              IF (LINE(18:22).EQ.'     ') THEN
                GOTO 50
              ELSE
                READ(LINE(18:22),1)IMIN,ISEC
1               FORMAT(I2,1X,I2)
                IF (ECLTIM(I2).LT.IMIN*60+ISEC) THEN
                  ECLTIM(I2)=IMIN*60+ISEC
                END IF
              END IF
              IF (LINE(2:4).NE.'  ') THEN
                READ(LINE(2:4),*)SVNECL(I2)
              END IF
60          CONTINUE
50        CONTINUE
          GOTO 25
        END IF
20    CONTINUE
25    CONTINUE
      GOTO 999
C
900   IRC=1
C
999   CLOSE(LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
