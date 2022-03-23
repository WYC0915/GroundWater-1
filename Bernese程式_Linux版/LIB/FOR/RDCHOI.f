      MODULE s_RDCHOI
      CONTAINS

C*
      SUBROUTINE RDCHOI(LFN,IFRMAT,TMJD,POS,VEL,IRCODE)
CC
CC NAME       :  RDCHOI
CC
CC PURPOSE    :  READ INFROMATION FOR ONE OBSERVATION EPOCH FROM A
CC               CHAMP ORBIT FILE FORMAT CHORB
CC               (ASSUMING THAT THE FILE WAS OPENED)
CC
CC PARAMETERS :
CC         IN :  LFN    : LOGICAL FILE NUMBER FOR INPUT FILE   I*4
CC               IFRMAT : FORMAT TYPE                          I*4
CC                        =0 : FIRST CHORB FORMAT
CC        OUT :  TMJD : TIME OF EPOCH                          T_EPOCH
CC               POS(K),K=1,2,3: SATELLITE POSITIONS (M)       R*8
CC               VEL(K),K=1,2,3: SATELLITE VELOCITY (M/S)      R*8
CC               IRCODE : RETURN CODE
CC                        0 - ALL OK
CC                        1 - EOF
CC
CC REMARKS    :
CC
CC AUTHOR     :  D.SVEHLA
CC
CC VERSION    :  5.0
CC
CC CREATED    :  23-JAN-01
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
CC
C*
      USE m_epoch, ONLY: t_epoch
C
C DECLARATIONS
C ------------
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFRMAT, IRCODE, LFN
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER*4 IDAY,JDAY
      REAL*8    FMJD,TEPOCH,POS(3),VEL(3)
      REAL*8    ROLL,PITCH,YAW,DENS

      CHARACTER*1  MANEUV,LANWAT,ACSDES,ECLIPS
C
      TYPE(t_epoch) :: tmjd
C
C READ CHORB RECORDS
C HEADER HAS BEEN READ - NEXT RECORD MUST BE ORBIT RECORD
C -------------------------------------------------------
      IRCODE=0
      READ (LFN,100,END=400) IDAY,JDAY,TEPOCH,POS(1),POS(2),POS(3),
     1  VEL(1),VEL(2),VEL(3),ROLL,PITCH,YAW,DENS,MANEUV,LANWAT,
     2  ACSDES,ECLIPS
100   FORMAT(I5,I1,F11.0,6F12.0,3F7.0,F5.0,4A1)

c     XDAY=XDAY*1.D-1                                ![SINCE J2000.0]
c     TEPOCH=TEPOCH*1.D-6                            ![s]
c     TEPOCH=(TEPOCH-(51.184))/86400.D0+XDAY+51544.5 ![MJD]

      TMJD%DAY =IDAY+51544                           ! since J2000.0 -> MJD
      FMJD     =JDAY/10D0+0.5D0                      ! fractional days
      TMJD%FRAC=FMJD+(TEPOCH/1D6-51.184D0)/86400D0   ! plus seconds
      DO 110 I=1,3
        POS(I)=POS(I)*1.D-3                          ![m]
        VEL(I)=VEL(I)*1.D-7                          ![m/s]
110   CONTINUE

C     ROLL=ROLL*1.D-3                                ![deg]
C     PITCH=PITCH*1.D-3                              ![deg]
C     YAW=YAW*1.D-3                                  ![deg]
C     DENS=DENS*1.D-10                               ![g/m**3]

      GOTO 999

400   IRCODE=1
      GOTO 999

C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE

      END MODULE
