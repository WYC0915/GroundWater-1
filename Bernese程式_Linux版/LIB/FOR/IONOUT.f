      MODULE s_IONOUT
      CONTAINS

C*
      SUBROUTINE IONOUT(MODTYP,CAMPGN,NFIL,MTYPS,TIMRES,INUMOB,STNAMS,
     1                  NFTOT,MOBS,NIONP,LOCQ,XX,RMS,RMSIP)
CC
CC NAME       :  IONOUT
CC
CC PURPOSE    :  OUTPUT OF RESULTS FOR THE PROGRAM IONEST
CC
CC PARAMETERS :
CC         IN :  MODTYP : TYPE OF IONOSPHERE MODEL
CC                        (1:SINGLE LAYER)                    I*4
CC               CAMPGN : NAME OF CURRENT CAMPAIGN           CH*16
CC               NFIL   : NUMBER OF FILES OF CURRENT SESSION  I*4
CC               MTYPS  : MEASUREMENT TYPE
CC                        MTYPS(I),I=1,NFIL                   I*4(*)
CC               TIMRES : REFERENCE EPOCH
CC                        TIMRES(I),I=1,NFIL                  I*4(*)
CC               INUMOB : NUMBER OF OBSERVATION OF ONE FILE
CC                        INUMOB(I),I=1,NFIL                  I*4(*)
CC               STNAMS : STATION NAMES
CC                        STNAMS(I,J),I=1,2,J=1,NFIL         CH*16(*)
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               MOBS   : TOTAL NUMBER OF OBSERVATIONS        I*4
CC               NIONP  : NUMBER OF IONOSPHERE PARAMETERS     I*4
CC               LOCQ   : PARAMETER DEFINITION
CC                        LOCQ(I,J),I=1,MAXLCQ,J=1,MAXPAR     I*4(*,*)
CC               XX     : PARAMETERS
CC                        XX(I),I=1,MAXPAR                    R*8(*)
CC               RMS    : RMS OF UNIT WEIGHT                  R*8
CC               RMSIP  : RMS OF IONOSPHERE PARAMETERS
CC                        RMSIP(I),I=1,MAXPAR                 R*8(*)
CC        OUT :  ---
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  1.0
CC
CC CREATED    :  89/08/04 09:45
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc removed, M_BERN added
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE s_jmt
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIL  , IFTOT , IMO   , IY    , JJ    , MOBS  ,
     1          MODTYP, MXCFIL, MXCLCQ, MXCPAR, MXCSAT, NFIL  , NFTOT ,
     2          NIONP
C
      REAL*8    DAY   , RMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C COMMONS
C -------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C DECLARATIONS
C ------------
      REAL*8        TIMRES(MXCFIL),RMSIP(MXCPAR),XX(MXCPAR)
C
      INTEGER*4     MTYPS(MXCFIL),INUMOB(MXCFIL)
      INTEGER*4     LOCQ(MXCLCQ,MXCPAR)
C
      CHARACTER*16  CAMPGN,STNAMS(2,MXCFIL)
      CHARACTER*6   MXNFIL,MXNPAR,MXNSAT,MXNLCQ
      CHARACTER*3   OBSTYP
C
C LOGICAL FILE NUMBERS
C --------------------
C
      IF (MODTYP .EQ. 1) THEN
C
C OUTPUT OF RESULTS FOR SINGLE LAYER MODEL
C ----------------------------------------
        WRITE(LFNPRT,1)
1       FORMAT(/' RESULTS OF SINGLE LAYER MODEL:',/,
     1          ' ------------------------------',/)
C
        WRITE(LFNPRT,2) CAMPGN
2       FORMAT(' CAMPAIGN : ',A16,/)
C
        WRITE(LFNPRT,3)
3       FORMAT( ' STATION              DATE     OBSTYP   NUM OF OBS',/,
     1          ' -------------------------------------------------')
        JJ = 0
        DO 10 IFIL = 1,NFIL
          IF (MTYPS(IFIL) .EQ. 1) THEN
            OBSTYP = 'PHA'
          ELSE
            OBSTYP = 'COD'
          ENDIF
          CALL JMT(TIMRES(IFIL),IY,IMO,DAY)
          IF (INUMOB(IFIL) .NE. 0) THEN
            WRITE(LFNPRT,4) STNAMS(1,IFIL),IY,IMO,IDINT(DAY),
     1                      OBSTYP,INUMOB(IFIL)
4           FORMAT(1X,A16,2X,I4,'-',I2.2,'-',I2.2,2X,A3,6X,I5)
          ELSE
            JJ = JJ + 1
          ENDIF
10      CONTINUE
        IFTOT = NFIL - JJ
C
        WRITE(LFNPRT,5) IFTOT,MOBS,NIONP,RMS
5       FORMAT(//' TOTAL NUMBER OF FILES           =',I5,/,
     1           ' TOTAL NUMBER OF OBSERVATIONS    =',I5,/,
     2           ' NUMBER OF MODEL PARAMETERS      =',I5,/,
     3           ' RMS                             =',F10.3,' M',/)
C
        WRITE(LFNPRT,6)
6       FORMAT(' LAT DEG      TIM DEG          COEFF        RMS',/,
     1         ' ------------------------------------------------')
        DO 20 I=1,NIONP
          WRITE(LFNPRT,7) LOCQ(3,I),LOCQ(4,I),XX(I),RMSIP(I)
7         FORMAT(I5,8X,I5,8X,F10.4,' +-',F10.4)
20      CONTINUE
        WRITE(LFNPRT,8)
8       FORMAT(' ------------------------------------------------'//)
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
