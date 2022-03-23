      MODULE s_PRITRP
      CONTAINS

C*
      SUBROUTINE PRITRP(IPART ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1                  STNAME,XSTELL,IEXTRA,ITROPO,ITRGRD,ZENMAX,
     2                  IPAR  ,TRPLMS)
CC
CC NAME       :  PRITRP
CC
CC PURPOSE    :  PRINT SITE-SPECIFIC TROPOSPHERE PARAMETERS (AND
CC               GRADIENT INFORMATION).
CC
CC PARAMETERS :
CC         IN :  IPART  : RESULTS PART 1(=1) OR 2 (=2)        I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8(*)
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: INVERSE OF   R*8(*)
CC                        NORMAL EQUATION MATRIX
CC               RMS    : RMS ERROR OF UNIT WEIGHT            R*8
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16(*)
CC               XSTELL(K,I),K=1,2,3;I=1,..,NSTAT: ELLIPS.    R*8
CC                        STATION COORDINATES
CC               IEXTRA : EXTRAPOLATED METEO USED             I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: ESTIMATED VALUES USED
CC               ITROPO : TROPOSPHERIC MODEL                  I*4
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               ZENMAX : MAXIMUM ZENITH DISTANCE IN RAD      R*8
CC               IPAR   : PARAMETER INDEX                     I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC CREATED    :  14-APR-97
CC
CC CHANGES    :  15-APR-97 : SS: CHECK WHETHER ALL COMPONENTS AVAILABLE
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               28-AUG-00 : MR: PARAMETERS IEXTRA,ITROPO,XSTELL FOR
CC                               SUBROUTINE TRPVEC
CC               20-MAY-00 : MM: SOME CHANGES DUE TO PW LINEAR TROPO
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               24-AUG-06 : AG: TRPLMS FOR TRPVEC ADDED
CC               01-SEP-11 : LP: MODIFIED CALL TO TRPVEC
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: PI
      USE f_ikf
      USE s_trpvec
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , I2    , ICOM  , IEXTRA, IPAR  , IPART ,
     1          IREQ  , IRQ2  , IRQOLD, ISTA  , ITROPO, IVAL1 , IVAL2 ,
     2          MXCLCQ, NCOM  , NPAR
C
      REAL*8    RMS   , RMS1  , XELE  , XELERF, XELETL, XFAC  , ZENMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TEXT
      CHARACTER*16  STNAME(*)
      CHARACTER*6   MXNLCQ
C
      REAL*8 XXX(*),ANOR(*),XSTELL(3,*)
      REAL*8 GRDINF(12)
      REAL*8 TRPLMS(2,*)
C
      INTEGER*4 LOCQ(MXCLCQ,*)
      INTEGER*4 IFIRST(2),ITRGRD(*),DUMMY(4)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
      DATA IFIRST/1,1/
C
      IF (IFIRST(IPART).EQ.1) THEN
C
C PRINT ELEVATION CUT-OFF ANGLE AND ASSOCIATED GRADIENT FACTOR
C ------------------------------------------------------------
        XELE=PI/2.D0-ZENMAX
        XELETL=PI/180.D0*3.D0
C
        IF (ITRGRD(1).GT.0 .AND. XELE.GE.XELETL) THEN
          IF (ITRGRD(1).EQ.1.OR.ITRGRD(1).EQ.3) THEN
            XELERF=51.83D0
            XFAC=DTAN(ZENMAX)/DCOS(ZENMAX)
          ELSE
            XELERF=45.D0
            XFAC=DTAN(ZENMAX)
          ENDIF
C
          WRITE(LFNPRT,110) XELERF,180.D0/PI*XELE,XFAC
110       FORMAT(' REFERENCE ELEVATION ANGLE OF GRADIENT TERMS : ',
     1      F6.1,' DEGREES',/,
     2           ' MINIMUM ELEVATION ANGLE                     : ',
     3      F6.1,' DEGREES',/,
     4           ' MAPPING FACTOR AT MINIMUM ELEVATION ANGLE   : ',
     5      F6.1,/)
        ENDIF
C
C PRINT HEADER LINE FOR PARAMETERS
C --------------------------------
        WRITE(LFNPRT,210)
210     FORMAT('                               CORRECTIONS (',
     1    'M)             RMS ERRORS (M)             ZE',
     2    'NITH VECTOR (")         ERROR ELLIPSE (M)   ',/,
     3    ' REQU. STATION NAME        NORTH    EAST    ',
     4    'ZENITH     NORTH    EAST    ZENITH     ANGLE',
     5    '   RMS   RATIO  AZI   MAX RMS  MIN RMS  AZI ',/,
     6    1X,131('-'),/)
C
        IRQOLD=0
C
        IFIRST(IPART)=0
      END IF
C
C PRINT PARAMETERS
C ----------------
      IREQ=LOCQ(2,IPAR)
      ISTA=LOCQ(3,IPAR)
      ICOM=LOCQ(4,IPAR)
      NCOM=LOCQ(5,IPAR)
      IRQ2=LOCQ(7,IPAR)
C
      TEXT=' '
C
      IF (ITRGRD(1).GT.0) THEN
        IF (ICOM.NE.3) RETURN
        CALL TRPVEC(IPAR  ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,ITRGRD,
     1              IEXTRA,ITROPO,XSTELL(1,ISTA),TRPLMS(1,IREQ),GRDINF,
     2              DUMMY)
C
        IVAL1=IDNINT(GRDINF(4))
        IVAL2=IDNINT(GRDINF(8))
C
        IF (ITRGRD(1).EQ.1) THEN
          WRITE(TEXT(80:132),310) (GRDINF(I1),I1=1,3),IVAL1,
     1      (GRDINF(I2),I2=6,7),IVAL2
310       FORMAT(2X,3F7.1,I5,1X,2F9.5,I5)
        ELSE
          WRITE(TEXT(80:132),320) IVAL1,
     1      (GRDINF(I2),I2=6,7),IVAL2
320       FORMAT(23X,I5,1X,2F9.5,I5)
        ENDIF
C
        WRITE(TEXT(25:33),'(F9.5)') GRDINF(9)
        WRITE(TEXT(34:42),'(F9.5)') GRDINF(11)
        WRITE(TEXT(53:61),'(F9.5)') GRDINF(10)
        WRITE(TEXT(62:70),'(F9.5)') GRDINF(12)
      END IF
C
      RMS1=RMS*DSQRT(ANOR(IKF(IPAR,IPAR)))
      WRITE(TEXT(1:24),'(I5,2X,A16)') IREQ,STNAME(ISTA)
      WRITE(TEXT(43:51),'(F9.5)') XXX(IPAR)
      WRITE(TEXT(71:79),'(F9.5)') RMS1
C
      WRITE(LFNPRT,'(A)') TEXT(1:LENGT1(TEXT))
C
      RETURN
      END SUBROUTINE

      END MODULE
