      MODULE s_LINSAV
      CONTAINS

C*
      SUBROUTINE LINSAV(TITNEW,MODNAM,MAXTYP,OBSTYP,NPAR,PARNAM,
     1                  PARTIM,RMS,XXXAPR,XXX,ANOR)
CC
CC NAME       :  LINSAV
CC
CC PURPOSE    :  SAVE PIECE-WISE LINEAR PARAMETERS IN RESIDUAL FILE
CC               FORMAT
CC
CC PARAMETERS :
CC        IN  :  TITNEW : GENERAL TITLE OF PROGRAM RUN          CH*80
CC               MODNAM : MODEL NAME                            CH*16
CC               MAXTYP : MAXIMUM NUMBER OF OBSERVATION TYPES    I*4
CC               OBSTYP(I),I=1,..,MAXTYP: OBSERVATION TYPES     CH*2
CC               NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               PARNAM(I),I=1,..,NPAR: PARAMETER NAMES         CH*20
CC               PARTIM(2,I),I=1,..,NPAR: PARAMETER WINDOWS      R*8
CC                          FROM,TO IN MJD
CC               RMS    : SUM OF O-C**2                          R*8
CC               XXXAPR(I),I=1,..,NPAR: A PRIORI VALUES OF PARA. R*8
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR             R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: NORMAL EQUATION R*8
CC                        MATRIX
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  01-APR-98
CC
CC CHANGES    :  16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      USE s_opnfil
      USE s_opnerr
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IOSTAT, IPAR  , IRCSAV, ITYP  , MAXTYP, NPAR
C
      REAL*8    ERPEPO, ERPMOD, ERPOBS, ERPSIG, OMC   , RESIDU, RMS
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*80 TITNEW
      CHARACTER*32 FILSAV
      CHARACTER*20 PARNAM(*)
      CHARACTER*16 MODNAM
      CHARACTER*2  OBSTYP(MAXTYP)
C
      REAL*8       PARTIM(2,*),XXXAPR(*),XXX(*),ANOR(*)
C
C
C GET OUTPUT FILE NAME
C --------------------
      CALL GTFLNA(0,'LINSAV ',FILSAV,IRCSAV)
C
      IF (IRCSAV.NE.0) RETURN
C
C OPEN OUTPUT FILE, WRITE TITLE LINES
C -----------------------------------
      CALL OPNFIL(LFNLOC,FILSAV,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSAV,'LINSAV')
C
      WRITE(LFNLOC,1001) TITNEW
1001  FORMAT(A,/,80('-'),/,
     1       ' OBS.  TYP  EPOCH(MJD)   OBSERV.(mas)  MODEL(mas)   ',
     2       ' O-C(mas)    RESIDUAL(mas)   SIGMA(mas)',/)
C
C WRITE PARAMETER VALUES
C ----------------------
      DO 100 IPAR=1,NPAR
        IF (PARNAM(IPAR)(3:9).EQ.' LINEAR') THEN
C
C OBSERVATION TYPE
          DO ITYP=1,MAXTYP
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP(ITYP)) GOTO 10
          ENDDO
          GOTO 100
C
10        CONTINUE
          ERPEPO=(PARTIM(1,IPAR)+PARTIM(2,IPAR))/2.D0
          ERPOBS=XXXAPR(IPAR)+XXX(IPAR)
          ERPMOD=XXXAPR(IPAR)
          OMC=XXX(IPAR)
          RESIDU=0.D0
          ERPSIG=RMS*DSQRT(ANOR(IKF(IPAR,IPAR)))
C
C WRITE INTO RESIDUAL FILE
          WRITE(LFNLOC,1002) IPAR,ITYP,ERPEPO,ERPOBS,ERPMOD,OMC,RESIDU,
     1                       ERPSIG
1002      FORMAT(I6,I4,F13.5,5F13.7)
        ENDIF
100   CONTINUE
C
C CLOSE OUTPUT FILE
C -----------------
      CLOSE(UNIT=LFNLOC)
C
C END
C ---
      RETURN
      END SUBROUTINE

      END MODULE
