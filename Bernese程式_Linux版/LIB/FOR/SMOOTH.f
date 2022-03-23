      MODULE s_SMOOTH
      CONTAINS

C*
      SUBROUTINE SMOOTH(ITYPE ,IVINFO,WIDTH ,TIMINT,TIMTOL,NUMRAW,
     1                  TIMRAW,DATRAW,NUMSMT,TIMSMT,DATSMT,VARINF)
CC
CC NAME       :  SMOOTH
CC
CC PURPOSE    :  SMOOTH TIME SERIES - AND COMPUTE VARIANCE INFO
CC               LIKE AUTOCOVARIANCE FUNCTION OR AUTOCORRELATION
CC               FUNCTION
CC
CC PARAMETERS :
CC         IN :  ITYPE  : TYPE OF SMOOTHING                   I*4
CC                        =0: NULL - NO SMOOTHING
CC                        =1: GAUSSIAN KERNEL
CC                        =2: RECTANGULAR
CC                        =3: TRIANGULAR
CC                        =4: EXPONENTIAL
CC               IVINFO : VARIANCE INFO REQUESTED             I*4
CC                        =0: NONE
CC                        =1: VARIANCE ONLY
CC                        =2: AUTOCOVARIANCE FUNCTION
CC                        =3: AUTOCORRELATION FUNCTION
CC               WIDTH  : PARAMETER CHARACTERIZING WIDTH OF   R*8
CC                        FLOATING WINDOW IN SAME UNITS AS
CC                        USED FOR THE TIME ARGUMENTS (OR IN
CC                        UNITS OF "TIMINT" IN CASE OF
CC                        EQUIDISTANT TIME SERIES)
CC               TIMINT : TEMPORAL SPACING OF RAW DATA        R*8
CC                        =0: EQUIDISTANT - NO TIME ARGUMENTS
CC                            NEEDED
CC               TIMTOL : DEVIATION ALLOWED                   R*8
CC                        =0: NO TOLERANCE
CC               NUMRAW : NUMBER OF RAW DATA POINTS           I*4
CC               TIMRAW(I),I=1,..,NUMRAW: TIME ARGUMENTS      R*8(*)
CC               DATRAW(I),I=1,..,NUMRAW: RAW DATA            R*8(*)
CC                        =1D20: NOT AVAILABLE
CC               NUMSMT : NUMBER OF SMOOTHED DATA POINTS      I*4
CC                        =0: NUMSMT=NUMRAW - TIME ARGUMENTS
CC                            OF SMOOTHED DATA NOT NEEDED
CC               TIMSMT(I),I=1,..,NUMSMT: TIME ARGUMENTS      R*8(*)
CC        OUT :  DATSMT(I),I=1,..,NUMSMT: SMOOTHED DATA       R*8(*)
CC               VARINF(I),I=1,..,NUMRAW: VARIANCE INFO       R*8(*)
CC                        ACCORDING TO "IVINFO"; THE I-TH
CC                        ELEMENT OF "VARINF" CONTAINS THE
CC                        COVARIANCE OR CORRELATION WITH
CC                        RESPECT TO LAG (I-1)*TIMINT, I.E.,
CC                        "VARINF(1)" CORRESPONDS TO THE
CC                        VARIANCE OR IS 1
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  01-JAN-98
CC
CC CHANGES    :  13-MAY-98 : SS: IGNORE "VARINF" WHEN "IVINFO=0"
CC               13-MAY-98 : MR: REPLACE "DFLOAT" BY "DBLE"
CC               15-MAY-98 : SS: EXPLANATION ADDED
CC               06-JUL-98 : SS: EXPONENTIAL KERNEL IMPLEMENTED
CC               19-OCT-00 : SS: SPEED UP COMPUTATION IN CASE OF
CC                               EQUIDISTANT TIME SERIES
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_minlr8
      USE f_maxlr8
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IELE  , ILAG  , IRAW  , IRAW1 , IRAW2 , ISMT  , ITYPE ,
     1          IVINFO, IWIDTH, NELE  , NMIS  , NRAW1 , NRAW2 , NSMT  ,
     2          NUMRAW, NUMSMT, NVAL
C
      REAL*8    TIMBEG, TIMEND, TIMINT, TIMTOL, WIDTH , XDIF  , XDIF1 ,
     1          XDIF2 , XFAC  , XLAG  , XTIM  , XVAL  , XVAR,XWGT,dummy
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8        TIMRAW(*),DATRAW(*),TIMSMT(*),DATSMT(*)
      REAL*8        VARINF(*)
C
C
C SOME CHECKS
C -----------
      IF (NUMSMT.NE.0 .AND. IVINFO.GT.0) THEN
        WRITE(LFNERR,901)
901     FORMAT(/,' *** SR SMOOTH: "NUMSMT" MUST BE 0 WHEN ',
     1    'COMPUTING VARIANCE INFO',/)
        CALL EXITRC(2)
      ENDIF
C
      IF (NUMSMT.NE.0 .AND. TIMINT.EQ.0.D0) THEN
        WRITE(LFNERR,902)
902     FORMAT(/,' *** SR SMOOTH: "NUMSMT" MUST BE 0 WHEN ',
     1    'PROCESSING EQUIDISTANT DATA',/)
        CALL EXITRC(2)
      ENDIF
C
      IF (WIDTH.LE.0.D0 .AND. ITYPE.GT.0) THEN
        WRITE(LFNERR,903)
903     FORMAT(/,' *** SR SMOOTH: INVALID "WIDTH"',/)
        CALL EXITRC(2)
      ENDIF
C
      IF (IVINFO.GT.1 .AND. TIMINT.NE.0.D0) THEN
        dummy = MINLR8(NUMRAW,TIMRAW,TIMBEG)
        dummy = MAXLR8(NUMRAW,TIMRAW,TIMEND)
C
        NELE=IDNINT((TIMEND-TIMBEG)/TIMINT)+1
        IF (NELE.GT.NUMRAW) THEN
          WRITE(LFNERR,904)
904       FORMAT(/,' ### SR SMOOTH: ARRAY "VARINF" CONTAINING ',
     1      'COVARIANCE INFO TRUNCATED',/)
        ENDIF
      ENDIF
C
C SMOOTH TIME SERIES
C ------------------
      IF (ITYPE.GT.0) THEN
        IF (NUMSMT.EQ.0) THEN
          NSMT=NUMRAW
        ELSE
          NSMT=NUMSMT
        ENDIF
C
        DO ISMT=1,NSMT
          XVAL=0.D0
          XWGT=0.D0
          IF (TIMINT.EQ.0.D0) THEN
            IF (ITYPE.EQ.1) THEN
              IWIDTH=NINT(3.5D0*WIDTH)
            ELSE
              IWIDTH=NINT(WIDTH)
            ENDIF
            NRAW1=MAX0(ISMT-IWIDTH,1)
            NRAW2=MIN0(ISMT+IWIDTH,NUMRAW)
          ELSE
            NRAW1=1
            NRAW2=NUMRAW
          ENDIF
          DO 110 IRAW=NRAW1,NRAW2
            IF (DATRAW(IRAW).EQ.1.D20) GOTO 110
            IF (NUMSMT.EQ.0) THEN
              IF (TIMINT.NE.0.D0) THEN
                XLAG=TIMRAW(ISMT)-TIMRAW(IRAW)
              ELSE
                XLAG=DBLE(ISMT-IRAW)
              ENDIF
            ELSE
              XLAG=TIMSMT(ISMT)-TIMRAW(IRAW)
            ENDIF
C
            IF (ITYPE.EQ.1) THEN
C
C GAUSSIAN KERNEL
              XFAC=DEXP(-(XLAG/WIDTH)**2/2.D0)
            ELSEIF (ITYPE.EQ.2) THEN
C
C RECTANGULAR
              IF (DABS(XLAG).LE.WIDTH) THEN
                XFAC=1.D0
              ELSE
                XFAC=0.D0
              ENDIF
            ELSEIF (ITYPE.EQ.3) THEN
C
C TRIANGULAR
              XFAC=1.D0-DABS(XLAG)/WIDTH
              IF (XFAC.LE.0.D0) XFAC=0.D0
            ELSEIF (ITYPE.EQ.4) THEN
C
C EXPONENTIAL
              XFAC=DEXP(-DABS(XLAG)/WIDTH)
            ELSE
C
C INVALID SMOOTHING TYPE
              WRITE(LFNERR,911) ITYPE
911           FORMAT(/,' *** SR SMOOTH: INVALID SMOOTHING TYPE',/,
     1          16X,'SMOOTHING TYPE: ',I2,/)
              CALL EXITRC(2)
            ENDIF
C
            XVAL=XVAL+DATRAW(IRAW)*XFAC
            XWGT=XWGT+XFAC
110       CONTINUE
C
          IF (XWGT.NE.0.D0) THEN
            DATSMT(ISMT)=XVAL/XWGT
          ELSE
            IF (TIMINT.EQ.0.D0) THEN
              XTIM=DBLE(ISMT-1)
            ELSE
              XTIM=TIMSMT(ISMT)
            ENDIF
            WRITE(LFNERR,912) XTIM
912         FORMAT(/,' *** SR SMOOTH: SMOOTHED DATA POINT NOT DEFINED',/,
     1        16X,'TIME ARGUMENT: ',F9.2,/)
            CALL EXITRC(2)
          ENDIF
        ENDDO
      ENDIF
C
C COMPUTE VARIANCE INFO
C ---------------------
C
C COUNT NUMBER OF MISSING DATA POINTS
      NMIS=0
      DO IRAW=1,NUMRAW
        IF (DATRAW(IRAW).EQ.1.D20) NMIS=NMIS+1
      ENDDO
C
      IF (IVINFO.GT.1) THEN
C
C AUTOCOVARIANCE FUNCTION
        DO IELE=1,NUMRAW
          VARINF(IELE)=0.D0
        ENDDO
        DO 210 IRAW1=1,NUMRAW
          IF (DATRAW(IRAW1).EQ.1.D20) GOTO 210
          IF (ITYPE.GT.0) THEN
            XDIF1=DATRAW(IRAW1)-DATSMT(IRAW1)
          ELSE
            XDIF1=DATRAW(IRAW1)
          ENDIF
          DO 220 IRAW2=IRAW1,NUMRAW
            IF (DATRAW(IRAW2).EQ.1.D20) GOTO 220
            IF (ITYPE.GT.0) THEN
              XDIF2=DATRAW(IRAW2)-DATSMT(IRAW2)
            ELSE
              XDIF2=DATRAW(IRAW2)
            ENDIF
C
            IF (TIMINT.NE.0.D0) THEN
              XLAG=TIMRAW(IRAW2)-TIMRAW(IRAW1)
              ILAG=IDNINT(XLAG/TIMINT)
C
              XDIF=DABS(XLAG-ILAG*TIMINT)
              IF (XDIF.GT.TIMTOL) THEN
                WRITE(LFNERR,921) XDIF,TIMTOL
921             FORMAT(/,' *** SR SMOOTH: TOO LARGE DEVIATION OF ',
     1            'TIME ARGUMENT',/,
     2            16X,'DEVIATION : ',F9.6,/,
     3            16X,'TOLERANCE : ',F9.6,/)
                CALL EXITRC(2)
              ENDIF
            ELSE
              ILAG=IRAW2-IRAW1
            ENDIF
C
            IELE=ILAG+1
            IF (IELE.LE.NUMRAW) VARINF(IELE)=VARINF(IELE)+XDIF1*XDIF2
220       CONTINUE
210     CONTINUE
C
        DO IELE=1,NUMRAW
C
C REMARK: USE NVAL=NUMRAW INSTEAD OF NVAL=NUMRAW-ILAG TO GUARANTEE THAT
C COVARIANCE MATRICES ARE POSITIVE DEFINITE
          ILAG=IELE-1
          NVAL=NUMRAW-NMIS
          VARINF(IELE)=VARINF(IELE)/NVAL
        ENDDO
C
C AUTOCORRELATION FUNCTION
        IF (IVINFO.EQ.3) THEN
          XVAR=VARINF(1)
          IF (XVAR.EQ.0.D0) THEN
            WRITE(LFNERR,922)
922         FORMAT(/,' *** SR SMOOTH: UNEXPECTED ERROR',/)
            CALL EXITRC(2)
          ENDIF
          DO IRAW=1,NUMRAW
            VARINF(IRAW)=VARINF(IRAW)/XVAR
          ENDDO
        ENDIF
      ELSEIF (IVINFO.EQ.1) THEN
C
C VARIANCE ONLY
        XVAR=0.D0
        DO 310 IRAW=1,NUMRAW
          IF (DATRAW(IRAW).EQ.1.D20) GOTO 310
          IF (ITYPE.GT.0) THEN
            XVAR=XVAR+(DATRAW(IRAW)-DATSMT(IRAW))**2
          ELSE
            XVAR=XVAR+DATRAW(IRAW)**2
          ENDIF
310     CONTINUE
        VARINF(1)=XVAR/(NUMRAW-NMIS)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
