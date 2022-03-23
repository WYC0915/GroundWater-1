      MODULE s_PRISPV
      CONTAINS

C*
      SUBROUTINE PRISPV(NANSPV,NSASPV,SATSPV,GNRSPV,NPTSPV,SIGSPV,
     1  NADMAX)
CC
CC NAME       :  PRISPV
CC
CC PURPOSE    :  PRINT SATELLITE ANTENNA PHASE CENTER VARIATION
CC               PARAMETERS
CC
CC PARAMETERS :
CC         IN :  NANSPV : NUMBER OF SATELLITE ANTENNA PHASE   I*4
CC                        CENTER GROUPS TO BE ESTIMATED
CC               NSASPV(I),I=1,..,NANSPV: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO ANTENNA
CC                        PHASE CENTER GROUP I
CC               SATSPV(J,I),J=1,..,NSASPV(I),I=1,..,NANSPV:  I*4
CC                        SATELLITE NUMBERS OF EACH ANTENNA
CC                        PHASE CENTER GROUP
CC               GNRSPV(I),I=1,..,NANSPV: USER-DEFINED NUMBER I*4
CC                        FOR SATELLITE ANTENNA PHASE CENTER
CC                        GROUP I
CC               NPTSPV(J,I),J=1,2, I=1,..,NANSPV: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               SIGSPV(I),I=1,..,NANSPV: A PRIORI SIGMAS IN  R*8
CC                        METERS
CC               NADMAX:  MAXIMUM NADIR ANGLE ALLOWED FOR     R*8
CC                        SAT. ANT. PATTERN ESTIMATION
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER/R.SCHMID
CC
CC VERSION    :  4.0
CC
CC CREATED    :  07-JAN-96/13-NOV-01
CC               DERIVED FROM SR PRIPHC
CC
CC CHANGES    :  07-MAR-03 DS: MXNSAT DECLARED
CC               24-APR-03 RS: OUTPUT LAYOUT CORRECTION
CC               11-NOV-03 RS: GRPSPV -> GNRSPV, ADD NADMAX
CC               26-MAY-05 RD: USE DIMENSION MAXSGR FOR SATOFF AND SATSPV
CC               16-JUN-05 MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 MM: UNUSED VARIABLES REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: PI
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ISPV  , MAXELV, MXCSGR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXELV=19)
C
      CHARACTER*6   MXNSGR
C
      REAL*8        SIGSPV(*),NADMAX,NMXDEG
C
      INTEGER*4     NANSPV,NSASPV(*),NPTSPV(2,*)
      INTEGER*4     SATSPV(MXCSGR,*),GNRSPV(*)
C
      COMMON/MCMSGR/MXCSGR,MXNSGR
C
C PRINT SATELLITE ANTENNA PHASE CENTER VARIATION PARAMETERS
C ---------------------------------------------------------
      IF (NANSPV.GT.0) THEN
        NMXDEG = NADMAX/pi*180.D0
C
C GROUPS
        WRITE(LFNPRT,'(/,2(A,/),/,2(A,/))')
     1       ' SATELLITE ANTENNA PHASE CENTER VARIATION PARAMETERS:',
     2       ' --------------------------------------------------- ',
     3       ' REQ.  GROUP   #SAT   SATELLITE NUMBERS',
     4       ' ----------------------------------------------------' //
     4       '-----------------------------------------------------' //
     4       '--------------------------'
C
        DO 220 ISPV=1,NANSPV
          WRITE(LFNPRT,211) ISPV,GNRSPV(ISPV),NSASPV(ISPV),
     1                      (SATSPV(I,ISPV),I=1,NSASPV(ISPV))
211       FORMAT(I4,2X,I4,4X,I4,3X,30I4)
220     CONTINUE
C
C ESTIMATION PARAMETERS
        WRITE(LFNPRT,'(/,/,3(A,/))')
     1       '               #PNTS#',
     2       ' REQ.  GROUP   EL.  AZ.  NADMAX   SIGMA (M)',
     3       ' ----------------------------------------------------' //
     4       '-----------------------------------------------------' //
     5       '--------------------------'
C
        DO 240 ISPV=1,NANSPV
          WRITE(LFNPRT,231) ISPV,GNRSPV(ISPV),NPTSPV(1,ISPV),
     1                      (NPTSPV(2,ISPV)-1),NMXDEG,SIGSPV(ISPV)
231       FORMAT(I4,2X,I4,3X,2(I4,1X),F8.2,F11.5)
240     CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
