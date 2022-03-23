      MODULE f_poldif
      CONTAINS

C*
      FUNCTION POLDIF(IPER,RINT,NPOINT,IPOINT,XXX)
CC
CC NAME       :  POLDIF
CC
CC PURPOSE    :  COMPUTE THE FACTOR FOR A POLYGON MODEL
CC PARAMETERS :
CC         IN :  IPER   ... PERIODICITY FLAG                  I*4
CC               RINT   ... INTERVAL LENGTH                   R*8
CC               NPOINT ... NUMBER OF POINTS                  I*4
CC               IPOINT ... CURRENT POINT NUMBER              I*4
CC               XXX    ... ARGUMENT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC VERSION    :  3.5
CC
CC CREATED    :  27-MAY-94
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IPER  , IPOINT, NPOINT
C
      REAL*8    DD    , DS    , POLDIF, RINT  , XXX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      IF (IPER.EQ.0) THEN
        IF (NPOINT.GT.1) THEN
          DD     = RINT/(NPOINT-1.D0)
          DS     = (IPOINT-1.D0)*DD
          POLDIF = DABS((XXX-DS)/DD)
        ELSE
          POLDIF = 0.D0
        END IF
      ELSE
        IF (NPOINT.GT.2) THEN
          DD     = RINT/(NPOINT-1.D0)
          DS     = (IPOINT-1.D0)*DD
          POLDIF = DMOD(XXX-DS,RINT)
          IF (POLDIF.GT. RINT/2.D0) POLDIF=POLDIF-RINT
          IF (POLDIF.LT.-RINT/2.D0) POLDIF=POLDIF+RINT
          POLDIF  = DABS( POLDIF/DD )
        ELSE
          POLDIF = 0.D0
        END IF
      END IF
C
      RETURN
      END FUNCTION

      END MODULE
