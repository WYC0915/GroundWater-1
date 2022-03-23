      MODULE s_BIMDER
      CONTAINS

C*
      SUBROUTINE BIMDER(BIMCOE,BIMARG,XTEC  ,INTD  ,XDER  )
CC
CC NAME       :  BIMDER
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVES WRT BROADCAST
CC               IONOSPHERE MODEL (BIM) COEFFICIENTS
CC
CC PARAMETERS :
CC         IN :  BIMCOE(I=1,2,J=1,..,4): BIM COEFFICIENTS     R*8(2,4)
CC                        I=1: ION ALPHAS
CC                        I=2: ION BETAS
CC               BIMARG(I=1,2,3): ARGUMENTS                   R*(*)
CC                        I=1: FRACTIONAL PART OF DAY (DAYS)
CC                        I=2: LATITUDE (RAD)
CC                        I=3: LONGITUDE (RAD)
CC        OUT :  XTEC(I=1,2): RESULTS                         R*8(*)
CC                        I=1: TEC (TECU)
CC                        I=2: PERIOD (SEC)
CC               INTD   : NIGHT-TIME DATA WRT BIM             I*4
CC                        =0/1: NO/YES
CC               XDER(I=1,2,J=1,..,4): PARTIAL DERIVATIVES    R*8(2,4)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.3
CC
CC CREATED    :  22-JUN-00
CC
CC CHANGES    :  23-JUN-00 : SS: "INTD" INTRODUCED
CC               08-AUG-00 : SS: ADJUST WIDTH AUTOMATICALLY
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2000     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_bimtec
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , INTD , INTDM, INTDP, J
C
      REAL*8    XFAC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8        BIMCOE(2,4),BIMARG(*),XTEC(*),XDER(2,4)
      REAL*8        COE(2,4),WID(2),XTECP(2),XTECM(2)
C
C
      DATA XFAC/1.D-6/
C
      DO I=1,2
        WID(I)=0.D0
        DO J=1,4
          COE(I,J)=BIMCOE(I,J)
          WID(I)=DMAX1(XFAC*DABS(BIMCOE(I,J)),WID(I))
        ENDDO
        IF (WID(I).EQ.0.D0) THEN
          WRITE(LFNERR,901)
901       FORMAT(/,' *** SR BIMDER: UNEXPECTED ERROR',/)
          CALL EXITRC(2)
        ENDIF
      ENDDO
C
      CALL BIMTEC(COE,BIMARG,XTEC,INTD)
C
      DO I=1,2
        DO J=1,4
          COE(I,J)=BIMCOE(I,J)+WID(I)
          CALL BIMTEC(COE,BIMARG,XTECP,INTDP)
          COE(I,J)=BIMCOE(I,J)-WID(I)
          CALL BIMTEC(COE,BIMARG,XTECM,INTDM)
          COE(I,J)=BIMCOE(I,J)
C
          XDER(I,J)=(XTECP(1)-XTECM(1))/(2.D0*WID(I))
C
          IF (INTDP.NE.INTDM) THEN
            WRITE(LFNERR,902)
902         FORMAT(/,' ### SR BIMDER: PARTIAL DERIVATIVE INACCURATE',/)
            XDER(I,J)=0.D0
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE
