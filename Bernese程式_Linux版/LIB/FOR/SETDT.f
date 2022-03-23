      MODULE s_SETDT
      CONTAINS

C*
      SUBROUTINE SETDT(ITYP,N,A,B,H,DT)
CC
CC NAME       :  SETDT
CC
CC PURPOSE    :  SELECTION OF POINTS IN INTEGRATION INTERVAL,
CC               WHERE DIFFERENTIAL EQUATION SYSTEM IS EXACTLY
CC               SATISFIED
CC
CC PARAMETERS :
CC         IN :  ITYP   : TYPE OF PARTITION OF INTERVAL       I*4
CC                         =1: EQUIPARTITION
CC                         =2: TSCHEBYSCHEFF
CC               N      : NUMBER OF POINTS OF PARTITION       I*4
CC               A      : LEFT BOUNDARY
CC               B      : RIGHT BOUNDARY
CC        OUT :  H      : B-A                                 R*8
CC               DT(I),I=1,2,..,N: POINTS                     R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/30 07:10
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: PI
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ITYP  , N
C
      REAL*8    A     , B     , FAKTOR, H     , PHI   , T0
C
CCC         IMPLICIT REAL*8 (A-H,O-Z)
        REAL*8 DT(*)
C
C T0=CENTER OF INTERVAL, H=LENGTH OF INTERVAL
        T0=(A+B)/2
        H=B-A
C
C EQUIPARTITION OF INTERVAL
        IF(ITYP.EQ.1)THEN
          DO 10 I=1,N
            DT(I)=A+(I-1)*H/(N-1)
            IF(DABS(DT(I)).LT.1.D-6)DT(I)=0.D0
10        CONTINUE
        ELSE IF(ITYP.EQ.2)THEN
C
C PARTITION ACCORDING TO ROOTS OF TSCHEBYSHEFF POLYNOMIALS
          DO 30 I=1,N
            PHI=(I-.5D0)*PI/N
            DT(N+1-I)=H*DCOS(PHI)/2
            IF(DABS(DT(N+1-I)).LT.1.D-6)DT(N+1-I)=0.D0
30        CONTINUE
          FAKTOR=(H/DT(N))/2
          DO 40 I=1,N
            DT(I)=T0+FAKTOR*DT(I)
40        CONTINUE
        ELSE
999       WRITE(LFNERR,888) ITYP
888       FORMAT(/,' *** SR SETDT : ILLEGAL INTERVAL PARTITION TYPE',/,
     1                         16X,'PARTITION TYPE:',I3,/)
          CALL EXITRC(2)
        END IF
        RETURN
        END SUBROUTINE

      END MODULE
