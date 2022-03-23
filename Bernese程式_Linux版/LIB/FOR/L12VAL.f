      MODULE s_L12VAL
      CONTAINS

C*
      SUBROUTINE L12VAL(SVN,RHS,N1,N5,IWLSCR,IONOS,RESL3)
CC
CC NAME       :  L12VAL
CC
CC PURPOSE    :  GIVEN THE (POSSIBLY BIASED) TRIPLE DIFFERENCE
CC               RESIDUALS R1*, R2*, RETURN THE RESIDUAL FOR L3,
CC               AND THE ESTIMATED IONOSPHERE CHANGES FROM L1 AND
CC               L2.
CC
CC               N1*LAMBDA1/IWLSCR(1) +   I1 = R1
CC               N2*LAMBDA2/IWLSCR(2) + F*I1 = R2, F=F1**2/F2**2
CC               --------------------------------
CC               I1    = R1-N1*LAMBDA1/IWLSCR(1) = R1*
CC               F*I1  = R2-N2*LAMBDA2/IWLSCR(2) = R2*
CC               -------------------------------------
CC               IWLSCR5 = 1, IF IWLSCR(I)=1,I=1,2
CC                       = 2, ELSE
CC
CC               N5=N1-N2, IF IWLSCR(I)=1,I=1,2, OR IF IWLSCR(I)=2,I=1,2
CC               N5=2*N1-N2, IF IWLSCR(1)=1, IWLSCR(2)=2
CC               N5=N1-2*N2, IF IWLSCR(1)=2, IWLSCR(2)=1
CC
CC PARAMETERS :
CC         IN :  SVN    : SATELLITE NUMBER                    I*4
CC               RHS(I),I=1,2: R1, R2                         R*8
CC               N1, N5 : L1 AND L5 AMBIGUITIES (L5=WIDELANE) R*8
CC               IWLSCR(I),I=1,2: WAVELENGTH FACTOR FOR       I*4
CC                        SCREENING
CC        OUT :  IONOS(I),I=1,2: CHANGE OF IONOSPHERE IN L1   R*8
CC                        AS COMPUTED FROM L1 (I=1) OR L2 (I=2)
CC               IONOS(3) : IONOS(1)-IONOS(2)
CC               RESL3  : RMS OF L3 OBSERVABLE                R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/11/14 17:47
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               14-OCT-98 : MR: GLONASS MODIFICATIONS
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 K
C
      REAL*8    COE  , RESL3
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4 IWLSCR(2),SVN
C
      REAL*8 RHS(2),IONOS(3)
      REAL*8 RHSLOC(2),N1,N2,N5
C
      INCLUDE 'COMFREQ.inc'
C
C COMPUTE RIGHT HAND SIDES USING N1 AND N5
C ----------------------------------------
      IF(IWLSCR(1).EQ.IWLSCR(2))THEN
        N2=N1-N5
      ELSE IF(IWLSCR(1).EQ.1.AND.IWLSCR(2).EQ.2)THEN
        N2=2*N1-N5
      ELSE
        WRITE(LFNERR,901) (IWLSCR(K),K=1,2)
901     FORMAT(/,' *** SR L12VAL: WL-FACTOR COMBINATION NOT ALLOWED',/,
     1                       16X,'WL-FACTOR L1:',I2,/,
     2                       16X,'WL-FACTOR L2:',I2,/)
        CALL EXITRC(2)
      END IF
C
C COMPUTE IONOSPHERE FROM L1 AND L2
C ---------------------------------
      RHSLOC(1)=RHS(1)-N1*WLGT(1,SVN)/IWLSCR(1)
      RHSLOC(2)=RHS(2)-N2*WLGT(2,SVN)/IWLSCR(2)
C
C COMPUTE SOLUTION OF EQUATION SYSTEM
C -----------------------------------
      COE=(FRQ(1,SVN)/FRQ(2,SVN))**2
      IONOS(1)=RHSLOC(1)
      IONOS(2)=RHSLOC(2)/COE
      IONOS(3)=IONOS(1)-IONOS(2)
      RESL3=FACLIN(3,1,SVN)*RHSLOC(1)+FACLIN(3,2,SVN)*RHSLOC(2)
C
      RETURN
      END SUBROUTINE

      END MODULE
