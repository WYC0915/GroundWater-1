      MODULE s_PRORET
      CONTAINS

C*
      SUBROUTINE PRORET(INDTRM,RMS,XXX,ANOR,AMPLI,AMPRMS)
CC
CC NAME       :  PRORET
CC
CC PURPOSE    :  COMPUTE PRO- AND RETROGRADE AMPLITUDES AND THEIR
CC               FORMAL ERRORS FROM DEPS AND DPSI COS AND SIN TERMS
CC               CONVENTION USED BY T.A. HERRING, JGR, 96, B5,
CC               P. 8272 FOR A+IN, A+OUT, A-IN A-OUT (RETROGRADE/
CC               PROGRADE AMPLITUDES)
CC
CC PARAMETERS :
CC        IN  :  INDTRM(I),I=1,..,4: INDEX WHERE DEPS, DPSI      I*4
CC                        PARAMETERS ARE FOUND IN "XXX","ANOR"
CC                        (1): AMPLITUDE OF DEPS SIN TERM (BI'')
CC                        (2): AMPLITUDE OF DEPS COS TERM (BI  )
CC                        (3): AMPLITUDE OF DPSI SIN TERM (AI  )
CC                        (4): AMPLITUDE OF DPSI COS TERM (AI'')
CC               RMS    : RMS OF ADJUSTMENT (MAS)
CC                        IF RMS = 0.D0: NO COMPUTATION OF THE
CC                                       FORMAL ERRORS "AMPRMS"
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR (MAS)       R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: NORMAL EQUATION R*8
CC                        MATRIX INVERTED
CC               AMPLI(I),I=1,..,4: AMPLITUDES OF PRO- AND       R*8
CC                        RETROGRADE TERMS
CC                        (1): AMPLI. OF RETROGR. TERM (A+IN)
CC                        (2): AMPLI. OF RETROGR. TERM (A+OUT)
CC                        (3): AMPLI. OF PROGRADE TERM (A-IN)
CC                        (4): AMPLI. OF PROGRADE TERM (A-OUT)
CC               AMPRMS(I),I=1,..,4: CORRESPONDING RMS ERRORS    R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: ars
      USE f_ikf
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDE1  , IDE2  , IFIRST, ITR1  , ITR2  , ITRM
C
      REAL*8    EPS0  , EPS0AS, RMS   , SINE0
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8       XXX(*),ANOR(*),AMPLI(4),AMPRMS(4),AMPDER(4,4)
C
      INTEGER*4    INDTRM(4)
C
      DATA IFIRST/1/
      DATA EPS0AS/84381.412D0/
C
C OBLIQUITY OF ECLIPTIC
C ---------------------
      IF (IFIRST.EQ.1) THEN
        IFIRST=0
        EPS0=EPS0AS/ars
        SINE0=DSIN(EPS0)
      ENDIF
C
C COMPUTE PRO- AND RETROGRADE AMPLITUDES
C --------------------------------------
      AMPLI(1)=-(XXX(INDTRM(3))+SINE0*XXX(INDTRM(1)))/2.D0
      AMPLI(2)=-(XXX(INDTRM(4))-SINE0*XXX(INDTRM(2)))/2.D0
      AMPLI(3)=-(XXX(INDTRM(3))-SINE0*XXX(INDTRM(1)))/2.D0
      AMPLI(4)= (XXX(INDTRM(4))+SINE0*XXX(INDTRM(2)))/2.D0
C
C COMPUTE FORMAL ERRORS OF AMPLITUDES
C -----------------------------------
      IF (RMS.NE.0.D0) THEN
C
        DO ITR1=1,4
          DO ITR2=1,4
            AMPDER(ITR1,ITR2)=0.D0
          ENDDO
        ENDDO
C
        AMPDER(1,3)=-0.5D0
        AMPDER(1,1)=-0.5D0*SINE0
        AMPDER(2,4)=-0.5D0
        AMPDER(2,2)= 0.5D0*SINE0
        AMPDER(3,3)=-0.5D0
        AMPDER(3,1)= 0.5D0*SINE0
        AMPDER(4,4)= 0.5D0
        AMPDER(4,2)= 0.5D0*SINE0
C
        DO ITRM=1,4
          AMPRMS(ITRM)=0.D0
          DO IDE1=1,4
            DO IDE2=1,4
              AMPRMS(ITRM)=AMPRMS(ITRM)+
     1                     AMPDER(ITRM,IDE1)*AMPDER(ITRM,IDE2)*
     2                     ANOR(IKF(INDTRM(IDE1),INDTRM(IDE2)))
            ENDDO
          ENDDO
          AMPRMS(ITRM)=RMS*DSQRT(AMPRMS(ITRM))
        ENDDO
C
      ELSE
        DO ITRM=1,4
          AMPRMS(ITRM)=0.D0
        ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
