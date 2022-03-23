      MODULE s_JESPLT
      CONTAINS

C*
      SUBROUTINE JESPLT(TT,FR)
CC
CC NAME       :  JESPLT
CC
CC PURPOSE    :  THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P.
CC               INTEGER AND A D.P. FRACTIONAL PART.
CC
CC PARAMETERS :
CC         IN :  RDF    : IF .TRUE. OPEN EPHEMERIS FILE WITH      L*1
CC                        STATUS='OLD' AND READ CONSTANTS....
CC         IN :  TT     : INPUT NUMBER                            R*8
CC        OUT :  FR(I),I=1,2: OUTPUT NUMBERS                      R*8
CC                        FR(1) CONTAINS INTEGER PART
CC                        FR(2) CONTAINS FRACTIONAL PART
CC                        FOR NEGATIVE INPUT NUMBERS, FR(1)
CC                        CONTAINS THE NEXT MORE NEGATIVE INTEGER;
CC                        FR(2) CONTAINS A POSITIVE FRACTION.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  JPL
CC
CC VERSION    :  3.4
CC
CC CREATED    :  22-OCT-92
CC
CC COPYRIGHT  :  NONE
CC      1992
CC
C*
      IMPLICIT NONE
      REAL*8 TT,FR(2)
C
C     MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS
C
      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)
C
      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN
C
C     MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER
C
      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0
C
      RETURN
C
      END SUBROUTINE

      END MODULE
