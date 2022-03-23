      MODULE s_NUVEL1
      CONTAINS

      SUBROUTINE NUVEL1(PSIT,T0,X0,Y0,Z0,T,X,Y,Z,VX,VY,VZ)
C
C   ABSMO_NUVEL TAKE A SITE SPECIFIED BY ITS INITIAL COORDINATES X0,Y0,Z0
C   AT TIME T0, AND COMPUTES ITS UPDATED POSITIONS X,Y,Z AT TIME T,
C   BASED ON THE GEOLOGICAL "absolute", (NO NET ROTATION) PLATE MOTION
C   MODEL AM0-2 (MINSTER AND JORDAN, 1978).
C
C   ORIGINAL AUTHOR: J.B. MINSTER, SCIENCE HORIZONS.
C   DFA: REVISED BY DON ARGUS, NORTHWESTERN UNIVERSITY
C   DFA: USES ABSOLUTE MODEL NNR-NUVEL1
C
C   TRANSCRIBED FROM USNO CIRCULAR 167 "Project Merit Standards" BY
C   TONY MALLAMA WITH SLIGHT MODIFICATION TO THE DOCUMENTATION AND CODE.
C
C   TIMES ARE GIVEN IN YEARS, E.G. 1988.0 FOR JAN 1, 1988.
C
C   PSIT IS THE FOUR CHARACTER ABBREVIATION FOR THE PLATE NAME, IF PSIT
C   IS NOT RECOGNIZED THEN THE NEW POSITIONS ARE RETURNED AS ZERO.
C
C   CHANGES:  03-AUG-99 : JJ: REPLACE DATA STATEMENT
C             18-DEC-03 : PS: INITIALIZE VELOCITIES WITH ZERO
C             10-JUL-05 : HU: DOUBLE PRECISION CONSTANTS
C
      IMPLICIT NONE
      CHARACTER*4        PSIT,PNM(14)
      REAL*8             OMX(14),OMY(14),OMZ(14)
      REAL*8             X0,Y0,Z0
      REAL*8             X,Y,Z,T,T0,VX,VY,VZ
      REAL*8             ORX,ORY,ORZ
      INTEGER*4          IPSIT,I
C
C   DFA: NNR-NUVEL1
C
C     DATA    (PNM(I),     OMX(I),     OMY(I),     OMZ(I), I = 1,14)
C     DATA     OMX( 1),    OMY( 1),    OMZ( 1),
C    &         OMX( 2),    OMY( 2),    OMZ( 2),
C    &         OMX( 3),    OMY( 3),    OMZ( 3),
C    &         OMX( 4),    OMY( 4),    OMZ( 4),
C    &         OMX( 5),    OMY( 5),    OMZ( 5),
C    &         OMX( 6),    OMY( 6),    OMZ( 6),
C    &         OMX( 7),    OMY( 7),    OMZ( 7),
C    &         OMX( 8),    OMY( 8),    OMZ( 8),
C    &         OMX( 9),    OMY( 9),    OMZ( 9),
C    &         OMX(10),    OMY(10),    OMZ(10),
C    &         OMX(11),    OMY(11),    OMZ(11),
C    &         OMX(12),    OMY(12),    OMZ(12),
C    &         OMX(13),    OMY(13),    OMZ(13),
C    &         OMX(14),    OMY(14),    OMZ(14),
C    &        / -0.0907,     0.2902,    -0.5976,
C    &           0.0532,    -0.1856,     0.2348,
C    &          -0.0494,    -0.1018,     0.2218,
C    &           0.4003,    -0.0311,     0.4049,
C    &           0.4695,     0.3072,     0.3762,
C    &          -0.0109,    -0.2027,     0.0945,
C    &          -0.6249,    -1.2944,     0.6544,
C    &          -0.0590,    -0.1434,     0.1887,
C    &           0.3995,     0.0026,     0.4066,
C    &          -0.0921,    -0.5138,     0.5756,
C    &           0.0152,    -0.2155,    -0.0094,
C    &          -0.0624,    -0.0906,    -0.0523,
C    &           0.2995,     0.4805,    -0.2936,
C    &           0.5913,    -0.4412,    -0.5976 /
C
      PNM( 1)='PCFC'
      PNM( 2)='AFRC'
      PNM( 3)='ANTA'
      PNM( 4)='ARAB'
      PNM( 5)='AUST'
      PNM( 6)='CARB'
      PNM( 7)='COCO'
      PNM( 8)='EURA'
      PNM( 9)='INDI'
      PNM(10)='NAZC'
      PNM(11)='NOAM'
      PNM(12)='SOAM'
      PNM(13)='JUFU'
      PNM(14)='PHIL'
C
      OMX( 1)= -0.0907D0
      OMX( 2)=  0.0532D0
      OMX( 3)= -0.0494D0
      OMX( 4)=  0.4003D0
      OMX( 5)=  0.4695D0
      OMX( 6)= -0.0109D0
      OMX( 7)= -0.6249D0
      OMX( 8)= -0.0590D0
      OMX( 9)=  0.3995D0
      OMX(10)= -0.0921D0
      OMX(11)=  0.0152D0
      OMX(12)= -0.0624D0
      OMX(13)=  0.2995D0
      OMX(14)=  0.5913D0
C
      OMY( 1)=  0.2902D0
      OMY( 2)= -0.1856D0
      OMY( 3)= -0.1018D0
      OMY( 4)= -0.0311D0
      OMY( 5)=  0.3072D0
      OMY( 6)= -0.2027D0
      OMY( 7)= -1.2944D0
      OMY( 8)= -0.1434D0
      OMY( 9)=  0.0026D0
      OMY(10)= -0.5138D0
      OMY(11)= -0.2155D0
      OMY(12)= -0.0906D0
      OMY(13)=  0.4805D0
      OMY(14)= -0.4412D0
C
      OMZ( 1)= -0.5976D0
      OMZ( 2)=  0.2348D0
      OMZ( 3)=  0.2218D0
      OMZ( 4)=  0.4049D0
      OMZ( 5)=  0.3762D0
      OMZ( 6)=  0.0945D0
      OMZ( 7)=  0.6544D0
      OMZ( 8)=  0.1887D0
      OMZ( 9)=  0.4066D0
      OMZ(10)=  0.5756D0
      OMZ(11)= -0.0094D0
      OMZ(12)= -0.0523D0
      OMZ(13)= -0.2936D0
      OMZ(14)= -0.5976D0
C
C   AM0-2
C
C    &        /'PCFC',   -0.12276,    0.31163,   -0.65537,
C    &         'AFRC',    0.05660,   -0.19249,    0.24016,
C    &         'ANTA',   -0.05286,   -0.09492,    0.21570,
C    &         'ARAB',    0.27885,   -0.16744,    0.37359,
C    &         'CARB',   -0.02787,   -0.05661,    0.10780,
C    &         'COCO',   -0.63726,   -1.33142,    0.72556,
C    &         'EURA',   -0.03071,   -0.15865,    0.19605,
C    &         'INDI',    0.48372,    0.25011,    0.43132,
C    &         'NAZC',   -0.09086,   -0.53281,    0.63061,
C    &         'NOAM',    0.03299,   -0.22828,   -0.01427/
C    &         'SOAM',   -0.05604,   -0.10672,   -0.08642,
C
C
C   INITIALIZE THINGS PROPERLY
C
      IPSIT = -1
      X = 0.0D0
      Y = 0.0D0
      Z = 0.0D0
      VX = 0.0D0
      VY = 0.0D0
      VZ = 0.0D0
C
C   LOOK UP THE PLATE IN THE LIST.
C
      DO 20 I = 1,14
   20 IF (PSIT .EQ. PNM(I)) IPSIT = I
C
C   IF PLATE NAME IS NOT RECOGNIZED RETURN THE NEW PLATE POSITION AS ZERO.
C
      IF (IPSIT .EQ. -1) RETURN
C
C   CONVERT FROM DEGREE/MY TO RADIANS/YR.
C
      ORX = OMX(IPSIT) * 1.7453292D-08
      ORY = OMY(IPSIT) * 1.7453292D-08
      ORZ = OMZ(IPSIT) * 1.7453292D-08
C
C   COMPUTE THE NEW COORDINATES
C
      X = X0 + (ORY*Z0 - ORZ*Y0) * (T-T0)
      Y = Y0 + (ORZ*X0 - ORX*Z0) * (T-T0)
      Z = Z0 + (ORX*Y0 - ORY*X0) * (T-T0)
C
C  COMPUTE VELOCITIES BY NNR-NUVEL1
C
       VX =  (ORY*Z0 - ORZ*Y0)
       VY =  (ORZ*X0 - ORX*Z0)
       VZ =  (ORX*Y0 - ORY*X0)
C
C
C   FINISH UP
C
      RETURN
      END SUBROUTINE


      END MODULE
