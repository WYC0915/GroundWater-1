      MODULE s_PRICON
      CONTAINS

C*
      SUBROUTINE PRICON(PRIOPT,TITLES)
CC
CC NAME       :  PRICON
CC
CC PURPOSE    :  PRINT CONSTANTS
CC
CC PARAMETERS :
CC         IN :  PRIOPT : FLAG, WHETHER CONSTANTS SHOULD BE   I*4
CC                        PRINTED
CC                         =0 : NO PRINT
CC                         =1 : PRINT
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/11 14:36
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               13-FEB-92 : ??: REMOVE PRINTING OF POLE INFORMATION
CC               08-JAN-93 : ??: USE OF SR "OPNFIL" TO OPEN FILE
CC               01-MAR-93 : ??: PRINT ELEVATION DEPENDENCE OF PHASE
CC                               CENTERS
CC               06-MAR-94 : ??: "GMM" AND "GMS" INTERCHANGED IN PRINTING
CC               08-APR-94 : MR: ALLOW FORMAT =2 FOR PHASE CENTER FILE
CC               10-AUG-94 : MR: CALL EXITRC
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               31-AUG-95 : SS: MEAN RADIUS OF THE EARTH INTRODUCED
CC               31-AUG-95 : SS: PRINT CONSTANTS FOR METEO MODEL
CC               26-MAR-96 : MR: REMOVE PRINTING OF PHASE CENTERS,
CC                               PHASE CENTERS ARE PRINTED IN PRIPHC
CC               04-MAY-98 : SS: "FACTEC" ADDED
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
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
      USE d_const, ONLY: AE, C, CONRE, ETUT, FACTEC, FREQ, FREQP, GM,
     1                   GMM, GMS, HREF, HUMREF, OMEGA, P0, PREF,
     2                   TREF, WGTCOD, WGTPHA
      USE f_lengt1
      IMPLICIT NONE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      INTEGER*4     PRIOPT
C
C
C PRINT TITLE LINES
C -----------------
      IF(PRIOPT.EQ.0) THEN
        RETURN
C
C PRINT CONSTANTS
C ---------------
      ELSE
        WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
2       FORMAT(//,A,/,A,/,' ',131('-'),//)
C
        WRITE(LFNPRT,"(
     1       ' 10. CONSTANTS'
     2    ,/,' -------------'
     3    ,/,1X)")
C
        WRITE(LFNPRT,111) C,FREQ(1),FREQ(2),FREQP,GM,GMS,GMM,AE,CONRE,
     1                    FACTEC,P0,OMEGA,ETUT,WGTPHA,WGTCOD,HREF,PREF,
     2                    TREF,HUMREF
111     FORMAT(' VELOCITY OF LIGHT              :',D25.12,'  M/SEC'
     1    /,' L1-CARRIER FREQUENCY           :',D25.12,'  1/SEC'
     2    /,' L2-CARRIER FREQUENCY           :',D25.12,'  1/SEC'
     3    /,' P-CODE     FREQUENCY           :',D25.12,'  1/SEC'
     4    /,' GRAVITY CONSTANT * EARTH MASS  :',D25.12,'  M**3/SEC**2'
     5    /,' GRAVITY CONSTANT * SOLAR MASS  :',D25.12,'  M**3/SEC**2'
     6    /,' GRAVITY CONSTANT * LUNAR MASS  :',D25.12,'  M**3/SEC**2'
     7    /,' EQUATORIAL RADIUS OF THE EARTH :',D25.12,'  M'
     8    /,' MEAN       RADIUS OF THE EARTH :',D25.12,'  M'
     9    /,' IONOSPHERIC FACTOR             :',D25.12,'  M/SEC**2/TECU'
     1    /,' NOMINAL RAD.PRESS. ACCELERATION:',D25.12,'  M/SEC**2'
     2    /,' ANGULAR VELOCITY OF THE EARTH  :',D25.12,'  RAD/SEC'
     3    /,' EPHEM.TIME (ET) MINUS UTC      :',D25.12,'  SEC'
     4    /,' WEIGHT FOR PHASE OBSERVATIONS  :',D25.12,'  '
     5    /,' WEIGHT FOR CODE  OBSERVATIONS  :',D25.12,'  '
     6    /,' REFERENCE HEIGHT               :',D25.12,'  M'
     7    /,' PRESSURE    AT REF. HEIGHT     :',D25.12,'  MBAR'
     8    /,' TEMPERATURE AT REF. HEIGHT     :',D25.12,'  C'
     9    /,' HUMIDITY    AT REF. HEIGHT     :',D25.12,'  %')
        WRITE(LFNPRT,'(/)')
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
