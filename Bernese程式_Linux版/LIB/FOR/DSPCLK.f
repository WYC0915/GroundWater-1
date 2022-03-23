      MODULE s_DSPCLK
      CONTAINS

C*
      SUBROUTINE DSPCLK(ITITLE,ISUMRY,NCLKEV,LSTCLK,IRC)
CC
CC NAME       :  DSPCLK
CC
CC
CC PURPOSE    :  DISPLAY CLOCK EVENTS FOR ZERO DIFFERENCE FILES
CC
CC PARAMETERS :
CC         IN :  ITITLE : DISPLAY GENERAL TITLE               I*4
CC                        =0: NO GENERAL TITLE
CC                        =1: GENERAL TITLE DISPLAYED
CC               ISUMRY : DISPLAY SUMMARY OF MARKED AREAS     I*4
CC                        =0: NO SUMMARY
CC                        =1: SHORT SUMMARY DISPLAYED
CC                        =2: LONG  SUMMARY DISPLAYED
CC               NCLKEV : NUMBER OF CLOCK EVENTS IN LIST      I*4
CC               LSTCLK : (1,I): EPOCH OF THE CLOCK EVENT
CC                        (2,I): MAGNITUDE OF THE CLOCK EVENT
CC                               IN NANOSECONDS
CC                        (3,I): CLOCK ESTIMATE FROM CODSPP
CC                               IN NANOSECONDS
CC                        (4,I): ACTION (0:NONE,1:MARK,2:AMB)
CC               IRC    : RETURN CODE                         I*4
CC                        =0 : AT LEAST ONE CLOCK EVENT
CC                             DISPLAYED
CC                        =1 : NO CLOCK EVENTS DISPLAYED WITH
CC                             ENTERED SPECIFICATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  R.DACH
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  05-DEC-02
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               02-JUL-10 : RD: NCLKEV MAY BE EXCEED MXCCLK
CC               01-DEC-10 : MF: LF95: WRITE STATEMENT CORRECTED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IACT  , ICLK  , ICOD  , IEPO  , IPHA  , IRC   , ISUMRY,
     1          ITITLE, NCLKEV, MXCCLK
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*3  ACTEXT(4)
      CHARACTER*6  MXNCLK
C
      INTEGER*4    LSTCLK(4,*)
C
      COMMON/MCMCLK/MXCCLK,MXNCLK
C
      DATA ACTEXT/'   ','MRK','AMB','CYC'/
C
C DISPLAY SUMMARY OF MARKED AREAS
C -------------------------------
      IF(ISUMRY.GE.1) THEN
C
C WRITE GENERAL TITLE
        IF(ITITLE.EQ.1) THEN
          WRITE(LFNPRT,1)
1         FORMAT(//,1X,72('-'),
     1            /,' SUMMARY OF CLOCK EVENTS',
     2            /,1X,72('-'),/)
        ENDIF
C
C SUMMARY
        WRITE(LFNPRT,11) NCLKEV
11      FORMAT(' NUMBER OF CLOCK EVENTS:',I5)
        IRC=0
        GOTO 999
      ENDIF
C
C DISPLAY LIST OF CLOCK EVENTS
C ----------------------------
C
C WRITE GENERAL TITLE
      IF(ITITLE.EQ.1) THEN
        WRITE(LFNPRT,21)
21      FORMAT(//,1X,72('-'),
     1          /,' LIST OF CLOCK EVENTS',
     2          /,1X,72('-'))
      ENDIF
C
      IF(NCLKEV.EQ.0) THEN
        WRITE(LFNPRT,31)
31      FORMAT(/,' NO CLOCK EVENTS FOUND IN THIS FILE',/)
        IRC=1
        GOTO 999
      ENDIF
C
C TITLE
C -----
        WRITE(LFNPRT,41)
41      FORMAT(/,1X,'NUMB   EPOCH      CLOCK(PHASE)       CLOCK(CODE)',
     1         '     ACTION'/,1X,72('-'),/)
C
C
C LOOP OVER ALL CLOCK EVENTS
      DO 100 ICLK=1,NCLKEV
        IF (NCLKEV.GT.MXCCLK) THEN
          WRITE(LFNPRT,'(/,1X,A,/)') ' ...'
          EXIT
        ENDIF
        IEPO=LSTCLK(1,ICLK)
        IPHA=LSTCLK(2,ICLK)
        ICOD=LSTCLK(3,ICLK)
        IACT=LSTCLK(4,ICLK)+1
C
C DISPLAY MARKED AREA
        WRITE(LFNPRT,101) ICLK,IEPO,IPHA,ICOD,ACTEXT(IACT)
101      FORMAT(I5,I8,2(I15,1X,'NS'),7X,A)
C
100   CONTINUE
      WRITE(LFNPRT,51)
51    FORMAT(/,1X,72('-'))
      IRC=0
C
C RETURN
999   RETURN
      END SUBROUTINE

      END MODULE
