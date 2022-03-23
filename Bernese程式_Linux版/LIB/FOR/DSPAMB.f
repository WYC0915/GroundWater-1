      MODULE s_DSPAMB
      CONTAINS

C*
      SUBROUTINE DSPAMB(NSATEL,NUMSAT,NNEWAM,LSTAMB,IRC)
CC
CC NAME       :  DSPAMB
CC
CC PURPOSE    :  DISPLAY MULTIPLE AMBIGUITIES
CC
CC PARAMETERS :
CC         IN :  NSATEL : TOTAL NUMBER OF SATELLITES           I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS    I*4
CC               NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPROC. PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : LAST EPOCH WITH OBSERVATIONS
CC        OUT :  IRC    : RETURN CODE                          I*4
CC                        =0 : AT LEAST ONE MULTIPLE AMBIGUITY
CC                             DISPLAYED
CC                        =1 : NO MULTIPLE AMBIGUITIES FOUND
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/10 16:09
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               06-DEC-02 : RD: ADD AMBIGUITY BECAUSE OF CLOCKS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
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
      INTEGER*4 IAMB  , IMULTI, IRC   , ISATEL, ITITLE, MXCAMB, MXCSAT,
     1          NSATEL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*6  MXNAMB,MXNSAT
      CHARACTER*3  TEXT(6)
      CHARACTER*4  AMBTYP
C
      INTEGER*4    NUMSAT(*),NNEWAM(MXCSAT),LSTAMB(3,MXCSAT,*)
C
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA TEXT/'FIL','CYC','USR','GAP','PRP','CLK'/
C
C DISPLAY MULTIPLE AMBIGUITIES
C ----------------------------
C
C WRITE GENERAL TITLE
      WRITE(LFNPRT,1)
1     FORMAT(//,1X,72('-'),
     1        /,' MULTIPLE AMBIGUITIES',
     2          /,1X,72('-'))
C
      ITITLE=1
      IMULTI=0
      DO 20 ISATEL=1,NSATEL
        DO 10 IAMB=2,NNEWAM(ISATEL)
          IMULTI=IMULTI+1
          IF(ITITLE.EQ.1) THEN
            ITITLE=0
            WRITE(LFNPRT,2)
2           FORMAT(/,' NUMB  TYP  SATELLITE  EPOCH',
     1             /,1X,72('-'),/)
          ENDIF
          IF (LSTAMB(2,ISATEL,IAMB).GT.0) THEN
            AMBTYP = ' '//TEXT(LSTAMB(2,ISATEL,IAMB))
          ELSE
            AMBTYP = '*'//TEXT(-LSTAMB(2,ISATEL,IAMB))
          END IF
          WRITE(LFNPRT,3) IMULTI,AMBTYP,NUMSAT(ISATEL),
     1                    LSTAMB(1,ISATEL,IAMB)
3         FORMAT(I4,2X,A4,I8,I9)
10      CONTINUE
20    CONTINUE
C
      IF(IMULTI.EQ.0) THEN
        WRITE(LFNPRT,21)
21      FORMAT(/,' NO MULTIPLE AMBIGUITIES FOUND',/)
        IRC=1
        GOTO 999
      ENDIF
C
      WRITE(LFNPRT,22)
22    FORMAT(/,1X,72('-'))
      IRC=0
C
C RETURN
999   RETURN
      END SUBROUTINE

      END MODULE
