      MODULE s_WTMSGS
      CONTAINS

C*
      SUBROUTINE WTMSGS(IMSG,ITITL2,IEPOCH,SATMSG,SATREF,
     1                  IFREQ,WLF,SLIP,XSLIP)
CC
CC NAME       :  WTMSGS
CC
CC PURPOSE    :  WRITE MESSAGES FOR PROGRAMS MANPRP AND AUTPRP
CC               DIFFERENT MESSAGES ARE SELECT3ED WITH "IMSG"
CC
CC PARAMETERS :
CC         IN :  IMSG   : MESSAGE TYPE TO BE DISPLAYED        I*4
CC                        = 1: CYCLE SLIP (SNG) ACCEPTED
CC                        = 2: CYCLE SLIP (CLK)
CC                        = 3: OUTLIER DETECTED (SNG)
CC                        = 4: OUTLIER MARKED (SNG)
CC                        = 5: OUTLIER MARKED (DUA)
CC                        = 6: MAX. INTERVAL EXCEEDED (SNG)
CC                        = 7: MAX. INTERVAL EXCEEDED (DUA)
CC                        = 8: CYCLE SLIP OR OUTLIER (DUA)
CC                        = 9: CYCLE SLIP (SNG) NOT ACCEPTED
CC                        =10: MILLI-SEC JUMP DETECTED
CC                        =11: CLOCK EVENT DETECTED
CC                        =12: CYCLE SLIP CORRECTION REMOVED
CC               ITITL2 : FLAG IF TITLE HAS TO BE PRINTED     I*4
CC                        =0: TITLE ALREADY PRINTED
CC                        =1: PRINT TITLE FIRST
CC               IEPOCH : EPOCH NUMBER                        I*4
CC               SATMSG : SATELLITE NUMBER                    I*4
CC               SATREF : REFERENCE SATELLITE                 I*4
CC               IFREQ  : FREQUENCY INVOLVED                  I*4
CC               WLF(I),  I=1,2,3: WAVELENGTH FACTOR(S)       I*4
CC               SLIP(I), I=1,2,3: CYCLE SLIP(S)              R*8
CC               XSLIP(I),I=1,2,3: FRACTIONAL PART OF SLIP(S) R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/09/01 14:10
CC
CC CHANGES    :  16-JUN-93 : ??: ADD OPTION "ALL SATELLITES"=99
CC               10-AUG-94 : MR: CALL EXITRC
CC               01-AUG-02 : RD: HANDLE MILLI-SEC JUMPS FOR ZD PHASE
CC               06-DEC-02 : RD: MARK BECAUSE OF A CLOCK EVENT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-FEB-08 : RD: INDICATE CYCLE SLIP PROBLEMS
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNPRT, LFNERR
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IEPOCH, IFREQ , IMSG  , ITITL2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*72 LINE
      INTEGER*4    SATMSG,SATREF,WLF(3)
      REAL*8       SLIP(3),XSLIP(3)
C
C
C PRINT TITLE LINE
C ----------------
      IF(ITITL2.EQ.1) THEN
        ITITL2=0
        WRITE(LFNPRT,1)
1       FORMAT(//,1X,72('-'),
     1          /,' CYCLE SLIP DETECTION AND OUTLIER REJECTION',
     2          /,1X,72('-'),/,
     3          /,' EPOCH  DESCRIPTION               TYP SAT ',
     4            'REF FRQS WLF        SLIP   FRAC',
     5          /,1X,72('-'),/)
      ENDIF
C
C WRITE APPROXIMATE CONTENT OF MESSAGE LINE
C -----------------------------------------
      WRITE(LINE,2) IEPOCH,SATMSG,SATREF,
     1              IFREQ,WLF(1),SLIP(1),XSLIP(1)
2     FORMAT(I5,31X,4I4,F13.0,F7.2)
C
      IF(SATMSG.EQ.0)  LINE(37:40)=' '
      IF(SATMSG.EQ.99) LINE(37:40)=' ALL'
      IF(SATREF.EQ.0)  LINE(41:44)=' '
      IF(IFREQ .EQ.3)  LINE(47:49)='1,2'
C
C BRANCH ACCORDING TO MESSAGE TYPE
C --------------------------------
      GOTO (10,20,30,40,50,60,70,80,90,100,110,120) IMSG
C
C INVALID MESSAGE
      WRITE(LFNERR,901) IMSG
901   FORMAT(/,' *** WTMSGS: INVALID MESSAGE TYPE',/,
     1                  16X,'MESSAGE TYPE:',I3,/)
      CALL EXITRC(2)
C
C CYCLE SLIP (SNG) ACCEPTED:
C -------------------------
10    LINE( 8:32)='CYCLE SLIP: ACCEPTED'
      IF(IFREQ.EQ.5) LINE(11:32)=' '
      LINE(34:36)='SNG'
      GOTO 900
C
C CYCLE SLIP (CLK):
C ----------------
20    LINE( 8:32)='CYCLE SLIP'
      LINE(34:36)='CLK'
      LINE(66:72)=' '
      GOTO 900
C
C OUTLIER DETECTED (SNG):
C ----------------------
30    LINE( 8:32)='OUTLIER DETECTED'
      LINE(34:36)='SNG'
      LINE(49:65)=' '
      GOTO 900
C
C OUTLIER MARKED (SNG):
C --------------------
40    LINE( 8:32)='OUTLIER MARKED; RECHECK'
      LINE(34:36)='SNG'
      LINE(50:72)=' '
      GOTO 900
C
C OUTLIER MARKED (DUA):
C --------------------
50    LINE( 8:32)='OUTLIER MARKED; RECHECK'
      LINE(34:36)='DUA'
      LINE(50:72)=' '
      GOTO 900
C
C MAX. INTERVAL EXCEEDED (SNG):
C ----------------------------
60    LINE( 8:32)='MAX. INTERVAL EXCEEDED'
      LINE(34:36)='SNG'
      LINE(49:65)=' '
      GOTO 900
C
C MAX. INTERVAL EXCEEDED (DUA):
C ----------------------------
70    LINE( 8:32)='MAX. INTERVAL EXCEEDED'
      LINE(34:36)='DUA'
      LINE(50:72)=' '
      GOTO 900
C
C CYCLE SLIP OR OUTLIER (DUA):
C ---------------------------
80    LINE( 8:32)='CYCLE SLIP OR OUTLIER'
      LINE(34:36)='DUA'
      GOTO 900
C
C CYCLE SLIP (SNG) NOT ACCEPTED:
C -----------------------------
90    LINE( 8:32)='CYCLE SLIP: NOT ACCEPTED'
      IF(IFREQ.EQ.5) LINE(11:32)=' '
      LINE(34:36)='SNG'
      GOTO 900
C
C MILLI-SEC. JUMP:
C ---------------
100   LINE( 8:32)='MILLI-SEC. JUMP DETECTED'
      LINE(34:36)='JMP'
      LINE(66:72)=' '
      GOTO 900
C
C CLOCK EVENT:
C ------------
110   LINE( 8:32)='MARK BECASUE CLOCK EVENT'
      LINE(34:36)='CLK'
      LINE(50:72)=' '
      GOTO 900
C
C CLOCK EVENT:
C ------------
120   LINE( 8:32)='CYCLE SLIP REMOVED'
      LINE(34:36)='PRP'
      LINE(50:72)=' '
      GOTO 900
C
C WRITE MESSAGE LINE
C ------------------
900   WRITE(LFNPRT,991) LINE
991   FORMAT(1X,A72)
C
C ADD TO MORE LINE FOR DUAL FREQ. CYCLE SLIPS
C -------------------------------------------
      IF(IMSG.EQ.8) THEN
        WRITE(LFNPRT,992) (WLF(I),SLIP(I),XSLIP(I),I=2,3)
992     FORMAT(48X,'2',I4,F13.0,F7.2,/,
     1         48X,'5',I4,F13.0,F7.2)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
