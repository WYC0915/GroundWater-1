      MODULE s_MPPREP
      CONTAINS

C*
      SUBROUTINE MPPREP(IPRNT2,ITITL2 ,IEPOCH,NITER ,NSNG  ,NMARK ,
     1                  NDUAEP,EPCLK0,RMSSNG ,NPAR  ,PARSNG,SIGPAR,
     2                  SNGFLG)
CC
CC NAME       :  MPPREP
CC
CC PURPOSE    :  PRINT THE RESULTS OF THE EPOCH(DIFFERENCE) SOLUTION
CC               IN MAUPRP (CALLED BY SR DETSLP)
CC
CC PARAMETERS :
CC         IN :  IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : INDEX: IS THERE AN OUTPUT OR NOT (0/1)
CC               IEPOCH : EPOCH NUMBER                        I*4
CC               NITER  : ITERATION NUMBER                    I*4
CC               NSNG   : NUMBER OF SINGLE DIFFERENCES        I*4
CC               NMARK  : NUMBER OF MARKED OBSERVATIONS       I*4
CC               NDUAEP : NUMBER OF REJECTED OBS. IN EPOCH    I*4
CC               EPCLK0 : APRIORI EPOCH CLOCK                 R*8
CC               RMSSNG : MEAN ERROR OF SINGLE DIFFERENCE     R*8
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               SNGFLG : SINGLE DIFFERENCE SOLUTION FLAG    CH*1
CC                        ='G': GOOD SOLUTION
CC                        ='B': BAD SOLUTION
CC                        ='R': TOO HIGH RMS
CC               PARSNG(I),I=1,2,3,4: SOLUTION VECTOR         R*8
CC               SIGPAR(I),I=1,2,3,4: RMS OF SOLUTION VECTOR  R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  R. DACH
CC
CC VERSION    :  5.0  (AUG 02)
CC
CC CREATED    :  02/08/09 08:12
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2002     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPOCH, IPRNT2, ITITL2, NDUAEP, NITER , NMARK , NPAR  ,
     1          NSNG
C
      REAL*8    EPCLK0, EPOCLK, RMSSNG
C
CCC       IMPLICIT REAL*8    (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*80 LINE
      CHARACTER*1  SNGFLG
C
      REAL*8       PARSNG(4),SIGPAR(4)
C
C
C WRITE THE HEADER FOR EPOCH SOLUTION, EXTENDED OUTPUT ONLY
C ---------------------------------------------------------
      IF (IPRNT2.EQ.2.AND.NITER.EQ.1) THEN
        WRITE(LFNPRT,'(2(/,A))')
     1       ' EPOCH SOLUTION:',
     2       ' EPOCH  ITER  PARM   OBS  MARK   DUA'
        ITITL2=0
      ENDIF
C
C REPORT RMS AND REMARK:
C ----------------------
      IF (IPRNT2.EQ.2) THEN
        LINE=' '
        WRITE(LINE,'(6I6,A,F12.4,A)')
     1        IEPOCH,NITER,NPAR,NSNG,NMARK,NDUAEP,'  RMS = ',RMSSNG,' M'
C
        IF (SNGFLG.EQ.'R')                  LINE(65:80)='BIG RMS'
        IF (SNGFLG.EQ.'B')                  LINE(65:80)='BAD SOL.'
        IF (SNGFLG.EQ.'B'.AND.NSNG.LE.NPAR) LINE(65:80)='TOO FEW OBS.'
C
        WRITE(LFNPRT,'(A)') TRIM(LINE)
C
C WRITE THE RESULT OF THE EPOCH SOLUTION:
C --------------------------------------
C
C REPORT KINEMATIC SOLUTION:
        IF (NPAR.GE.3) THEN
          WRITE(LFNPRT,'(36X,2(A,F12.4),A)')
     1          '  DX  = ',parsng(1),' +- ',sigpar(1),' M'
          WRITE(LFNPRT,'(36X,2(A,F12.4),A)')
     1          '  DY  = ',parsng(2),' +- ',sigpar(2),' M'
          WRITE(LFNPRT,'(36X,2(A,F12.4),A)')
     1          '  DZ  = ',parsng(3),' +- ',sigpar(3),' M'
        ENDIF
C
C REPORT CLOCK SOLUTION:
        EPOCLK=parsng(npar)+epclk0
        IF (NPAR.EQ.1.OR.NPAR.EQ.4) THEN
          IF (DABS(epoclk).GT.1D6) THEN
            WRITE(LFNPRT,'(36X,2(A,E12.4),A)')
     1          '  CLK = ',epoclk,' +- ',sigpar(NPAR),' NSEC'
          ELSE
            WRITE(LFNPRT,'(36X,2(A,F12.4),A)')
     1          '  CLK = ',epoclk,' +- ',sigpar(NPAR),' NSEC'
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
