      MODULE s_WTLIEP
      CONTAINS

C*
      SUBROUTINE WTLIEP(LFN   ,IINTER,TSAV  ,T0    ,XXX   ,QNOR  ,
     1                  NPAR  ,LOCQ  ,RMS   ,TPOL  ,NSTA  ,NFIX  ,
     2                  NFTOT ,SATNUM,NSATEL,npol)
CC
CC NAME       :  WTLIEP
CC
CC PURPOSE    :  PREPARE THE POLE INFORMATION FOR THE TIME TSAVE AND
CC               WRITE ONE PARAMETER SET (=ONE LINE) INTO THE POLE FILE.
CC               FORMAT CREATED BY IERS CENTRAL BUREAU FOR THE IGS
CC               CAMPAIGN.
CC
CC PARAMETERS :
CC         IN :  LFN    : LOGICAL FILE NUMBER OF OUTPUT FILE    I*4
CC               IINTER : USE ERP-SET NR IINTER TO COMPUTE THE  I*4
CC                        CORRECTION
CC               TSAV   : FOR TSAV INFORMATION WILL BE STORED   R*8
CC               T0     : POLYNOM DEVELOPMENT TIME              R*8
CC               XXX    : SOLUTION VECTOR                       R*8(*)
CC               QNOR   : UPPER TRIANGULAR PART OF COVARI-      R*8(*)
CC                        ANCE MATRIX
CC               NPAR   : NUMBER OF PARAMETERS                  I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,2,...              I*4
CC                        CHARACTERIZATION OF PARAMETERS
CC               RMS    : MEAN ERROR OF OBSERVATION             R*8
CC               TPOL   : START AND END TIME OF INTERVAL FOR    R*8(2,*)
CC                        ONE SET OF PARAMETERS
CC                        1,2 :=BEGIN,END TIME, *:= 1..MAXPOL
CC               NSTA    : NUMBER OF CONTRIBUTING STATIONS      I*4
CC               NFIX    : NUMBER OF FIXED STATIONS             I*4
CC               NFTOT   : NUMBER OF FILES                      I*4
CC               SATNUM(K,I),K=1,..,MAXSAT,I=...: SATELLITE     I*4
CC                        NUMBERS
CC               NSATEL(I),I=1,2,..,NFTOT: NUMBER OF SATEL-     I*4(*)
CC                        LITES PER FILE
CC               NPOL   : NUMBER OF POLE PARAMETER SETS         I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  09-JUL-92
CC
CC CHANGES    : 06-SEP-92: ??: CHANGE PARAMETER NAME HLPSAT-->IHLPST
CC                             CORRECT HANDLING OF CORREL, IF PARAM NOT EST.
CC              20-MAR-93: ??: CHANGES DUE TO NEW ERP MODELS
CC              19-APR-94: RW: CPO-MODEL INCLUDED
CC              22-JUN-94: SF: NEW IGS POLE FORMAT
CC              07-JUL-94: MR: LEAP SECOND HANDLING FOR LOD COMPUTATION
CC              06-JUN-96: TS: ADDED SUBDAILY POLE MODEL
CC              16-JUL-98: DI: USE NEW SUBROUTINE 'WTIEPI'
CC              15-JUN-98: JJ: RM UNUSED VARS IERR, IRATE, IRASIG,
CC                             IPOLCO, SOME LWR CASE TO UPPER IN CODE
CC              23-JUN-05: MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC              28-FEB-07: AG: USE 206264... FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: ars
      USE s_lcerps
      USE s_poldef
      USE s_cpodef
      USE s_wtiepi
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , IC    , IFIL  , IHLPST, IINTER, ISAIER, ISAT  ,
     1          LFN   , MAXHLP, MXCLCQ, MXCSAT, NFIX  , NFTOT , NPAR  ,
     2          NPOL  , NSAT  , NSTA
C
      REAL*8    GPSHLP, GPSUTC, RMS   , T0    , TIMINT, TSAV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (IHLPST=50,MAXHLP=30)
C
C GLOBAL DECLARATION
C ------------------
      CHARACTER*6  MXNLCQ,MXNSAT
C
      REAL*8       XXX(*),QNOR(*),TPOL(2,*)
C
      INTEGER*4    LOCQ(MXCLCQ,*)
      INTEGER*4    NSATEL(*),SATNUM(MXCSAT,*),NRFPOL(3)
C
C INTERNAL DECLARATION
C --------------------
      REAL*8       POLCOR(5),CORR(5),ERR(5),CORREL(10)
      REAL*8       CRDHLP(5,2),CORRH(5),ERRH(5),CORREH(10)
      REAL*8       RATE(5),RATSIG(5),RDUMMY(5)
      real*8       thlp(2)
C
      INTEGER*4    NSNUM(IHLPST)
      INTEGER*4    INTHLP(2)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C GET THE APRIORI INFORMATION FROM POLE FILE (")
C ----------------------------------------------
      CALL POLDEF(TSAV,0,POLCOR(1),POLCOR(2),POLCOR(3),GPSUTC)
      POLCOR(1)=POLCOR(1)*ARS
      POLCOR(2)=POLCOR(2)*ARS
      POLCOR(3)=POLCOR(3)*86400.D0
      GPSUTC=DNINT(GPSUTC*86400.D0)
C
C GET THE APRIORI CPO-INFORMATION FROM POLE FILE (")
C ----------------------------------------------
      CALL CPODEF(TSAV,POLCOR(4),POLCOR(5))
      POLCOR(4)=POLCOR(4)*ARS
      POLCOR(5)=POLCOR(5)*ARS
C
C POLYNOMIAL VALUE AND ASSOCIATED ERROR
C -------------------------------------
      CALL LCERPS(IINTER,NPAR,LOCQ,XXX,QNOR,
     1            RMS,TPOL,TSAV,CORR,ERR,RATSIG,CORREL)
C
C CORRECT THE APRIORI INFORMATION
C -------------------------------
      DO 40 IC=1,5
        POLCOR(IC)=POLCOR(IC)+CORR(IC)
40    CONTINUE
C
C PREPARE NUMBER OF SATELLITES
C ----------------------------
      DO 170 I1=1,IHLPST
        NSNUM(I1)=0
170   CONTINUE
      NSAT=0
      DO 300 IFIL=1,NFTOT
        DO 310 ISAT=1,NSATEL(IFIL)
          DO 320 ISAIER=1,NSAT
            IF (NSNUM(ISAIER).EQ.SATNUM(ISAT,IFIL)) GOTO 330
320       CONTINUE
          IF(NSAT.LT.IHLPST)THEN
            NSAT=NSAT+1
            NSNUM(NSAT)=SATNUM(ISAT,IFIL)
          END IF
330       CONTINUE
310     CONTINUE
300   CONTINUE
C
C LOD, DRIFT RATES IN X-P, Y-P AND CORRESPONDING SIGMAS
C -----------------------------------------------------
      if ( tpol(1,iinter) .ne. tpol(2,iinter) ) then
        thlp(1) = tpol(1,iinter)
        thlp(2) = tpol(2,iinter)
        inthlp(1) = iinter
        inthlp(2) = iinter
      else
        if ( iinter .lt. npol) then
          thlp(1) = tpol(1,iinter)
          thlp(2) = tpol(1,iinter+1)
          inthlp(1) = iinter
          inthlp(2) = iinter + 1
        else
          thlp(1) = tpol(1,iinter-1)
          thlp(2) = tpol(1,iinter)
          inthlp(1) = iinter - 1
          inthlp(2) = iinter
        end if
      end if
c
      timint  = thlp(2) - thlp(1)
c
      do i1 = 1, 2
        call poldef( thlp(i1),0,crdhlp(1,i1),crdhlp(2,i1),
     &               crdhlp(3,i1),gpshlp )
        call lcerps( inthlp(i1),npar,locq,xxx,qnor,
     &               rms,tpol,thlp(i1),corrh,errh,rdummy,correh )
        crdhlp(1,i1) = crdhlp(1,i1) * ARS + corrh(1)
        crdhlp(2,i1) = crdhlp(2,i1) * ARS + corrh(2)
        crdhlp(3,i1) = crdhlp(3,i1) * 86400.d0     + corrh(3)
      end do
C
      DO 420 I1=1,3
        IF (I1.NE.3) THEN
          RATE(I1)=(CRDHLP(I1,2)-CRDHLP(I1,1))/TIMINT
        ELSE
          IF (CRDHLP(I1,2)-CRDHLP(I1,1).LT.0.5D0) THEN
            RATE(I1)=(CRDHLP(I1,2)-CRDHLP(I1,1))/TIMINT
          ELSE
            RATE(I1)=(CRDHLP(I1,2)-1.D0-CRDHLP(I1,1))/TIMINT
          ENDIF
        ENDIF
420   CONTINUE
C
C WRITE ONE SET OF POLE COORDINATES TO THE FILE
C ---------------------------------------------
      NRFPOL(1)=NSTA
      NRFPOL(2)=NFIX
      NRFPOL(3)=NSAT
C
      CALL WTIEPI(LFN,TSAV,POLCOR,RATE,ERR,RATSIG,CORREL,NRFPOL)
C
C RETURN CODES
C
999   RETURN
      END SUBROUTINE

      END MODULE
