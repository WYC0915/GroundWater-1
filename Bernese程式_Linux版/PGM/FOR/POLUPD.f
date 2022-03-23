C*
      PROGRAM POLUPD
CC
CC TITLE      :  CREATE POLE FILES IN BERNESE FORMAT
CC
CC PURPOSE    :  CREATE POLE FILE IN BERNESE FORMAT (INCL. DPSI, DEPS)
CC               INPUT FORMATS: BULLETIN B OF IERS 5-DAY VALUES
CC                                                 1-DAY VALUES
CC                              RAPID POLE FROM MC CARTHY
CC                              WEEKLY UPDATED C04 POLE FROM IERS
CC                              IGS STATIONS (AT LEAST SOME OF THEM)
CC
CC COMMON     :  ---
CC
CC INCLUDE    :  ---
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC CREATED    :  15-MAR-94
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               04-JUN-96 : TS: ADDED SUBDAILY POLE MODEL
CC               04-FEB-97 : JJ: MR: ADD GSI FORMAT AND RATES FOR IT
CC               26-AUG-97 : MR: CORRECT OVERWRITE IF SEVERAL FILES
CC                               CONCATENATED
CC               16-JUL-98 : MR: NEW PARAMETER "RMSRAT" IN CALL RDPOL
CC               22-JUL-98 : TV: ADD IGR FORMAT
CC               20-AUG-98 : RW: ADD PRINTOUT MESSAGES
CC               09-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               27-SEP-01 : HU: CORRECT RECONSTRUCTION OF UT1 BASED ON UTR
CC                               REVISED JOB OUTPUT
CC               24-JUN-02 : HU: USE CHR*80 FOR OUTFIL
CC               07-AUG-02 : HU: NO STOP IF NO INPUT FILES AVAILABLE
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               06-OCT-02 : HU: WRITE INPUT ERP FORMAT TYPE TO JOB OUTPUT
CC               22-NOV-02 : PS: USE SUBDAILY MODEL NAME
CC               07-JAN-03 : PS: USE NUTATION MODEL NAME
CC                               CHECKBOX FOR NUTATION OFFSETS
CC               04-FEB-03 : PS: NEW PARAMETER "SUBNAM" IN CALL WTPOLH
CC               11-FEB-03 : PS: CALL TO PUINPT CHANGED
CC               19-MAY-03 : RD: INIT TIME WINDOW TO (/0D0,1D20/)
CC               01-NOV-03 : HU: DIFFERENT HANDLING OF EQUAL EPOCHS
CC                               CHECK NUTATION AND SUBDAILY MODEL
CC                               WRITE NAMES FROM INPUT FILES, IF AVAILABLE
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               14-JUL-04 : HU: WARNING MESSAGES REFORMULATED
CC               18-MAY-05 : HU: IDOUBLE ADDED, IRC REMOVED FROM CALL TO PUINPT
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               12-JUL-06 : JD: CORRECT HANDLING OF LEAP SECOND WITH RATES
CC               27-FEB-07 : AG: CALL DEFCON
CC               21-JUL-08 : PS: INCREASE MAXVAL 10000->15000
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               14-APR-11 : PS: INCREASE MAXVAL 15000->25000
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: fileNameLength80, lfnPrt, lfnErr, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_puinpt
      USE s_opnfil
      USE s_pritit
      USE s_dattim
      USE s_wtpolh
      USE s_wtpoli
      USE s_opnsys
      USE s_defcon
      USE s_gtflna
      USE s_dordup
      USE s_prflna
      USE s_gtfile
      USE s_rdpol
      USE s_readinpf
      USE s_opnerr
      USE s_ut1red
      USE s_prfile
      USE s_exitrc
      USE f_dgpsut
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , I1     , I1STVA , I2     , ICP    , IDOUBLE,
     1          IEND   , IERB1D , IFORM  , II     , IOSTAT , IR     ,
     2          IRC    , IRHLP  , IUSRAT , IV     , IVSAV  , MAXFIL ,
     3          MAXVAL , NFLCOL , NVAL
C
      REAL*8    AKORR  , AKORRM , FACRAT , HLPTIM , HLPUTC , LEAP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXVAL=25000)
      PARAMETER (MAXFIL=100)
C
C MAXVAL: MAXIMUM NUMBER OF POLE POSITIONS
C
C DECLARATIONS
C ------------
      CHARACTER*80 TITLE,CODTIT
      CHARACTER*32 INFIL(MAXFIL),infil2(maxfil)
      CHARACTER*16 SUBNAM,NUTNAM
      CHARACTER*16 SUBNAMI,NUTNAMI
      CHARACTER*16 SUBNAM1,NUTNAM1
      CHARACTER(LEN=fileNameLength80) :: OUTFIL
      CHARACTER*9  DATES
      CHARACTER*5  TIMES
      CHARACTER*3  REM(MAXVAL),HLPREM
C
      REAL*8       WINDOW(2)
      REAL*8       POLTIM(MAXVAL),POLCOO(5,MAXVAL),RMSPOL(5,MAXVAL)
      REAL*8       POLRAT(5),RMSRAT(5)
      REAL*8       GPSUTC(MAXVAL)
      REAL*8       HLPCOO(5),RMHPOL(5)
C
      INTEGER*4    INDEX(MAXVAL),IREM(MAXVAL)
      INTEGER*4    POLTYP(2),POLIND(2)
      INTEGER*4    nfil, nfil1, nfil2,INUTOFF
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C WRITE TITLE
C -----------
      CALL PRITIT('POLUPD','Convert IERS pole files to Bernese format')
      CALL PRFLNA
C
C READ INFO OF ALL INPUT POLE FILES
C ---------------------------------
      IV=0
      NFLCOL=1
      CALL GTFILE('ERPFIL',NFLCOL,MAXFIL,NFIL1,INFIL)
      IF (infil(1) .EQ. ' ') nfil1 = 0
      CALL GTFILE('IEPFIL',NFLCOL,MAXFIL,NFIL2,INFIL2)
      IF (infil2(1) .EQ. ' ') nfil2 = 0
      nfil = nfil1 + nfil2
      IF (nfil .EQ. 0) THEN
        WRITE(lfnerr,"(/,' ### PG POLUPD: No input files found',/)")
        CALL exitrc(0)
      ELSEIF (nfil .LE. maxfil) THEN
        infil(nfil1+1:nfil) = infil2(1:nfil2)
      ELSE
        WRITE(lfnerr,"(/,' *** PG POLUPD: Too many files, MAXFIL:',I6)")
     1                                 MAXFIL
        CALL exitrc(2)
      END IF
      IF (nfil1 > 0) CALL PRFILE('ERPFIL',' ',1)
      IF (nfil2 > 0) CALL PRFILE('IEPFIL',' ',1)
C
C READ INPUT FILE
C ---------------
      CALL PUINPT(TITLE,IERB1D,WINDOW,POLTYP,IUSRAT,IDOUBLE,INUTOFF,
     1            SUBNAM,NUTNAM)
C
      DO 30 I1=1,NFIL
        IFORM=0
        CALL OPNFIL(LFN001,INFIL(I1),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,INFIL(I1),'POLUPD')
        DO 40 II=1,MAXVAL
          IV=IV+1
          IF (IV.GT.MAXVAL) GOTO 910
          CALL RDPOL(LFN001,IERB1D,IFORM,CODTIT,POLIND,POLTIM(IV),
     1               POLCOO(1,IV),POLRAT,GPSUTC(IV),REM(IV),
     2               RMSPOL(1,IV),RMSRAT,IEND,NUTNAMI,SUBNAMI)
          IF (IEND.GT.0) GOTO 50
          IF (IFORM.EQ.5.OR.IFORM.EQ.6) THEN
            IF (REM(IV).EQ.'DEF')REM(IV)='B5F'
            IF (REM(IV).EQ.'EXT')REM(IV)='B5E'
          ENDIF
C
C NAME OF SUBDAILY AND NUTATION MODELS
          IF (IV.EQ.1) THEN
            NUTNAM1=NUTNAMI
            SUBNAM1=SUBNAMI
          ELSE
            IF (NUTNAMI.NE.NUTNAM1) THEN
              WRITE(LFNERR,907) TRIM(INFIL(I1)),NUTNAMI,NUTNAM1
907           FORMAT(/,' ### PG POLUPD: NUTATION MODEL HAS CHANGED ',
     1           /,16X,'EXTRACTION POLE FILE : ',A,
     2           /,16X,'NUTATION MODEL       : ',A,
     3           /,16X,'NUTATION MODEL BEFORE: ',A,/)
              NUTNAM1=NUTNAMI
            ENDIF
            IF (SUBNAMI.NE.SUBNAM1) THEN
              WRITE(LFNERR,907) TRIM(INFIL(I1)),SUBNAMI,SUBNAM1
908           FORMAT(/,' ### PG POLUPD: SUBDAILY POLE MODEL',
     1                 ' HAS CHANGED ',
     2           /,16X,'EXTRACTION POLE FILE      : ',A,
     3           /,16X,'SUBDAILY POLE MODEL       : ',A,
     4           /,16X,'SUBDAILY POLE MODEL BEFORE: ',A,/)
              SUBNAM1=SUBNAMI
            ENDIF
          ENDIF
C
C SPECIAL TREATMENT OF RATES FOR IGR AND GSI (JPL) FILES
          IF (IUSRAT.EQ.1) THEN
            IVSAV=IV
            AKORRM=0D0
            DO 33 IR=1,2
              IV=IV+1
              IF (IV.GT.MAXVAL) GOTO 910
C EPOCHS +-0.5 DAYS
              FACRAT=(-1)**IR*0.5D0
              POLTIM(IV)=POLTIM(IVSAV)+FACRAT
              GPSUTC(IV)=DGPSUT(POLTIM(IV))
              LEAP=GPSUTC(IV)-GPSUTC(IVSAV)
              DO 35 I=1,5
                IF(I.EQ.3)THEN
                  POLCOO(I,IV)=POLCOO(I,IVSAV)+FACRAT*POLRAT(I)+LEAP
                ELSE
                  POLCOO(I,IV)=POLCOO(I,IVSAV)+FACRAT*POLRAT(I)
                ENDIF
                RMSPOL(I,IV)=RMSPOL(I,IVSAV)
                REM(IV)=REM(IVSAV)
                IF (I.EQ.3) THEN
C ADD UT1-UT1R FOR GSI (JPL FORMAT)
                  CALL UT1RED(POLTIM(IV),AKORR)
                  AKORRM=AKORRM+AKORR
                  IF (IFORM.NE.14) AKORR=0.D0
                  POLCOO(I,IV)=POLCOO(I,IV)+AKORR/1000.D0
                ENDIF
C SEPARATE EQUAL EPOCHS 0.001 SEC IN ORDER TO ORDER EPOCHS
C CORRECTLY IN DORDUP
                POLTIM(IV)=POLTIM(IV)-(-1)**IR*0.0005D0/86400D0
35            CONTINUE
33          CONTINUE
C ADD UT1-UT1R FOR GSI (JPL FORMAT)
            CALL UT1RED(POLTIM(IVSAV),AKORR)
C ASSUME CONSTANT LODR
            IF (IFORM.EQ.14) THEN
              POLCOO(3,IVSAV)=POLCOO(3,IVSAV)+AKORR/1000.D0
            ELSE
              AKORR=AKORRM/2D0-AKORR
              POLCOO(3,IVSAV+1)=POLCOO(3,IVSAV+1)+AKORR/1000D0
              POLCOO(3,IVSAV+2)=POLCOO(3,IVSAV+2)+AKORR/1000D0
            ENDIF
C ALLOW DOUBLE EPOCHS
          ELSEIF (IDOUBLE.EQ.1) THEN
C SEPARATE EQUAL EPOCHS BY 0.001 SEC IN ORDER TO ORDER EPOCHS
C CORRECTLY IN DORDUP
            IF (IV.EQ.1) THEN
              POLTIM(IV)  =POLTIM(IV)  +0.0005D0/86400D0
            ELSE
              IF (POLTIM(IV).EQ.POLTIM(IV-1)) THEN
                POLTIM(IV-1)=POLTIM(IV-1)-0.0005D0/86400D0
                POLTIM(IV)  =POLTIM(IV)  +0.0005D0/86400D0
              ENDIF
            ENDIF
          ENDIF
40      CONTINUE
        GOTO 910
50      CONTINUE
        IV=IV-1
        CLOSE(LFN001)
30    CONTINUE
C
      NVAL=IV
C
C SORT THE ARRAYS
C ---------------
      CALL DORDUP(POLTIM,NVAL,INDEX)
      DO I1=NVAL,2,-1
        IF (POLTIM(INDEX(I1)).EQ.POLTIM(INDEX(I1-1))) THEN
          INDEX(I1-1)=INDEX(I1)
        ENDIF
      ENDDO
C
C CHECK SUBDAILY AND NUTATION MODEL NAMES
C ---------------------------------------
      IF (NUTNAM1.EQ.' ') THEN
        WRITE(LFNERR,917) NUTNAM
917     FORMAT(/,' ### PG POLUPD: NUTATION MODEL NOT SPECIFIED ',
     1                                       'IN INPUT ERP FILE',
     1           /,16X,'USING NUTATION MODEL NAME : ',A,/)
        NUTNAM1=NUTNAM
      ENDIF
      IF (SUBNAM1.EQ.' ') THEN
        WRITE(LFNERR,918) SUBNAM
918     FORMAT(/,' ### PG POLUPD: SUBDAILY POLE MODEL NOT SPECIFIED ',
     1                                            'IN INPUT ERP FILE',
     1           /,16X,'USING SUBDAILY POLE MODEL NAME : ',A,/)
        SUBNAM1=SUBNAM
      ENDIF
      IF (NUTNAM1.NE.NUTNAM) THEN
        WRITE(LFNERR,927) NUTNAM,NUTNAM1
927     FORMAT(/,' ### PG POLUPD: NUTATION MODEL NAME',
     1                          ' NOT EQUAL DEFAULT',
     2           /,16X,'DEFAULT NUTATION MODEL NAME : ',A,
     3           /,16X,'NUTATION MODEL NAME IN FILE : ',A,/)
c       NUTNAM1=NUTNAM
      ENDIF
      IF (SUBNAM1.NE.SUBNAM) THEN
        WRITE(LFNERR,928) SUBNAM,SUBNAM1
928     FORMAT(/,' ### PG POLUPD: SUBDAILY POLE MODEL NAME',
     1                          ' NOT EQUAL DEFAULT',
     2           /,16X,'DEFAULT SUBDAILY POLE MODEL NAME : ',A,
     3           /,16X,'SUBDAILY POLE MODEL NAME IN FILE : ',A,/)
c       SUBNAM1=SUBNAM
      ENDIF
C
C  DATE/TIME FOR TITLE IN OUTPUT POLE FILE
C  ---------------------------------------
      CALL DATTIM(DATES,TIMES)
      WRITE(TITLE(65:79),3) DATES,TIMES
3     FORMAT(A9,1X,A5)
C
C WRITE NEW POLE FILE
C -------------------
      CALL GTFLNA(1,'OUTFIL ',OUTFIL,IRC)
      CALL OPNFIL(LFN001,OUTFIL,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,OUTFIL,'POLUPD')
C
      CALL WTPOLH(LFN001,1,TITLE,POLTYP,NUTNAM,SUBNAM)
      I1STVA=0
      DO 60 I1=1,NVAL
        IF (WINDOW(1).NE.0D0.AND.POLTIM(INDEX(I1)).LT.WINDOW(1)) THEN
          DO 80 I2=I1+1,NVAL
           IF (POLTIM(INDEX(I2)).GT.POLTIM(INDEX(I1)).AND.
     1         POLTIM(INDEX(I2)).LE.WINDOW(1)) GOTO 60
80        CONTINUE
        ELSE IF (WINDOW(2).NE.1D20.AND.
     1           POLTIM(INDEX(I1)).GT.WINDOW(2)) THEN
          DO 90 I2=I1-1,1,-1
           IF (POLTIM(INDEX(I2)).LT.POLTIM(INDEX(I1)).AND.
     1         POLTIM(INDEX(I2)).GE.WINDOW(2)) GOTO 60
90        CONTINUE
        ENDIF
        I1STVA=I1STVA+1
        IF (REM(INDEX(I1)).EQ.'C04')THEN
          IREM(INDEX(I1))=1
        ELSE IF (REM(INDEX(I1)).EQ.'B11') THEN
          IREM(INDEX(I1))=2
        ELSE IF (REM(INDEX(I1)).EQ.'B12') THEN
          IREM(INDEX(I1))=3
        ELSE IF (REM(INDEX(I1)).EQ.'B5F') THEN
          IREM(INDEX(I1))=4
        ELSE IF (REM(INDEX(I1)).EQ.'A  ') THEN
          IREM(INDEX(I1))=5
        ELSE IF (REM(INDEX(I1)).EQ.'COD') THEN
          IREM(INDEX(I1))=6
        ELSE IF (REM(INDEX(I1)).EQ.'B5E') THEN
          IREM(INDEX(I1))=7
        ELSE IF (REM(INDEX(I1)).EQ.'AE ') THEN
          IREM(INDEX(I1))=8
        ELSE
          IREM(INDEX(I1))=9
        ENDIF
        IF (I1.EQ.1.OR.I1STVA.EQ.1) THEN
          ICP=1
        ELSE
C
C WRITE RECORD
          IF (POLTIM(INDEX(I1)).GT.HLPTIM) THEN
            CALL WTPOLI(LFN001,HLPTIM,HLPCOO,HLPUTC,HLPREM,RMHPOL)
            ICP=1
          ELSE IF (IRHLP.GT.IREM(INDEX(I1))) THEN
            ICP=1
          ELSE
            ICP=0
          ENDIF
        ENDIF
        IF (ICP.EQ.1) THEN
          HLPTIM=POLTIM(INDEX(I1))
          HLPUTC=GPSUTC(INDEX(I1))
          HLPREM=REM(INDEX(I1))
          IRHLP=IREM(INDEX(I1))
          DO 70 I2=1,5
            HLPCOO(I2)=POLCOO(I2,INDEX(I1))
            RMHPOL(I2)=RMSPOL(I2,INDEX(I1))
70        CONTINUE

C DO NOT INCLUDE NUTATION OFFSETS
          IF(INUTOFF.EQ.0) THEN
             HLPCOO(4)=0.0
             HLPCOO(5)=0.0
          ENDIF

          ICP=0
        ENDIF
60    CONTINUE

      CALL WTPOLI(LFN001,HLPTIM,HLPCOO,HLPUTC,HLPREM,RMHPOL)
      CLOSE (LFN001)
C
      WRITE(LFNPRT,"(/,' Input file format type          : ',I5,
     1               /,' Nutation model name written     :    ',A,
     2               /,' Subdaily model name written     :    ',A/)")
     3                   IFORM,NUTNAM1,SUBNAM1
      GOTO 999
C
C ERROR HANDLING
C --------------
910   WRITE(LFNERR,916)
916   FORMAT(/,' *** PGM POLUPD: TOO MANY POLE VALUES.',/,
     1         '                 INCREASE PARAMTER MAXVAL!')
      CALL EXITRC(2)
C
999   CALL EXITRC(0)
      END
