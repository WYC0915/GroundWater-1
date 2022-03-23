C*
      PROGRAM BV3RXN
CC
CC NAME       :  BV3RXN
CC
CC PURPOSE    :  READ BERNESE VERSION 3 BROADCAST FILES AND
CC               COPY THEM INTO RINEX FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER
CC
CC CREATED    :  89/04/11 14:50
CC
CC CHANGES    :  22-DEC-92 : ??: SR "OPNFIL" USED TO OPEN FILES
CC               10-FEB-93 : ??: USE FORMAT * TO READ EPH AND CLK
CC               07-JUL-93 : ??: DIMENSION EPHRNX(32)
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-OCT-01 : DI: SWITCH TO NEW MENU SYSTEM
CC               22-DEC-01 : HU: INTERFACE TO PRFLNA ADDED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: fileNameLength, keyValueLength,
     1                    lfnPrt, lfnLoc, lfnErr, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: DATE, TIME
C
      USE s_gtfile2
      USE s_opnfil
      USE s_prflna
      USE s_pritit
      USE s_rxwtnh
      USE s_readinpf
      USE s_opnerr
      USE s_rxwtnr
      USE s_readkeys
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_v3rxbr

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFIL   , INEW   , IOSTAT , IRC    , ISVN   , K      ,
     1          LFNRNX , MAXCOM , NCOL   , NCOM   , NFILE  , NFLCOL ,
     2          NMESS  , NOUTFIL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATIONS
C ------------
      CHARACTER*53 TITLE*53
      CHARACTER*32 FILOLD,FILPRT
      REAL*8       EPH(20),CLOCK(20)
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: FILNAM
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
C
C  RXWTNH
      PARAMETER(MAXCOM=1)
      CHARACTER    RUNBY*20,PRGNAM*20,COMENT(MAXCOM)*60
C
C  RXWTOR
      REAL*8       EPHRNX(32)
C
C COMMON BLOCKS
C -------------
C
      DATA PRGNAM/'BV3RXN'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNRNX=LFN001
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(FILNAM)
      NULLIFY(keyValue)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C PRINT GENERAL TITLE
C -------------------
      CALL pritit('BV3RXN',
     1     'Transfer Bernese navigation files to RINEX')
C
C PRINT GENERAL FILE NAMES
C ------------------------
      NCOL=79
      CALL PRFLNA(NCOL)
C
      WRITE(LFNPRT,11)
11    FORMAT(//,' FILE  BERNESE BROADCAST FILE NAME       ',
     1                 'RINEX FILE NAME                   #MESS',
     2        /,' ',4('-'),2X,32('-'),2X,32('-'),2X,5('-'),/)
C
C READ INPUT/OUTPUT FILENAMES
C ---------------------------
      NFLCOL=2
      CALL GTFILE2('BRDFIL ',NFLCOL,NFILE,FILNAM)
C
      CALL readkeys('RXNFIL', keyValue, irc)
      nOutFil = SIZE(keyValue)
C
      IF (keyValue(1) /= ' ') THEN
        IF (nOutFil==1) THEN
          FILNAM(2,:) = TRIM(keyValue(1))
        ELSEIF (nOutFil==NFILE) THEN
          FILNAM(2,:) = keyValue(:)
        ELSE
          WRITE(lfnerr,'(/,A,A,/,16X,A,I4,/,16X,A,I4,/)')
     1      ' *** PG BV3RXN: Number of input files not equal to number',
     2      ' of output files:',
     3      'Number of specified broadcast input files: ', NFILE,
     4      'Number of specified RINEX output files:    ', nOutFil
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C READ 'RUN BY' INFORMATION
C -------------------------
      CALL readkeys('RUNBY', keyValue, irc)
      IF (irc == 0) THEN
        READ(keyValue(1),'(A)') RUNBY
      ELSE
        RUNBY = 'AGENCY NOT SPECIFIED'
      END IF
C
C LOOP OVER THE INPUT FILES
C -------------------------
      DO 100 IFIL=1,NFILE
C
        IF(FILNAM(2,IFIL).EQ.' ') GOTO 90
C
        NMESS=0
        CALL OPNFIL(LFNLOC,FILNAM(1,IFIL),'OLD','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM(1,IFIL),'BV3RXN')
C
        READ(LFNLOC,101,END=210) TITLE
101     FORMAT(A53)
        INEW=0
        IF(IFIL.EQ.1) THEN
          INEW=1
        ELSEIF(FILNAM(2,IFIL).NE.FILOLD) THEN
          INEW=1
          CLOSE(UNIT=LFNRNX)
        END IF
C
C  OPEN NEW RINEX FILE, CREATE HEADER
        IF(INEW.NE.0) THEN
          CALL OPNFIL(LFNRNX,FILNAM(2,IFIL),'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRNX,IOSTAT,FILNAM(2,IFIL),'BV3RXN')
          NCOM=1
          COMENT(1)=TITLE
          CALL RXWTNH(LFNRNX,LFNERR,PRGNAM,RUNBY,DATE,TIME,
     1                NCOM,COMENT,IRC)
          IF(IRC.NE.0) GOTO 999
        END IF
C
C  READ ONE BROADCAST MESSAGE AT A TIME
C
200       READ(LFNLOC,201,END=210) ISVN
201       FORMAT(11X,I3)
          IF(ISVN.EQ.0) GOTO 210
          READ(LFNLOC,*) (EPH(K),K=1,20)
          READ(LFNLOC,*) (CLOCK(K),K=1,20)
C
C  TRANSFORM MESSAGE INTO RINEX ARRAY
          CALL V3RXBR(EPH,CLOCK,EPHRNX)
C
C  WRITE RINEX ARRAY ONTO OUTPUT FILE
          CALL RXWTNR(LFNRNX,LFNERR,ISVN,EPHRNX,IRC)
          IF(IRC.NE.0) GOTO 999
C
          NMESS=NMESS+1
C
          GOTO 200
C
210     CLOSE(UNIT=LFNLOC)
        FILOLD=FILNAM(2,IFIL)
C
C WRITE INPUT AND OUTPUT FILE NAMES AND NUMBER OF MESSAGES
C --------------------------------------------------------
90      IF(FILNAM(2,IFIL).NE.' ') THEN
          FILPRT=FILNAM(2,IFIL)
        ELSE
          FILPRT='  ---'
        ENDIF
        WRITE(LFNPRT,211) IFIL,FILNAM(1,IFIL),FILPRT,NMESS
211     FORMAT(I5,2X,A32,2X,A32,I7)
C
100   CONTINUE
C
      WRITE(LFNPRT,'( )')
999   CALL EXITRC(0)
      END
