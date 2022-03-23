C*
      PROGRAM SATCLK
CC
CC NAME       :  SATCLK
CC
CC PURPOSE    :  EXTRACT SATELLITE CLOCK COEFFICIENTS OUT OF THE
CC               BROADCAST MESSAGES OF ONE OR MORE FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  88/11/03 09:06
CC
CC CHANGES    :  23-APR-92 : ??: NEW INTERNAL FILE NAME "SATCLRS"
CC               15-JUN-92 : ??: MANOEUVRE-SATELLITES WITH NUMBERS > 50
CC                               HAVE TO BE CONVERTED BACK TO THE NORMAL
CC                               SATELLITE NUMBER
CC               23-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               08-DEC-95 : MR: REPLACE CALL NGSINP BY CALL SCINPT
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               07-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               20-MAY-03 : RD: INIT TIME WINDOW TO (/ 0D0, 1D20 /)
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.INC REPLACED BY D_CONST
CC               21-JUN-05 : MM: (COM)LFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               02-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnPrt, lfnRes, lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: DATE, TIME
      USE s_opnfil
      USE s_pritit
      USE s_gtbrdc
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_prflna
      USE s_gtfile
      USE s_scinpt
      USE s_readinpf
      USE s_opnerr
      USE s_exitrc
      USE f_modsvn
      USE f_gpsmjd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPH  , IEPH0 , IEPH01, IFIL  , IM    , IOSTAT, IRC   ,
     1          ISAT  , ISAT1 , IWEEK , MAXEPH, MAXFIL, MXCEPH,
     2          MXCFIL, MXCSAT, NCLK  , NCLOCK, NFIL  , NFLCOL, NRSATM,
     3          NSAT
C
      REAL*8    A0    , A1    , A2    , TMJD  , TOC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=50,MAXEPH=200)
C
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE TRANSFORMED
C MAXEPH: MAXIMUM NUMBER OF EPHEMERIS PARAMETERS PER SATELLITE
C
C DECLARATIONS
C ------------
      CHARACTER*53 TITLE,TITLE1
      CHARACTER*32 FILSAC,FILBRD(MAXFIL)
      CHARACTER*6  MXNFIL,MXNSAT,MXNEPH
C
      REAL*8       WINDOW(2),EPH(20*MAXEPH,MAXSAT)
      REAL*8       CLOCK(20*MAXEPH,MAXSAT)
C
      INTEGER*4    NRSAT(MAXSAT),NEPH(MAXSAT)
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE/FILBRD,EPH,CLOCK
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXCFIL=MAXFIL
      MXCEPH=MAXEPH
      MXNSAT='MAXSAT'
      MXNFIL='MAXFIL'
      MXNEPH='MAXEPH'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
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
C PRINT TITLE
C -----------
      CALL pritit('SATCLK','Extract satellite clocks')
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C PRINT GENERAL FILE NAMES
C ------------------------
       CALL prflna
C
C PRINT GENERAL TITLE
C -------------------
      WRITE(LFNPRT,4) DATE,TIME
4     FORMAT(/,1X,79('*'),
     1       /,' SATCLK: EXTRACTS SATELLITE CLOCKS OUT OF THE ',
     2         'BROADCAST MESSAGES',1X,A9,1X,A5,
     3       /,1X,79('*'),/)

C
C SET NUMBER OF CLOCK COEFFICIENTS PER SATELLITE AND MESSAGE
C ----------------------------------------------------------
      NCLK=3
C
C READ OPTION INPUT FILE
C ----------------------
      CALL scinpt(title,window)
C
C READ BROADCAST FILE NAMES
C -------------------------
      NFLCOL=1
      CALL GTFILE('BRDFIL ',NFLCOL,MAXFIL,NFIL,FILBRD)
C
C GET SATELLITE CLOCK FILE NAME
C -----------------------------
      CALL GTFLNA(1,'SATCLRS',FILSAC,IRC)
      CALL OPNFIL(LFNRES,FILSAC,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILSAC,'SATCLK')
C
C WRITE TITLE LINES TO CLOCK FILE
C -------------------------------
      WRITE(LFNRES,1) TITLE,DATE,TIME
1     FORMAT(A53,12X,A9,1X,A5,/,80('-'),//,
     1       'SAT WEEK   TOC   #PAR     A0 (SEC)',7X,
     2       'A1 (SEC/SEC)    A2 (SEC/SEC**2)',/)
C
C LOOP OVER ALL INPUT FILES
C -------------------------
      NCLOCK = 0
      DO 100 IFIL=1,NFIL
C
C READ NAVIGATION MESSAGES OF ONE BROADCAST FILE
C ----------------------------------------------
        CALL GTBRDC(FILBRD(IFIL),TITLE1,NSAT,NRSAT,NEPH,EPH,CLOCK)
C
C ORDER MANOEUVRE SATELLITES CORRECTLY (NO MANOEUVRES IN CLOCK FILE)
C ------------------------------------------------------------------
        DO 60 ISAT=1,NSAT
          IF (MODSVN(NRSAT(ISAT)).NE.NRSAT(ISAT)) THEN
            NRSATM=MODSVN(NRSAT(ISAT))
            DO 50 ISAT1=1,NSAT
              IF (NRSAT(ISAT1).EQ.NRSATM) THEN
                DO 40 IEPH=1,NEPH(ISAT)
                  IEPH0 =(IEPH-1)*20
                  IEPH01=(NEPH(ISAT1)+IEPH-1)*20
                  DO 30 IM=1,20
                    EPH(IEPH01+IM,ISAT1)=EPH(IEPH0+IM,ISAT)
                    CLOCK(IEPH01+IM,ISAT1)=CLOCK(IEPH0+IM,ISAT)
30                CONTINUE
40              CONTINUE
                NEPH(ISAT1)=NEPH(ISAT1)+NEPH(ISAT)
                GOTO 60
              ENDIF
50          CONTINUE
          ENDIF
60      CONTINUE
C
C LOOP OVER ALL SATELLITES AND MESSAGES
C -------------------------------------
        DO 20 ISAT=1,NSAT
          IF (MODSVN(NRSAT(ISAT)).NE.NRSAT(ISAT)) GOTO 20
          DO 10 IEPH=1,NEPH(ISAT)
C
C EPOCH OF CLOCK COEFFICIENTS (MJD)
C ---------------------------------
            IEPH0=(IEPH-1)*20
            IWEEK=IDNINT(CLOCK(IEPH0+1,ISAT))
            TOC  =CLOCK(IEPH0+11,ISAT)
            TMJD=GPSMJD(TOC,IWEEK)
C
C CLOCK SET WITHIN WINDOW ?
C -------------------------
            IF((WINDOW(1).NE.0D00 .AND. TMJD.LT.WINDOW(1)) .OR.
     1         (WINDOW(2).NE.1D20 .AND. TMJD.GT.WINDOW(2))) GOTO 10
C
C CLOCK COEFFICIENTS
C ------------------
            A0=CLOCK(IEPH0+14,ISAT)
            A1=CLOCK(IEPH0+13,ISAT)
            A2=CLOCK(IEPH0+12,ISAT)
C
C WRITE SATELLITE CLOCK COEFFICIENTS TO CLOCK FILE
C ------------------------------------------------
            WRITE(LFNRES,2) NRSAT(ISAT),IWEEK,TOC,NCLK,A0,A1,A2
2           FORMAT(I3,I5,F9.0,I3,1X,3D17.9)
            NCLOCK=NCLOCK+1
10        CONTINUE
20      CONTINUE
100   CONTINUE
C
C WRITE BLANK LINE AT THE END
C ---------------------------
      WRITE(LFNRES,3)
3     FORMAT(' ')
C
C CLOSE SATELLITE CLOCK FILE
C --------------------------
      CLOSE(UNIT=LFNRES)
C
      WRITE(LFNPRT,'(A,I12,/)')
     1      ' NUMBER OF CLOCK RECORDS CONVERTED:',NCLOCK
C
      CALL EXITRC(0)
C
      END
