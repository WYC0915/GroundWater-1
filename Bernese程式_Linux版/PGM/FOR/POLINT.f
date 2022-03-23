C*
      PROGRAM POLINT
CC
CC NAME       :  POLINT
CC
CC PURPOSE    :  THIS PROGRAM EXTRACTS THE POLE VALUES OUT OF IERS POLE
CC               RESULT FILE FROM GPSEST. IT COMPUTES THE DIFFERENCES
CC               BETWEEN AN A PRIORI POLE FILE AND THE SOLUTION POLE
CC               FILES.
CC               THIS PROGRAM ALSO CREATES A PLOT FILE.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.5
CC
CC CREATED    :  25-MAY-95
CC
CC CHANGES    :  09-FEB-96 : MR: ADD IERS OUTPUT FILE
CC               21-AUG-97 : TS: RESTITUTE SUBDAILY POLE
CC               02-MAR-98 : MR: REMOVE CONVERSION TO UT1R !!!
CC                               CORRECT LOD FOR SUBDAILY MODEL
CC               03-MAR-98 : MR: SHIFT REMOVAL OF EPOCHS WITH "IDOUBLE"
CC               15-APR-98 : MR: CORRECT X-RATE, Y-RATE FOR SUBDAILY
CC               16-JUL-98 : DI: USE NEW SR 'RDIEPI' AND 'WTIEPI'
CC               16-AUG-99 : JJ: RM UNUSED VARS REMARK, FILPOL, POLTXT
CC               05-MRZ-03 : PS: SWITCH TO NEW MENU SYSTEM
CC               12-AUG-03 : PS: ADDED INTERFACE TO SR PIINPT,
CC                               FORMAT CORRECTED
CC               13-SEP-03 : HU: INTERFACE FOR SUBPOL
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               13-JAN-04 : PS: USE RDPOL INSTEAD OF RDPOLH AND RDPOLI
CC               14-MAY-04 : PS: MAXFIL 2000 -> 4000
CC               17-MAY-04 : PS: SUBNAM "NONE" IF SUBDAILY MODEL RESTITUTED
CC               02-JUN-04 : PS: MERGING BERN/MUNICH
CC               17-MAY-05 : HU: SAMPLING IMPLEMENTED
CC               20-MAY-05 : HU: DO NOT READ A PRIORI POLE IF NO PLOT REQUESTED
CC               16-JUN-05 : MM: UNUSED d_const REMOVED
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               27-APR-12 : RD: NULLIFY POINTERS, USE M_BERN WITH ONLY
CC               27-APR-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnplt, lfnerr, lfnres, lfn001,
     1                    fileNameLength
      USE m_cpu,    ONLY: cpu_start
      USE d_const,  ONLY: ars
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_wtskel
      USE s_opnfil
      USE s_pritit
      USE s_cpodef
      USE s_wtpolh
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_gtfile2
      USE s_piinpt
      USE s_prflna
      USE s_poldef
      USE s_gtfile
      USE s_rdpol
      USE s_readinpf
      USE s_opnerr
      USE s_subpol
      USE s_exitrc
      USE s_radgms
      USE s_jmt
      USE s_wtiepi
      USE f_djul

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1       , ICONT    , IDOUBLE  , IFIL     , IHOUR    ,
     1          II       , ILIN     , IMONTH   , IOSTAT   , IRCIER   ,
     2          IRCODE   , IRCPLT   , IRCSKL   , ISMPL    ,
     3          ISUBAP   , ISUBFL   , IYEAR    , MAXFIL   , MAXFLD   ,
     4          MIN      , NFIL     , NFLCOL
C
      REAL*8    DAY      , GPSUTCNEW,
     1          GPSUTCOLD, POLTIM   , POLTIO   ,
     2          SEC      , T0PLT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL= 4000,MAXFLD=  1)
C
C MAXFIL: MAXIMUM NUMBER OF COORDINATE FILES
C MAXFLD: MAXIMUM NUMBER OF FIELDS IN PLOT SKELETON FILE
C
C DECLARATIONS
C ------------
      CHARACTER*80  TITNEW,FIELDS(MAXFLD)
      CHARACTER*80  CODTIT
      CHARACTER*32  FILNAM(2,MAXFIL),FILIER,FILPLT,FILSKL
      CHARACTER*16  NUTNAM,SUBNAM,NUTDUM,SUBDUM
      CHARACTER*3   REM
      CHARACTER*1   SNGSTR

      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER   :: filinp
C
      REAL(r8b),DIMENSION(3)           :: erpsub
      REAL(r8b),DIMENSION(3)           :: erpsur
      REAL*8        POLXYT(5),RMSXYT(5),POLAPR(5),DJUMP(5)
      REAL*8        POLRAT(5),RMSRAT(5),POLSAV(5),POLCOR(10)
      REAL*8        GPSUTC

C
      INTEGER*4     IPLFLG(MAXFLD),POLTYP(2),NRFPOL(3)

      INTEGER*4     IERB1D, IFORM, SAMPL
      INTEGER(i4b),DIMENSION(2)           :: records

C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(filinp)
C
C GET THE FILE OF FILES
C ---------------------
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
C GET POLE IERS RESULT FILE NAMES
C -------------------------------
      NFLCOL=1
      CALL GTFILE('IERSPOL',NFLCOL,MAXFIL,NFIL,FILNAM)

C
C WRITE TITLE AND FILE NAMES
C --------------------------
      CALL pritit ('POLINT','Differences of Pole Files')
      CALL prflna
C
C READ INPUT OPTIONS
C ------------------
      CALL PIINPT(TITNEW,ICONT,ISUBAP,IDOUBLE,
     1            NUTNAM,SUBNAM,RECORDS,SAMPL)

C
C OPEN PLOT-FILE
C --------------
      CALL GTFLNA(0,'PLOTRS ',FILPLT,IRCPLT)
      IF (IRCPLT.EQ.0) THEN
        CALL OPNFIL(LFNPLT,FILPLT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILPLT,'POLINT')
C
C WRITE HEADER FOR PLOT FILE (SKELETON KEY: A)
        CALL GTFLNA(0,'PLOTSKL',FILSKL,IRCSKL)
        IF (IRCSKL.EQ.0) THEN
          FIELDS(1)=TITNEW(1:61)
          IPLFLG(1)=1
          CALL WTSKEL('A  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        ENDIF
      ENDIF

C
C READ FILE LIST AND RECORD NUMBERS (uniline)
C ---------------------------------
      CALL gtfile2('EPFIL', 1, nfil, filinp)



C WRITE HEADER OF POLE FILE IN IERS FORMAT
C ----------------------------------------
      CALL GTFLNA(0,'IERSPOL',FILIER,IRCIER)
      IF (IRCIER.EQ.0) THEN
        CALL OPNFIL(LFNRES,FILIER,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILIER,'POLINT')

        IF (ISUBAP .EQ. 1) THEN
          SUBNAM = 'NONE'
        END IF
C
        CALL WTPOLH(LFNRES,2,TITNEW,POLTYP,NUTNAM,SUBNAM)
      ENDIF
C
      DO 10 I1=3,5
        DJUMP(I1)=0.D0
10    CONTINUE
C
      ISUBFL=0
      GPSUTCOLD=0.D0
      POLTIO=0.D0
C
C LOOP OVER ALL POLE FILES IN IERS FORMAT
C ---------------------------------------

      IERB1D=1
      DO 100 IFIL=1,NFIL
C
C OPEN POLE RESULT FILE
        CALL OPNFIL(LFN001,filinp(1,ifil),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,filinp(1,IFIL),'POLINT')
        IFORM=0
C
C READ POLE FILES
C ---------------

C READ ALL RECORDS AND FORM THE DIFFERENCES TO THE A PRIORI POLE
         ISMPL=-1
         DO 90 ILIN=1,10000
           CALL RDPOL(LFN001,IERB1D,IFORM,CODTIT,POLTYP,POLTIM,POLXYT,
     1           POLRAT,GPSUTC,REM,RMSXYT,RMSRAT,IRCODE,NUTDUM,SUBDUM)

          IF(IRCODE.EQ.1) GOTO 95
C
          IF (ILIN.LT.RECORDS(1)) GOTO 90
          ISMPL=ISMPL+1
          IF (MOD(ISMPL,SAMPL).NE.0) GOTO 90
          IF (ILIN.GT.RECORDS(2)) GOTO 95

! ADD SUBDAILY MODEL IF NECESSARY
! -------------------------------
          IF (ISUBAP.EQ.1) THEN
            CALL subpol(POLTIM,subnam,erpsub,erpsur)
            POLXYT(1)=POLXYT(1)+erpsub(1)
            POLXYT(2)=POLXYT(2)+erpsub(2)
            POLXYT(3)=POLXYT(3)+erpsub(3)

! CORRECT X-RATE, Y-RATE, AND LOD
            POLRAT(1) = POLRAT(1)+erpsur(1)
            POLRAT(2) = POLRAT(2)+erpsur(2)
            POLRAT(3) = POLRAT(3)+erpsur(3)
          ENDIF
C
C CONTINUITY BETWEEN SETS
          IF (ICONT.EQ.1) THEN
            IF (IFIL.NE.1 .AND. ILIN.EQ.RECORDS(1)) THEN
              DO 220 I1=3,5
                DJUMP(I1)=POLXYT(I1)-POLSAV(I1)
220           CONTINUE
            ENDIF
            DO 210 I1=3,5
              POLXYT(I1)=POLXYT(I1)-DJUMP(I1)
              IF (ILIN.EQ.RECORDS(2)) POLSAV(I1)=POLXYT(I1)
210         CONTINUE
          ENDIF
C
C SKIP DOUBLE EPOCHS (IF FLAG IDOUBLE=0)
C --------------------------------------
          IF (IDOUBLE.EQ.0 .AND.
     1        DABS(POLTIM-POLTIO).LT.1.D-6) GOTO 90
C
          POLTIO=POLTIM
C
C GET A PRIORI POLE VALUES (FOR PLOT OR GPS-UTC FOR BERNESE FORMAT)
C -----------------------------------------------------------------
          IF (IRCPLT.EQ.0) THEN
            CALL POLDEF(POLTIM,ISUBFL,POLAPR(1),POLAPR(2),
     1                  POLAPR(3),GPSUTC)
C
            GPSUTCNEW=GPSUTC
            IF (GPSUTCOLD.EQ.0.0) THEN
              GPSUTCOLD=GPSUTC
            ENDIF
            IF (GPSUTCNEW-GPSUTCOLD.GT.0.0) THEN
              GPSUTCOLD=GPSUTCNEW
            ENDIF
C
            POLAPR(1)=POLAPR(1)*ars
            POLAPR(2)=POLAPR(2)*ars
            POLAPR(3)=POLAPR(3)*86400.D0
            GPSUTC   =GPSUTC*86400.D0
C
C GET A PRIORI CELESTIAL POLE OFFSETS
            CALL CPODEF(POLTIM,POLAPR(4),POLAPR(5))
            POLAPR(4)=POLAPR(4)*ars
            POLAPR(5)=POLAPR(5)*ars
          ENDIF
C
C WRITE IERS POLE FILE RECORD
          IF (IRCIER.EQ.0) THEN
C
C ROUND TO NEAREST MINUTE
            POLTIM=POLTIM+30.D0/86400.D0
            CALL RADGMS(3,POLTIM,SNGSTR,IHOUR,MIN,SEC)
            POLTIM=DINT(POLTIM)+IHOUR/24.D0+MIN/1440.D0
C
            CALL WTIEPI(LFNRES,POLTIM,POLXYT,POLRAT,RMSXYT,
     1                  RMSRAT,POLCOR,NRFPOL)
C
          ENDIF
C
C WRITE PLOT FILE
          IF (IRCPLT.EQ.0) THEN
C
C REFERENCE TIME FOR PLOT
            IF (IFIL.EQ.1. AND. ILIN.EQ.RECORDS(1)) THEN
              CALL JMT(POLTIM,IYEAR,IMONTH,DAY)
              T0PLT=DJUL(IYEAR,1,1.D0)
            ENDIF
            DAY=POLTIM-T0PLT+1
C
            WRITE(LFNPLT,121) IFIL,DAY,POLTIM,
     1                        (POLXYT(II)-POLAPR(II),II=1,5)
121         FORMAT(I4,F10.4,F15.4,5F15.8)
          ENDIF
90      CONTINUE
C
C CLOSE FILE
95      CLOSE(UNIT=LFN001)
C
C NEXT POLE FILE
C --------------
100   CONTINUE

C CLOSE IERS POLE FILE
      IF (IRCIER.EQ.0) THEN
        CLOSE(UNIT=LFNRES)
      ENDIF
C
C WRITE LAST PART OF PLOT FILE
      IF (IRCPLT.EQ.0.AND.IRCSKL.EQ.0) THEN
        CALL WTSKEL('B  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        CLOSE(UNIT=LFNPLT)
      ENDIF
C
      GOTO 999

C
C END
C ---
999   CONTINUE
      CALL EXITRC(0)
      END
