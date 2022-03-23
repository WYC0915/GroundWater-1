      MODULE s_WRTRNX
      CONTAINS

C*
      SUBROUTINE WRTRNX(MAXREC,MAXCMB,FILOLD,FILNEW,IFXSLP,IFLCOD,
     1                  IFLPHS,NRSAT ,NUMSAT,NEPOCH,OBSTIM,OBSREC,
     2                  OBSFLG,SMPINT,CODTYP,USEGEOS,GOBSDEF)
CC
CC NAME       :  WRTRNX
CC
CC PURPOSE    :  READ ALL THE OBSERVATIONS OF A RINEX FILE INTO MEMORY
CC
CC PARAMETERS :
CC        IN  :  MAXREC : MAXIMUM NUMBER OF RECORDS           I*4
CC               MAXCMB : MAXIMUM NUMBER OF OBSERVATION TYPES I*4
CC                        AND COMBINATIONS
CC               FILOLD : NAME OF THE RINEX FILE READ        CH*(*)
CC               FILNEW : NAME OF THE RINEX FILE WRITE       CH*(*)
CC               IFXSLP : FIX CYCLE SLIPS OR NOT (0/1)        I*4
CC               IFLCOD : USE RAW OR SMOOTH CODE : 0= RAW     I*4
CC                                                 1= SMOOTH
CC               IFLPHS : FLAG BAD PHASE  OR NOT (0/1)        I*4
CC               NRSAT  : NUMBER OF OBSERVED SATELLITES       I*4
CC               NUMSAT : SATELLITE NUMBERS                   I*4(*)
CC               NEPOCH : NUMBER OF POINTS IN OBS ARRAYS      I*4
CC               OBSTIM : ARRAY WITH TIME VALUES              R*8(*)
CC               OBSREC : ARRAY WITH OBSERVATIONS             R*8(*,*,*)
CC               OBSFLG : ARRAY WITH OBSERVATION FLAGS       CH*1(*,*,*)
CC               SMPINT : REQUESTED SAMPLING INTERVAL (SEC)   R*8
CC                        =0: TAKE ALL OBSERVATIONS
CC               CODTYP : FIRST FREQENCY CODE TYPE (P1/C1)   CH*2
CC               USEGEOS: USE GIOVE EXTERN. OBS. SELECTION    I*4
CC               GOBSDEF: GIOVE EXTERN. OBS. SEL. INFO
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC CREATED    :  22-JUL-1996
CC
CC CHANGES    :  22-JUL-96 : TS: CREATED
CC               13-MAY-97 : MR: REMOVE UNUSED VARIABLE "CH1"
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               20-MAY-98 : TS: ADDED PHASE FLAG OPTION (IFLPHS)
CC               31-AUG-98 : DI: SET MAXCHN=24
CC               15-JAN-02 : MM: SET MAXTYP=10, DIMTST
CC               15-AUG-02 : SS: UPDATE RINEX INTERVAL VALUE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : SS: CONSIDER FIRST FREQENCY CODE TYPE
CC               20-MAY-03 : HU: OPEN FILENAME LENGTH
CC               30-JUN-03 : HB: ADOPT FOR CHANGES in SR R2RDOH
CC                               (SPACEBORNE)
CC               05-FEB-04 : RD: WRITE NO P1==P2 DATA RECORDS
CC               29-APR-04 : RD: SN-FLAG==2 MEANS COPY OF INPUT FOR CODE+PHASE
CC               03-MAY-04 : RD: COPY ALSO INCOMPLETE CODE/PHASE DATASETS
CC               13-JAN-05 : RD: SOLVE ROUNDING PROBLEM
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-AUG-06 : HU: MAXTYP 10 -> 12
CC               16-MAY-07 : AG: MAXCHN 24 -> 40, IF (MOD(IRXVRS,100) == 3 -> 2
CC               08-JUL-10 : MM: ANTTYP and RECTYP LEN=20 -> LEN=40
CC               03-NOV-10 : DT: MAXTYP 12->18
CC               18-JAN-11 : SL: MAXTYP FROM D_RINEX3
CC               26-JAN-11 : LP: Sat-specific obs types; DEFREQ changes
CC               13-SEP-11 : LP: MAXCOM 10->30
CC               09-FEB-12 : LP: MAXCOM 30->60
CC               29-FEB-12 : RD: USE R2WTOH and R2WTOR AS MODULE
CC               26-APR-12 : LP: MAXCOM 60->150
CC               30-MAY-12 : LP: WRITE GEOS COMMENT ONLY IF OBSTYPES AVAILABLE
CC               07-MAR-13 : SS: MAXCOM FROM 150 TO 300 (DUE TO BISK/POUS)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern
      USE m_maxdim,  ONLY: MAXSAT
      USE d_rinex3,  ONLY: maxtyp,t_gobsdef
      USE s_dimtst
      USE s_opnfil
      USE f_tstflg
      USE s_r2rdoh
      USE s_r2wtoh
      USE s_opnerr
      USE s_r2wtor
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPO  , IFLAG , IFLCOD, IFLPHS, IFXSLP, INTER , INTER0,
     1          IOSTAT, IRC   , IRCODE, IRXVRS, ISAT  , MAXCHN, MAXCMB,
     2          MAXCOM, MAXREC, NCOM  , NEPOCH, NOBSTP, NRANT ,
     3          NRSAT , NRUNIT, NSATEL, NSATEP, NUMLIN, NWLSAT,ipos,
     4          listi4, igeos , satnumg
C
      REAL*8    SMPINT, TFIRST, TLAST
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
C
      PARAMETER(MAXCHN=40,MAXCOM=300)
C
C MAXCHN: MAXIMUM NUMBER OF CHANNELS
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXTYP: MAXIMUM NUMBER OF OBSERVATION TYPES IN RINEX FILE
C MAXCOM: MAXIMUM NUMBER OF COMMENT LINES
C MAXREC: MAXIMUM NUMBER OF OBS. PER SATELLITE IN FILE (24 HOURS WITH 30SEC SAMPLING)
C MAXARC: MAXIMUM NUMBER OF ARCS PER SATELLITE
C MAXCMB: MAXIMUM NUMBER OF OBSERVATION TYPES USED
C
C
C GLOBAL VARIABLES
C ----------------
      REAL*8       OBSTIM(MAXREC),OBSREC(MAXREC,MAXCMB,MAXSAT)
C
      CHARACTER*(*) FILOLD,FILNEW
      CHARACTER*2  CODTYP
      CHARACTER*1  OBSFLG(MAXREC,MAXCMB,MAXSAT)
C
      TYPE(t_gobsdef)  :: gobsdef ! Giove External Obs. Selection info
C
C LOCAL VARIABLES
C ---------------
      CHARACTER    OPNAME*20,AGENCY*40,ANTTYP*40,RECTYP*40
      CHARACTER    COMENT(MAXCOM)*60
      CHARACTER    CRDATE*9,CRTIME*5
      CHARACTER    PRGNAM*20,RCVERS*20,OBSTYP(MAXTYP)*2
      CHARACTER    RUNBY*20,SITNAM*60,SITNUM*40
C
      INTEGER*4    IWLFAC(2),IWLSAT(3,MAXSAT),NUMSAH(MAXSAT)
      INTEGER*4    SATEPO(MAXCHN),NUMSAT(MAXSAT),NUMOBS(MAXSAT,MAXTYP)
      INTEGER*4    LLI(MAXCHN,MAXTYP),ISIGN(MAXCHN,MAXTYP),SATCOM
      INTEGER*4    ISNMIN(MAXTYP),ISNTHR(MAXTYP),ISNMAX(MAXTYP),usegeos
C
      REAL*8       POSECC(3,3),SIGNAL(MAXCHN,MAXTYP)
      REAL*8       POSXYZ(3),OBSEPO(MAXCHN,MAXTYP),EPOCH(2)
C
      COMMON/CCRINX/SIGNAL,OBSEPO,NUMOBS,LLI,ISIGN
C
C
C OPEN ORIGINAL RINEX FILE
C ------------------------
      CALL OPNFIL(LFN001,FILOLD,'OLD','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILOLD,'WRTRNX')
C
C READ ORIGINAL RINEX HEADER
C --------------------------
      NUMLIN=0
      CALL R2RDOH(LFN001,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1            PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2            SITNAM,SITNUM,OPNAME,AGENCY,
     3            NRUNIT,RECTYP,RCVERS,
     4            NRANT ,ANTTYP,POSXYZ,POSECC,
     5            IWLFAC,IWLSAT,NWLSAT,
     6            NOBSTP,OBSTYP,INTER, TFIRST,TLAST,
     7            NSATEL,NUMSAH,NUMOBS,IRXVRS,IRC)
      IF(IRC.NE.0) THEN
        WRITE(LFNERR,901)IRC
901     FORMAT(/,' ### SR: WRTRNX: ERROR READING RINEX HEADER!',/,
     1                        16X,'RET.CODE OF SR R2RDOH: ',I4,/,
     2                        16X,'STOP PROCESSING THIS FILE',/)
        IRC=1
        RETURN
      END IF
C
C CHECK DIMENSIONS
C ----------------
      CALL DIMTST(1,2,2,'WRTRNX','MAXTYP',
     1            'NUMBER OF OBSERVATION TYPES',' ',NOBSTP,MAXTYP,IRC)
C
C SET OBSTYP TO 4 (L1, L2, P1, P2)
C --------------------------------
      NOBSTP=4
      OBSTYP(1)='L1'
      OBSTYP(2)='L2'
      OBSTYP(3)=CODTYP
      OBSTYP(4)='P2'
C
C UPDATE RINEX INTERVAL VALUE
C ---------------------------
      INTER0=NINT(SMPINT)
      IF (INTER.LT.INTER0 .AND. INTER.NE.0) INTER=INTER0
C
C CLOSE ORIGINAL RINEX FILE
C -------------------------
      CLOSE(LFN001)
C
C OPEN NEW RINEX FILE
C -------------------
      CALL OPNFIL(LFN001,FILNEW,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNEW,'WRTRNX')
C
C ADD COMMENT ABOUT RNXSMT
C ------------------------
      IF (NCOM.LT.MAXCOM) THEN
        NCOM=NCOM+1
      ELSE
        NCOM=MAXCOM
      ENDIF
      WRITE(COMENT(NCOM),11)
11    FORMAT(1X,' ### PG RNXSMT: RINEX FILE CHANGED')
C
C ADD COMMENT ABOUT Sat-specific obs. selection
C ---------------------------------------------
      IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
        SATCOM = 0
        DO igeos=1,gobsdef%norec
         satnumg=gobsdef%sat(igeos)%sysnum*100+gobsdef%sat(igeos)%satnum
         IF ((gobsdef%sat(igeos)%obstyp(1).EQ.'   ').AND.
     1       (gobsdef%sat(igeos)%obstyp(2).eq.'   ').AND.
     2       (gobsdef%sat(igeos)%obstyp(3).eq.'   ').AND.
     3       (gobsdef%sat(igeos)%obstyp(4).eq.'   ')) CYCLE
         IF (NCOM.LT.MAXCOM) THEN
          NCOM=NCOM+1
         ELSE
          NCOM=MAXCOM
          WRITE(LFNERR,12)
12        FORMAT(/,'### PG RNXSMT: MAXIMUM #
     1           OF COMMENT LINES REACHED',/)
         ENDIF
         SATCOM=SATCOM+1
         WRITE(COMENT(NCOM),13)SATCOM,gobsdef%sat(igeos)%syschar,satnumg
     1        ,gobsdef%sat(igeos)%obstyp(1),gobsdef%sat(igeos)%obstyp(2)
     2        ,gobsdef%sat(igeos)%obstyp(3),gobsdef%sat(igeos)%obstyp(4)
13       FORMAT('GEOS:',1X,I2,1X,A1,I3,1X,A3,1X,A3,1X,A3,1X,A3)
        ENDDO
      ENDIF
C
C WRITE NEW RINEX HEADER
C ----------------------
      IF (MOD(IRXVRS,100) == 3) IRXVRS = IRXVRS - 1
      CALL R2WTOH(LFN001,LFNERR,MAXSAT,
     1            PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2            SITNAM,SITNUM,OPNAME,AGENCY,
     3            NRUNIT,RECTYP,RCVERS,
     4            NRANT ,ANTTYP,POSXYZ,POSECC,
     5            IWLFAC,IWLSAT,NWLSAT,
     6            NOBSTP,OBSTYP,INTER, TFIRST,TLAST,
     7            NSATEL,NUMSAH,NUMOBS,IRXVRS,IRC)
      IF(IRC.NE.0) THEN
        WRITE(LFNERR,902)IRC
902     FORMAT(/,' ### SR: WRTRNX: ERROR WRITING RINEX HEADER!',/,
     1                        16X,'RET.CODE OF SR R2WTOH: ',I4,/,
     2                        16X,'STOP PROCESSING THIS FILE',/)
        IRC=1
        RETURN
      END IF
C
C LOOP OVER ALL RECORDS
C ---------------------
      ISNMIN(1)=1
      ISNMIN(2)=1
      ISNMIN(3)=1
      ISNMIN(4)=1
      ISNTHR(1)=5
      ISNTHR(2)=5
      ISNTHR(3)=5
      ISNTHR(4)=5
      ISNMAX(1)=9
      ISNMAX(2)=9
      ISNMAX(3)=9
      ISNMAX(4)=9
      DO 100 IEPO=1,NEPOCH
        NSATEP=0
        EPOCH(1)=OBSTIM(IEPO)
        EPOCH(2)=0D0
        IFLAG=0
        DO 110 ISAT=1,NRSAT
          IF ((OBSREC(IEPO,1,ISAT).NE.0D0) .AND.
     1        (OBSREC(IEPO,2,ISAT).NE.0D0) .AND.
     2        (OBSREC(IEPO,3,ISAT).NE.0D0) .AND.
     3        (OBSREC(IEPO,4,ISAT).NE.0D0)) THEN
            NSATEP=NSATEP+1
            SATEPO(NSATEP)=NUMSAT(ISAT)
            OBSEPO(NSATEP,1)=OBSREC(IEPO,1,ISAT)
            OBSEPO(NSATEP,2)=OBSREC(IEPO,2,ISAT)
            OBSEPO(NSATEP,3)=OBSREC(IEPO,3,ISAT)
            OBSEPO(NSATEP,4)=OBSREC(IEPO,4,ISAT)
C
C OBSERVATION RECORDS WITH P1=P2 ARE ELIMINATED IN R2RDOR
C -------------------------------------------------------
            IF (DNINT(OBSEPO(NSATEP,3)*1000d0).EQ.
     1          DNINT(OBSEPO(NSATEP,4)*1000d0))
     2        OBSEPO(NSATEP,3)=OBSEPO(NSATEP,3)+0.001d0
C
C SET SIGNAL TO NOISE (1: BAD OBS; 5: CYCLE SLIP REPAIRED; 9: OK)
C ---------------------------------------------------------------
            IF (TSTFLG(OBSFLG(IEPO,1,ISAT),0)) THEN
              SIGNAL(NSATEP,1)=1D0
              SIGNAL(NSATEP,2)=1D0
              SIGNAL(NSATEP,3)=1D0
              SIGNAL(NSATEP,4)=1D0
            ELSE IF (TSTFLG(OBSFLG(IEPO,1,ISAT),2)) THEN
              SIGNAL(NSATEP,1)=5D0
              SIGNAL(NSATEP,2)=5D0
              SIGNAL(NSATEP,3)=5D0
              SIGNAL(NSATEP,4)=5D0
            ELSE
              SIGNAL(NSATEP,1)=9D0
              SIGNAL(NSATEP,2)=9D0
              SIGNAL(NSATEP,3)=9D0
              SIGNAL(NSATEP,4)=9D0
            ENDIF
C
C SET SIGNAL TO NOISE (2: COPYIED FROM INPUT)
C -------------------------------------------
            IF (IFLPHS.NE.1) THEN
              SIGNAL(NSATEP,1)=2D0
              SIGNAL(NSATEP,2)=2D0
            ENDIF
C
            IF (IFLCOD.NE.1) THEN
              SIGNAL(NSATEP,3)=2D0
              SIGNAL(NSATEP,4)=2D0
            ENDIF
C
C SET CYCLE SLIP FLAG (L1 and L2 only)
C ------------------------------------
            IF ((TSTFLG(OBSFLG(IEPO,1,ISAT),1)  .OR.
     1           TSTFLG(OBSFLG(IEPO,1,ISAT),2)) .AND.
     2          (IFLPHS.EQ.1)) THEN
              LLI(NSATEP,1)=1
              LLI(NSATEP,2)=1
              LLI(NSATEP,3)=0
              LLI(NSATEP,4)=0
            ELSE
              LLI(NSATEP,1)=0
              LLI(NSATEP,2)=0
              LLI(NSATEP,3)=0
              LLI(NSATEP,4)=0
            ENDIF
          ELSE
C
C COPY ALL PHASE DATA IF NO PHASE PREPROCESSING REQUESTED
            IF ( IFLPHS.NE.1 .AND.
     1          (OBSREC(IEPO,1,ISAT).NE.0D0  .OR.
     2           OBSREC(IEPO,2,ISAT).NE.0D0)) THEN
C
              IF (NSATEP.EQ.0) THEN
                NSATEP=NSATEP+1
                SATEPO(NSATEP)=NUMSAT(ISAT)
C
                OBSEPO(NSATEP,1:4)=0d0
                SIGNAL(NSATEP,1:3)=0
                LLI(NSATEP,1:4)=0
              ELSEIF (SATEPO(NSATEP).NE.NUMSAT(ISAT)) THEN
                NSATEP=NSATEP+1
                SATEPO(NSATEP)=NUMSAT(ISAT)
C
                OBSEPO(NSATEP,1:4)=0d0
                SIGNAL(NSATEP,1:4)=0
                LLI(NSATEP,1:4)=0
              ENDIF
C
              OBSEPO(NSATEP,1)=OBSREC(IEPO,1,ISAT)
              OBSEPO(NSATEP,2)=OBSREC(IEPO,2,ISAT)
C
              SIGNAL(NSATEP,1)=2D0
              SIGNAL(NSATEP,2)=2D0
            ENDIF
C
C COPY ALL CODE DATA IF RAW-CODE REQUESTED
            IF ( IFLCOD.NE.1 .AND.
     1          (OBSREC(IEPO,3,ISAT).NE.0D0  .OR.
     2           OBSREC(IEPO,4,ISAT).NE.0D0)) THEN
C
              IF (NSATEP.EQ.0) THEN
                NSATEP=NSATEP+1
                SATEPO(NSATEP)=NUMSAT(ISAT)
C
                OBSEPO(NSATEP,1:4)=0d0
                SIGNAL(NSATEP,1:3)=0
                LLI(NSATEP,1:4)=0
              ELSEIF (SATEPO(NSATEP).NE.NUMSAT(ISAT)) THEN
                NSATEP=NSATEP+1
                SATEPO(NSATEP)=NUMSAT(ISAT)
C
                OBSEPO(NSATEP,1:4)=0d0
                SIGNAL(NSATEP,1:4)=0
                LLI(NSATEP,1:4)=0
              ENDIF
C
              OBSEPO(NSATEP,3)=OBSREC(IEPO,3,ISAT)
              OBSEPO(NSATEP,4)=OBSREC(IEPO,4,ISAT)
C
              SIGNAL(NSATEP,3)=2D0
              SIGNAL(NSATEP,4)=2D0
            ENDIF
          ENDIF
110     CONTINUE
C
C WRITE OBSERVATION EPOCH IN RINEX FILE
C -------------------------------------
        CALL R2WTOR(LFN001,LFNERR,MAXCHN,IRXVRS,
     1              NOBSTP,ISNMIN,ISNTHR,ISNMAX,
     2              EPOCH ,IFLAG ,NSATEP,SATEPO,
     3              OBSEPO,SIGNAL,LLI   ,IRCODE)
C
C NEXT RECORD
C -----------
100   CONTINUE
C
C END OF RINEX FILE
C -----------------
1000  CLOSE(LFN001)
C
      RETURN
      END SUBROUTINE

      END MODULE
