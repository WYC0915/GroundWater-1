C*
      PROGRAM OBSSPL
CC
CC NAME       :  OBSSPL
CC
CC PURPOSE    :  SPLIT OBSERVATION FILE INTO TWO FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, L.MERVART
CC
CC CREATED    :  89/03/28 10:16
CC
CC CHANGES    :  26-JUL-91 : ??: AMBIGUITIES OUTSIDE FIRST FILE WERE NOT
CC                               CORRECTLY REMOVED
CC               23-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               31-JUL-93 : ??: NEW FORMAT
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               12-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               03-JAN-01 : HB: INCREASED MAXFIL TO 1000 (SYNCHRONIZATION
CC                               WITH SR OBSSPS)
CC               20-SEP-01 : HB: SWITCH TO NEW MENU
CC               16-DEC-01 : HU: D_CONST ADDED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               09-OCT-03 : RD: BUG FIXED (COMPUTE IEPSPL FOR EACH FILE)
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               26-JAN-11 : LP: Sat.-specific obstypes
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC               05-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: fileNameLength, lfnErr, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY : maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY : date,time
      USE d_rinex3, ONLY: t_gobsdef
      USE s_rdobsi
      USE s_opnfil
      USE s_prflna
      USE s_wtobsi
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_obssin
      USE s_rdhead
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_wthead
      USE s_gtflna
      USE f_tstflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAM1  , IAM2  , IAMB  , ICHNXT, ICHR  , IDELTT, IEPO  ,
     1          IEPOCH, IFIL  , IFLG  , IFREQ , IFRMAT, IFRQ  , IOSTAT,
     2          IPART , IPARTF, IPROC , IRC   , IRETRN, IRMARK, ISA1  ,
     3          ISAT  , ISAT1 , ISATE1, LFNOBS, LFNSCR, MAXAMB, MAXFIL,
     4          MEATYP, MXCAMB, MXCFIL, MXCSAT, NDIFF , NEPFL1,
     5          NEPFLG, NEPOC1, NEPOCH, NFREQ , NFRQ  , NFTOT , NSAT  ,
     6          NSATE1, NSATEL, NUMAM1, NUMAMB
C
      REAL*8    OBSTIM, TIMLS1, TIMRE1, TIMREF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXAMB= 600,MAXFIL=1000)
C
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXAMB: MAXIMUM NUMBER OF AMBIGUITIES FOR ONE SATELLITE IN A FILE
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE PROCESSED
C
C DECLARATIONS
C ------------
C
      TYPE(t_gobsdef) :: gobsdef ! Giove External Obs. Selection info
C
      CHARACTER*53  TITLE
      CHARACTER*36  CHRORD
      CHARACTER*32  SCRFIL
      CHARACTER*16  CAMPGN,STANAM(2)
      CHARACTER*20  RECTYP(2),ANTTYP(2),OPRNAM(2)
      CHARACTER*9   CRDATE(2),CRDAT1(2)
      CHARACTER*6   MXNSAT,MXNAMB,MXNFIL
      CHARACTER*5   CRTIME(2),CRTIM1(2)
      CHARACTER*4   CSESS(2),CSESS1(2)
      CHARACTER*1   OBSFLG(MAXSAT,2),EPOFLG
      CHARACTER(LEN=fileNameLength),                                     ! list of
     1              DIMENSION(:,:), POINTER :: filNam                    ! observation files

C
      REAL*8     POSECC(3,2),AMBIGU(MAXAMB,3)
      REAL*8     AMBIG1(MAXAMB,3)
      REAL*8     OBSERV(MAXSAT,2),TIMSPL,DELTAT(2)
C
      INTEGER*4  IRUNIT(2),IANTEN(2),ICLOCK(2)
      INTEGER*4  NUMSA1(MAXSAT),NUMOB1(MAXSAT,2)
      INTEGER*4  NUMSAT(MAXSAT),NUMOBS(MAXSAT,2)
      INTEGER*4  NUMMRK(MAXSAT,2),AMBIEP(MAXAMB),AMBSAT(MAXAMB)
      INTEGER*4  NUMMR1(MAXSAT,2),AMBIE1(MAXAMB),AMBSA1(MAXAMB)
      INTEGER*4  AMBCLS(MAXAMB,3),AMBWLF(MAXAMB,2)
      INTEGER*4  AMBCL1(MAXAMB,3),AMBWL1(MAXAMB,2)
      INTEGER*4  IFRQS(2),NRSAT(MAXSAT),IEPSPL,USEGEOS
C
C COMMON BLOCKS
C -------------
      COMMON/LARGES/AMBIGU,AMBIG1,AMBIEP,AMBIE1
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFIL/MXCFIL,MXNFIL
C
      DATA CHRORD/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNOBS=LFN001
      LFNSCR=LFN001+1
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(filNam)
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
      IFRQS(1)=1
      IFRQS(2)=2

! Write title and file list
! -------------------------
      CALL pritit('OBSSPL','Split observation files')
      CALL prflna

C READ OPTION INPUT FILE
C ----------------------
      CALL OBSSIN(IEPSPL,TIMSPL,FILNAM,NFTOT)
C
C GET NAME OF SCRATCH FILE
C ------------------------
      CALL GTFLNA(1,'AUXFIL',SCRFIL,IRC)
C
C LOOP OVER ALL FILES
C -------------------
      DO 500 IFIL=1,NFTOT
C
C READ HEADER FILE
C ----------------
        USEGEOS=0
        GOBSDEF%NOREC=0
        CALL RDHEAD(FILNAM(1,IFIL),
     1              MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2              CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3              CRTIME,IRMARK,NEPFLG,IFRMAT,
     4              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5              OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7              1,USEGEOS=USEGEOS,GOBSDEF=GOBSDEF)
C
C DEFINE THE SPLIT EPOCH
C ----------------------
        IF (TIMSPL.NE.0.D0)
     1    IEPSPL=IDNINT((TIMSPL-TIMREF)*86400.D0/IDELTT+1.D0)

C
C CHECK WHETHER SPLIT EPOCH IS WITHIN THE FILE
C --------------------------------------------
        IF(IEPSPL.LT.0 .OR. IEPSPL.GT.NEPOCH) THEN
          WRITE(LFNERR,902) IEPSPL,FILNAM(1,IFIL)
902       FORMAT(/,' *** PG OBSSPL: SPLIT EPOCH OUTSIDE FILE',/,
     1                         16X,'SPLIT EPOCH NUMBER:',I6,/,
     2                         16X,'FILE              : ',A32,/)
          GOTO 500
        ENDIF
C
C GO TWICE THROUGH THE ORIGINAL OBSERVATION FILE
C ----------------------------------------------
        DO 400 IPART=1,2
          IPARTF=(IPART-1)*2+1
C
C OPEN OBSERVATION FILE AND SCRATCH FILE OR SECOND OUTPUT FILE
C ------------------------------------------------------------
          CALL OPNFIL(LFNOBS,FILNAM(2,IFIL),'OLD','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNOBS,IOSTAT,FILNAM(2,IFIL),'OBSSPL')
          IF(IPART.EQ.1) THEN
            CALL OPNFIL(LFNSCR,SCRFIL,'UNKNOWN','UNFORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNSCR,IOSTAT,SCRFIL,'OBSSPL')
          ELSE
            CALL OPNFIL(LFNSCR,FILNAM(4,IFIL),'UNKNOWN','UNFORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNSCR,IOSTAT,FILNAM(4,IFIL),'OBSSPL')
          ENDIF
C
C INITIALIZE NUMBER OF SATELLITES
C -------------------------------
          NSATE1=0
          TIMRE1=1.D20
          TIMLS1=0.D0
          NEPFL1=0
C
C LOOP OVER ALL EPOCHS
C --------------------
          DO 100 IEPO=1,NEPOCH
C
C READ OBSERVATIONS
C -----------------
            CALL RDOBSI(LFNOBS,IFRMAT,NFREQ,IFRQS,OBSTIM,DELTAT,EPOFLG,
     1                  NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
C
C CHECK END OF FILE
C -----------------
            IF(IRETRN.EQ.1) GOTO 110
C
C CHECK, IF EPOCH BELONGS TO THE FIRST OR SECOND PART
C ---------------------------------------------------
            IEPOCH=IDNINT((OBSTIM-TIMREF)*86400.D0/IDELTT+1.D0)
            IPROC=0
            IF(IPART.EQ.1) THEN
              IF(IEPOCH.LT.IEPSPL) IPROC=1
            ELSE
              IF(IEPOCH.GE.IEPSPL) IPROC=1
            ENDIF
            IF(IPROC.EQ.0) GOTO 100
C
C FIRST AND LAST OBSERVATION TIME
C -------------------------------
            IF(OBSTIM.LT.TIMRE1) TIMRE1=OBSTIM
            IF(OBSTIM.GT.TIMLS1) TIMLS1=OBSTIM
C
C COUNT EPOCH FLAGS
C -----------------
            IF(TSTFLG(EPOFLG,0)) NEPFL1=NEPFL1+1
C
C LOOP OVER ALL SATELLITES OF EPOCH
C ---------------------------------
            DO 50 ISAT=1,NSAT
C
C FIND SATELLITE IN SATELLITE LIST
              DO 10 ISATE1=1,NSATE1
                IF(NUMSA1(ISATE1).EQ.NRSAT(ISAT)) GOTO 30
10            CONTINUE
C
C NEW SATELLITE: UPDATE SATELLITE LIST, INITIALIZE ARRAYS
              NSATE1=NSATE1+1
              NUMSA1(NSATE1)=NRSAT(ISAT)
              DO 20 IFRQ=1,NFREQ
                NUMOB1(NSATE1,IFRQ)=0
                NUMMR1(NSATE1,IFRQ)=0
20            CONTINUE
              ISATE1=NSATE1
C
30            CONTINUE
C
C UPDATE NUMBER OF OBSERVATIONS
              DO 40 IFRQ=1,NFREQ
                IF(OBSERV(ISAT,IFRQ).NE.0.D0) THEN
                  IF(TSTFLG(OBSFLG(ISAT,IFRQ),0)) THEN
                    NUMMR1(ISATE1,IFRQ)=NUMMR1(ISATE1,IFRQ)+1
                  ELSE
                    NUMOB1(ISATE1,IFRQ)=NUMOB1(ISATE1,IFRQ)+1
                  ENDIF
                ENDIF
40            CONTINUE
50          CONTINUE
C
C WRITE OBSERVATIONS
C ------------------
            CALL WTOBSI(LFNSCR,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                  NSAT,NRSAT,OBSFLG,OBSERV)

100       CONTINUE
C
C CLOSE OBSERVATION AND SCRATCH FILE OR SECOND OUTPUT FILE
C --------------------------------------------------------
110       CLOSE(UNIT=LFNOBS)
          CLOSE(UNIT=LFNSCR)
C
C CREATION AND MODIFICATION DATE AND TIME
C ---------------------------------------
          IF(IPART.EQ.1) THEN
            CRDAT1(1)=CRDATE(1)
            CRDAT1(2)=DATE
            CRTIM1(1)=CRTIME(1)
            CRTIM1(2)=TIME
          ELSE
            CRDAT1(1)=DATE
            CRDAT1(2)=DATE
            CRTIM1(1)=TIME
            CRTIM1(2)=TIME
          ENDIF
C
C NUMBER OF EPOCHS
C ----------------
          NEPOC1=IDNINT((TIMLS1-TIMRE1)/IDELTT*86400.D0)+1
C
C SESSION NUMBERS
C ---------------
          CSESS1(1)=CSESS(1)
          IF(IPART.EQ.1) THEN
            CSESS1(2)=CSESS(2)
          ELSE
            ICHR  =INDEX(CHRORD,CSESS(2)(1:1))
            ICHNXT=MOD(ICHR+1,37)
            CSESS1(2)=CHRORD(ICHNXT:ICHNXT)
          ENDIF
C
C NUMBER OF FREQUENCIES FOR AMBIGUITIES
C -------------------------------------
          IF(NFREQ.EQ.1) THEN
            NFRQ=1
          ELSE
            NFRQ=3
          ENDIF
C
C UPDATE AMBIGUITIES
C ------------------
          NUMAM1=0
C
          IF (IPART.EQ.2) THEN
            DO 230 ISAT1=1,NSATE1
              NUMAM1=NUMAM1+1
              AMBIE1(NUMAM1) = -1
              AMBSA1(NUMAM1)=NUMSA1(ISAT1)
230         CONTINUE
          END IF
C
          DO 200 IAMB=1,NUMAMB
            IFLG=0
            DO 240 ISA1=1,NSATE1
              IF ((NUMSA1(ISA1).EQ.AMBSAT(IAMB)) .AND.
     1            (AMBIEP(IAMB).LT.IEPSPL .OR. IPART.EQ.2)) IFLG=1
240         CONTINUE
            IF (IFLG.EQ.0) GO TO 200
C
            IF (IPART.EQ.1 .AND. AMBIEP(IAMB).LT.IEPSPL) THEN
              NUMAM1=NUMAM1+1
              IAM2  =NUMAM1
              AMBIE1(IAM2) = AMBIEP(IAMB)
            ELSE IF (IPART.EQ.2 .AND. AMBIEP(IAMB).GT.IEPSPL) THEN
              NUMAM1=NUMAM1+1
              IAM2  =NUMAM1
              AMBIE1(IAM2) = AMBIEP(IAMB)-IEPSPL+1
            ELSE IF (IPART.EQ.2) THEN
              IAM2=0
              DO 210 IAM1=1,NSATE1
                IF ( AMBSA1(IAM1).EQ.AMBSAT(IAMB)  .AND.
     1              -AMBIE1(IAM1).LE.AMBIEP(IAMB)) THEN
                  AMBIE1(IAM1) = - AMBIEP(IAMB)
                  IAM2=IAM1
                END IF
210           CONTINUE
              IF (IAM2.EQ.0) GO TO 200
            ELSE
              GO TO 200
            END IF
C
            AMBSA1(IAM2)=AMBSAT(IAMB)
            DO 260 IFREQ=1,NFREQ
              AMBWL1(IAM2,IFREQ) = AMBWLF(IAMB,IFREQ)
260         CONTINUE
            DO 270 IFRQ=1,NFRQ
              AMBIG1(IAM2,IFRQ) = AMBIGU(IAMB,IFRQ)
              AMBCL1(IAM2,IFRQ) = AMBCLS(IAMB,IFRQ)
270         CONTINUE
200       CONTINUE
C
          IF (IPART.EQ.2) THEN
            DO 220 IAM1=1,NUMAM1
              IF (AMBIE1(IAM1).LT.0) AMBIE1(IAM1)=1
220         CONTINUE
          END IF
C
C WRITE HEADER FILE
C -----------------
          CALL WTHEAD(FILNAM(IPARTF,IFIL),
     1                MEATYP,NDIFF,NFREQ,NEPOC1,NSATE1,
     2                CSESS1,IDELTT,TIMRE1,CAMPGN,TITLE,CRDAT1,
     3                CRTIM1,IRMARK,NEPFL1,IFRMAT,
     4                STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5                OPRNAM,POSECC,ICLOCK,NUMSA1,NUMOB1,NUMMR1,
     6                NUMAM1,AMBSA1,AMBIE1,AMBWL1,AMBIG1,AMBCL1,
     7                USEGEOS,GOBSDEF)
C
C NEXT PART
400     CONTINUE
C
C REOPEN SCRATCH FILE AND ORIGINAL OBSERVATION FILE
C -------------------------------------------------
        CALL OPNFIL(LFNOBS,FILNAM(2,IFIL),'OLD','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOBS,IOSTAT,FILNAM(2,IFIL),'OBSSPL')
        CALL OPNFIL(LFNSCR,SCRFIL,'OLD','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNSCR,IOSTAT,SCRFIL,'OBSSPL')
C
C COPY FIRST PART OF ORIGINAL FILE FROM SCRATCH FILE TO ORIGINAL FILE
C -------------------------------------------------------------------
        DO 410 IEPO=1,NEPOCH
C
C READ OBSERVATIONS
          CALL RDOBSI(LFNSCR,IFRMAT,NFREQ,IFRQS,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
C
C CHECK END OF FILE
          IF(IRETRN.EQ.1) GOTO 420
C
C WRITE OBSERVATIONS
          CALL WTOBSI(LFNOBS,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV)
410     CONTINUE
C
C CLOSE OBSERVATION AND SCRATCH FILE
C ----------------------------------
420     CLOSE(UNIT=LFNOBS)
        CLOSE(UNIT=LFNSCR)
C
        IF (ASSOCIATED(GOBSDEF%SAT)) THEN
            DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
        ENDIF
C
500   CONTINUE
C
C END OF LOOP OVER ALL FILES
C ----------------------------------------------------------------------
      CALL EXITRC(0)
      END
