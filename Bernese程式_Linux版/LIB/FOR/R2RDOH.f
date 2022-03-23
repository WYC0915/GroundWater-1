      MODULE s_R2RDOH
      CONTAINS

C*
      SUBROUTINE R2RDOH(LFNOBS,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1                  PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2                  STANAM,STANUM,OPRNAM,AGENCY,
     3                  IRUNIT,RECTYP,RCVERS,
     4                  IANTEN,ANTTYP,POSXYZ,POSECC,
     5                  IWLFAC,IWLSAT,NWLSAT,
     6                  NUMTYP,OBSTYP,IDELTT,TFIRST,TLAST,
     7                  NSATEL,NUMSAT,NUMOBS,IRXVRS,IRCODE,USEGEOS,
     8                  GOBSDEF,RINSTAT)
CC
CC NAME       :  R2RDOH
CC
CC PURPOSE    :  READ THE ENTIRE HEADER INFORMATION OF A
CC               RINEX OBSERVATION FILE
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER OF RINEX FILE    I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES         I*4
CC                        CORRESPONDS TO ROW DECLARATION OF
CC                        ARRAY NUMOBS
CC               MAXCOM : MAXIMUM NUMBER OF COMMENT LINES      I*4
CC               NUMLIN : NUMBER OF LINES TO BE READ           I*4
CC                        0: FULL HEADER,
CC                           INITITIALIZE VARIABLES TO ZERO/BLANK
CC                       >0: NO INIT, NO TESTS
CC                       <0: INIT, READ ABS(NUMLIN) LINES
CC        OUT :  PRGNAM : PROGRAM NAME                        CH*20
CC               RUNBY  : NAME OF AGENCY CREATING RINEX FILE  CH*20
CC               CRDATE : CREATION DATE                       CH*9
CC               CRTIME : CREATION TIME                       CH*5
CC               NCOM   : NUMBER OF COMMENT LINES              I*4
CC               COMENT : COMENT LINES                        CH*60(*)
CC               STANAM : STATION/MARKER NAME                 CH*60
CC               STANUM : STATION/MARKER NUMBER   POS 1:20    CH*(*)
CC                        MARKER TYPE             POS 21:40
CC                                         IF LEN(STANUM) >= 40
CC               OPRNAM : OPERATOR NAME                       CH*20
CC               AGENCY : OPERATOR AGENCY                     CH*40
CC               IRUNIT : RECEIVER UNIT NUMBER                 I*4
CC               RECTYP : RECEIVER TYPE                       CH*(*)
CC               RCVERS : RECEIVER VERSION                    CH*20
CC               IANTEN : ANTENNA NUMBER                       I*4
CC               ANTTYP : ANTENNA  TYPE                       CH*(*)
CC               POSXYZ : APPROXIMATE MARKER POSITION (ITRS)   R*8(3)
CC               POSECC : POSITIONING ECCENTRICITIES IN LOCAL  R*8(3,*)
CC                        SYSTEM IN METERS
CC                          (ANTENNA BOTTOM PLANE - MARKER)
CC                          POSECC(1): ECC. IN LATITUDE
CC                          POSECC(2): ECC. IN LONGITUDE
CC                          POSECC(3): ECC. IN HEIGHT
CC                        FOR MARKER TYPE = "SPACEBORNE"
CC                          POSECC(1..3,1): DELTA X/Y/Z
CC                          POSECC(1..3,2): BORESIGHT X/Y/Z
CC                          POSECC(1..3,3): CENTER OF MASS X/Y/Z
CC               IWLFAC : DEFAULT WAVELENGTH FACTORS (L1/L2)   I*4(2)
CC                          1: CYCLE AMBIGUITIES
CC                          2: HALF-CYCLE AMBIGUITIES
CC               IWLSAT : SAT.DEPENDENT WL FACTORS (L1/L2)     I*4(3,*)
CC                          1: CYCLE AMBIGUITIES
CC                          2: HALF-CYCLE AMBIGUITIES
CC                          I=1: WLFAC(L1), I=2: WLFAC(L2), I=3: SVN
CC               NWLSAT : NUMBER OF WL-FACTORS/SVN IN LIST     I*4
CC               NUMTYP : NUMBER OF DIFFERENT OBS.TYPES        I*4
CC               OBSTYP : LIST OF OBSERVATION TYPES           CH*2(*)
CC                        FOR MARKER TYPE = "SPACEBORNE":
CC                          OBSTYP(NUMTYP+K) MAY CONTAIN FACTOR FOR
CC                          OBSTYP(K) (2 CHARACTERS HEX DATA)
CC                          --> DECLARATION OF OBSTYP 2*NUMTYP!
CC               IDELTT : OBSERVATION INTERVAL (SEC)           I*4
CC               TFIRST : FIRST OBSERVATION EPOCH              R*8
CC               TLAST  : LAST  OBSERVATION EPOCH              R*8
CC               NSATEL : NUMBER OF SATELLITES                 I*4
CC               NUMSAT : LIST OF SATELLITE NUMBERS            I*4(*)
CC               NUMOBS : NUMBER OF OBSERVATIONS USED          I*4(*,*)
CC                          NUMOBS(I,J): SATELLITE I, OBSTYP J
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC               IRCODE : RETURN CODE                          I*4
CC                        0: OK
CC                        1: WRONG FILE TYPE
CC                        2: NOT ANTICIPATED VERSION NUMBER
CC                        3: END OF FILE WITHIN HEADER
CC                        4: ERROR DECODING DATA
CC                        5: FIRST LINE NOT VERSION LINE
CC                        8: FEATURE NOT YET HANDLED
CC                        9: END OF FILE AT BEGINNING OF HEADER
CC                       10: TOO MANY SATELLITES
CC                       11: TOO MANY COMMENT LINES
CC               USEGEOS: USE GIOVE EXTERN. OBS. SELECTION     (OPT)I*4
CC               GOBSDEF: GIOVE EXTERN. OBS. SEL. INFO         (OPT)
CC               RINSTAT: OBS STATISTICS FOR A RINEX FILE      (OPT)
CC
CC REMARKS    :  - NUMLIN>0: EXACTLY NUMLIN LINES ARE READ
CC               - GLONASS FILE: ADD 100 TO IRXVRS
CC                 GALILEO FILE: ADD 200 TO IRXVRS
CC                 GEO     FILE: ADD 300 TO IRXVRS
CC                 MIXED   FILE: ADD 500 TO IRXVRS
CC                 ADD_GNSS_HERE CONFLICT BETWEEN M AND J
CC               - IF DECLARED LENGTH OF RECTYP AND ANTTYP IS 40
CC                 RECTYP(21:40) AND ANTTYP(21:40) WILL CONTAIN
CC                 ALPHANUMERIC SERIAL NUMBERS, IRUNIT AND IANTEN WILL
CC                 BE SET TO ZERO
CC               - TFIRST < 0: TIME SYSTEM IS "UTC" (GLONASS OR
CC                 MIXED GPS/GLONASS FILES)
CC               - MARKER TYPE "SPACEBORNE": SPECIAL DECLARATIONS FOR
CC                 OBSTYP, STANUM, POSECC NEEDED!
CC
CC AUTHOR     :  W.GURTNER
CC
CC CREATED    :  30-APR-91
CC
CC CHANGES    :  14-NOV-91 : ??: LEFT-JUSTIFY TEXT INPUT
CC               18-JUN-92 : EB: NO SAT STATISTIK AFTER NUMBER OF SAT
CC               06-OCT-92 : ??: SPECIAL ERROR MESSAGE IF UNKNOWN HEADER
CC                               RECORD
CC               02-JUN-93 : ??: NUMLIN<0: READ ABS(NUMLIN) LINES,
CC                               INITIALIZE VARIABLES
CC               22-FEB-94 : ??: TFIRST < 0: UTC USED AS TIME SYSTEM (GLONASS)
CC                               GLONASS FILE: ADD 100 TO IRXVRS
CC                               MIXED   FILE: ADD 500 TO IRXVRS
CC               16-AUG-94 : ??: ALPHANUMERIC SERIAL NUMBERS
CC               13-OCT-94 : ??: ALLOW FOR FREE ORDER FOR 'PRN # / NUM OBS' REC
CC               16-OCT-96 : ??: ADDED "###" TO MESSAGE "SR R2RDOR" FOR ROUTINE
CC                               CHECKS
CC               30-OCT-96 : MR: ADD "END=" FOR IBM AIX IN READ(STR,*,...)
CC               13-SEP-97 : TS: ALLOW "WAVELENGHT" JPL TYPO!!!
CC               25-SEP-97 : DI: RENAME PARAMETER MXLSAT TO MAXWLF (NEW=7!)
CC                               TEST MAXIMAL DIMENSION -> NEW IRCODE=10
CC               21-JAN-98 : WG: ALLOW FOR SATELLITE CHARACTER IN SATELLITE LIST
CC               31-MAR-98 : WG: ACCEPT LEAP SECONDS LINE
CC               22-OCT-98 : WG: GLONASS IN WAVELENGTH FACTOR HEADER LINE
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               25-OCT-99 : WG: RCV CLK OFF APPL HEADER LINE
CC               04-JAN-00 : SS: ACCEPT SHIFTED "PGM / RUN BY / DATE" RECORD
CC               30-NOV-00 : TS: INITIALIZE IWLFAC TO 1/1
CC               04-JAN-02 : ??: REMOVE TRAILING <CR>
CC               01-MAR-02 : DI: ADD ERR= TO ALL READ STATEMENTS
CC               22-AUG-02 : MR: ADD "MARKER TYPE"
CC               08-JAN-03 : DS: RENAME TYP "LP1" TO " LP" IN CHAMP FORMAT
CC               20-JAN-03 : DS: CHECK FOR NCHR
CC               30-JAN-03 : WG: GEO ADDED
CC               02-APR-03 : WG: LEO ADDED
CC               23-JUN-03 : HB: CHANGE ORDER OF READING ANTENNA OFFSETS
CC                               FOR LEOS
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-AUG-06 : WG: ALLOW MORE THAN 9 OBSTYPES
CC               24-MAY-07 : AG: FUNCTION PRN2PRN ADDED
CC               28-MAY-07 : AG: RINEX 3 IMPLEMENTED
CC               28-NOV-07 : RD: FORMAT CORRECTION FOR >9 TYPES OF OBSERV.
CC               20-AUG-08 : SS: INCREMENT "NLINE" FOR CONTINUATION LINES
CC               25-OCT-10 : SL: REAL TO INTEGER CONVERSION BUG CORRECTED
CC               18-JAN-11 : SL: MAXTYP 12->26
CC               20-JAN-11 : LP,SL: MAXTYP DEFINED IN D_RINEX3
CC               26-JAN-11 : LP: Satellite-specific observation types
CC               21-MAR-12 : SL: NEW LABEL 'SYS / PHASE SHIFT'
CC               28-MAR-12 : RD: USE LISTC1 AS MODULE NOW
CC               24-APR-12 : LP: Generalization of sat-specific obs types
CC                               (all satsys, [in RINEX2 not yet active as long
CC                                as IRXVRS issue is not solved])
CC               01-MAY-12 : LP: Replace r32r2 by OBSTYPESR2, OBSTYPESR3, and
CC                               RIN2RIN3 for RINEX3 obstype selection;indx,
CC                               listc1 removed; gobsdef applied also to RINEX2
CC               09-MAY-12 : LP: SR RINSTAT introduced
CC               21-MAY-12 : LP: Observation scale factors added to gobsdef
CC               16-AUG-12 : LP: IRCODE 11 INTRODUCED (MAXCOM EXCEEDED)
CC               20-AUG-12 : LP: Bugfix: obsindR212 index added to t_satsig for RINEX2.12
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1991     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_global, ONLY: g_rnxsys,maxsys
      USE d_rinex3, ONLY: rxohead, OBSTYPESR3, OBSTYPESR2, maxtyp,
     1                    t_gobsdef, indxs, obslistbasic, RIN2RIN3,
     2                    t_rinstat
      USE s_rdr3oh
      USE f_djul
      USE s_sjustl
      USE f_iyear4
      USE s_upperc
      USE f_prn2prn
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IANTEN, ICLKOF, IDAY  , IDELTT, IFACT , IHOUR ,
     1          IRCODE, IRUNIT, IRVERS, IRXVRS, ITEST , IYEAR ,
     2          J     , JANT  , JECC  , JFACT , JOPR  , JPGM  , JPOS  ,
     3          JRCV  , JSTA  , JTF   , JWLF  , K     , KWLSAT, LEAP  ,
     4          LFNERR, LFNOBS, MAXCOM, MAXSAT, MAXWLF, MINUTE,
     5          MONTH , NCHR  , NCOM  , NFACT , NLINE , NSATEL, NUMLIN,
     6          NUMLST, NUMTYP, NWLSAT, K1    , K2    , isat  , igeos
C
      REAL*8    DAY   , SEC   , TFIRST, TLAST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER    STANAM*60,AGENCY*40
      CHARACTER*20 PRGNAM,RUNBY,RCVERS,OPRNAM
      CHARACTER    ANTTYP*(*),RECTYP*(*),STANUM*(*)
      CHARACTER    COMENT(*)*60,CRDATE*9,CRTIME*5
      CHARACTER    OBSTYP(*)*2,CHRSAT*1
      REAL*8       POSECC(3,*),POSXYZ(3)
      INTEGER*4    IWLFAC(2),IWLSAT(3,*)
      INTEGER*4    NUMSAT(*),NUMOBS(MAXSAT,*)
      INTEGER*4,OPTIONAL       :: USEGEOS
      type(t_gobsdef),OPTIONAL :: GOBSDEF
      type(t_rinstat),OPTIONAL :: RINSTAT
C
C  LOCAL DECLARATIONS
C  ------------------
      PARAMETER (MAXWLF=7)
      CHARACTER    HEAD*20,RXTYPE*1,STRING*60,RXT*1,SSY*1
      CHARACTER    CWLSAT(MAXWLF)*1,TIMSYS*3,STATYP*20
      CHARACTER    FACTYP(MAXTYP)*2,OBSRIN3*3,OBSRIN2*2
      INTEGER*4    JWLSAT(MAXWLF),KWLFAC(2),FACTOR(MAXTYP)
      INTEGER*4    isys,obs,ll,kk,dummy,iprn,irc,NEWSAT
      LOGICAL      found,first
C
C RINEX FILE TYPE
      DATA RXTYPE/'O'/
C
C MAXIMUM RINEX FORMAT VERSION
      DATA IRVERS/3/
C
      NCOM   = 0
      NUMLST = 0
      IF(NUMLIN.LE.0) THEN
        PRGNAM = ' '
        RUNBY  = ' '
        CRDATE = ' '
        CRTIME = ' '
        JPGM   = 0
        STANAM = ' '
        JSTA   = 0
        STANUM = ' '
        STATYP = ' '
        OPRNAM = ' '
        AGENCY = ' '
        JOPR   = 0
        IRUNIT = 0
        RECTYP = ' '
        RCVERS = ' '
        JRCV   = 0
        IANTEN = 0
        ANTTYP = ' '
        JANT   = 0
        POSXYZ(1)=0.D0
        POSXYZ(2)=0.D0
        POSXYZ(3)=0.D0
        JPOS   = 0
        POSECC(1,1)=0.D0
        POSECC(2,1)=0.D0
        POSECC(3,1)=0.D0
        JECC   = 0
        IWLFAC(1)=1
        IWLFAC(2)=1
        NWLSAT = 0
        JWLF   = 0
        NUMTYP = 0
        IDELTT = 0
        TFIRST = 0.D0
        JTF    = 0
        TLAST  = 0.D0
        NSATEL = 0
        LEAP   = 0
        NFACT  = 0
      END IF
C   RINEX VERSION / TYPE
      IF(NUMLIN.EQ.0) THEN
        READ(LFNOBS,222,END=990,ERR=940) STRING,HEAD
222     FORMAT(A60,A20)
        ITEST=INDEX(HEAD,CHAR(13))
        IF(ITEST.NE.0) HEAD(ITEST:ITEST)=' '
        CALL UPPERC(STRING)
        CALL UPPERC(HEAD)
CCC        IF(HEAD.NE.'RINEX VERSION / TYPE') GOTO 950
        IF(INDEX(HEAD,'RINEX VERSION').EQ.0) GOTO 950
        READ(STRING,1,ERR=940) IRXVRS,RXT,SSY
1       FORMAT(I6,14X,A1,19X,A1)
        IF(RXT.NE.RXTYPE) GOTO 910
        IF(IRXVRS.LE.0.OR.IRXVRS.GT.IRVERS) GOTO 920
        IF(SSY.NE.' '.AND.SSY.NE.'G'.AND.SSY.NE.'R'.AND.
     1     SSY.NE.'E'.AND.SSY.NE.'S'.AND.SSY.NE.'C'.AND.
     2     SSY.NE.'M') GOTO 980
c        IF(SSY.NE.' '.AND.SSY.NE.'G'.AND.SSY.NE.'R'.AND.
c     1     SSY.NE.'E'.AND.SSY.NE.'S'.AND.SSY.NE.'C'.AND.
c     2     SSY.NE.'J'.AND.SSY.NE.'M') GOTO 980
c       ADD_GNSS_HERE CONFLICT BETWEEN J AND M
        JPGM   = 1
        IF(SSY.EQ.'R') IRXVRS=IRXVRS+100
        IF(SSY.EQ.'E') IRXVRS=IRXVRS+200
        IF(SSY.EQ.'S') IRXVRS=IRXVRS+300
        IF(SSY.EQ.'C') IRXVRS=IRXVRS+400
c        IF(SSY.EQ.'J') IRXVRS=IRXVRS+500
        IF(SSY.EQ.'M') IRXVRS=IRXVRS+500
c
c       ADD_GNSS_HERE conflict between M and J
c
      END IF
C
C IF RINEX 3 CALL RDR3OH
C ========================
      IF (MOD(IRXVRS,100) == 3) THEN
        first=.false.
        IF(NUMLIN.EQ.0) first=.true.
        CALL RDR3OH(LFNOBS,first)
C Fill RINEX 3 structure in RINEX 2 variables and arrays
C ------------------------------------------------------
        IF (rxohead%end /= 0) THEN
          IRCODE = rxohead%end
          GOTO 999
        ENDIF
        PRGNAM = rxohead%prognam
        RUNBY  = rxohead%runby
        CRDATE = rxohead%crdate(1:8)
        CRTIME = rxohead%crdate(10:13)
        STANAM = rxohead%mrknam
        STANUM = rxohead%mrknum
        IF (LEN(STANUM).GE.40) STANUM(21:40) = rxohead%mrktyp
        STATYP = rxohead%mrktyp
        OPRNAM = rxohead%observ
        AGENCY = rxohead%agency
        IF(LEN(RECTYP).EQ.40.AND.LEN(ANTTYP).EQ.40) THEN
          RECTYP(21:40) = rxohead%rectyp
        ELSE
          READ(rxohead%recnum,"(I6)",iostat=irc)IRUNIT
        ENDIF
        RECTYP(1:20) = rxohead%rectyp
        RCVERS = rxohead%recvers
        IF(LEN(RECTYP).EQ.40.AND.LEN(ANTTYP).EQ.40) THEN
          ANTTYP(21:40) = rxohead%antnum
        ELSE
          READ(rxohead%antnum,"(I6)",iostat=irc)IANTEN
        END IF
        ANTTYP(1:20) = rxohead%anttyp
        POSXYZ = rxohead%aprpos
        POSECC(:,1) = rxohead%anthen
        IF (STATYP.EQ.'SPACEBORNE') THEN
          POSECC(:,1) = rxohead%antxyz
          POSECC(:,2) = rxohead%antbore
          POSECC(:,3) = rxohead%comxyz
        ENDIF
C ---
        IDELTT = NINT(rxohead%obsint)
        LEAP =rxohead%leapsec
        TFIRST =rxohead%tfirst
        IF(TIMSYS.EQ.'UTC'.OR.TIMSYS.EQ.'GLO') TFIRST=-TFIRST
        TIMSYS =rxohead%timsys
        TLAST =rxohead%tlast
        IF(TIMSYS.EQ.'UTC'.OR.TIMSYS.EQ.'GLO') TLAST=-TLAST
        NSATEL = rxohead%nsat
C ---
        ICLKOF = rxohead%recloff
C
C WAVE LENGHT FACTOR ???
C
C Satellite-specific obs. selection (RINEX3)(sys-specific selection removed)
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF (USEGEOS==1.AND.gobsdef%norec>0) THEN
            NUMTYP=4
            indxs = 0
            DO isys=0,(maxsys-1)
              IF (rxohead%otyp(isys)%obsnum < 1) CYCLE
              DO isat=1,49
                DO igeos=1,gobsdef%norec
                  IF (gobsdef%sat(igeos)%sysnum.NE.isys) CYCLE
                  IF (gobsdef%sat(igeos)%satnum.NE.isat) CYCLE
                  DO obs=1,4
                    IF (gobsdef%sat(igeos)%obstyp(obs) == '   ') CYCLE
                    DO kk=1,rxohead%otyp(isys)%obsnum
                      IF(rxohead%otyp(isys)%obstyp(kk)==
     1                 gobsdef%sat(igeos)%obstyp(obs)) THEN
                       indxs(isys,isat,obs) = kk
                       IF (obs==1.OR.obs==2) gobsdef%sat(igeos)%nfreqc
     1                          = gobsdef%sat(igeos)%nfreqc+1
                       IF (obs==3.OR.obs==4) gobsdef%sat(igeos)%nfreqp
     1                          = gobsdef%sat(igeos)%nfreqp+1
                       gobsdef%sat(igeos)%obstyp2(obs)=obslistbasic(obs)
C
C                      Apply observation scale factor from RINEX file
                       IF (gobsdef%sat(igeos)%factor(obs) <
     1                     rxohead%otyp(isys)%factor(kk)) THEN
                           gobsdef%sat(igeos)%factor(obs) =
     1                     rxohead%otyp(isys)%factor(kk)
                       ENDIF
                       EXIT
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
C
C Remove unavailable observation types
            DO igeos=1,gobsdef%norec
             DO obs=1,4
              IF (gobsdef%sat(igeos)%obstyp2(obs)=='  ') THEN
                  gobsdef%sat(igeos)%obstyp(obs)='   '
              ENDIF
             ENDDO
            ENDDO

C Check whether one of the satellites has observations on two frequencies
            DO igeos=1,gobsdef%norec
              IF (gobsdef%nfreqc==0) THEN
                IF (gobsdef%sat(igeos)%nfreqc==1) gobsdef%nfreqc=1
              ENDIF
              IF (gobsdef%sat(igeos)%nfreqc==2) THEN
                gobsdef%nfreqc=2
                EXIT
              ENDIF
            ENDDO
            DO igeos=1,gobsdef%norec
              IF (gobsdef%nfreqp==0) THEN
                IF (gobsdef%sat(igeos)%nfreqp==1) gobsdef%nfreqp=1
              ENDIF
              IF (gobsdef%sat(igeos)%nfreqp==2) THEN
                gobsdef%nfreqp=2
                EXIT
              ENDIF
            ENDDO
C
C OBSTYPE COUNTER ADAPTED TO GOBSDEF
            NEWSAT = 1
            isat = 0
            DO isys=0,(maxsys-1)
             IF (rxohead%otyp(isys)%obsnum == 0) CYCLE
             DO iprn=1,49
              DO igeos=1,gobsdef%norec
               IF (gobsdef%sat(igeos)%sysnum.NE.isys) CYCLE
               IF (gobsdef%sat(igeos)%satnum.NE.iprn) CYCLE
               NEWSAT = 1
               DO obs=1,4
                IF (gobsdef%sat(igeos)%obstyp(obs).EQ.'   ') CYCLE
                IF (indxs(isys,iprn,obs)==0) CYCLE
                IF (rxohead%otyp(isys)%numobs(indxs(isys,iprn,obs),iprn)
     1              /= 0) THEN
                  IF (NEWSAT==1) THEN
                    isat=isat+1
                    NEWSAT=0
                    IF (isat>maxsat) write(LFNERR,*)
     1                '### SR R2RDOH: Too many satellites.'
                  ENDIF
                  NUMSAT(isat) = iprn + isys*100
                  NUMOBS(isat,obs) =
     1            rxohead%otyp(isys)%numobs(indxs(isys,iprn,obs),iprn)
                ENDIF
               ENDDO
              ENDDO
             ENDDO
            ENDDO
         ENDIF
        ENDIF
C
C Collect information for RINEX obstype statistics structure
c ----------------------------------------------------------
        IF (PRESENT(RINSTAT)) THEN
          DO isys=0,(maxsys-1)
           IF (rxohead%otyp(isys)%obsnum.eq.0) CYCLE
           DO kk=1,rxohead%otyp(isys)%obsnum
            DO J=1,MAXTYP
             IF(rxohead%otyp(isys)%obstyp(kk).eq.OBSTYPESR3(J)) THEN
               rinstat%sys(isys)%syschar     = g_rnxsys(isys)
               rinstat%sys(isys)%sysnum      = isys
               rinstat%sys(isys)%indxs(J)    = kk
               EXIT
             ENDIF
            ENDDO
           ENDDO
          ENDDO
        ENDIF
C ---
      ELSE
C START OF RINEX 2 SECTION
C ==============================
C  LOOP OVER ALL REMAINING LINES
        NLINE=0
200     READ(LFNOBS,222,END=930,ERR=940) STRING,HEAD
        ITEST=INDEX(HEAD,CHAR(13))
        IF(ITEST.NE.0) HEAD(ITEST:ITEST)=' '
        CALL UPPERC(STRING)
        CALL UPPERC(HEAD)
        NLINE=NLINE+1
C
        IF(HEAD.EQ.'COMMENT') THEN
          IF(NCOM.EQ.MAXCOM) THEN
            GOTO 985
          ELSE
            NCOM=NCOM+1
            COMENT(NCOM)=STRING
          END IF
C
C  PGM / RUN BY / DATE
        ELSEIF(HEAD.EQ.'PGM / RUN BY / DATE '.OR.
     1    HEAD(2:20).EQ.'PGM / RUN BY / DATE'.OR.
     2    HEAD(1:19).EQ.'GM / RUN BY / DATE ') THEN
          READ(STRING,2,ERR=940) PRGNAM,RUNBY,CRDATE,CRTIME
2         FORMAT(A20,A20,A9,1X,A5)
          JPGM  = 1
C
C  MARKER NAME
        ELSEIF (HEAD.EQ.'MARKER NAME         ') THEN
          STANAM=STRING
          CALL SJUSTL(STANAM)
          JSTA  = 1
C
C  MARKER NUMBER
        ELSEIF (HEAD.EQ.'MARKER NUMBER       ') THEN
          IF(IRXVRS.EQ.1) GOTO 940
          STANUM=STRING(1:20)
          CALL SJUSTL(STANUM(1:20))
C  MARKER TYPE
        ELSEIF(HEAD.EQ.'MARKER TYPE         '.AND.LEN(STANUM).GE.40)THEN
          IF(IRXVRS.EQ.1) GOTO 940
          STANUM(21:40)=STRING(1:20)
          CALL SJUSTL(STANUM(21:40))
          STATYP=STANUM(21:40)
C
C  OBSERVER / AGENCY
        ELSEIF (HEAD.EQ.'OBSERVER / AGENCY   ') THEN
          READ(STRING,5,ERR=940) OPRNAM,AGENCY
5         FORMAT(A20,A40)
          CALL SJUSTL(OPRNAM)
          CALL SJUSTL(AGENCY)
          JOPR   = 1
C
C  REC # / TYPE / VERS
        ELSEIF (HEAD.EQ.'REC # / TYPE / VERS ') THEN
          IF(LEN(RECTYP).EQ.40.AND.LEN(ANTTYP).EQ.40) THEN
            RECTYP(21:40)=STRING(1:20)
            CALL SJUSTL(RECTYP(21:40))
          ELSE
            READ(STRING(1:20),*,END=1026,ERR=1026) IRUNIT
          END IF
1026      READ(STRING(21:60),1016,ERR=940) RECTYP(1:20),RCVERS
1016      FORMAT(A20,A20)
          CALL SJUSTL(RECTYP(1:20))
          CALL SJUSTL(RCVERS)
          JRCV   = 1
C
C  ANT # TYPE
        ELSEIF (HEAD.EQ.'ANT # / TYPE        ') THEN
          IF(LEN(RECTYP).EQ.40.AND.LEN(ANTTYP).EQ.40) THEN
            ANTTYP(21:40)=STRING(1:20)
            CALL SJUSTL(ANTTYP(21:40))
          ELSE
            READ(STRING(1:20),*,END=1027,ERR=1027) IANTEN
          END IF
1027      READ(STRING(21:40),1017,ERR=940) ANTTYP(1:20)
1017      FORMAT(A20)
          CALL SJUSTL(ANTTYP(1:20))
          JANT   = 1
C
C  APPROX POSITION XYZ
        ELSEIF (HEAD.EQ.'APPROX POSITION XYZ ') THEN
          READ(STRING,8,ERR=940) (POSXYZ(K),K=1,3)
8         FORMAT(3F14.4)
          JPOS   = 1
C
C  ANTENNA: DELTA H/E/N
        ELSEIF (HEAD.EQ.'ANTENNA: DELTA H/E/N') THEN
          READ(STRING,9,ERR=940) POSECC(3,1),POSECC(2,1),POSECC(1,1)
9         FORMAT(3F14.4)
          JECC   = 1
        ELSEIF (HEAD.EQ.'ANTENNA: DELTA X/Y/Z'.AND.
     1                STATYP.EQ.'SPACEBORNE') THEN
          READ(STRING,9,ERR=940) POSECC(1,1),POSECC(2,1),POSECC(3,1)
          JECC   = 1
        ELSEIF (HEAD.EQ.'ANTENNA: B.SIGHT XYZ'.AND.
     1                STATYP.EQ.'SPACEBORNE') THEN
          READ(STRING,9,ERR=940) POSECC(1,2),POSECC(2,2),POSECC(3,2)
        ELSEIF (HEAD.EQ.'CENTER OF MASS: XYZ '.AND.
     1                STATYP.EQ.'SPACEBORNE') THEN
          READ(STRING,9,ERR=940) POSECC(1,3),POSECC(2,3),POSECC(3,3)
C
C  WAVELENGTH FACT L1/2
        ELSEIF((HEAD.EQ.'WAVELENGTH FACT L1/2') .or.
     1                (HEAD.EQ.'WAVELENGHT FACT L1/2')) THEN
          READ(STRING,10,ERR=940) KWLFAC(1),KWLFAC(2),
     1                KWLSAT,(CWLSAT(K),JWLSAT(K),K=1,KWLSAT)
10        FORMAT(2I6,I6,7(3X,A1,I2))
          JWLF   = 1
          IF(KWLSAT.EQ.0) THEN
            IWLFAC(1)=KWLFAC(1)
            IWLFAC(2)=KWLFAC(2)
          ELSE
            IF(IRXVRS.EQ.1) GOTO 940
            DO 210 K=1,KWLSAT
              IF(CWLSAT(K).NE.' '.AND.CWLSAT(K).NE.'G'.AND.
     1                    CWLSAT(K).NE.'R'.AND.CWLSAT(K).NE.'S')GOTO 980
              IF(CWLSAT(K).EQ.'R') JWLSAT(K)=JWLSAT(K)+100
              IF(CWLSAT(K).EQ.'E') JWLSAT(K)=JWLSAT(K)+200
C  CREATE/UPDATE LIST OF SAT.DEPENDENT WL-FACT
              DO 220 I=1,NWLSAT
                IF(IWLSAT(3,I).EQ.JWLSAT(K)) GOTO 230
220           CONTINUE
              NWLSAT=NWLSAT+1
              IF (NWLSAT.GT.MAXSAT) GOTO 996
              I=NWLSAT
              IWLSAT(3,I)=JWLSAT(K)
230           IWLSAT(1,I)=KWLFAC(1)
              IWLSAT(2,I)=KWLFAC(2)
210         CONTINUE
          END IF
C
C  # / TYPES OF OBSERV
        ELSEIF (HEAD.EQ.'# / TYPES OF OBSERV ') THEN
          READ(STRING,'(I6)',ERR=940) NUMTYP
          IF (NUMTYP>MAXTYP) THEN
            write(LFNERR,*) '*** SR R2RDOH: MAXTYP exceeded for
     1           station ',STANAM
            call exitrc(2)
          ENDIF
          K1=1
215       K2=K1+8
C GFZ OBSERVABLE
          NCHR=INDEX(STRING,'LP1')
          IF (NCHR.NE.0) STRING(NCHR:NCHR+2)=' LP'
C
          READ(STRING,11,ERR=940) (OBSTYP(K),K=K1,MIN(K2,NUMTYP))
11        FORMAT(6X,9(4X,A2))
          K1=K1+9
          IF(K2.LT.NUMTYP)THEN
            READ(LFNOBS,222,ERR=940) STRING,HEAD
            CALL UPPERC(STRING)
            NLINE=NLINE+1
            GOTO 215
          ENDIF
C
C  INTERVAL
        ELSEIF (HEAD.EQ.'INTERVAL            ') THEN
          READ(STRING,12,ERR=940) IDELTT
12        FORMAT(I6)
C
C  LEAP SECONDS
        ELSEIF (HEAD.EQ.'LEAP SECONDS        ') THEN
          READ(STRING,12,ERR=940) LEAP
C
C  TIME OF FIRST OBS
        ELSEIF (HEAD.EQ.'TIME OF FIRST OBS   ') THEN
          READ(STRING,13,ERR=940) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,
     1                TIMSYS
13        FORMAT(5I6,F12.6,6X,A3)
          IYEAR  = IYEAR4(IYEAR)
          DAY=IDAY+IHOUR/24.D0+MINUTE/1440.D0+SEC/86400.D0
          TFIRST=DJUL(IYEAR,MONTH,DAY)
          IF(TIMSYS.EQ.'UTC'.OR.TIMSYS.EQ.'GLO') TFIRST=-TFIRST
          JTF    = 1
C
C  TIME OF LAST OBS
        ELSEIF (HEAD.EQ.'TIME OF LAST OBS    ') THEN
          READ(STRING,14,ERR=940) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,
     1                TIMSYS
14        FORMAT(5I6,F12.6,6X,A3)
          IYEAR  = IYEAR4(IYEAR)
          DAY=IDAY+IHOUR/24.D0+MINUTE/1440.D0+SEC/86400.D0
          TLAST =DJUL(IYEAR,MONTH,DAY)
          IF(TIMSYS.EQ.'GLO'.OR.TIMSYS.EQ.'UTC') TLAST=-TLAST
C
C  SYS / PHASE SHIFT
        ELSEIF(HEAD.EQ.'SYS / PHASE SHIFT   ' .OR.
     1         HEAD.EQ.'SYS / PHASE SHIFTS  ') THEN
C
C  # OF SATELLITES
        ELSEIF (HEAD.EQ.'# OF SATELLITES     ') THEN
          READ(STRING,15,ERR=940) NSATEL
15        FORMAT(I6)
C
C  PRN / # OF OBS
        ELSEIF (HEAD.EQ.'PRN / # OF OBS      ') THEN
          NUMLST=NUMLST+1
          IF(NUMLST.GT.MAXSAT) GOTO 994
          READ(STRING,'(3X,A1,I2)',ERR=940) CHRSAT,NUMSAT(NUMLST)
          K1=1
225       K2=K1+8
          READ(STRING,16,ERR=940)
     1                (NUMOBS(NUMLST,K),K=K1,MIN(K2,NUMTYP))
16        FORMAT(6X,9I6)
          K1=K1+9
          IF(K2.LT.NUMTYP)THEN
            READ(LFNOBS,222,ERR=940) STRING,HEAD
            CALL UPPERC(STRING)
            NLINE=NLINE+1
            GOTO 225
          ENDIF
          IF(SSY.EQ.'R') NUMSAT(NUMLST)=NUMSAT(NUMLST)+100
          IF(SSY.EQ.'E') NUMSAT(NUMLST)=NUMSAT(NUMLST)+200
          IF(SSY.EQ.'S') NUMSAT(NUMLST)=NUMSAT(NUMLST)+300
          IF(SSY.EQ.'C') NUMSAT(NUMLST)=NUMSAT(NUMLST)+400
C          IF(SSY.EQ.'J') NUMSAT(NUMLST)=NUMSAT(NUMLST)+500
          IF(SSY.EQ.'M') THEN
            IF(CHRSAT.EQ.'R') NUMSAT(NUMLST)=NUMSAT(NUMLST)+100
            IF(CHRSAT.EQ.'E') NUMSAT(NUMLST)=NUMSAT(NUMLST)+200
            IF(CHRSAT.EQ.'S') NUMSAT(NUMLST)=NUMSAT(NUMLST)+300
            IF(CHRSAT.EQ.'C') NUMSAT(NUMLST)=NUMSAT(NUMLST)+400
C            IF(CHRSAT.EQ.'J') NUMSAT(NUMLST)=NUMSAT(NUMLST)+500
          END IF
c         ADD_GNSS_HERE conflict between M and J
CC RENAME SAT IF NECESSARY
        NUMSAT(NUMLST)=PRN2PRN(NUMSAT(NUMLST),ABS((TLAST-TFIRST)/2))
C
C  RCV CLOCK OFFS APPL
        ELSEIF (HEAD.EQ.'RCV CLOCK OFFS APPL ') THEN
          READ(STRING,17,ERR=940) ICLKOF
17        FORMAT(I6)
C
C  OBS SCALE FACTOR
        ELSEIF (HEAD.EQ.'OBS SCALE FACTOR    ') THEN
          READ(STRING,18,ERR=940) IFACT,JFACT,
     1                (FACTYP(K),K=NFACT+1,MIN(NFACT+JFACT,MAXTYP))
18        FORMAT(2I6,8(4X,A2))
          DO 180 K=1,JFACT
            FACTOR(NFACT+K)=IFACT
            CALL UPPERC(FACTYP(NFACT+K))
180       CONTINUE
          NFACT=MIN(NFACT+JFACT,MAXTYP)
C
C  BLANK LINE OR END OF HEADER
        ELSEIF ((HEAD.EQ.'END OF HEADER').OR.
     1                (IRXVRS.EQ.1.AND.STRING.EQ.' ')) THEN
          GOTO 300
        ELSE
C  UNKNOWN RECORD TYPE
          GOTO 970
        END IF
C
C  READ NEXT RECORD
        IF(NLINE.NE.IABS(NUMLIN)) GOTO 200
C
C  ARE ALL NECESSARY RECORDS READ?
300     IF(NUMLIN.EQ.0) THEN
          IF(JPGM.EQ.0) WRITE(LFNERR,301) '"PGM / RUN BY / DATE "'
          IF(JSTA.EQ.0) WRITE(LFNERR,301) '"MARKER NAME         "'
          IF(JOPR.EQ.0) WRITE(LFNERR,301) '"OBSERVER / AGENCY   "'
          IF(JRCV.EQ.0) WRITE(LFNERR,301) '"REC # / TYPE / VERS "'
          IF(JANT.EQ.0) WRITE(LFNERR,301) '"ANT # / TYPE        "'
          IF(JPOS.EQ.0) WRITE(LFNERR,301) '"APPROX POSITION XYZ "'
          IF(JECC.EQ.0) WRITE(LFNERR,301) '"ANTENNA: DELTA H/E/N"'
          IF(JWLF.EQ.0) WRITE(LFNERR,301) '"WAVELENGTH FACT L1/2"'
          IF(JTF .EQ.0) WRITE(LFNERR,301) '"TIME OF FIRST OBS   "'
301       FORMAT(' ### SR R2RDOH: ',A,' RECORD MISSING')
        END IF
C
C  SPECIAL OBSERVATION TYPES FOR SPACEBORNE RECEIVERS
        IF(STATYP.EQ.'SPACEBORNE'.AND.NFACT.NE.0) THEN
         DO 186 J=1,NUMTYP
          OBSTYP(NUMTYP+J)='01'
          DO 185 K=1,NFACT
            IF(FACTYP(K).EQ.OBSTYP(J).AND.FACTOR(K).LE.255.AND.
     1         FACTOR(K).GT.0) THEN
              WRITE(OBSTYP(NUMTYP+J),'(Z2)') FACTOR(K)
              GOTO 186
            ENDIF
185       CONTINUE
186      CONTINUE
        ELSE
          OBSTYP(NUMTYP+1)=' '
        ENDIF
C
      END IF
C
C END OF RINEX 2 SECTION
C ==========================================================
C
      IRCODE=0
      GOTO 999
C
C  WRONG FILE TYPE
910   IRCODE=1
      WRITE(LFNERR,911) RXT
911   FORMAT(' SR R2RDOH: WRONG FILE TYPE: "',A,'"')
      BACKSPACE LFNOBS
      GOTO 999
C
C  NOT ANTICIPATED VERSION NUMBER
920   IRCODE=2
      WRITE(LFNERR,921) IRXVRS
921   FORMAT(' SR R2RDOH: NOT ANTICIPATED VERSION NUMBER:',I5)
      GOTO 999
C
C  END OF FILE WITHIN HEADER
930   IRCODE=3
      WRITE(LFNERR,931)
931   FORMAT(' SR R2RDOH: END OF FILE WITHIN HEADER')
      GOTO 999
C
C  ERROR DECODING DATA
940   IRCODE=4
      WRITE(LFNERR,941) STRING,HEAD,NLINE+1
941   FORMAT(' SR R2RDOH: ERROR DECODING DATA ON THE FOLLOWING LINE:',
     1       /,1X,2A,/,' HEADERLINE ',I3)
      GOTO 999
C
C  FIRST LINE NOT VERSION LINE
950   IRCODE=5
      WRITE(LFNERR,951) STRING,HEAD
951   FORMAT(' SR R2RDOH: FIRST LINE NOT "VERSION"-LINE:',
     1       /,1X,2A)
      GOTO 999
C
C  UNKNOWN RECORD TYPE
970   IRCODE=4
      WRITE(LFNERR,971) STRING,HEAD
971   FORMAT(' SR R2RDOH: UNKNOWN HEADER RECORD TYPE:',
     1       /,1X,2A)
      GOTO 999
C
C  FEATURE NOT YET HANDLED
980   IRCODE=8
      WRITE(LFNERR,981) STRING,HEAD
981   FORMAT(' SR R2RDOH: FEATURE NOT YET HANDLED IN THE LINE:',
     1       /,1X,2A)
      GOTO 999
C
C  END OF FILE AT FIRST RECORD
990   IRCODE=9
      GOTO 999
C
C  DIMENSION OF ARRAY TOO SMALL
994   IRCODE=10
      WRITE(LFNERR,995) NUMLST,MAXSAT
995   FORMAT(/,' *** SR R2RDOH : NUMBER OF SAT. (NUMLST) > MAXSAT',/,
     1   17X,'NUMBER OF SATELLITES : ',I3 ,/,
     2   17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/)
      GOTO 999
C
996   IRCODE=10
      WRITE(LFNERR,997) NWLSAT,MAXSAT
997   FORMAT(/,' *** SR R2RDOH : NUMBER OF SAT. (NWLSAT) > MAXSAT',/,
     1   17X,'NUMBER OF SATELLITES : ',I3 ,/,
     2   17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/)
      GOTO 999
C
C  TOO MANY COMMENT LINES
985   IRCODE=11
      WRITE(LFNERR,986) MAXCOM
986   FORMAT(/,' ### SR R2RDOH : TOO MANY COMMENT LINES',/,
     1       17X,'MAXCOM: ',I3,/)
      GOTO 999
C
c999   RETURN
999   CONTINUE
C
C
C Satellite-specific obstype selection (RINEX2)
C ---------------------------------------------
      IF (MOD(IRXVRS,100) < 3) THEN
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF (USEGEOS==1.AND.gobsdef%norec>0) THEN
           indxs = 0
           DO isys=0,(maxsys-1)
             IF (NUMTYP.eq.0) CYCLE
             DO isat=1,49
               DO igeos=1,gobsdef%norec
                 IF (gobsdef%sat(igeos)%sysnum.NE.isys) CYCLE
                 IF (gobsdef%sat(igeos)%satnum.NE.isat) CYCLE
                 DO obs=1,4
                   IF ((gobsdef%sat(igeos)%obstyp(obs) == '   ').OR.
     1                 (gobsdef%sat(igeos)%obsindR212(obs).EQ.0)) CYCLE
                   DO kk=1,NUMTYP
                     OBSRIN3 = '   '
                     IF (OBSTYP(kk).eq.'  ') CYCLE
                     OBSRIN3 = RIN2RIN3(OBSTYP(kk))
                     IF(OBSRIN3.eq.'   ') THEN
                       WRITE(LFNERR,912) OBSTYP(kk),STANAM
                       CYCLE
                     ENDIF
                     IF(OBSTYP(kk).EQ.
     1                  OBSTYPESR2(gobsdef%sat(igeos)%obsindR212(obs)))
     2                 THEN
                       indxs(isys,isat,obs) = kk
                       IF (obs==1.OR.obs==2) gobsdef%sat(igeos)%nfreqc
     1                          = gobsdef%sat(igeos)%nfreqc+1
                       IF (obs==3.OR.obs==4) gobsdef%sat(igeos)%nfreqp
     1                          = gobsdef%sat(igeos)%nfreqp+1
                       gobsdef%sat(igeos)%obstyp2(obs)=obslistbasic(obs)
C                      Apply observation scale factor from RINEX file
                       DO K=1,NFACT
                        IF (OBSTYP(kk).eq.FACTYP(K)) THEN
                         IF (gobsdef%sat(igeos)%factor(obs) <
     1                     FACTOR(K)) THEN
                           gobsdef%sat(igeos)%factor(obs) = FACTOR(K)
                         ENDIF
                        ENDIF
                       ENDDO
                       EXIT
                     ENDIF
                   ENDDO
                 ENDDO
               ENDDO
             ENDDO
           ENDDO
C
C Remove unavailable observation types
           DO igeos=1,gobsdef%norec
             DO obs=1,4
               IF (gobsdef%sat(igeos)%obstyp2(obs)=='  ') THEN
                   gobsdef%sat(igeos)%obstyp(obs)='   '
               ENDIF
             ENDDO
           ENDDO
C
C Check whether one of the satellites has observations on two frequencies
           DO igeos=1,gobsdef%norec
             IF (gobsdef%nfreqc==0) THEN
               IF (gobsdef%sat(igeos)%nfreqc==1) gobsdef%nfreqc=1
             ENDIF
             IF (gobsdef%sat(igeos)%nfreqc==2) THEN
                 gobsdef%nfreqc=2
                 EXIT
             ENDIF
           ENDDO
           DO igeos=1,gobsdef%norec
             IF (gobsdef%nfreqp==0) THEN
               IF (gobsdef%sat(igeos)%nfreqp==1) gobsdef%nfreqp=1
             ENDIF
             IF (gobsdef%sat(igeos)%nfreqp==2) THEN
               gobsdef%nfreqp=2
               EXIT
             ENDIF
           ENDDO
C
C OBSTYPE COUNTER (INFO FROM RINEX HEADER) ADAPTED TO GOBSDEF
           NEWSAT = 1
           isat = 0
           DO isys=0,(maxsys-1)
             IF (NUMTYP.eq.0) CYCLE
             DO iprn=1,49
               DO igeos=1,gobsdef%norec
                IF (gobsdef%sat(igeos)%sysnum.NE.isys) CYCLE
                IF (gobsdef%sat(igeos)%satnum.NE.iprn) CYCLE
                NEWSAT = 1
                DO obs=1,4
                  IF (gobsdef%sat(igeos)%obstyp(obs).EQ.'   ') CYCLE
                  IF (indxs(isys,iprn,obs)==0) CYCLE
                  IF (NUMOBS(iprn,indxs(isys,iprn,obs))
     1                /= 0) THEN
                    IF (NEWSAT==1) THEN
                      isat=isat+1
                      NEWSAT=0
                      IF (isat>maxsat) write(LFNERR,*)
     1                  '### SR R2RDOH: Too many satellites.'
                    ENDIF
                    NUMSAT(isat) = iprn + isys*100
                    NUMOBS(isat,obs) = NUMOBS(iprn,indxs(isys,iprn,obs))
                  ENDIF
                ENDDO
               ENDDO
             ENDDO
           ENDDO
         ENDIF
        ENDIF
C
C
C Collect information for RINEX obstype statistics structure (RINEX2)
c -------------------------------------------------------------------
        IF (PRESENT(RINSTAT)) THEN
          DO isys=0,(maxsys-1)
            IF (NUMTYP.eq.0) CYCLE
            DO kk=1,NUMTYP
              IF (OBSTYP(kk).eq.'  ') CYCLE
              OBSRIN3 = RIN2RIN3(OBSTYP(kk))
              IF(OBSRIN3.eq.'   ') THEN
                WRITE(LFNERR,912) OBSTYP(kk),STANAM
912           FORMAT(' SR R2RDOH: RINEX2 OBSTYPE COULD NOT BE ',/,
     1               12X,'TRANSLATED TO RINEX3: ',A,/,
     2               12X,'STATION: ',A,/)
                CYCLE
              ENDIF
              DO J=1,MAXTYP
               IF(OBSTYP(kk).eq.OBSTYPESR2(J)) THEN
                 rinstat%sys(isys)%syschar     = g_rnxsys(isys)
                 rinstat%sys(isys)%sysnum      = isys
                 rinstat%sys(isys)%indxs(J)    = kk
                 EXIT
               ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE

