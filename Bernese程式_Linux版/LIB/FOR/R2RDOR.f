      MODULE s_R2RDOR
      CONTAINS

C*
      SUBROUTINE R2RDOR(LFNOBS,LFNERR,MAXSAT,IRXVRS,
     1                  NUMTYP,OBSTYP,EPOCH,IFLAG,NSATEP,SATEPO,
     2                  OBSEPO,ISIGN,LLI,IRCODE,USEGEOS,GOBSDEF,
     3                  RINSTAT)
CC
CC NAME       :  R2RDOR
CC
CC PURPOSE    :  READ  OBSERVATION RECORDS OF A
CC               RINEX OBSERVATION FILE (VERSIONS 1 AND 2)
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES         I*4
CC                        -> CORRESPONDS TO ROW DECLARATIONS OF
CC                           ARRAYS OBSEPO,SIGNAL,LLI
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC               NUMTYP : NUMBER OF DIFFERENT OBS.TYPES        I*4
CC               OBSTYP : LIST OF OBSERVATION TYPES            CH*2(*)
CC                        MULTIPLICATION FACTORS IN
CC                                  OBSTYP(NUMTYP+K)
CC               USEGEOS: USE GIOVE EXTERN. OBS. SELECTION     (OPT)I*4
CC               GOBSDEF: GIOVE EXTERN. OBS. SEL. INFO         (OPT)
CC               RINSTAT: OBS STATISTICS FOR A RINEX FILE      (OPT)
CC        OUT :  EPOCH  : OBSERVATION EPOCH (RECEIVER TIME)    R*8
CC                        IF NUMTYP<0: EPOCH(2) MAY CONTAIN
CC                        RCVR CLOCK OFFSET
CC               IFLAG  : FLAG FOR CURRENT RECORD              I*4
CC               NSATEP : NUMBER OF SATELLITES                 I*4
CC               SATEPO : LIST OF SATELLITE NUMBERS            I*4(*)
CC               OBSEPO : LIST OF OBSERVATIONS                 R*8(*,*)
CC                        OBSEPO(I,J), I: SATELLITE, J: OBS.TYPE
CC               ISIGN  : S/N RATIOS                           I*4(*,*)
CC                        ISIGN (I,J), I: SATELLITE, J: OBS.TYPE
CC               LLI    : LOSS OF LOCK INDICATORS              I*4(*,*)
CC                        LLI   (I,J), I: SATELLITE, J: OBS.TYPE
CC               IRCODE : RETURN CODE                          I*4
CC                        0: OK
CC                        3: END OF FILE WITHIN OBS.RECORD
CC                        4: ERROR DECODING DATA
CC                        5: START OF NEW HEADER FOUND
CC                        8: FEATURE NOT YET HANDLED
CC                        9: END OF FILE
CC
CC REMARKS    :  EVENT FLAG 6: THE CYCLE SLIP RECORDS ARE SKIPPED
CC               SATEPO :   1 -  99 : GPS
CC               SATEPO : 101 - 199 : GLONASS
CC               SATEPO : 201 - 299 : GALILEO
CC               SATEPO : 301 - 399 : GEO SBAS: EGNOS/WAAS/MSAS
CC               SATEPO : 401 - 499 : COMPASS/BEIDOU
CC               SATEPO : 501 - 599 : QZSS
CC               SATEPO : 901 - 999 : LEO
CC               IRXVRS : 100 ADDED IF GLONASS
CC               IRXVRS : 200 ADDED IF GALILEO
CC               IRXVRS : 300 ADDED IF GEO SBAS
CC               IRXVRS : 400 ADDED IF COMPASS/BEIDOU
CC               IRXVRS : 500 ADDED IF MIXED or QZSS
CC               IRXVRS : 600 ADDED IF GPS/GAL
CC               IRXVRS : 700 ADDED IF GPS/GLO
CC               IRXVRS : 800 ADDED IF GLO/GAL
CC               IRXVRS : 900 ADDED IF LEO
C                ADD_GNSS_HERE conflict between M and J
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  89/04/07 08:00
CC
CC CHANGES    :  17-JUN-92 : ??: NUMTYP>5: CORRECT SKIPPING OF CYCLE
CC                                         SLIP RECORDS
CC               22-SEP-92 : ??: NOT CORRECT IF MORE THAN 5 OBS.TYPES
CC               18-NOV-92 : ??: SUPPRESS MULTIPLE OCCURRENCES OF SATELLITES
CC               15-DEC-92 : ??: REMOVE CHECK FOR MAX. LOCAL DIMENSION
CC               23-FEB-93 : ??: INCLUDE GLONASS AND TRANSIT OBSERVATIONS
CC               25-NOV-93 : ??: JPL PROBLEM: EVENT FLAG 4 AND NSATEP=0:
CC                               DETERMINE NUMBER OF LINES TO FOLLOW
CC               17-JUN-94 : ??: SKIP RECORDS CONTAINING ASCII NUL CHARACTERS
CC               26-AUG-94 : ??: ALLOW FOR MORE THAN 12 SATELLITES PER EPOCH
CC                8-JAN-98 : ??: IF NUMTYP<0: ALLOW TO PUT RCVR CLOCK CORRECTION
CC                               INTO EPOCH(2)
CC                5-MAR-98 : ??: READ CONTINUATION LINE (OLD AND NEW FORMAT)
CC               10-AUG-98 : ??: ALLOW BLANK FOR GPS SATELLITES
CC               15-MAR-99 : TS: SKIP LINE FOLLOWING EPOCH FLAG=3
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               15-AUG-99 : JJ: RM UNUSED VAR IFIRST
CC               08-OCT-99 : TS/SS: CHECK WHETHER P1/C1=P2
CC               25-OCT-99 : DI: REJECT DATA ONLY IF P2 NOT ZERO
CC               10-FEB-00 : WG: PROBLEM WHEN IFLAG=4 AND MANY COMMENTS
CC                4-JAN-02 : ??: REMOVE TRAILING <CR>
CC                1-MAR-02 : ??: ADD ERR= EXIT TO STRING READ
CC                               FOR NSATEP>24: WRONG IMPLICIT LOOP START
CC               27-MAR-02 : DS: LEO NUMBERCC
CC               27-MAR-02 : DS: LEO NUMBER
CC               19-JUL-02 : WG: SKIP UNWANTED SATELLITES (DEPENDING ON IRXVRS)
CC               30-JAN-03 : WG: GEO ADDED
CC               02-APR-03 : WG: OBSERVATION FACTORS INCLUDED
CC               20-JAN-04 : HB: CORRECT ERROR MESSAGE
CC               05-FEB-04 : RD: LIMIT FOR P1/C1=P2 FROM 0.001D0 to 0.0001D0
CC               22-JUN-04 : RD: SPECIAL CASE: 0 LINES IN IFLAG=4 RECORD
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               09-JAN-07 : MM/SS: IGNORE /DEV/NULL SATELLITES (WITH NUMBER 0)
CC               24-MAY-07 : AG: FUNCTION PRN2PRN ADDED
CC               24-MAY-07 : AG: RINEX 3 IMPLEMENTED
CC               29-JUL-09 : SS: HANDLE C2
CC               11-MAY-10 : RD: INCREASE SIZE OF CSATEP FOR >24 SATELLITES
CC               18-JAN-11 : SL: MAXTYP 20->26
CC               20-JAN-11 : LP/SL: MAXTYP DEFINED IN D_RINEX3
CC               26-JAN-11 : LP: Satellite-specific observation types;indx zero check
CC               09-FEB-11 : RD: TAKE CARE IF 1ST SAT. OF AN EPO. IS REJECTED
CC               24-APR-12 : LP: Generalization of sat-specific obs types
CC                               (all satsys, [in RINEX2 not yet active as long
CC                                as IRXVRS issue is not solved])
CC               01-MAY-12 : LP: Apply gobsdef also to RINEX2;indx removed
CC               15-MAY-12 : LP: RINSTAT introduced
CC               29-MAY-12 : LP: SKIP ALSO LINES IN CASE OF EVENT FLAG 4
CC                               (OCCURS FOR MIXED TRIMBLE RECEIVERS)
CC               27-JUN-12 : LP: SKIP EPOCH IF MAXCHN EXCEEDED (RINEX3)
CC               29-AUG-12 : LP: WRITE WARNING FOR FLAG 4 ONLY IN CASE OF NEW HEADER
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_global, ONLY: maxsys
      USE d_rinex3, ONLY: rxoobs, rxohead, OBSTYPESR2, maxtyp,
     1                    t_gobsdef, indxs, obslistbasic,t_rinstat
      USE s_rdr3or
      USE f_djul
      USE s_exitrc
      USE f_iyear4
      USE s_upperc
      USE f_prn2prn
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IC1   , IDAY  , IFLAG , IHOUR , IL1   , IL2   ,
     1          IOSTAT, IP1   , IP2   , IRCODE, IREJ  , IRXVRS, IS    ,
     2          IST   , ITEST , ITYP  , IYEAR , J     , K     , MAXCHN,
     3          K1    , K2    , L     , LFNERR, LFNOBS, LREC  , MAXSAT,
     4          MAXSKP, MINUTE, MONTH , NLINES, NSATEP, NUMSKP,
     5          NUMTYP, IC2   , prn   , igeos , cyclflag,NUMTYPHLP
C
      REAL*8    DAY   , EPCORR, SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER    OBSTYP(*)*2
      INTEGER*4    SATEPO(*),LLI(MAXSAT,*),ISIGN(MAXSAT,*)
      INTEGER*4    LLIGEOS(MAXSAT,MAXTYP),ISIGNGEOS(MAXSAT,MAXTYP)
      REAL*8       OBSEPO(MAXSAT,*),EPOCH(*),OBSEPOGEOS(MAXSAT,MAXTYP)
      type(t_gobsdef),OPTIONAL :: GOBSDEF
      INTEGER*4,OPTIONAL       :: USEGEOS
      type(t_rinstat),OPTIONAL :: RINSTAT
C
C LOCAL DECLARATIONS
C ------------------
      PARAMETER (MAXSKP=10,MAXCHN=40)
      CHARACTER    STRHLP*80,STRING*80,CSATEP(MAXCHN)*1,SYSSKP(MAXSKP)*1
      INTEGER      FACTOR(MAXTYP)
      INTEGER      isys,obs,jsys,jsat,HELPF
      DATA NUMSKP/0/
C
      IF(IABS(NUMTYP).GT.MAXTYP) THEN
        WRITE(lfnErr,'(A,/,2(16X,A,I4,/))')
     1              '### SR R2RDOR: MAXTYP exceeded!',
     2                             'MAXTYP          : ',MAXTYP,
     3                             'Number of types : ',IABS(NUMTYP)
        CALL EXITRC(2)
      ENDIF
C
C GET POSITIONS FOR C1, C2, P1, P2, L1, L2
C ----------------------------------------
      IC1=0
      IC2=0
      IP1=0
      IP2=0
      IL1=0
      IL2=0
      DO ITYP=1,IABS(NUMTYP)
        IF (OBSTYP(ITYP).EQ.'C1') IC1 = ITYP
        IF (OBSTYP(ITYP).EQ.'C2') IC2 = ITYP
        IF (OBSTYP(ITYP).EQ.'P1') IP1 = ITYP
        IF (OBSTYP(ITYP).EQ.'P2') IP2 = ITYP
        IF (OBSTYP(ITYP).EQ.'L1') IL1 = ITYP
        IF (OBSTYP(ITYP).EQ.'L2') IL2 = ITYP
        IF (OBSTYP(IABS(NUMTYP)+1).NE.' ') THEN
          READ(OBSTYP(IABS(NUMTYP)+ITYP),'(Z2)',IOSTAT=IST) FACTOR(ITYP)
          IF(IST.NE.0) FACTOR(ITYP)=1
        ELSE
          FACTOR(ITYP)=1
        END IF
      ENDDO
C
      IF (MOD(IRXVRS,100) == 3) THEN
C Start of RINEX 3 section
C ========================
        CALL rdr3or(lfnobs)
C Fill RINEX 3 structure in RINEX 2 variables and arrays
C ------------------------------------------------------
        IF (rxohead%end /= 0) THEN
          IRCODE = rxohead%end
          GOTO 999
        ELSE
          IRCODE=0
        ENDIF
        EPOCH(1) = rxoobs%epoch
        EPOCH(2) = rxoobs%recloff
        IFLAG = rxoobs%flag
        NSATEP = rxoobs%nusat
C
        IF(NSATEP.GT.MAXCHN) THEN
          WRITE(lfnErr,'(A,/,2(16X,A,I4,/),A,/)')
     1              ' ### SR R2RDOR: MAXCHN exceeded!',
     2                             'MAXCHN              : ',MAXCHN,
     3                             'Satellites in epoch : ',NSATEP,
     4                             'Epoch skipped.'
          NSATEP = 0
          GOTO 999
        ENDIF
C
C
C Satellite-specific observation type selection (RINEX3)
        OBSEPOGEOS(:,:) = 0.d0
        LLIGEOS(:,:)    = 0
        ISIGNGEOS(:,:)  = 0
C
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF (USEGEOS==1.AND.gobsdef%norec>0) THEN
C
          DO K=1,NSATEP
           CSATEP(K) = rxoobs%obsrec(K)%satcod
           SATEPO(K) = rxoobs%obsrec(K)%prn
           IF (SATEPO(K)==0) CYCLE
           isys = INT(SATEPO(K)/100)
           prn = SATEPO(K)-isys*100
           DO igeos=1,gobsdef%norec
            IF (gobsdef%sat(igeos)%sysnum==isys
     1                  .AND.gobsdef%sat(igeos)%satnum==prn) THEN
             DO J=1,4
              IF (gobsdef%sat(igeos)%obstyp2(J) == '  ') CYCLE
              DO obs=1,4
               IF (indxs(isys,prn,obs)==0) CYCLE
               IF (gobsdef%sat(igeos)%obstyp2(J) == obslistbasic(obs))
     1         THEN
                 OBSEPOGEOS(K,J) = rxoobs%obsrec(K)%obs(indxs(isys,prn,
     1                             obs))
                 LLIGEOS(K,J)    = rxoobs%obsrec(K)%lli(indxs(isys,prn,
     1                             obs))
                 ISIGNGEOS(K,J)  = rxoobs%obsrec(K)%streng(indxs(isys,
     1                             prn,obs))
               ENDIF
              ENDDO
             ENDDO
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDIF
C
C
C Count available observations for RINEX obs stat structure (sat-wise)
C --------------------------------------------------------------------
        IF (PRESENT(RINSTAT)) THEN
         DO K=1,NSATEP
          CSATEP(K) = rxoobs%obsrec(K)%satcod
          SATEPO(K) = rxoobs%obsrec(K)%prn
          IF (SATEPO(K)==0) CYCLE
          isys = INT(SATEPO(K)/100)
          prn = SATEPO(K)-isys*100
          DO jsys=0,(maxsys-1)
           IF (jsys.ne.isys) CYCLE
           IF (rinstat%sys(jsys)%sysnum.ne.isys) THEN
             write(LFNERR,*) '### SR R2RDOR: Sat-system not defined in RINEX
     1                   header: sys=',isys,', SAT=',SATEPO(K)
             EXIT
           ENDIF
           DO jsat=1,49
            IF (jsat.ne.prn) CYCLE
            rinstat%sys(jsys)%sat(jsat)%eposatind = K
            IF (rinstat%sys(jsys)%sat(jsat)%satname.eq.'   ') THEN
                WRITE(rinstat%sys(jsys)%sat(jsat)%satname,'(A1,I2.2)')
     1                CSATEP(K),jsat
                rinstat%sys(jsys)%sat(jsat)%satnum = jsat
            ENDIF
            DO J=1,MAXTYP
             IF (rinstat%sys(jsys)%indxs(J).eq.0) CYCLE
             IF (rxoobs%obsrec(K)%obs(rinstat%sys(jsys)%indxs(J))
     1         .NE.0.D0) THEN
               rinstat%sys(jsys)%sat(jsat)%numobs(J) =
     1         rinstat%sys(jsys)%sat(jsat)%numobs(J) + 1
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
C
        DO 219 K=1,NSATEP
C IGNORE /DEV/NULL SATELLITES (WITH NUMBER 0)
C         ADD_GNSS_HERE    CONFLICT between M and J in IRXVRS!
          IF(SATEPO(K).EQ.0) CYCLE
          IF(IRXVRS/100.EQ.0) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'G') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.1) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'R') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.2) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'E') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.3) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'S') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.4) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'C') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.5) THEN
            IF(CSATEP(K).NE.'R'.AND.CSATEP(K).NE.'G'.AND.
     1         CSATEP(K).NE.'E'.AND.CSATEP(K).NE.'L'.AND.
     2         CSATEP(K).NE.'S'.AND.CSATEP(K).NE.' ') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.6) THEN
            IF(CSATEP(K).NE.'E'.AND.CSATEP(K).NE.'G') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.7) THEN
            IF(CSATEP(K).NE.'R'.AND.CSATEP(K).NE.'G') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.8) THEN
            IF(CSATEP(K).NE.'E'.AND.CSATEP(K).NE.'R') SATEPO(K)=0
          ELSEIF(IRXVRS/100.EQ.9) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'L') SATEPO(K)=0
          END IF
          IF(SATEPO(K).EQ.0.AND.NUMSKP.LT.MAXSKP) THEN
            DO J=1,NUMSKP
              IF(SYSSKP(J).EQ.CSATEP(K)) GOTO 219
            ENDDO
            NUMSKP=NUMSKP+1
            SYSSKP(NUMSKP)=CSATEP(K)
            WRITE(LFNERR,233) CSATEP(K)
          END IF
219     CONTINUE
C
C       Rearrange indices for removed satellites
        K=0
        DO I=1,NSATEP
          NUMTYPHLP = NUMTYP
          IF(SATEPO(I).NE.0) THEN
            K=K+1
            SATEPO(K)=SATEPO(I)
            CSATEP(K)=CSATEP(I)
            isys = INT(SATEPO(K)/100)
            prn = SATEPO(K)-isys*100
            IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
              IF (USEGEOS==1.AND.gobsdef%norec>0) THEN
               DO igeos=1,gobsdef%norec
                IF (gobsdef%sat(igeos)%sysnum==isys
     1                  .AND.gobsdef%sat(igeos)%satnum==prn) THEN
                    gobsdef%sat(igeos)%eposatind=K
                    NUMTYPHLP=4
                ENDIF
               ENDDO
              ENDIF
            ENDIF
C
            DO J=1,IABS(NUMTYPHLP)
                OBSEPO(K,J) = OBSEPOGEOS(I,J)
                LLI(K,J)    = LLIGEOS(I,J)
                ISIGN(K,J)  = ISIGNGEOS(I,J)
            ENDDO
          ENDIF
        ENDDO
        NSATEP = K
C
      ELSE
C Start of RINEX 2 section
C=========================
C RECORD 1        EPOCH/SAT
200     READ(LFNOBS,222,END=990,ERR=940) STRING
222     FORMAT(A)
        IF(STRING(1:1).EQ.CHAR(0)) GOTO 200
        ITEST=INDEX(STRING,CHAR(13))
        IF(ITEST.NE.0) STRING(ITEST:ITEST)=' '
        IF(STRING.EQ.' ') GOTO 200
        CALL UPPERC(STRING)
        IF(STRING(61:80).EQ.'RINEX VERSION / TYPE') GOTO 950
        READ(STRING,1,ERR=940) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,IFLAG,
     1              NSATEP,(CSATEP(K),SATEPO(K),K=1,MIN(NSATEP,12))

        WRITE(STRHLP,1) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,IFLAG,
     1              NSATEP,(CSATEP(K),SATEPO(K),K=1,MIN(NSATEP,12))
1       FORMAT(5I3,F11.7,I3,I3,12(A1,I2))
        READ(STRING,2,IOSTAT=IOSTAT) EPCORR
2       FORMAT(68X,F12.9)
        IF(NUMTYP.LT.0) EPOCH(2)=EPCORR
        IF(IFLAG.GE.2.AND.IFLAG.NE.6) GOTO 210
        IF(NSATEP.GT.12) THEN
          READ(LFNOBS,222,END=930,ERR=940) STRING
          ITEST=INDEX(STRING,CHAR(13))
          IF(ITEST.NE.0) STRING(ITEST:ITEST)=' '
          IF(STRING(1:3).EQ.'   ') THEN
            READ(STRING,11,ERR=940)
     1                  (CSATEP(K),SATEPO(K),K=13,MIN(NSATEP,24))
11          FORMAT(32X,12(A1,I2))
          ELSE
            READ(STRING,12,ERR=940)
     1                  (CSATEP(K),SATEPO(K),K=13,MIN(NSATEP,24))
12          FORMAT(12(A1,I2))
          END IF
        END IF
        IF(NSATEP.GT.24) THEN
          READ(LFNOBS,222,END=930,ERR=940) STRING
          ITEST=INDEX(STRING,CHAR(13))
          IF(ITEST.NE.0) STRING(ITEST:ITEST)=' '
          IF(STRING(1:3).EQ.'   ') THEN
            READ(STRING,11,ERR=940)
     1                  (CSATEP(K),SATEPO(K),K=25,MIN(NSATEP,36))
          ELSE
            READ(STRING,12,ERR=940)
     1                  (CSATEP(K),SATEPO(K),K=25,MIN(NSATEP,36))
          END IF
        ENDIF
        IF(NSATEP.GT.36) THEN
          READ(LFNOBS,222,END=930,ERR=940) STRING
          ITEST=INDEX(STRING,CHAR(13))
          IF(ITEST.NE.0) STRING(ITEST:ITEST)=' '
          IF(STRING(1:3).EQ.'   ') THEN
            READ(STRING,11,ERR=940)
     1                  (CSATEP(K),SATEPO(K),K=37,MIN(NSATEP,48))
          ELSE
            READ(STRING,12,ERR=940)
     1                  (CSATEP(K),SATEPO(K),K=37,MIN(NSATEP,48))
          END IF
        END IF
C
        DAY=DBLE(IDAY)+DBLE(IHOUR)/24.D0+DBLE(MINUTE)/1440.D0+SEC/864.D2

        IYEAR = IYEAR4(IYEAR)
        IF(DAY.EQ.0.D0) THEN
          EPOCH(1)=0.D0
        ELSE
          EPOCH(1)=DJUL(IYEAR,MONTH,DAY)
        END IF
C SATELLITES FROM OTHER SYSTEMS?
        DO 220 K=1,NSATEP
C IGNORE /DEV/NULL SATELLITES (WITH NUMBER 0)
          IF(SATEPO(K).EQ.0) GOTO 220
          IF(IRXVRS/100.EQ.0) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'G') THEN
              SATEPO(K)=0
            END IF
          END IF
          IF(IRXVRS/100.EQ.1) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'R') THEN
              SATEPO(K)=0
            ELSE
              SATEPO(K)=SATEPO(K)+100
            END IF
          END IF
          IF(IRXVRS/100.EQ.2) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'E') THEN
              SATEPO(K)=0
            ELSE
              SATEPO(K)=SATEPO(K)+200
            END IF
          END IF
          IF(IRXVRS/100.EQ.3) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'S') THEN
              SATEPO(K)=0
            ELSE
              SATEPO(K)=SATEPO(K)+300
            END IF
          END IF
          IF(IRXVRS/100.EQ.9) THEN
            IF(CSATEP(K).NE.' '.AND.CSATEP(K).NE.'L') THEN
              SATEPO(K)=0
            ELSE
              SATEPO(K)=SATEPO(K)+900
            END IF
          END IF
          IF(IRXVRS/100.EQ.5) THEN
            IF(CSATEP(K).NE.'R'.AND.CSATEP(K).NE.'G'.AND.
     1                  CSATEP(K).NE.'E'.AND.CSATEP(K).NE.'L'.AND.
     2                  CSATEP(K).NE.'S'.AND.CSATEP(K).NE.' ') THEN
C                       ADD_GNSS_HERE CONFLICT BETWEEN J AND M
              SATEPO(K)=0
            ELSE
              IF(CSATEP(K).EQ.'R') SATEPO(K)=SATEPO(K)+100
              IF(CSATEP(K).EQ.'E') SATEPO(K)=SATEPO(K)+200
              IF(CSATEP(K).EQ.'S') SATEPO(K)=SATEPO(K)+300
              IF(CSATEP(K).EQ.'L') SATEPO(K)=SATEPO(K)+900
C             ADD_GNSS_HERE
            END IF
          END IF
          IF(IRXVRS/100.EQ.6) THEN
            IF(CSATEP(K).NE.'E'.AND.CSATEP(K).NE.'G'
     1                         .AND.CSATEP(K).NE.' ') THEN
              SATEPO(K)=0
            ELSE
              IF(CSATEP(K).EQ.'E') SATEPO(K)=SATEPO(K)+200
            END IF
          END IF
          IF(IRXVRS/100.EQ.7) THEN
            IF(CSATEP(K).NE.'R'.AND.CSATEP(K).NE.'G'
     1                         .AND.CSATEP(K).NE.' ') THEN
              SATEPO(K)=0
            ELSE
              IF(CSATEP(K).EQ.'R') SATEPO(K)=SATEPO(K)+100
            END IF
          END IF
          IF(IRXVRS/100.EQ.8) THEN
            IF(CSATEP(K).NE.'E'.AND.CSATEP(K).NE.'R') THEN
              SATEPO(K)=0
            ELSE
              IF(CSATEP(K).EQ.'R') SATEPO(K)=SATEPO(K)+100
              IF(CSATEP(K).EQ.'E') SATEPO(K)=SATEPO(K)+200
CC              ADD_GNSS_HERE
            END IF
          END IF
          IF(SATEPO(K).EQ.0.AND.NUMSKP.LT.MAXSKP) THEN
            DO 230 J=1,NUMSKP
              IF(SYSSKP(J).EQ.CSATEP(K)) GOTO 220
230         CONTINUE
            NUMSKP=NUMSKP+1
            SYSSKP(NUMSKP)=CSATEP(K)
            WRITE(LFNERR,233) CSATEP(K)
233         FORMAT(' SR R2RDOR: SATELLITES SKIPPED! SYSTEM: "',A,'"',/)
          END IF
C RENAME SAT IF NECESSARY
          SATEPO(K)=PRN2PRN(SATEPO(K),EPOCH(1))
220     CONTINUE
C
C  HANDLE EVENT FLAG'S:
C  --------------------
C
C SKIP LINES IN CASE OF FLAG=3 (new site occupation)
C --------------------------------------------------
210     IF((IFLAG.EQ.3).OR.((IFLAG.EQ.4).AND.(NSATEP.NE.0))) THEN
C
C DUMMY READ
C ----------
          HELPF=0
          DO I=1,NSATEP
            READ(LFNOBS,222,END=930,ERR=940) STRING
            IF ((STRING(61:71).EQ.'MARKER NAME').OR.
     1          (STRING(61:73).EQ.'MARKER NUMBER').OR.
     2          (STRING(61:79).EQ.'REC # / TYPE / VERS').OR.
     3          (STRING(61:72).EQ.'ANT # / TYPE')) THEN
                HELPF=1
            ENDIF
          ENDDO
C
          IF (PRESENT(RINSTAT)) THEN
          ELSEIF ((IFLAG.EQ.3).OR.((IFLAG.EQ.4).AND.(HELPF.EQ.1))) THEN
            WRITE(LFNERR,993) IFLAG,NSATEP
993         FORMAT(/,' ### SR R2RDOR: EPOCH FLAG DETECTED: ',I3,
     1                /,16X,'TRYING TO CONTINUE : '
     2                /,16X,'LINES SKIPPED      : ',I3,/)
          ENDIF
          GOTO 200
C
        ELSEIF(IFLAG.GT.1.AND.IFLAG.NE.6) THEN
C
C  EVENT FLAG: JPL PROBLEM
          IF(IFLAG.EQ.4) THEN
            IF(NSATEP.EQ.0) THEN
              NLINES=1
231           READ(LFNOBS,222,END=232,ERR=940) STRING
              ITEST=INDEX(STRING,CHAR(13))
              IF(ITEST.NE.0) STRING(ITEST:ITEST)=' '
              NLINES=NLINES+1
              IF(STRING(61:80).NE.'END OF HEADER') GOTO 231
              NSATEP=NLINES-1
232           CONTINUE
              DO 234 IS=1,NLINES
                BACKSPACE LFNOBS
234           CONTINUE
            END IF
          END IF
          GOTO 900
        END IF
C
C  MULTIPLE OCCURRENCES OF THE SAME SATELLITE?
        DO 250 K=2,NSATEP
          DO 260 L=1,K-1
            IF(SATEPO(K).EQ.SATEPO(L).AND.CSATEP(K).EQ.CSATEP(L)) THEN
              SATEPO(K)=0
              GOTO 250
            END IF
260       CONTINUE
250     CONTINUE
C
C RECORD 2 FF     OBSERVATIONS
        J=0
        LREC=(IABS(NUMTYP)-1)/5+1
        DO 100 I=1,NSATEP
          DO 110 L=1,LREC
            K1=(L-1)*5+1
            K2=MIN(K1+4,IABS(NUMTYP))
            READ(LFNOBS,222,END=930,ERR=940) STRING
            ITEST=INDEX(STRING,CHAR(13))
            IF(ITEST.NE.0) STRING(ITEST:ITEST)=' '
            IF(SATEPO(I).NE.0) THEN
              IF(L.EQ.1) THEN
                J=J+1
                SATEPO(J)=SATEPO(I)
                CSATEP(J)=CSATEP(I)
              END IF
              READ(STRING,111,ERR=940) (OBSEPO(J,K),LLI(J,K),ISIGN(J,K),
     1                    K=K1,K2)
111           FORMAT(5(F14.3,2I1))
C  OBSERVATION FACTOR
              DO 120 K=K1,K2
                OBSEPO(J,K)=OBSEPO(J,K)/FACTOR(K)
120           CONTINUE
            END IF
110       CONTINUE
C
C REJECT OBSERVATIONS WHERE P1/C1=P2 OR L1=L2 - AND WHERE P2 OBSERVATIONS
C OF GPS-GLONASS RECEIVERS ACTUALLY AVAILABLE
C (CHECK THAT THE FIRST SATELLLITE IN THE LIST IS NOT REJECTED
          IF (J.NE.0) THEN
            IREJ=0
            IF (IC1.NE.0 .AND. IP2.NE.0) THEN
              IF (DABS(OBSEPO(J,IC1)-OBSEPO(J,IP2)).LT.0.0001D0 .AND.
     1                    OBSEPO(J,IP2).NE.0.D0) IREJ=1
            ENDIF
            IF (IP1.NE.0 .AND. IP2.NE.0) THEN
              IF (DABS(OBSEPO(J,IP1)-OBSEPO(J,IP2)).LT.0.0001D0 .AND.
     1                     OBSEPO(J,IP2).NE.0.D0) IREJ=1
            ENDIF
C
C WAVELENGTHS REQUIRED TO DO THIS
CC          IF (IL1.NE.0 .AND. IL2.NE.0) THEN
CC            IF (DABS(OBSEPO(J,IL1)-OBSEPO(J,IL2)).LT.0.001D0) IREJ=1
CC          ENDIF
            IF (IREJ.EQ.1) J=J-1
          ENDIF
100     CONTINUE
C
C  SKIP CYCLE SLIPS REPORTED
        IF(IFLAG.EQ.6) GOTO 200
C
        NSATEP=J
        IF(NSATEP.EQ.0) GOTO 200
C
900     IRCODE=0
        GOTO 999
C
C  END OF FILE WITHIN OBS.RECORD
930     IRCODE=3
        WRITE(LFNERR,931)
931     FORMAT(' SR R2RDOR: END OF FILE WITHIN OBS.RECORD')
        GOTO 999
C
C  ERROR DECODING DATA
940     IRCODE=4
        WRITE(LFNERR,941) STRING
941     FORMAT(' SR R2RDOR: ERROR DECODING DATA ON THE FOLLOWING LINE:',
     1              /,1X,A)
        GOTO 999
C
C  START OF NEW HEADER FOUND
950     IRCODE=5
        BACKSPACE LFNOBS
        WRITE(LFNERR,951) STRING
951     FORMAT(' SR R2RDOR: START OF A NEW HEADER FOUND:',
     1              /,1X,A)
        GOTO 999
C
C  FEATURE NOT YET HANDLED
980     IRCODE=8
        WRITE(LFNERR,981) STRING
981     FORMAT(' SR R2RDOR: FEATURE NOT YET HANDLED IN THE LINE:',
     1              /,1X,A)
        GOTO 999
C
C  END OF FILE
990     IRCODE=9
        GOTO 999
      ENDIF
C
C  END OF RINEX2 SECTION
C  =====================
C
c999   RETURN
999   CONTINUE
C
      IF (MOD(IRXVRS,100) < 3) THEN
C
C Satellite-specific obstype selection for GIOVE (RINEX2)
C =======================================================
        OBSEPOGEOS(:,:) = 0.d0
        LLIGEOS(:,:)    = 0
        ISIGNGEOS(:,:)  = 0
C
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF (USEGEOS==1.AND.gobsdef%norec>0.AND.NSATEP>0) THEN
C
          DO K=1,NSATEP
           IF (SATEPO(K)==0) CYCLE
           isys = INT(SATEPO(K)/100)
           prn = SATEPO(K)-isys*100
           DO igeos=1,gobsdef%norec
            IF (gobsdef%sat(igeos)%sysnum==isys
     1                  .AND.gobsdef%sat(igeos)%satnum==prn) THEN
             gobsdef%sat(igeos)%eposatind=K
             DO J=1,4
              IF (gobsdef%sat(igeos)%obstyp2(J) == '  ') CYCLE
              DO obs=1,4
               IF (indxs(isys,prn,obs)==0) CYCLE
               IF (gobsdef%sat(igeos)%obstyp2(J) == obslistbasic(obs))
     1         THEN
                 OBSEPOGEOS(K,J) = OBSEPO(K,indxs(isys,prn,obs))
                 LLIGEOS(K,J)    = LLI(K,indxs(isys,prn,obs))
                 ISIGNGEOS(K,J)  = ISIGN(K,indxs(isys,prn,obs))
               ENDIF
              ENDDO
             ENDDO
            ENDIF
           ENDDO
          ENDDO
c
c  Fill up output arrays
          DO K=1,MAXSAT
            DO J=1,MAXTYP
              OBSEPO(K,J) = 0.d0
              OBSEPO(K,J) = OBSEPOGEOS(K,J)
              LLI(K,J)    = 0
              LLI(K,J)    = LLIGEOS(K,J)
              ISIGN(K,J)  = 0
              ISIGN(K,J)  = ISIGNGEOS(K,J)
           ENDDO
          ENDDO
c          NUMTYP=4
         ENDIF
        ENDIF
C
C
C Count available observations for RINEX obs stat
C structure (sat-wise; RINEX2))
C -----------------------------------------------
        IF (PRESENT(RINSTAT).AND.(NSATEP.ne.0)) THEN
          DO K=1,NSATEP
            IF (SATEPO(K)==0) CYCLE
            isys = INT(SATEPO(K)/100)
            prn = SATEPO(K)-isys*100
            DO jsys=0,(maxsys-1)
             IF (jsys.ne.isys) CYCLE
             IF (rinstat%sys(jsys)%sysnum.ne.isys) THEN
               write(LFNERR,*) '### R2RDOR: Sat-system not defined in RINEX
     1                   header: sys=',isys,', SAT=',SATEPO(K)
               EXIT
             ENDIF
             DO jsat=1,49
              IF (jsat.ne.prn) CYCLE
              rinstat%sys(jsys)%sat(jsat)%eposatind = K
              IF (rinstat%sys(jsys)%sat(jsat)%satname.eq.'   ') THEN
                  WRITE(rinstat%sys(jsys)%sat(jsat)%satname,'(A1,I2.2)')
     1                CSATEP(K),jsat
                  rinstat%sys(jsys)%sat(jsat)%satnum = jsat
              ENDIF
              DO J=1,MAXTYP
               IF (rinstat%sys(jsys)%indxs(J).eq.0) CYCLE
               IF (OBSEPO(K,rinstat%sys(jsys)%indxs(J))
     1         .NE.0.D0) THEN
                 rinstat%sys(jsys)%sat(jsat)%numobs(J) =
     1           rinstat%sys(jsys)%sat(jsat)%numobs(J) + 1
               ENDIF
              ENDDO
             ENDDO
            ENDDO
          ENDDO
        ENDIF
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
