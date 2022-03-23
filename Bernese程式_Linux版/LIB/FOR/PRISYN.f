      MODULE s_PRISYN
      CONTAINS

C*
      SUBROUTINE PRISYN(PRIOPT,CORSTR,NFTOT ,CSESS ,STFIL ,STNAME,
     1                  XMAXCL,NDIFF ,ELEVMM,NOBELV,ZENMAX,NOBNAD,
     2                  NADIMM,NSATEL,SATNUM,NOBAZIS,TIMFIL)
CC
CC NAME       :  PRISYN
CC
CC PURPOSE    :  PRINT MAXIMUM SYNCHRONIZATION ERROR
CC
CC PARAMETERS :
CC         IN :  PRIOPT(I),I=1,..: PRINT OPTIONS              I*4
CC               CORSTR : CORRELATION STRATEGY                I*4
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               CSESS(2,I),I=1,..,NFTOT: SESSION IDENTIFIERS CH*4
CC               STFIL(2,I),I=1,..,NFTOT: STATIONS NUMBERS OF I*4
CC                        FILE I
CC               STNAME(I),I1,..,NSTAT: STATION NAMES         CH*16
CC               XMAXCL(I),I=1,..,NFTOT: MAXIMUM SYNCHRONI-   R*8
CC                        ZATION ERRORS
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               ELEVMM(2,I),I=1,..,NFTOT: MIN. AND MAX.      R*8
CC                        ELEVATION ANGLE
CC               NOBELV(I,J,K),I=1,2,J=1,..,18,K=1,..,NFTOT   I*4
CC                        5-DEG BIN OBSERVATION STATISTICS
CC               ZENMAX : MAXIMUM ZENITH DISTANCE IN RAD      R*8
CC               NOBNAD(I,J,K),I=1,2,J=1,..,30,K=1,..,NFTOT   I*4
CC               NADIMM(2,I),I=1,..,NFTOT: MIN. AND MAX.      R*8
CC                        NADIR ANGLE
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SAT. IN    I*4
CC                        FILE I
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT    I*4
CC                        SATELLITES
CC               NOBAZIS(L,K,J,I),L=1,2,K=1,..,36,            I*4
CC                        J=1,..,NSATEL(I),I=1,..,NFTOT
CC                        OBSERVATION STATISTICS FOR THE
CC                        AZIMUTH ANGLE AT THE SATELLITE
CC               TIMFIL(1..NFTOT) TIME INT. CONNECTED BY AMB. t_ambTime(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 09:32
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               06-MAY-96 : TS: CORRECT ZERO-DIFFERENCE PRINTING
CC               27-JAN-97 : MR: MINIMUM AND MAXIMUM ELEV. ANGLES
CC               13-AUG-97 : SS: FORMAT MODIFIED
CC               22-SEP-97 : SS: 5-DEG BIN OBSERVATION STATISTICS
CC               31-OCT-97 : SS: USE TEMPORARY VARIABLE "XOBELV"
CC               15-NOV-02 : RS: NADIR ANGLE OBSERVATION STATISTICS
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               24-APR-03 : RS: CORRECT SKELETON FILE SUBSTITUTES
CC               13-APR-04 : RS: ADD NSATEL,SATNUM AND NOBAZIS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-JAN-06 : RD/HB: STATISTICS ON PHASE-CONNECTED EPOCHS
CC               23-JAN-06 : HB: REPLACE ALLOCATED WITH ASSOCIATED
CC               31-JAN-08 : SL: MODIFIED PRIOPT LIST
CC               20-NOV-09 : RD: AMBIGUITY INTERVALS PER GNSS
CC               19-OCT-10 : SL: TIMFIL INITIALIZED WITH NFTOT INSTEAD OF *
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnPrt, timStrgLength2
      USE m_global, ONLY: maxsys, g_strsys
      USE d_const,  ONLY: PI
      USE p_gpsest, ONLY: t_ambTime
      USE s_timst2
      USE s_iordup
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBIN  , IF    , IFIL  , II    , IPF  , ISAT  ,
     1          ISAT1 , ISAT2 , ISTA  , MXCSAT, NFTOT , NOBAZ0,
     1          NOBEL0, NOBNA0, NSAM  , NSAT
C
      REAL*8    XELE  , XELE0 , XOBEL0, XOBNA0, ZENMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      TYPE(t_ambTime), DIMENSION(0:MAXSYS,NFTOT)   :: TIMFIL
C
      CHARACTER(LEN=132)                    :: LINE
      CHARACTER(LEN=timStrgLength2)         :: EPOSTR
C
C
C COMMON BLOCKS
C -------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      CHARACTER*16  STNAME(*)
      CHARACTER*6   MXNSAT
      CHARACTER*4   CSESS(2,*),CHRSES
C
      REAL*8        XMAXCL(*),ELEVMM(2,*),XOBELA(18),XOBELV(18)
      REAL*8        XOBNAA(30),XOBNAD(30),NADIMM(2,*)
      REAL*8        XOBAZI(36)
C
      INTEGER*4     PRIOPT(*),CORSTR,STFIL(2,*),NDIFF(*)
      INTEGER*4     NOBELV(2,18,*),NOBNAD(2,30,*)
      INTEGER*4     NSATEL(*),SATNUM(MXCSAT,*)
      INTEGER*4     NOBAZIS(2,36,MXCSAT,*),NOBAZ1(36,MXCSAT)
      INTEGER*4     IDXSAT(MXCSAT),SATNU1(MXCSAT)
C
C
C PRINT INTERVALS CONNECTED BY PHASE
C ----------------------------------
      IF (PRIOPT(20).EQ.1) THEN
        WRITE(LFNPRT,'(//,2(A,/),2(/,A))')
     1  ' TIME INTERVALS CONNECTED BY AT LEAST ONE PHASE OBSERVATION',
     2  ' ----------------------------------------------------------',
     3  ' SESS  FILE  STATION NAME 1    STATION NAME 2            FR' //
     4  'OM                 TO                    FROM           TO',
     6  ' -------------------------------------------'//
     7  '--------------------------------------------'//
     8  '-------------------------------------------'
        DO IF=1,NFTOT
          DO IPF=1,TIMFIL(0,IF)%NINTER
            LINE=''
            IF (ASSOCIATED(TIMFIL(0,IF)%AMBINT)) THEN
              CALL TIMST2(1,2,TIMFIL(0,IF)%AMBINT(IPF)%INT%T,EPOSTR)
              IF (NDIFF(IF).EQ.0) THEN
                WRITE(LFNPRT,'(1X,A4,I5,3X,A16,2X,A3,13X,2X,A,2F15.6)')
     1                      CSESS(1,IF),IF,STNAME(STFIL(1,IF)),'---',
     2                      EPOSTR,TIMFIL(0,IF)%AMBINT(IPF)%INT%T(1:2)
              ELSE
                WRITE(LFNPRT,'(1X,A4,I5,3X,A16,2X,A16,2X,A,2F15.6)')
     1                      CSESS(1,IF),IF,(STNAME(STFIL(I,IF)),I=1,2),
     2                      EPOSTR,TIMFIL(0,IF)%AMBINT(IPF)%INT%T(1:2)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        WRITE(LFNPRT,*)
C
C And not for each GNSS
        DO ii = 1,maxsys
          LINE='FIRST'
          DO IF=1,NFTOT
            DO IPF=1,TIMFIL(ii,IF)%NINTER
              IF (ASSOCIATED(TIMFIL(ii,IF)%AMBINT)) THEN
                IF (LINE .EQ. 'FIRST') THEN
        WRITE(LFNPRT,'(//,2A,/,A,/,2(/,A))')
     1  ' SYSTEM-SPECIFIC INTERVALS: ',g_strsys(ii-1),
     2  ' ----------------------------------------------------------',
     3  ' SESS  FILE  STATION NAME 1    STATION NAME 2            FR' //
     4  'OM                 TO                    FROM           TO',
     6  ' -------------------------------------------'//
     7  '--------------------------------------------'//
     8  '-------------------------------------------'
                  LINE=''
                ENDIF
                CALL TIMST2(1,2,TIMFIL(ii,IF)%AMBINT(IPF)%INT%T,EPOSTR)
                IF (NDIFF(IF).EQ.0) THEN
                 WRITE(LFNPRT,'(1X,A4,I5,3X,A16,2X,A3,13X,2X,A,2F15.6)')
     1                      CSESS(1,IF),IF,STNAME(STFIL(1,IF)),'---',
     2                      EPOSTR,TIMFIL(ii,IF)%AMBINT(IPF)%INT%T(1:2)
                ELSE
                  WRITE(LFNPRT,'(1X,A4,I5,3X,A16,2X,A16,2X,A,2F15.6)')
     1                      CSESS(1,IF),IF,(STNAME(STFIL(I,IF)),I=1,2),
     2                      EPOSTR,TIMFIL(ii,IF)%AMBINT(IPF)%INT%T(1:2)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          WRITE(LFNPRT,*)
        ENDDO
      ENDIF
C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' MIN. AND MAX. ELEVATION/NADIR ANGLES AND MAX.'
     2    ,' SYNCHRONIZATION ERRORS:'
     3  ,/,' ---------------------------------------------'
     3    ,'-----------------------'
     4  ,/,' '
     5  ,/,' SESS  FILE  STATION NAME 1    STATION NAME 2    MIN/MAX'
     5    ,' ELEV.  MIN/MAX NADIR  SYNCH. ERR. (NS)'
     6  ,/,1X,131('-')
     7  ,/,1X)")
C
C MAXIMUM SYNCHRONIZATION ERRORS
C ------------------------------
      DO 20 IF=1,NFTOT
        CHRSES=CSESS(1,IF)
        IF(CORSTR.EQ.1) WRITE(CHRSES,'(I4.4)') IF
        IF (NDIFF(IF).EQ.0) THEN
          WRITE(LFNPRT,3) CHRSES,IF,STNAME(STFIL(1,IF)),
     1                    (ELEVMM(II,IF),II=1,2),(NADIMM(II,IF),II=1,2),
     2                    XMAXCL(IF)
3         FORMAT(1X,A4,I5,1X,2X,A16,2X,'---',13X,F7.1,F6.1,F9.1,F6.1,
     1           F18.1)
        ELSE
          WRITE(LFNPRT,13) CHRSES,IF,(STNAME(STFIL(I,IF)),I=1,2),
     1                    (ELEVMM(II,IF),II=1,2),(NADIMM(II,IF),II=1,2),
     2                    XMAXCL(IF)
13        FORMAT(1X,A4,I5,1X,2(2X,A16),F7.1,F6.1,F9.1,F6.1,F18.1)
        ENDIF
20    CONTINUE
C
C
C PRINT HISTOGRAM OF NUMBER OF OBSERVATIONS (ELEVATION)
C -----------------------------------------------------
      IF (PRIOPT(11).EQ.1) THEN
        WRITE(LFNPRT,111) (5*IBIN,IBIN=1,18)
111     FORMAT(//,' HISTOGRAM OF OBSERVATIONS BY 5-DEGREE ELEVATION ',
     1    'BINS:',
     2    /,1X,52('-'),
     3    //,' FILE  STATION NAME         0',18(' - ',I2),'     #OBS',
     4    /,1X,131('-'),/)
C
        DO IFIL=1,NFTOT
          DO ISTA=1,NDIFF(IFIL)+1
            NOBEL0=0
            DO IBIN=1,18
              NOBEL0=NOBEL0+NOBELV(ISTA,IBIN,IFIL)
            ENDDO
C
            IF (NOBEL0.GT.0) THEN
              DO IBIN=1,18
                XOBELV(IBIN)=1.D2*NOBELV(ISTA,IBIN,IFIL)/NOBEL0
              ENDDO
            ELSE
              DO IBIN=1,18
                XOBELV(IBIN)=0.D0
              ENDDO
            ENDIF
C
            WRITE(LFNPRT,112) IFIL,STNAME(STFIL(ISTA,IFIL)),
     1        (XOBELV(IBIN),IBIN=1,18),NOBEL0
112         FORMAT(I5,2X,A16,4X,18F5.1,I11)
          ENDDO
        ENDDO
C
        NSAM=0
        DO IBIN=1,18
          XOBELA(IBIN)=0.D0
          DO IFIL=1,NFTOT
            DO ISTA=1,NDIFF(IFIL)+1
              IF (IBIN.EQ.1) NSAM=NSAM+1
              XOBELA(IBIN)=XOBELA(IBIN)+NOBELV(ISTA,IBIN,IFIL)
            ENDDO
          ENDDO
        ENDDO
C
        XOBEL0=0.D0
        DO IBIN=1,18
          XOBELA(IBIN)=XOBELA(IBIN)/NSAM
          XOBEL0=XOBEL0+XOBELA(IBIN)
        ENDDO
C
        IF (XOBEL0.GT.0.D0) THEN
          DO IBIN=1,18
            XOBELV(IBIN)=1.D2*XOBELA(IBIN)/XOBEL0
          ENDDO
        ELSE
          DO IBIN=1,18
            XOBELV(IBIN)=0.D0
          ENDDO
        ENDIF
C
        WRITE(LFNPRT,113) (XOBELV(IBIN),IBIN=1,18),XOBEL0
113     FORMAT(/,1X,131('-'),
     1    /,' MEAN #OBS',17X,18F5.1,F13.1)
C
C COMPUTE MEAN OBSERVATION DENSITIES (IN 1/STERADIAN)
        XOBEL0=XOBEL0/(2.D0*PI*(1.D0-DCOS(ZENMAX)))
C
        DO IBIN=1,18
          XELE=PI/36.D0*IBIN
          XELE0=PI/36.D0*(IBIN-1)
C
          XOBELA(IBIN)=XOBELA(IBIN)/(2.D0*PI*(DSIN(XELE)-DSIN(XELE0)))
        ENDDO
C
        IF (XOBEL0.GT.0.D0) THEN
          DO IBIN=1,18
            XOBELV(IBIN)=XOBELA(IBIN)/XOBEL0
          ENDDO
        ELSE
          DO IBIN=1,18
            XOBELV(IBIN)=0.D0
          ENDDO
        ENDIF
C
        WRITE(LFNPRT,114) (XOBELV(IBIN),IBIN=1,18),XOBEL0
114     FORMAT(1X,131('-'),
     1    /,' MEAN OBS DENSITY (1/SR)',4X,18F5.2,F12.1,
     2    /,1X,131('-'))
      ENDIF
C
C
C PRINT HISTOGRAM OF NUMBER OF OBSERVATIONS (NADIR ANGLE)
C -------------------------------------------------------
      IF (PRIOPT(13).EQ.1) THEN
        WRITE(LFNPRT,211) ((0.5*IBIN),IBIN=1,30)
211     FORMAT(//,' HISTOGRAM OF OBSERVATIONS BY 0.5-DEGREE NADIR ',
     1    'ANGLE BINS:',
     2    /,1X,56('-'),
     3    //,' FILE  STATION NAME       0.0',30(' - ',F4.1),'     #OBS',
     4    /,1X,251('-'),/)
C
        DO IFIL=1,NFTOT
          DO ISTA=1,NDIFF(IFIL)+1
            NOBNA0=0
            DO IBIN=1,30
              NOBNA0=NOBNA0+NOBNAD(ISTA,IBIN,IFIL)
            ENDDO
C
            IF (NOBNA0.GT.0) THEN
              DO IBIN=1,30
                XOBNAD(IBIN)=1.D2*NOBNAD(ISTA,IBIN,IFIL)/NOBNA0
              ENDDO
            ELSE
              DO IBIN=1,30
                XOBNAD(IBIN)=0.D0
              ENDDO
            ENDIF
C
            WRITE(LFNPRT,212) IFIL,STNAME(STFIL(ISTA,IFIL)),
     1        (XOBNAD(IBIN),IBIN=1,30),NOBNA0
212         FORMAT(I5,2X,A16,2X,30F7.1,I13)
          ENDDO
        ENDDO
C
        NSAM=0
        DO IBIN=1,30
          XOBNAA(IBIN)=0.D0
          DO IFIL=1,NFTOT
            DO ISTA=1,NDIFF(IFIL)+1
              IF (IBIN.EQ.1) NSAM=NSAM+1
              XOBNAA(IBIN)=XOBNAA(IBIN)+NOBNAD(ISTA,IBIN,IFIL)
            ENDDO
          ENDDO
        ENDDO
C
        XOBNA0=0.D0
        DO IBIN=1,30
          XOBNAA(IBIN)=XOBNAA(IBIN)/NSAM
          XOBNA0=XOBNA0+XOBNAA(IBIN)
        ENDDO
C
        IF (XOBNA0.GT.0.D0) THEN
          DO IBIN=1,30
            XOBNAD(IBIN)=1.D2*XOBNAA(IBIN)/XOBNA0
          ENDDO
        ELSE
          DO IBIN=1,30
            XOBNAD(IBIN)=0.D0
          ENDDO
        ENDIF
C
        WRITE(LFNPRT,213) (XOBNAD(IBIN),IBIN=1,30),XOBNA0
213     FORMAT(/,1X,251('-'),
     1    /,' MEAN #OBS',15X,30F7.1,F15.1,/,1X,251('-'))
C
      ENDIF
C
C
C PRINT HISTOGRAM OF NUMBER OF OBSERVATIONS (AZIMUTH ANGLE AT THE SAT.)
C ---------------------------------------------------------------------
      IF (PRIOPT(13).EQ.1) THEN
        DO ISAT=1,MXCSAT
          IDXSAT(ISAT)=0
          SATNU1(ISAT)=0
        ENDDO
C
        NSAT=NSATEL(1)
        CALL IORDUP(SATNUM(1,1),NSAT,IDXSAT)
        DO ISAT=1,NSAT
          SATNU1(ISAT)=SATNUM(IDXSAT(ISAT),1)
        ENDDO
C
        DO IFIL=2,NFTOT
          DO ISAT=1,NSATEL(IFIL)
            IF (SATNUM(ISAT,IFIL).LT.SATNU1(1)) THEN
              NSAT=NSAT+1
              DO I=NSAT,2,-1
                SATNU1(I)=SATNU1(I-1)
              ENDDO
              SATNU1(1)=SATNUM(ISAT,IFIL)
              CYCLE
            ENDIF
            DO ISAT1=1,(NSAT-1)
              IF (SATNUM(ISAT,IFIL).GT.SATNU1(ISAT1) .AND.
     1            SATNUM(ISAT,IFIL).LT.SATNU1(ISAT1+1)) THEN
                NSAT=NSAT+1
                DO I=NSAT,ISAT1+2,-1
                  SATNU1(I)=SATNU1(I-1)
                ENDDO
                SATNU1(ISAT1+1)=SATNUM(ISAT,IFIL)
                CYCLE
              ENDIF
            ENDDO
            IF (SATNUM(ISAT,IFIL).GT.SATNU1(NSAT)) THEN
              NSAT=NSAT+1
              SATNU1(NSAT)=SATNUM(ISAT,IFIL)
            ENDIF
          ENDDO
        ENDDO
C
        DO ISAT=1,NSAT
          DO IBIN=1,36
            NOBAZ1(IBIN,ISAT)=0
          ENDDO
        ENDDO
C
        DO IFIL=1,NFTOT
          DO ISTA=1,NDIFF(IFIL)+1
            DO IBIN=1,36
              DO ISAT1=1,NSATEL(IFIL)
                DO ISAT2=1,NSAT
                  IF (SATNUM(ISAT1,IFIL).EQ.SATNU1(ISAT2)) THEN
                    NOBAZ1(IBIN,ISAT2)=
     1              NOBAZ1(IBIN,ISAT2)+NOBAZIS(ISTA,IBIN,ISAT1,IFIL)
                    CYCLE
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C
        WRITE(LFNPRT,311) ((10*IBIN),IBIN=1,36)
311     FORMAT(//,' HISTOGRAM OF OBSERVATIONS BY 10-DEGREE AZIMUTH ',
     1    'ANGLE BINS (STATION SEEN FROM SATELLITE):',
     2    /,1X,87('-'),
     3    //,'  SAT   0',36(' - ',I3),'     #OBS',
     4    /,1X,251('-'),/)
C
        DO ISAT=1,NSAT
          NOBAZ0=0
          DO IBIN=1,36
            NOBAZ0=NOBAZ0+NOBAZ1(IBIN,ISAT)
          ENDDO
C
          IF (NOBAZ0.GT.0) THEN
            DO IBIN=1,36
              XOBAZI(IBIN)=1.D2*NOBAZ1(IBIN,ISAT)/NOBAZ0
            ENDDO
          ELSE
            DO IBIN=1,36
              XOBAZI(IBIN)=0.D0
            ENDDO
          ENDIF
C
          WRITE(LFNPRT,312) SATNU1(ISAT),
     1      (XOBAZI(IBIN),IBIN=1,36),NOBAZ0
312       FORMAT(I5,1X,36F6.1,I12)
        ENDDO
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
