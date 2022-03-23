C*
      PROGRAM AMBCHK
CC
CC NAME       :  AMBCHK
CC
CC PURPOSE    :  COMPARE AMBIGUITIES IN TWO HEADER FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC CREATED    :  13-APR-94
CC
CC CHANGES    :  02-NOV-95 : MR: ADD NUMBER OF CLUSTERS
CC               06-JUN-96 : MR: REMOVED UNUSED VARIABLES
CC               17-JUN-96 : MR: USE CALL EXITRC
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               20-SEP-01 : MM: SWITCH TO NEW MENU
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               26-JAN-11 : LP: CALL TO RDHEAD CHANGED
CC               10-JAN-12 : SL: TITLE STRING FOR PRITIT CHANGED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
C
      USE s_opnfil
      USE s_defcon
      USE s_prflna
      USE s_gtfile
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_readkeys
      USE s_stripdir
      USE s_rdhead
      USE s_exitrc
      USE s_opnsys
      USE s_gtflna

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAM11 , IAM12 , IAM3  , IAMB  , ICL1  , ICL2  , ICLU  ,
     1          IDELTT, IDIF  , IFIL  , IFRMAT, IFRQ  , II    , IMIAMB,
     2          IOSTAT, IRC   , IRMARK, JJ    , MAXAMB, MAXFIL, MEATYP,
     3          MINBAD, MXCAMB, MXCSAT, NAMB  , NAMBAT, NAMCLU, NDIFF ,
     4          NEPFLG, NEPOCH, NFIL  , NFLCOL, NFRE1 , NFRE2 , NFREQ ,
     5          NSATEL, NUMAM1, NUMAM2, NUMAMB, READGEOS
C
      REAL*8    AMBIGU, TIMREF
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=500,MAXAMB=600)
C
C MAXFIL: MAXIMUM NUMBER OF FILES
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXAMB: MAXIMUM NUMBER OF AMBIGUITIES PER SATELLITE
C
C DECLARATIONS
C ------------
      CHARACTER*53 TITLE
      CHARACTER*32 FILNAM(2,MAXFIL),FILSUM
      CHARACTER*16 CAMPGN,STANAM(2)
      CHARACTER*20 RECTYP(2),ANTTYP(2),OPRNAM(2)
      CHARACTER*9  CRDATE(2)
      CHARACTER*6  MXNSAT,MXNAMB
      CHARACTER*5  CRTIME(2)
      CHARACTER*4  CSESS(2)
C
      REAL*8       AMBIG1(MAXAMB,3),AMBIG2(MAXAMB,3)
      REAL*8       POSECC(3,2)
C
      INTEGER*4    IRUNIT(2),IANTEN(2),ICLOCK(2)
      INTEGER*4    NUMSAT(MAXSAT),NUMOBS(MAXSAT,2),NUMMRK(MAXSAT,2)
      INTEGER*4    AMBIE1(MAXAMB),AMBSA1(MAXAMB),AMBCL1(MAXAMB,3)
      INTEGER*4    AMBWL1(MAXAMB,2)
      INTEGER*4    AMBIE2(MAXAMB),AMBSA2(MAXAMB),AMBCL2(MAXAMB,3)
      INTEGER*4    AMBWL2(MAXAMB,2)
      INTEGER*4    NCLUST(2,3),AMBCLU(2,MAXAMB,3)
      INTEGER*4    NBAD(3),NAMBAD(MAXAMB,3),IDIFMX(3),NISCLU(3)
C
      CHARACTER(LEN=keyValueLength),
     1                     DIMENSION(:), POINTER  :: keyValue
      CHARACTER(LEN=8)                            :: extension
      CHARACTER(LEN=32)                           :: help, wldcard
      INTEGER(i4b)                                :: lenDot, len1, len2
C
C COMMON FOR MAXIMAL DIMENSIONS, COMMON FOR CONSTANTS
C ---------------------------------------------------
      COMMON/LARGE/AMBIGU,FILNAM
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXCAMB=MAXAMB
      MXNSAT='MAXSAT'
      MXNAMB='MAXAMB'
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
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
C CREATE SOME OUTPUT
C ------------------
      CALL pritit('AMBCHK','Compare ambiguities')
      CALL prflna
C
C GET EXTENSION AND WILDCARDS
C ---------------------------
      CALL readkeys('EXTENS', keyValue, irc)
      IF (irc == 0) extension = TRIM(keyValue(1))
      CALL readkeys('WLDCARD', keyValue, irc)
      IF (irc == 0) wldcard = TRIM(keyValue(1))
C
C SUMMARY FILE
C ------------
      CALL GTFLNA(0,'SUMMARY',FILSUM,IRC)
      CALL OPNFIL(LFN001,FILSUM,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILSUM,'AMBCHK')
C
C GET OBSERVATION FILE NAMES
C --------------------------
      NFLCOL=1
      CALL GTFILE('OBSFIL ',NFLCOL,MAXFIL,NFIL,FILNAM(1,:))
C
C CREATE SECOND FILENAMES
C -----------------------
      DO ii=1,nFil
        help = TRIM(FILNAM(1,ii))
        CALL stripdir (help)
        lenDot = INDEX(help, '.', BACK=.TRUE.)
C check for wildcards
        DO jj=1,lenDot-1
          IF (wldcard(jj:jj) /= '?' .AND. wldcard(jj:jj) /= '%' .AND.
     1        wldcard(jj:jj) /= ' ') THEN
            help(jj:jj) = wldcard(jj:jj)
          ENDIF
        ENDDO
C copy help to FILNAM(2,ii)
        len1 = LEN_TRIM(FILNAM(1,ii))
        len2 = LEN_TRIM(help)
        FILNAM(2,ii) = TRIM(FILNAM(1,ii))
        FILNAM(2,ii)((len1-len2+1):len1) = help(1:len2)
        FILNAM(2,ii) = TRIM(FILNAM(2,ii))//extension
      ENDDO
C
C PRINT FILE NAMES AND SUMMARY HEADER
C -----------------------------------
      WRITE(LFN001,1001)
1001  FORMAT('NUM  FILENAME 1                       FILENAME 2',/,
     1       78('-'))
      DO IFIL=1,NFIL
        WRITE(LFN001,1002) IFIL,(FILNAM(II,IFIL),II=1,2)
1002    FORMAT(I3,1X,2(1X,A32))
      ENDDO
C
      WRITE(LFN001,1003)
1003  FORMAT(//,
     1       19X,'L1 FREQUENCY',30X,'L2 FREQUENCY',30X,'L5 FREQUENCY',/,
     2       'NUM ',3(2X,'#CL1 #CL2 %RE1 %RE2  #OK #BAD #ISCL MDIF'),/,
     3       130('-'))
C
C LOOP OVER ALL PAIRS OF FILES
C ----------------------------
      READGEOS=0
      DO 900 IFIL=1,NFIL
C
C READ FIRST HEADER FILE
C ----------------------
        CALL RDHEAD(FILNAM(1,IFIL),
     1              MEATYP,NDIFF,NFRE1,NEPOCH,NSATEL,
     2              cSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3              CRTIME,IRMARK,NEPFLG,IFRMAT,
     4              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5              OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6              NUMAM1,AMBSA1,AMBIE1,AMBWL1,AMBIG1,AMBCL1,
     7              READGEOS)
C
C READ SECOND HEADER FILE
C ----------------------
        CALL RDHEAD(FILNAM(2,IFIL),
     1              MEATYP,NDIFF,NFRE2,NEPOCH,NSATEL,
     2              CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3              CRTIME,IRMARK,NEPFLG,IFRMAT,
     4              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5              OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6              NUMAM2,AMBSA2,AMBIE2,AMBWL2,AMBIG2,AMBCL2,
     7              READGEOS)
C
        IF (NUMAM1.NE.NUMAM2) THEN
          WRITE(LFNERR,901) NUMAM1,NUMAM2,FILNAM(1,IFIL),FILNAM(2,IFIL)
901       FORMAT(/,' *** PG AMBCHK: NUMBER OF AMBIGUITIES NOT THE SAME',
     1           /,16X,'# AMB. IN FILE 1:',I5,
     2           /,16X,'# AMB. IN FILE 2:',I5,
     3           /,16X,'FILENAME 1      : ',A,
     4           /,16X,'FILENAME 2      : ',A,
     5           /,16X,'FILE PAIR SKIPPED !',/)
          GOTO 900
        ENDIF
        NUMAMB=NUMAM1
C
C NUMBER OF AMBIGUITY CLUSTERS
C ----------------------------
        NFREQ=MIN(NFRE1,NFRE2)
        IF (NFREQ.EQ.1) THEN
          NAMB=1
        ELSE
          NAMB=3
        ENDIF
        DO 20 IAM3=1,NAMB
          NCLUST(1,IAM3)=0
          NCLUST(2,IAM3)=0
20      CONTINUE
C
        DO 100 IAM3=1,NAMB
          DO 90 IAMB=1,NUMAMB
            DO 80 ICLU=1,NCLUST(1,IAM3)
              IF (AMBCLU(1,ICLU,IAM3).EQ.AMBCL1(IAMB,IAM3)) GOTO 90
80          CONTINUE
            NCLUST(1,IAM3)=NCLUST(1,IAM3)+1
            AMBCLU(1,NCLUST(1,IAM3),IAM3)=AMBCL1(IAMB,IAM3)
90        CONTINUE
C
          DO 95 IAMB=1,NUMAMB
            DO 85 ICLU=1,NCLUST(2,IAM3)
              IF (AMBCLU(2,ICLU,IAM3).EQ.AMBCL2(IAMB,IAM3)) GOTO 95
85          CONTINUE
            NCLUST(2,IAM3)=NCLUST(2,IAM3)+1
            AMBCLU(2,NCLUST(2,IAM3),IAM3)=AMBCL2(IAMB,IAM3)
95        CONTINUE
C
100     CONTINUE
C
C CHECK AMBIGUITY VALUES
C ----------------------
        DO 800 IFRQ=1,NAMB
          NBAD(IFRQ)=0
          IDIFMX(IFRQ)=0
          NISCLU(IFRQ)=0
C
          DO 210 IAMB=1,NUMAMB
            NAMBAD(IAMB,IFRQ)=0
210       CONTINUE
C
C LOOP OVER ALL CLUSTERS OF THE FIRST FILE
C ----------------------------------------
          DO 700 ICL1=1,NCLUST(1,IFRQ)
C
C LOOP OVER ALL CLUSTERS OF THE SECOND FILE
C -----------------------------------------
            DO 600 ICL2=1,NCLUST(2,IFRQ)
C
              NAMCLU=0
              NAMBAT=0
C
              DO 200 IAM11=1,NUMAMB
C
C CORRECT FIRST CLUSTER ?
                IF (AMBCL1(IAM11,IFRQ).NE.AMBCLU(1,ICL1,IFRQ)) GOTO 200
C
                DO 150 IAM12=IAM11+1,NUMAMB
C
C CORRECT FIRST CLUSTER ?
                  IF (AMBCL1(IAM12,IFRQ).NE.AMBCLU(1,ICL1,IFRQ))
     1              GOTO 150
C
C CORRECT SECOND CLUSTER ?
                  IF (AMBCL2(IAM11,IFRQ).NE.AMBCLU(2,ICL2,IFRQ))
     1              GOTO 150
                  IF (AMBCL2(IAM12,IFRQ).NE.AMBCLU(2,ICL2,IFRQ))
     1              GOTO 150
C
                  NAMCLU=NAMCLU+1
                  IF (NAMCLU.EQ.1) NISCLU(IFRQ)=NISCLU(IFRQ)+1
                  IDIF=(AMBIG1(IAM11,IFRQ) - AMBIG1(IAM12,IFRQ)) -
     1                 (AMBIG2(IAM11,IFRQ) - AMBIG2(IAM12,IFRQ))
                  IF (IDIF.NE.0) THEN
                    NAMBAT=NAMBAT+1
                    NAMBAD(IAM11,IFRQ)=NAMBAD(IAM11,IFRQ)+1
                    NAMBAD(IAM12,IFRQ)=NAMBAD(IAM12,IFRQ)+1
                    IF (IABS(IDIF).GT.IDIFMX(IFRQ)) THEN
                      IDIFMX(IFRQ)=IABS(IDIF)
                    ENDIF
                    WRITE(LFNPRT,2001) IAM11,IAM12,IFRQ,IDIF
2001                FORMAT (1X,'PAIR NOT OK', 2I5,' IFRQ=',I5,
     1                     ' DIFF=',I5)
                  END IF
150             CONTINUE
200           CONTINUE
C
              IF (NAMBAT.EQ.0) GOTO 600
C
C SELECTED AMBIGUITY WITH MINIMUM NUMBER OF BAD ENTRIES AS REFERENCE
C ------------------------------------------------------------------
              MINBAD=10000
              DO 250 IAMB=1,NUMAMB
                IF (AMBCL1(IAMB,IFRQ).NE.AMBCLU(1,ICL1,IFRQ) .OR.
     1              AMBCL2(IAMB,IFRQ).NE.AMBCLU(2,ICL2,IFRQ)) GOTO 250
                IF (NAMBAD(IAMB,IFRQ).LT.MINBAD) THEN
                  MINBAD=NAMBAD(IAMB,IFRQ)
                  IMIAMB=IAMB
                ENDIF
250           CONTINUE
C
              NBAD(IFRQ)=NBAD(IFRQ)+NAMBAD(IMIAMB,IFRQ)
C
600         CONTINUE
C
700       CONTINUE
C
800     CONTINUE
C
C WRITE SUMMARY LINE
C ------------------
        WRITE(LFN001,1004) IFIL,((NCLUST(II,JJ),II=1,2),
     1            ((NUMAMB-NCLUST(II,JJ))*100/NUMAMB,II=1,2),
     2            NUMAMB-1-NBAD(JJ),NBAD(JJ),NISCLU(JJ),
     3            IDIFMX(JJ),JJ=1,NAMB)
1004    FORMAT(I3,3(I7,7I5))
        WRITE(LFNPRT,2002) FILNAM(1,IFIL),
     1                     (NUMAMB-1-NBAD(II),NBAD(II),II=1,NAMB)
2002    FORMAT(1X,A32,6I7)
C
900   CONTINUE
C
      CLOSE(LFN001)
      CALL EXITRC(0)
      END
