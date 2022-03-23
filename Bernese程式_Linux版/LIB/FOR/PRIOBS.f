      MODULE s_PRIOBS
      CONTAINS

C*
      SUBROUTINE PRIOBS(PRIOPT,TITLES,NFTOT,HEADER,CAMPG1,NUMSAT,NUMOBS,
     1                  NUMMRK,NUMAMB,AMBSAT,AMBIEP,AMBWLF,
     2                  AMBIGU,AMBCLS)
CC
CC NAME       :  PRIOBS
CC
CC PURPOSE    :  PRINT NUMBER OF OBSERVATIONS AVAILABLE IN THE OBSER-
CC               VATION FILES
CC
CC PARAMETERS :
CC         IN :  PRIOPT : PRINT OPTION (=1: PRINT ELSE =0)    I*4
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               HEADER(I),I=1,..,NFTOT: HEADER FILE NAMES    CH*(*)
CC               CAMPG1 : CAMPAIGN NAME TO BE PRINTED         CH*16
CC      LOCAL :  NUMSAT : SATELLITE NUMBERS                   I*4(*)
CC               NUMOBS : NUMBER OF OBSERVATIONS              I*4(*,2)
CC               NUMMRK : NUMBER OF MARKED OBSERVATIONS       I*4(*,2)
CC               NUMAMB : NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J),J=1,..,NUMAMB : LOCAL AMBIGUITY    I*4
CC                        SATELLITE NUMBERS
CC               AMBIEP(J),J=1,..,NUMAMB : LOCAL STARTING     I*4
CC                        EPOCH NRS FOR AMBIGUITIES
CC               AMBIGU(L,K),L=1,..,NUMAMB(I), K=1,2,3 :      R*8
CC                        LOCAL AMBIGUITIES
CC               AMBWLF(K,J),K=1,..,J=1,2 : LOCAL WAVELENGTH  I*4
CC                        FACTORS :  K ... AMBIGUITY
CC                                   J ... FREQUENCY
CC                                   I ... FILE
CC               AMBCLS(L,K),L=1,..,NUMAMB, K=1,2,3 : LOCAL   I*4
CC                         AMBIGUITY CLUSTERS
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
CC               09-NOV-93 : MR: HEADER COMMENTS
CC               09-APR-94 : MR: PRINTING OF MORE THAN 24 SATELLITES
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20,
CC                               FILENAMES CHR(*)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               12-OCT-07 : RD: ALLOW FOR MORE THAN 46 SATELLITES
CC               26-JAN-11 : LP: CHANGED CALL TO RDHEAD
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_rdhead
      USE s_iordup
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDELTT, IF    , IFREQ , IFRMAT, IRMARK,
     1          MEATYP, MXCAMB, MXCSAT, NDIFF , NEPFLG, NEPOCH, NFREQ ,
     2          NFTOT , NSATEL, NUMAMB, IS1   , IS2
C
      REAL*8    TIMREF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*53  TITLE
      CHARACTER*(*) HEADER(*)
      CHARACTER*16  CAMPGN,CAMPG1,STANAM(2)
      CHARACTER*20  ANTTYP(2),RECTYP(2),OPRNAM(2)
      CHARACTER*9   CRDATE(2)
      CHARACTER*6   MXNSAT,MXNAMB
      CHARACTER*5   CRTIME(2)
      CHARACTER*4   CSESS(2)
C
      REAL*8        AMBIGU(MXCAMB,3)
      REAL*8        POSECC(3,2)
C
      INTEGER*4     PRIOPT,NUMSAT(*),NUMOBS(MXCSAT,2),NUMMRK(MXCSAT,2)
      INTEGER*4     AMBSAT(MXCAMB),AMBIEP(MXCAMB),AMBWLF(MXCAMB,2)
      INTEGER*4     AMBCLS(MXCAMB,3),IDX(MXCSAT)
      INTEGER*4     IRUNIT(2),IANTEN(2),ICLOCK(2),READGEOS
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C PRINT TITLE LINES
C -----------------
      IF(PRIOPT.NE.0) THEN
        WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
2       FORMAT(//,A,/,A,/,' ',131('-'),//)
C
        WRITE(LFNPRT,"(
     1       ' OBSERVATIONS AVAILABLE:'
     2    ,/,' ----------------------'
     3    ,/,' '
     4    ,/,' FILE  SATELLITES / #L1-OBS. OK / #L1-OBS. BAD '
     4      ,'(/ #L2-OBS. OK / #L2-OBS. BAD )'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C PRINT NUMBER OF OBSERVATIONS
C ----------------------------
        READGEOS=0
        DO 50 IF=1,NFTOT
          CALL RDHEAD(HEADER(IF),
     1                MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2                CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3                CRTIME,IRMARK,NEPFLG,IFRMAT,
     4                STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5                OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6                NUMAMB,AMBSAT,AMBIEP,AMBWLF,
     7                AMBIGU,AMBCLS,READGEOS)
          IF(CAMPGN.NE.CAMPG1) GOTO 50
C
C ORDER THE SATELLITES
C --------------------
          CALL iordup(numsat,nsatel,idx)
C
          IS1 = 1
          IS2 = 0
          DO WHILE (IS2.LT.NSATEL)
            IS2 = IS1 + 18
            IF (IS2.GT.NSATEL) IS2 = NSATEL
C
            IF (IS1.EQ.1) THEN
              WRITE(LFNPRT,1001) IF,(NUMSAT(IDX(I)),I=1,IS2)
1001          FORMAT(I5,'  SATEL. :',19(I6))
            ELSE
              WRITE(LFNPRT,1004) (NUMSAT(IDX(I)),I=IS1,IS2)
1004          FORMAT(/,5X,'  SATEL. :',19(I6))
            ENDIF
            DO IFREQ=1,NFREQ
              WRITE(LFNPRT,1002) IFREQ,(NUMOBS(IDX(I),IFREQ),I=IS1,IS2)
              WRITE(LFNPRT,1003) IFREQ,(NUMMRK(IDX(I),IFREQ),I=IS1,IS2)
1002          FORMAT(7X,'#L',I1,' OK :',19(I6))
1003          FORMAT(7X,'#L',I1,' BAD:',19(I6))
            ENDDO
C
            IS1 = IS2+1
          ENDDO
          WRITE(LFNPRT,'( )')
50      CONTINUE
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
