      MODULE s_AMBSTO
      CONTAINS

C*
      SUBROUTINE AMBSTO(NFIL,HEADER,AMBSAV,NPAR,NPARN,NREF,NPALCQ,
     1                  LOCQ,XXX,
     2                  NUMAMB,AMBSAT,AMBWLF,AMBIEP,AMBIGU,AMBCLS,
     3                  NUMSA1,NUMOB1,NUMMR1,
     4                  NUMAM1,AMBSA1,AMBWL1,AMBIE1,AMBIG1,AMBCL1)
CC
CC NAME       :  AMBSTO
CC
CC PURPOSE    :  STORE ESTIMATED AMBIGUITIES INTO HEADER FILES
CC
CC PARAMETERS :
CC         IN :  NFIL   : NUMBER OF FILES                     I*4
CC               HEADER : HEADER FILE NAMES                   CH*(*)
CC               AMBSAV(I),I=1,..,NFTOT: SAVE AMBIGUITIES     I*4
CC                        (SAVE=1) FOR EACH FILE
CC               NPAR   : NUMBER OF ESTIMATED PARAMETERS      I*4
CC               NPARN  : NUMBER OF NON-AMBIGUITY PARAMETERS  I*4
CC               NREF   : NUMBER OF REFERENCE AMBIGUITIES     I*4
CC               NPALCQ : NUMBER OF LOCQ ELEMENTS (ALL PARA-  I*4
CC                        METERS INCLUDING THE ELIMINATED
CC                        ONES AND THE REFERENCE AMBIGUITIES)
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4
CC                          DESCRIPTION OF PARAMETER NUMBER I
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBIGU(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     R*8
CC                        I=1,..,NFTOT: AMBIGUITIES
CC               AMBWLF(K,J,I),K=1,..,J=1,2,I=1,..,NFTOT:     I*4
CC                        WAVELENGTH FACTORS
CC                        K: AMBIGUITY
CC                        J: FREQUENCY
CC                        I: FILE
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC      LOCAL :  NUMSA1 : LOCAL SATELLITE NUMBERS             I*4(*)
CC               NUMOB1 : LOCAL NUMBER OF OBSERVATIONS        I*4(*,2)
CC               NUMMR1 : LOCAL NUMBER OF MARKED OBSERV.      I*4(*,2)
CC               NUMAM1 : NUMBER OF AMBIGU.    I*4
CC               AMBSA1(J),J=1,..,NUMAMB : LOCAL AMBIGUITY    I*4
CC                        SATELLITE NUMBERS
CC               AMBIE1(J),J=1,..,NUMAMB : LOCAL STARTING     I*4
CC                        EPOCH NRS FOR AMBIGUITIES
CC               AMBIG1(L,K),L=1,..,NUMAMB(I), K=1,2,3 :      R*8
CC                        LOCAL AMBIGUITIES
CC               AMBWL1(K,J),K=1,..,J=1,2 : LOCAL WAVELENGTH  I*4
CC                        FACTORS :  K ... AMBIGUITY
CC                                   J ... FREQUENCY
CC               AMBCL1(L,K),L=1,..,NUMAMB, K=1,2,3 : LOCAL   I*4
CC                         AMBIGUITY CLUSTERS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/09/30 17:09
CC
CC CHANGES    :  04-AUG-92 : ??: PARAMETER "NREF" FOR MULTIPLE REFERENCE
CC                               SATELLITE AMBIGUITIES
CC               07-AUG-92 : ??: NEW APPROACH TO REFERENCE AMBIGUITIES
CC               10-NOV-92 : ??: ADD PARAMETER "NPARN"
CC               17-FEB-93 : ??: HANDLE CASE IF NO REFERENCE AMBIGUITY
CC                               PRESENT
CC               19-FEB-93 : ??: NEW PARAMETER "NPALCQ". AMBIGUITIES
CC                               WITHOUT OBSERVATIONS HAVE TO BE SET TOO
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               08-SEP-03 : HU: FILENAMES DECLARED OPEN
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-JAN-11 : LP: Handle sat.-specific obstypes
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: DATE, TIME
      USE d_rinex3,ONLY: t_gobsdef
      USE s_rdhead
      USE s_wthead
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IDELTT, IFIL  , IFLQ  , IFOPEN, IFRMAT, IPAR  ,
     1          IRMARK, ITYP  , MEATYP, MXCAMB, MXCLCQ, MXCSAT, NDIFF ,
     2          NEPFLG, NEPOCH, NFIL  , NFREQ , NPALCQ, NPAR  , NPARN ,
     3          NREF  , NSATEL, NUMAM1, IRC   ,USEGEOS
C
      REAL*8    TIMREF
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*53 TITLE
      CHARACTER*(*) HEADER(*)
      CHARACTER*16 CAMPGN,STANAM(2)
      CHARACTER*20 RECTYP(2),OPRNAM(2),ANTTYP(2)
      CHARACTER*9  CRDATE(2)
      CHARACTER*6  MXNSAT,MXNAMB,MXNLCQ
      CHARACTER*5  CRTIME(2)
      CHARACTER*4  CSESS(2)
C
      REAL*8       XXX(*),POSECC(3,2)
      REAL*8       AMBIGU(MXCAMB,3,*),AMBIG1(MXCAMB,3)
C
      INTEGER*4    LOCQ(MXCLCQ,*)
      INTEGER*4    IRUNIT(2),IANTEN(2),ICLOCK(2),AMBSAV(*)
      INTEGER*4    NUMSA1(*),NUMOB1(MXCSAT,2),NUMMR1(MXCSAT,2)
      INTEGER*4    NUMAMB(*),AMBSAT(MXCAMB,*),AMBWLF(MXCAMB,2,*)
      INTEGER*4              AMBIEP(MXCAMB,*),AMBCLS(MXCAMB,3,*)
      INTEGER*4              AMBSA1(MXCAMB),  AMBWL1(MXCAMB,2)
      INTEGER*4              AMBIE1(MXCAMB),  AMBCL1(MXCAMB,3)
C
      TYPE(t_gobsdef) :: gobsdef ! Giove External Obs. Selection info
C
C MAXIMAL DIMENSIONS
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C LOOP OVER ALL FILES
C -------------------
      DO 500 IFIL=1,NFIL
        IF (AMBSAV(IFIL).NE.1) GOTO 500
C
        IFOPEN=0
C
C LOOP OVER ALL ESTIMATED PARAMETERS
C ----------------------------------
        DO 100 IPAR=NPARN+1,NPALCQ
C
          ITYP=LOCQ(1,IPAR)
          IF (ITYP.NE.4) GOTO 100
          IFLQ=LOCQ(2,IPAR)
          IF (IFLQ.NE.IFIL) GOTO 100
C
C READ OLD FILE HEADER
C --------------------
          IF (IFOPEN.EQ.0) THEN
            USEGEOS=0
            GOBSDEF%NOREC=0
            CALL RDHEAD(HEADER(IFIL),
     1                  MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2                  CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3                  CRTIME,IRMARK,NEPFLG,IFRMAT,
     4                  STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5                  OPRNAM,POSECC,ICLOCK,NUMSA1,NUMOB1,NUMMR1,
     6                  NUMAM1,AMBSA1,AMBIE1,AMBWL1,
     7                  AMBIG1,AMBCL1,1,USEGEOS=USEGEOS,
     8                  GOBSDEF=GOBSDEF)
C
            DO 200 IAMB=1,NUMAM1
              AMBIG1(IAMB,1)=AMBIGU(IAMB,1,IFIL)
              AMBIG1(IAMB,2)=AMBIGU(IAMB,2,IFIL)
              AMBIG1(IAMB,3)=AMBIGU(IAMB,3,IFIL)
C
              AMBCL1(IAMB,1)=AMBCLS(IAMB,1,IFIL)
              AMBCL1(IAMB,2)=AMBCLS(IAMB,2,IFIL)
              AMBCL1(IAMB,3)=AMBCLS(IAMB,3,IFIL)
200         CONTINUE
C
            IFOPEN=1
          ENDIF
100     CONTINUE
C
C WRITE HEADER WITH SAVED AMBIGUITIES
C -----------------------------------
        IF (IFOPEN.EQ.1) THEN
          CRDATE(2)=DATE
          CRTIME(2)=TIME
          CALL WTHEAD(HEADER(IFIL),
     1                MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2                CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3                CRTIME,IRMARK,NEPFLG,IFRMAT,
     4                STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5                OPRNAM,POSECC,ICLOCK,NUMSA1,NUMOB1,NUMMR1,
     6                NUMAM1,AMBSA1,AMBIE1,AMBWL1,
     7                AMBIG1,AMBCL1,USEGEOS,
     8                GOBSDEF)
          IF (ASSOCIATED(GOBSDEF%SAT)) THEN
              DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
          ENDIF
        ENDIF
C
C NEXT FILE
500   CONTINUE
C
C END
C ---
      RETURN
      END SUBROUTINE

      END MODULE
