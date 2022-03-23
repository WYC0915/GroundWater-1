      MODULE s_INTACT
      CONTAINS

C*
      SUBROUTINE INTACT(ISETOP,NFRCHK,MRK1O2,IIONO ,IPRNT1,MXINTR,
     1                  IUSFLG,IPPROC,Q     ,DISCLV,LTRIP ,MAXZEN,
     2                  SIGWGS,IPRNT2,MINCYC,SIGL12,IWLSCR,SWIDTH,
     3                  IRJECT,MXOGAP,MXIOND,IRESID,NFREQ ,NSATEL,
     4                  NUMSAT,NSLIP ,LSTSLP,SLPLST,SLPXXX,IONO  ,
     5                  NDEL  ,LSTDEL,NMAXI ,LSTMXI,NNEWAM,LSTAMB,
     6                  IRETC ,MXHOLE,MNCONT,IAMNEW,NTONLY,L5CLEA,
     7                  OMCMAX)
CC
CC NAME       :  INTACT
CC
CC PURPOSE    :  INTERACTION PART OF "MANUAL" PREPROCESSING
CC
CC PARAMETERS :
CC     IN/OUT :  ISETOP : OPTIONS ADJUSTED TO FILE HEADER INFO I*4
CC                         =0 : TAKE INPUT FILE OPTIONS IN ANY
CC                              CASE
CC                         =1 : ADJUST FREQUENCIES ETC TO FILE
CC                              HEADER INFORMATION
CC               NFRCHK : FREQUENCY(IES) TO BE CHECKED         I*4
CC                         =1 : L1
CC                         =2 : L2
CC                         =3 : L1 AND L2 TOGETHER (VIA L5 AND L3)
CC                         =4 : L1 AND L2 SEPARATELY
CC               MRK1O2 : FLAG TO MARK UNPAIRED L1/L2 OBSERVA- I*4
CC                        TIONS
CC                         =0 : NO MARKING DONE
CC                         =1 : L1 WITHOUT L2, OR L2 WITHOUT L1
CC                              OBSERVATIONS ARE MARKED
CC               IIONO  : IONOSPHERE INDEX                     I*4
CC                         =0 : IONOSPHERE MODELS NOT APPLIED
CC                         =1 : IONOSPHERE MODELS APPLIED
CC               IPRNT1 : PRINT LEVEL FOR NON-PARAM. SCREENING I*4
CC                         =0 : NO MESSAGES PRINTED
CC                         =1 : PRINT SUMMARY MESSAGES
CC                         =2 : PRINT ALL MESSAGES
CC               MXINTR : MAXIMUM INTERVAL LENGTH FOR          I*4
CC                        POLYNOMIAL FIT (MIN)
CC               IUSFLG : USE OR IGNORE OBS FILE FLAGS         I*4
CC                         =0 : IGNORE OBS. FILE FLAGS
CC                         =1 : USE    OBS. FILE FLAGS
CC               IPPROC : SCREENING FLAG                       I*4
CC                        IPPROC(1): SCREENING SINGLE DIFF.
CC                        IPPROC(2): SCREENING SINGLE DIFF.
CC                         =0 : SKIP SCREENING
CC                         =1 : SCREEN OBSERVATIONS
CC               Q      : POL. DEGREE USED FOR SCREENING       I*4
CC                        Q(1): FOR SINGLE DIFF. SCREENING
CC                        Q(2): FOR DOUBLE DIFF. SCREENING
CC               DISCLV : MAXIMUM ALLOWED DISCONTINUITY        R*8
CC                        DISCLV(1) FOR SINGLE DIFF. SCREENING
CC                        DISCLV(2) FOR DOUBLE DIFF. SCREENING
CC               LTRIP  : CARRIER TO BE USED FOR TRIPLE DIFF.  I*4
CC                        SOLUTION
CC                         =1 : USE L1
CC                         =2 : USE L2
CC                         =3 : USE L3
CC                         =5 : USE L5
CC               MAXZEN : MAXIMUM SATELLITE ZENITH DISTANCE    I*4
CC                        (DEGREE)
CC               SIGWGS(I),I=1,2,3: A PRIORI SIGMAS FOR        I*4
CC                        STATION COORDINATES (M)
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECTION I*4
CC                         =0 : NO MESSAGES PRINTED
CC                         =1 : PRINT SUMMARY MESSAGES
CC                         =2 : PRINT ALL MESSAGES
CC               MINCYC : ACCEPT CYCLE SLIPS > "MINCYC" CYCLES I*4
CC               SIGL12(L),L=1,2: RMS ERRORS FOR FREQUENCY L   R*8
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR FOR        I*4
CC                        FOR FREQUENCY L
CC                         =1 : SCREENING USING CYCLES
CC                         =2 : SCREENING USING HALF CYCLES
CC               SWIDTH(K),K=1,2: NUMBER OF NEAREST INTEGERS   I*4
CC                        TO BE TESTED
CC                         K=1 : IN L1/L2
CC                         K=2 : IN L5
CC               IRJECT : OUTLIER REJECTION (YES=1,NO=0)       I*4
CC               MXOGAP : MAXIMUM OBSERVATION GAP ALLOWED IN   I*4
CC                        OUTLIER REJECTION (SEC)
CC               MXIOND : MAXIMUM IONOSPHERE CHANGE BETWEEN    I*4
CC                        EPOCHS (IN % OF L1 CYCLES) FOR OUT-
CC                        LIER REJECTION
CC               IRESID : SAVE RESIDUALS IN FILE (YES=1)       I*4
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE        I*4
CC               NSATEL : NUMBER OF SATELLITES                 I*4
CC               NUMSAT(I),I=1,2,..,NSATEL: SV NUMBERS         I*4
CC               NSLIP  : NUMBER OF DETECTED SLIPS             I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP          I*4
CC                        SLIP DEFINITION
CC               SLPLST(I),I=1,2,..,NSLIP: SLIPS               R*8
CC               SLPXXX(K,I),K=1,2,I=1,2,..,NSLIP: REAL VALUED R*8
CC                        ESTIMATES FOR SLIPS - INTEGER SLIP
CC                        SINGLE FREQ.: L1/L2 ESTIMATES
CC                        DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               IONO(K,I),K=1,2,3, I=1,2,..: IONOSPHERE       R*8
CC                        INFORMATION FOR SLIP NUMBER I
CC               NDEL   : NUMBER OF DELETION/MARK INSTRUCTIONS I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL           I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): MARKED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC                               =3: UNPAIRED L1/L2 OBSERVATIONS
CC                               =4: USER
CC                               =5: ELEVATION
CC                               =6: SMALL PIECES
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               NMAXI  : NUMBER OF "MAX. INTERVAL EXCEEDED"   I*4
CC               LSTMXI(K,I),K=1,..,5, I=1,2,..,NMAXI          I*4
CC                        DEFINITION OF "MAX.INT.EXC" NUMBER I
CC                        (1,I): EPOCH
CC                        (2,I): SATELLITE
CC                        (3,I): FREQUENCY (1=L1, 2=L2, 3=BOTH)
CC                        (4,I): BREAK IN NUMBER OF EPOCHS
CC                        (5,I): DETECTED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC   IN/OUT :    NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC      OUT :    IRETC  : RETURN CODE                          I*4
CC                        =0: EXIT (WITH OR WITHOUT SAVE)
CC                        =1: REPROCESS FILE
CC               MXHOLE : MAXIMAL GAP IN OBSERVATIONS ALLOWED  I*4
CC                        TO BE CONSIDERED AS CONTINUOUS (SEC)
CC               MNCONT : MINIMAL TIME INTERVAL OF CONTINUOUS  I*4
CC                        OBSERVATIONS (SEC)
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES IN FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB (SEC)
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC               OMCMAX : MAXIMUM OBSERV-COMPUTED VALUE (M)    R*8
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 08:59
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               24-APR-95 : MR: ADD OPTION FOR MAXIMUM O-C
CC               29-SEP-95 : JJ: DECLARE STAFIX AS C*16
CC               05-MAR-96 : TS: SOME COSMETIC CHANGES
CC               23-SEP-97 : DI: REMOVE UNUSED PARAMETER 'MAXSAT'
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES FROM MODOPT
CC               27-MAR-12 : RD: USE PROMP1 AS MODULE NOW, USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnkbd, lfnprt
      USE s_optres
      USE s_optcyc
      USE s_optdel
      USE s_promp1
      USE s_dspmxi
      USE s_optamb
      USE s_upperc
      USE s_modopt
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , IDEL  , IDELNW, IFIRST, IIONO , IOPT  ,
     1          IOPT1 , IPRNT1, IPRNT2, IRC   , IRESID, IRETC , IRJECT,
     2          ISAT  , ISETOP, ISLP  , ISLPNW, ISUMRY, ITITLE,
     3          ITXT  , IUSFLG, L5CLEA, LSTMXI, LTRIP , MAXDLT, MAXOPT,
     4          MAXZEN, MINCYC, MNCONT, MRK1O2, MXCAMB, MXCSAT, MXHOLE,
     5          MXINTR, MXIOND, MXOGAP, NDEL  , NFRCHK, NFREQ , NMAXI ,
     6          NSATEL, NSLIP , NTEXT1, NTEXT2, NTONLY
C
      REAL*8    OMCMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXOPT=18,MAXDLT=7)
C
      CHARACTER*80 STRG(1)
      CHARACTER*40 TEXT(24)
      CHARACTER*6  MXNSAT,MXNAMB
      CHARACTER*2  OPTION(MAXOPT),OPTIO1
C
      REAL*8       SLPLST(*),SLPXXX(2,*),IONO(3,*)
      REAL*8       DISCLV(2),SIGWGS(3),SIGL12(2)
C
      INTEGER*4    IAMNEW(*),IPPROC(2),Q(2),IWLSCR(2),SWIDTH(2)
      INTEGER*4    NUMSAT(*),LSTSLP(6,*),LSTDEL(5,*)
      INTEGER*4    NNEWAM(MXCSAT),LSTAMB(2,MXCSAT,*)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
      DATA IFIRST/1/
      DATA OPTION/'1 ','2 ','3 ','4 ','5 ','6 ','R ','X ',
     1            '11','12','13','21','22','23','41','42','43','E '/
C
C DEFINE TEXT FOR POSSIBLE INTERACTIONS
C -------------------------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        TEXT( 1)='1   CYCLE SLIPS                  '
        TEXT( 2)='2   MARKED AREAS                 '
        TEXT( 3)='3   MAX.INTERVAL EXCEEDINGS      '
        TEXT( 4)='4   MULTIPLE AMBIGUITIES         '
        TEXT( 5)='5   RESIDUALS                    '
        TEXT( 6)='6   PROCESSING OPTIONS           '
        TEXT( 7)='                                 '
        TEXT( 8)='R   REPROCESS                    '
        TEXT( 9)='X   EXIT                         '
        TEXT(10)='11  DISPLAY CYCLE SLIPS          '
        TEXT(11)='12  ADD     CYCLE SLIPS          '
        TEXT(12)='13  RESET   CYCLE SLIPS          '
        TEXT(13)='                                 '
        TEXT(14)='E   END                          '
        TEXT(15)='21  DISPLAY MARKED AREAS         '
        TEXT(16)='22  ADD     MARKED AREAS         '
        TEXT(17)='23  RESET   MARKED AREAS         '
        TEXT(18)='                                 '
        TEXT(19)='E   END                          '
        TEXT(20)='41  DISPLAY MULTIPLE AMBIGUITIES '
        TEXT(21)='42  ADD     MULTIPLE AMBIGUITIES '
        TEXT(22)='43  REMOVE  MULTIPLE AMBIGUITIES '
        TEXT(23)='                                 '
        TEXT(24)='E   END                          '
C
      ENDIF
C
C DISPLAY INTERACTION LIST
C ------------------------
5     NTEXT1=1
      NTEXT2=9
C
10    WRITE(LFNPRT,'(/)')
      DO 20 ITXT=NTEXT1,NTEXT2
        WRITE(LFNPRT,11) TEXT(ITXT)
11      FORMAT(1X,A40)
20    CONTINUE
      WRITE(LFNPRT,*)
      STRG(1)='ENTER OPTION :'
      CALL PROMP1(1,STRG)
      READ(LFNKBD,22) OPTIO1
22    FORMAT(A)
      CALL UPPERC(OPTIO1)
C
C SEARCH FOR OPTION
C -----------------
      DO 30 IOPT=1,MAXOPT
        IF(OPTION(IOPT).EQ.OPTIO1) GOTO 40
30    CONTINUE
C
C OPTION NOT FOUND
      IF(OPTIO1.NE.' ') THEN
        WRITE(LFNPRT,31) OPTIO1
31      FORMAT(/,' INVALID OPTION: ',A)
      ENDIF
      GOTO 10
C
C BRANCH ACCORDING TO OPTION
C --------------------------
40    GOTO (100,200,300,400,500,600,700,700,
     1      150,150,150,250,250,250,450,450,450,5) IOPT
C
C OPTION 1: DISPLAY CYCLE SLIP MENU
C ---------------------------------
100   NTEXT1=10
      NTEXT2=14
      GOTO 10
C
C OPTION 11: DISPLAY CYCLE SLIPS
C ------------------------------
150   IOPT1=IOPT-8
      CALL OPTCYC(IOPT1,NFREQ,NSATEL,NUMSAT,IWLSCR,
     1            NSLIP,LSTSLP,SLPLST,SLPXXX,IONO)
      GOTO 5
C
C OPTION 2: DISPLAY MARKED AREAS MENU
C -----------------------------------
200   NTEXT1=15
      NTEXT2=19
      GOTO 10
C
C OPTIONS 21/22/23: MODIFY MARKED AREAS
C -------------------------------------
250   IOPT1=IOPT-11
      CALL OPTDEL(IOPT1,NFREQ,NSATEL,NUMSAT,NDEL,LSTDEL)
      GOTO 5
C
C OPTION 3: DISPLAY MAXIMUM INTERVAL EXCEEDINGS
C ---------------------------------------------
300   ITITLE=1
      ISUMRY=0
      CALL DSPMXI(ITITLE,ISUMRY,NFREQ,NMAXI,(/LSTMXI/),IRC)
      GOTO 5
C
C OPTION 4: DISPLAY MULTIPLE AMBIGUITIES MENU
C -------------------------------------------
400   NTEXT1=20
      NTEXT2=24
      GOTO 10
C
C OPTIONS 41/42/43: MODIFY MULTIPLE AMBIGUITIES
C ---------------------------------------------
450   IOPT1=IOPT-14
      CALL OPTAMB(IOPT1 ,NFREQ ,NSATEL,NUMSAT,NNEWAM,LSTAMB)
      GOTO 5
C
C OPTION 5: DISPLAY RESIDUALS
C ---------------------------
500   IF(IRESID.EQ.1) THEN
        CALL OPTRES(NFRCHK,IWLSCR)
      ELSE
        WRITE(LFNPRT,501)
501     FORMAT(/,' NO RESIDUAL FILE AVAILABLE')
      ENDIF
      GOTO 5
C
C OPTION 4: DISPLAY/CHANGE PROCESSING OPTIONS
C --------------------------------------------
600   CALL MODOPT(ISETOP,NFRCHK,IIONO,IUSFLG,
     1            MRK1O2,MAXZEN,MXHOLE,MNCONT,IPRNT1,
     2            MXINTR,IPPROC,Q,DISCLV,LTRIP,SIGWGS,
     3            IPRNT2,SIGL12,IWLSCR,SWIDTH,
     4            MINCYC,IRJECT,MXOGAP,MXIOND,IAMNEW,
     5            NTONLY,L5CLEA,OMCMAX)
      GOTO 5
C
C OPTION R OR X: REPROCESS OR EXIT
C --------------------------------
C
C ALL AMBIGUITIES SET "OLD",
C REMOVE "RESET" CYCLE SLIPS FROM CYCLE SLIP ARRAYS
700   DO 760 ISAT=1,NSATEL
        DO 770 IAMB=1,NNEWAM(ISAT)
          LSTAMB(2,ISAT,IAMB)=IABS(LSTAMB(2,ISAT,IAMB))
770     CONTINUE
760   CONTINUE
      ISLPNW=0
      DO 730 ISLP=1,NSLIP
        LSTSLP(6,ISLP)=IABS(LSTSLP(6,ISLP))
        IF(LSTSLP(6,ISLP).LT.5) THEN
          ISLPNW=ISLPNW+1
          DO 710 I=1,6
            LSTSLP(I,ISLPNW)=LSTSLP(I,ISLP)
710       CONTINUE
          SLPLST(ISLPNW)=SLPLST(ISLP)
          SLPXXX(1,ISLPNW)=SLPXXX(1,ISLP)
          SLPXXX(2,ISLPNW)=SLPXXX(2,ISLP)
          DO 720 I=1,3
            IONO(I,ISLPNW)=IONO(I,ISLP)
720       CONTINUE
C
C KEEP L5 WAVELENGTH FACTOR (IF CYCLE SLIP IN L1 AND L2 SIMULT.)
        ELSE IF(ISLP.GT.1) THEN
          IF(LSTSLP(5,ISLP-1).EQ.3) LSTSLP(5,ISLP-1)=LSTSLP(5,ISLP)
        ENDIF
730   CONTINUE
      NSLIP=ISLPNW
C
C REMOVE "RESET" MARKED AREAS FROM ARRAY LSTDEL
      IDELNW=0
      DO 750 IDEL=1,NDEL
        LSTDEL(5,IDEL)=IABS(LSTDEL(5,IDEL))
        IF(LSTDEL(5,IDEL).LT.MAXDLT+1) THEN
          IDELNW=IDELNW+1
          DO 740 I=1,5
            LSTDEL(I,IDELNW)=LSTDEL(I,IDEL)
740       CONTINUE
        ENDIF
750   CONTINUE
      NDEL=IDELNW
C
C SET RETURN CODE
      IF(IOPT.EQ.7) THEN
        IRETC=1
      ELSE
        IRETC=0
      ENDIF
      GOTO 999
C
C RETURN
C ------
999   RETURN
      END SUBROUTINE

      END MODULE
