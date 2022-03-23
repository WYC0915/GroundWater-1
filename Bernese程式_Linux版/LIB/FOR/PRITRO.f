      MODULE s_PRITRO
      CONTAINS

C*
      SUBROUTINE PRITRO(TITLES,ITROPO,IEXTRA,NTRREQ,NPARTR,TRPLIM,
     1                  SIGTRP,NTRSTA,STATRP,TRPLMS,SIGTRS,ISGTRS,
     2                  ITRMAP,ITRGRD,STNAME)
CC
CC NAME       :  PRITRO
CC
CC PURPOSE    :  PRINT TROPOSPHERE MODEL AND MODEL PARAMETERS
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               ITROPO : INDEX FOR TROPOSPHERE MODEL         I*4
CC               IEXTRA : =0: OBSERVED     METEO VALUES       I*4
CC                        =1: EXTRAPOLATED METEO VALUES
CC                        =2: ESTIMATED    METEO VALUES
CC               NTRREQ : NUMBER OF LOCAL TROPOS. MODEL       I*4
CC                        REQUESTS
CC               NPARTR(I),I=1,2,..,NTRREQ: NUMBER OF PARA-   I*4
CC                        METERS IN REQUEST I
CC               TRPLIM(K,I),K=1,2, I=1,2,..,NTRREQ: TIME     R*8
CC                        INTERVAL OF VALIDITY (MJD) FOR
CC                        REQUEST I
CC               SIGTRP(I),I=1,2,..,NTRREQ: A PRIORI SIGMAS   R*8
CC                        FOR TROPOSPHERE PARAMETERS IN
CC                        M, M/(100M), M/(100M)**2, ...
CC               NTRSTA : NUMBER OF TROPOSPHERE REQUESTS      I*4
CC                        FOR INDIVIDUAL STATIONS
CC               STATRP(I),I=1,2,..,NTRSTA: STATION NUMBER    I*4
CC                        FOR REQUEST I
CC               TRPLMS(K,I),K=1,2,I=1,..,NTRSTA: TROPOS-     R*8
CC                        PHERE PARAMETER EST. FROM .. TO
CC               SIGTRS(J,I),I=1,2,..,NTRSTA: A PRIORI SIGMA  R*8
CC                        IN M FOR INDIV. TROPOSPHERE PARAM.
CC                        J=1: NORTH (GRADIENT)
CC                        J=2: EAST (GRADIENT)
CC                        J=3: UP (ZENITH DELAY)
CC               ISGTRS(I),I=1,2,..,NTRSTA: TYPE OF A PRIORI  I*4
CC                        SIGMA: ABS=0, REL=1 .
CC               ITRMAP : MAPPING FUNCTION FOR TROPOSP.EST.   I*4
CC                        =1: 1/COS(Z)
CC                        =2: HOPFIELD
CC                        =3,4: DRY/WET NIELL
CC                        =5,6: DRY/WET GMF
CC                        =7,8: DRY/WET VMF
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                             =3: TANZ
CC                             =4: CHEN & HERRING
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               STNAME(I),I=1,..,NSTAT: LIST OF ALL STATION  CH*16
CC                        NAMES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 18:32
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               30-MAY-92 : ??: ABSOLUTE AND RELATIVE SIGMAS FOR TROPOS.
CC               29-JUL-94 : MR: TROPOSPHERE MAPPING FUNCTION
CC               05-OCT-95 : TS: ALLOW MARINI MURRAY TROPO MODEL
CC               24-JUN-96 : TS: ALLOW ESTIMATED TROPOSPHERE VALUES
CC               28-JAN-97 : MR: SKIP ONE MORE LINE IN TEXT FILE
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               15-APR-97 : MR: ALLOW EXPONENTIAL TROP. SIGMAS
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               28-AUG-00 : MR: ADD NIELL MODEL AND DRY NIELL
CC               31-AUG-00 : MR: PRINT A PRIORI MODEL EVEN FOR IEXTRA=2
CC               13-NOV-02 : HB: USE NO MORE GPSESTT.SKL,
CC                               OUTPUT WRITTEN IN STANDARD OF VERSION 5.0
CC               12-MAY-03 : MM: MODIFIED WRT PIECEWISE LINEAR TROPOSPHERE
CC               16-JUN-05 : MM: COMCONST.INC REPLACED BY D_CONST
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               03-FEB-06 : RD: MODULE SIGMA1 IS NOT USED HERE
CC               24-AUG-06 : AG: GMF AND TAN(Z) IMPLEMENTED
CC               30-JUN-08 : RD: VMF ADDED
CC               11-NOV-08 : DT: ADD TROP.MODEL MENDES-PAVLIS
CC               04-JAN-11 : PS: CHEN/HERRING GRADIENT MAPPING ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: HREF, HUMREF, PREF, TREF
      USE s_timst2
      USE s_jmt
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICOM  , ID1   , ID2   , IEXTRA, II    , IM1   ,
     1          IM2   , IREQ  , ISTREQ, ISTTRP, ITRMAP, ITROPO, IY1   ,
     2          IY2   , NPARMX, NTRREQ, NTRSTA
C
      REAL*8    DAY1  , DAY2  , HOUR1 , HOUR2 , SIGVAL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*16  STNAME(*)
      CHARACTER*11  SIGTXT(3)
      CHARACTER*3   TYPTRS(2)
      CHARACTER*132 TEXT
      CHARACTER*19  TIMSTR
C
      REAL*8        TRPLIM(2,*),SIGTRP(*),TRPLMS(2,*),SIGTRS(3,*)
C
      INTEGER*4     NPARTR(*),STATRP(*),ISGTRS(*),ITRGRD(*)
C
C
C
      DATA TYPTRS/'abs','rel'/
C
C Print title lines
C -----------------
      WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1            TITLES(2)(1:LENGT1(TITLES(2)))
1     FORMAT(//,A,/,A,/,' ',131('-'),//)

      WRITE(LFNPRT,'(A,/,A,/,/,A,/,A,/)')
     1            ' 6. ATMOSPHERE',
     2            ' -------------',
     3            ' TROPOSPHERE MODEL',
     4            ' -----------------'
C
C TROPOSPHERE MODEL
C -----------------
      TEXT = ' '
      TEXT(1:29) = ' A priori troposphere model: '
      IF (iTropo == 0) THEN
        TEXT(1:35) = ' No troposphere corrections applied'
      ELSEIF (iTropo == 1) THEN
        TEXT(30:41) = 'Saastamoinen'
      ELSEIF(iTropo == 2) THEN
        TEXT(30:47) = 'Hopfield (Remondi)'
      ELSEIF(iTropo == 3) THEN
        TEXT(30:43) = 'Essen + Froome'
      ELSEIF(iTropo == 4) THEN
        TEXT(30:42) = 'Marini Murray'
      ELSEIF(iTropo == 5) THEN
        TEXT(30:34) = 'Niell'
      ELSEIF(iTropo == 6) THEN
        TEXT(30:34) = 'GMF'
      ELSEIF(iTropo == 7) THEN
        TEXT(30:34) = 'VMF'
      ELSEIF(iTropo == 8) THEN
        TEXT(30:42) = 'Mendes-Pavlis'
      ELSEIF((iTropo > 5 .AND. iTropo < 11).OR.
     1              (iTropo == 13 .OR.iTropo == 14)) THEN
        TEXT(30:56) = ' '
      ELSEIF(iTropo == 11) THEN
        TEXT(30:56) = 'Saastamoinen, dry part only'
      ELSEIF(iTropo == 12) THEN
        TEXT(30:52) = 'Hopfield, dry part only'
      ELSEIF(iTropo == 15) THEN
        TEXT(30:49) = 'Niell, dry part only'
      ELSEIF(iTropo == 16) THEN
        TEXT(30:49) = 'GMF, dry part only'
      ELSEIF(iTropo == 17) THEN
        TEXT(30:49) = 'VMF, dry part only'
      ENDIF
      WRITE(LFNPRT,'(A)')TEXT(1:LENGT1(TEXT))

      TEXT = ' '
      TEXT(1:29) = ' Meteo/Trop.delay values   : '

      IF (iExtra == 0) THEN
        TEXT(30: ) = 'Observed (from meteo files)'
      ELSEIF (iExtra == 1) THEN
        TEXT(30:42) = 'Extrapolated'
      ELSEIF (iExtra == 2) THEN
        TEXT(30:93) =
     1'Estimated values introduced if available (from GPSEST or ADDNEQ)'
      ENDIF
      WRITE(LFNPRT,'(A)') TEXT(1:LENGT1(TEXT))

      IF (iExtra == 1) THEN
        WRITE(LFNPRT,'(/,A,F9.2,A,A,F9.2,A,/,
     1              34X,A,F9.2,A,/,34X,A,F9.2,A)')
     1              ' Reference height :',HREF,' m    ',
     2              'Temperature at ref. height:',TREF,' C',
     3              'Pressure    at ref. height:',PREF,' mbar',
     4              'Humidity    at ref. height:',HUMREF,' %'
      ENDIF
C
C LOCAL TROPOSPHERE MODEL PARAMETERS
C ----------------------------------
      IF(NTRREQ /= 0) THEN
        WRITE(LFNPRT,'(/,/,A,/,A,/,/,A,/," ",132("-"),/)')
     1              ' LOCAL TROPOSPHERE MODEL PARAMETERS',
     2              ' ----------------------------------',
     3              ' Model  #par   Validity start    Validity end'
        DO 60 IREQ=1,NTRREQ
          CALL JMT(TRPLIM(1,IREQ),IY1,IM1,DAY1)
          CALL JMT(TRPLIM(2,IREQ),IY2,IM2,DAY2)
          ID1=DAY1
          ID2=DAY2
          HOUR1=(DAY1-ID1)*24.D0
          HOUR2=(DAY2-ID2)*24.D0
          WRITE(LFNPRT,4) IREQ,NPARTR(IREQ),IY1,IM1,ID1,HOUR1,
     1                IY2,IM2,ID2,HOUR2
4         FORMAT(I4,I7,1X,2(3X,I4,2I3,F5.1))
60      CONTINUE
C
C A PRIORI SIGMAS
        NPARMX=0
        DO 70 IREQ=1,NTRREQ
          IF(NPARTR(IREQ).GT.NPARMX) NPARMX=NPARTR(IREQ)
70      CONTINUE
        WRITE(LFNPRT,'(/,/,A,/," ",132("-"),/)')
     1              ' Sigma0 (m), Sigma1 (m/(100m)), ...'

        WRITE(LFNPRT,5) (SIGTRP(I),I=1,NPARMX)
5       FORMAT(10(10F13.5,/))
      ENDIF
C
C TROPOSPHERE PARAMETERS FOR INDIVIDUAL STATIONS
C ----------------------------------------------
      IF( NTRSTA /= 0 ) THEN
        WRITE(lfnPrt,'(/,/,A,/,A,/)')
     1              ' SITE-SPECIFIC TROPOSPHERE PARAMETERS',
     2              ' ------------------------------------'
        TEXT =' '
        TEXT(1:45) =' Mapping function used for delay estimation: '
        IF (iTrMap == 1) THEN
          TEXT(46:68) = ' 1/cos(zenith-distance)'
        ELSEIF (iTrMap == 2) THEN
          TEXT(46:68) = ' Hopfield              '
        ELSEIF (iTrMap == 3) THEN
          TEXT(46:68) = ' Dry Niell             '
        ELSEIF (iTrMap == 4) THEN
          TEXT(46:68) = ' Wet Niell             '
        ELSEIF (iTrMap == 5) THEN
          TEXT(46:68) = ' Dry GMF               '
        ELSEIF (iTrMap == 6) THEN
          TEXT(46:68) = ' Wet GMF               '
        ELSEIF (iTrMap == 7) THEN
          TEXT(46:68) = ' Dry VMF               '
        ELSEIF (iTrMap == 8) THEN
          TEXT(46:68) = ' Wet VMF               '
        ENDIF
        WRITE(lfnPrt,'(A)')TEXT(1:LENGT1(TEXT))
C
C GRADIENT OPTION
        TEXT=' Troposphere gradient estimation           :  '
        IF (ITRGRD(1).EQ.0) THEN
          TEXT(47:132)='No'
        ELSEIF (ITRGRD(1).EQ.1) THEN
          TEXT(47:132)='Tilted mapping'
        ELSEIF (ITRGRD(1).EQ.2) THEN
          TEXT(47:132)='Linear correction with distance'
        ELSEIF (ITRGRD(1).EQ.3) THEN
          TEXT(47:132)='Tan(z)'
        ELSEIF (ITRGRD(1).EQ.4) THEN
          TEXT(47:132)='Chen/Herring'
        ENDIF
        WRITE(LFNPRT,'(A)') TEXT(1:LENGT1(TEXT))
C
        WRITE(lfnPrt,'(/,A,A,/," ",131("-"),/)')
     1 ' Par   Station name       Reference epoch     ',
     2 '  sig_n (m)  sig_e (m)  sig_u (m)    abs/rel'
C
        ISTTRP=0
C
        DO 120 IREQ=1,NTRSTA
C
C COUNT REQUESTS OF EACH STATION
          IF (ISTTRP.NE.STATRP(IREQ)) ISTREQ=0
          ISTREQ=ISTREQ+1
          ISTTRP=STATRP(IREQ)
C
C GET EPOCH STRING
          CALL TIMST2(2,1,TRPLMS(1,IREQ),TIMSTR)
C
C CREATE ONE OUTPUT LINE
          DO ICOM=1,3
            SIGVAL=SIGTRS(ICOM,IREQ)
            IF ((SIGVAL.GE.0.00001D0 .AND.
     1                  SIGVAL.LE.9.99999D0) .OR.
     2                  SIGVAL.EQ.0.00000D0) THEN
              WRITE(SIGTXT(ICOM),'(F11.5)') SIGVAL
            ELSE
              WRITE(SIGTXT(ICOM),'(4X,D7.1)') SIGVAL
            ENDIF
          ENDDO
C
          WRITE(TEXT,6) IREQ,STNAME(ISTTRP),TIMSTR,
     1                  (SIGTXT(II),II=1,3),TYPTRS(ISGTRS(IREQ)+1)

6         FORMAT(1X,I4,2X,A16,3X,A19,1X,3A11,6X,A3)
          IF (ITRGRD(1).EQ.0 .OR. MOD(ISTREQ-1,ITRGRD(2)).NE.0) THEN
            TEXT(47:68)=' '
          ENDIF
          WRITE(LFNPRT,'(A)') TEXT(1:LENGT1(TEXT))
120     CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
