      MODULE s_PRIPHC
      CONTAINS

C*
      SUBROUTINE PRIPHC(PRIOPT,TITLES,NANRAO,ANTRAO,NUMRAO,PRNRAO,
     1                  NFRRAO,SIGRAO,NEURAO,NANCAL,ANTCAL,NUMCAL,
     2                  PRNCAL,NFRCAL,NPTCAL,SIGCAL,NANSPV,RAPZENMAX)
CC
CC NAME       :  PRIPHC
CC
CC PURPOSE    :  PRINT PHASE CENTER CORRECTIONS AND PARAMETERS
CC
CC PARAMETERS :
CC         IN :  PRIOPT : PRINT FLAG FOR PHASE CENTER CORR.   I*4
CC                         =0 : NO PRINT
CC                         =1 : PRINT
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NANRAO : NUMBER OF RECEIVER ANTENNA OFFSETS  I*4
CC               ANTRAO(J,I),J=1,2, I=1,..,NANRAO:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR REQUEST I
CC               NUMRAO(2,I),I=1,..,NANRAO: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNRAO(I),I=1,..,NANRAO: SAT.SYSTEM FOR      I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               NFRRAO(I),I=1,..,NANRAO: FREQUENCY FOR       I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               SIGRAO(J,I),J=1,2,I=1,..,NANRAO: A PRIORI    R*8
CC                        SIGMAS IN METERS
CC                        J=1: HORIZONTAL COMPONENTS
CC                        J=2: VERTICAL COMPONENT
CC               NEURAO(I),I=1,3: COMPONENTS TO BE ESTIMATED  I*4
CC                        (I=1: NORTH, I=2: EAST, I=3: UP)
CC                        =1: ESTIMATION
CC                        =0: NO ESTIMATION
CC               NANCAL : NUMBER OF PHASE CENTER REQUESTS     I*4
CC               ANTCAL(J,I),J=1,2, I=1,..,NANCAL:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR REQUEST I
CC               NUMCAL(2,I),I=1,..,NANCAL: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNCAL(I),I=1,..,NANCAL: SAT.SYSTEM FOR REC. I*4
CC                        ANTENNA PHASE CENTER REQUEST I
CC               NFRCAL(I),I=1,..,NANCAL: FREQUENCY FOR REC.  I*4
CC                        ANTENNA PHASE CENTER REQUEST I
CC               NPTCAL(J,I),J=1,2, I=1,..,NANCAL: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               SIGCAL(I),I=1,..,NANCAL: A PRIORI SIGMAS IN  R*8
CC                        METERS
CC               NANSPV : NUMBER OF SATELLITE ANTENNA PHASE   I*4
CC                        CENTER GROUPS TO BE ESTIMATED
CC               RAPZENMAX: MAXIMUM ZENITH ANGLE ALLOWED FOR  R*8
CC                        REC. ANT. PATTERN ESTIMATION
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  07-JAN-96
CC
CC CHANGES    :  25-NOV-02 : RS: PRINT FIRST TITLE LINE, IF SATELLITE
CC                               ANTENNA PHASE CENTER VARIATIONS ARE EST.
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               24-APR-03 : RS: CORRECT SKELETON FILE SUBSTITUTES
CC               11-AUG-03 : RS: READ NEW ANT. PHASE CENTER FILE FORMAT
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               13-NOV-03 : RS: POLYGON-MODE: PRINT CORRECT NUMBER OF
CC                               POINTS IN AZIMUTH DIRECTION, ADD RAPZENMAX
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               06-JUL-07 : AG: WRITING OF PHASE CENTER INFORMATION ADAPTED
CC               20-FEB-08 : RD: BUG-FIX REPORTING REC-ANT MODEL TYPE
CC               03-JUL-09 : SL: WRITE STATEMENT CORRECTED (COMMA ADDED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_global, ONLY: g_svnsys,g_strsys
      USE d_const,  ONLY: PI
      USE d_phaecc, ONLY: recant,satant
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICAL  , IRAO  , JJ    , MAXELV, NANCAL,
     2          NANRAO, iant  , isys  , ifrq  , ii    , dummy
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXELV=19)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*20  ANTCAL(2,*),ANTRAO(2,*)
      CHARACTER*7   MODCAL
      CHARACTER*4   FRQSTR(5)
      CHARACTER*1   NOYES(2)
      CHARACTER*27  pcvs
      CHARACTER*40  pcvr
C
      REAL*8        SIGCAL(*),SIGRAO(2,*)
      REAL*8        RAPZENMAX,ZMXDEG
C
      INTEGER*4     PRIOPT
      INTEGER*4     NUMCAL(2,*),PRNCAL(*),NFRCAL(*),NPTCAL(2,*)
      INTEGER*4     NUMRAO(2,*),PRNRAO(*),NFRRAO(*),NEURAO(3)
      INTEGER*4     NANSPV
C
      LOGICAL       first
C
      DATA NOYES/'N','Y'/
      DATA FRQSTR/' L1 ',' L2 ','BOTH',' LC ','DIFF'/
C
C PRINT TITLE LINES
C -----------------
      IF(PRIOPT.EQ.0 .AND. NANRAO.EQ.0 .AND. NANCAL.EQ.0 .AND.
     1   NANSPV.EQ.0) THEN
        RETURN
      ELSE
        WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
2       FORMAT(//,A,/,A,/,' ',131('-'),//)
        WRITE(LFNPRT,"(
     1       ' 9. RECEIVER AND SATELLITE ANTENNA PHASE CENTERS'
     2    ,/,' -----------------------------------------------'
     3    ,/,1X)")
      ENDIF
      IF(PRIOPT.EQ.0 .AND. NANRAO.EQ.0 .AND. NANCAL.EQ.0) THEN
        RETURN
      ELSE
        WRITE(LFNPRT,"(
     1       ' A PRIORI PHASE CENTER OFFSETS AND VARIATIONS:'
     2    ,/,' --------------------------------------------'
     3    ,/,' '
     4    ,/,' ANTENNA/RADOME CODE  NUMBER FREQ PHASE CENTER ',
     5       'OFFSETS (MM)',/,37X,'NORTH    EAST      UP  ',
     6       'dEle dAzi MaxEle'
     7    ,/,1X,131('-'))")
C
C PRINT A PRIORI PHASE CENTER ECCENTRICITIES
C ------------------------------------------
        DO iant=1,SIZE(satant)
          first=.true.
          IF (satant(iant)%sys(0)%typ == 0) THEN
            pcvs = ' No PCV values used       '
          ELSEIF (satant(iant)%sys(0)%typ == 1) THEN
            pcvs = ' Phase center maps used   '
          ELSEIF (satant(iant)%sys(0)%typ >= 2) THEN
            pcvs = ' Spherical harmonics used '
          ENDIF
          DO ifrq=1,satant(iant)%sys(0)%nfreq
            IF (first) THEN
              WRITE(LFNPRT,"(1X,A20,1X,I6,2X,I2.2,3(1X,F8.2),
     1                      3(1X,I4), A)")
     2             satant(iant)%name,satant(iant)%numb,
     3             satant(iant)%sys(0)%freq(ifrq)%freq,
     4            (satant(iant)%sys(0)%freq(ifrq)%off(0,ii),ii=1,3),
     5             satant(iant)%sys(0)%resolu(2),
     6             satant(iant)%sys(0)%resolu(3),
     7             satant(iant)%sys(0)%resolu(4),TRIM(pcvs)
              first=.false.
            ELSE
              WRITE(LFNPRT,"(30X,I2.2,3(1X,F8.2))")
     1             satant(iant)%sys(0)%freq(ifrq)%freq,
     2            (satant(iant)%sys(0)%freq(ifrq)%off(0,ii),ii=1,3)
            ENDIF
          ENDDO
          WRITE(lfnprt,"()")
        ENDDO
        DO iant=1,SIZE(recant)
          first=.true.
          dummy = 0
          DO isys=0,2
            IF (isys > SIZE(recant(iant)%sys)-1) EXIT
            IF (recant(iant)%sys(isys)%typ == 0) THEN
              pcvr = ' No PCV values used for '//g_strsys(isys)
            ELSEIF (recant(iant)%sys(isys)%typ == 1) THEN
              pcvr = ' Phase center maps used for '//g_strsys(isys)
            ELSEIF (recant(iant)%sys(isys)%typ >= 2) THEN
              pcvr = ' Spherical harmonics used for '//g_strsys(isys)
            ENDIF
            DO ifrq=1,recant(iant)%sys(isys)%nfreq
              IF (first) THEN
                WRITE(LFNPRT,"(1X,A20,1X,I6,1X,A1,I2.2,3(1X,F8.2),
     1                          3(1X,I4),A)")
     2             recant(iant)%name,recant(iant)%numb,g_svnsys(isys),
     3             recant(iant)%sys(isys)%freq(ifrq)%freq,
     4            (recant(iant)%sys(isys)%freq(ifrq)%off(0,ii),ii=1,3),
     5             recant(iant)%sys(0)%resolu(2),
     6             recant(iant)%sys(0)%resolu(3),
     7             recant(iant)%sys(0)%resolu(4),TRIM(pcvr)
                first=.false.
              ELSE
                IF (dummy /= isys) THEN
                  dummy =isys
                  WRITE(LFNPRT,"(29X,A1,I2.2,3(1X,F8.2),3(1X,I4),A)")
     1            g_svnsys(isys),recant(iant)%sys(isys)%freq(ifrq)%freq,
     2            (recant(iant)%sys(isys)%freq(ifrq)%off(0,ii),ii=1,3),
     3             recant(iant)%sys(0)%resolu(2),
     4             recant(iant)%sys(0)%resolu(3),
     5             recant(iant)%sys(0)%resolu(4),TRIM(pcvr)
                ELSE
                  WRITE(LFNPRT,"(29X,A1,I2.2,3(1X,F8.2))")
     1            g_svnsys(isys),recant(iant)%sys(isys)%freq(ifrq)%freq,
     2            (recant(iant)%sys(isys)%freq(ifrq)%off(0,ii),ii=1,3)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          WRITE(lfnprt,"()")
        ENDDO
C
C PRINT RECEIVER ANTENNA PHASE CENTER OFFSET REQUESTS
C ---------------------------------------------------
        IF (NANRAO.NE.0) THEN
C
          WRITE(LFNPRT,"(
     1         ' '
     2      ,/,' RECEIVER ANTENNA OFFSET ESTIMATION:'
     3      ,/,' ----------------------------------'
     4      ,/,'                                                    ',
     5         'ANT. NUMBERS                       ESTIM.      SIGMA',
     6         ' (M)'
     7      ,/,' REQ.  ANTENNA NAME           RECEIVER NAME         ',
     8         ' FROM    TO     FRQ.   SYSTEM      N E U    HORIZ.  ',
     9         '  VERT.'
     1      ,/,1X,131('-')
     2      ,/,1X)")
C
          DO 170 IRAO=1,NANRAO
            WRITE(LFNPRT,161) IRAO,(ANTRAO(JJ,IRAO),JJ=2,1,-1),
     1                        (NUMRAO(JJ,IRAO),JJ=1,2),
     2                        FRQSTR(NFRRAO(IRAO)),
     3                        G_STRSYS(PRNRAO(IRAO)),
     4                        (NOYES(NEURAO(JJ)+1),JJ=1,3),
     5                        (SIGRAO(JJ,IRAO),JJ=1,2)
161         FORMAT(I4,3X,A20,3X,A20,1X,2I7,3X,A4,3X,A7,4X,3(1X,A1),
     1             2F10.5)
170       CONTINUE
          WRITE(LFNPRT,'( )')
        ENDIF
C
C PRINT RECEIVER ANTENNA PHASE CENTER VARIATION REQUESTS
C ------------------------------------------------------
        IF (NANCAL.NE.0) THEN
          ZMXDEG = RAPZENMAX/PI*180.D0
C
          WRITE(LFNPRT,"(
     1         ' '
     2      ,/,' RECEIVER ANTENNA PHASE CENTER VARIATION ESTIMATION:'
     3      ,/,' --------------------------------------------------'
     4      ,/,51X,' ANT. NUMBERS                            #PNTS/DEGR'
     5      ,/,' REQ.  ANTENNA NAME           RECEIVER NAME        ',
     6         '  FROM    TO     FRQ.   SYSTEM    MODEL    EL. AZ. ',
     7         ' ZENMAX   SIGMA (M)'
     8      ,/,1X,131('-')
     9      ,/,1X)")
C
          DO 70 ICAL=1,NANCAL
            IF (NPTCAL(1,ICAL).GT.0) THEN
              MODCAL='POLYGON'
              WRITE(LFNPRT,61) ICAL,(ANTCAL(JJ,ICAL),JJ=2,1,-1),
     1                         (NUMCAL(JJ,ICAL),JJ=1,2),
     2                         FRQSTR(NFRCAL(ICAL)),
     3                         G_STRSYS(PRNCAL(ICAL)),MODCAL,
     4                         NPTCAL(1,ICAL),(NPTCAL(2,ICAL)-1),
     5                         ZMXDEG,SIGCAL(ICAL)
61            FORMAT(I4,3X,A20,3X,A20,1X,2I7,3X,A4,2(3X,A7),2I4,F8.1,1X,
     1               F11.5)
            ELSE
              MODCAL='SPHERIC'
              WRITE(LFNPRT,61) ICAL,(ANTCAL(JJ,ICAL),JJ=2,1,-1),
     1                         (NUMCAL(JJ,ICAL),JJ=1,2),
     2                         FRQSTR(NFRCAL(ICAL)),
     3                         G_STRSYS(PRNCAL(ICAL)),MODCAL,
     4                         (IABS(NPTCAL(JJ,ICAL)),JJ=1,2),
     5                         ZMXDEG,SIGCAL(ICAL)
            ENDIF
70        CONTINUE
          WRITE(LFNPRT,'( )')
        ENDIF
C
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
