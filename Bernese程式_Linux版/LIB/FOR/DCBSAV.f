      MODULE s_DCBSAV
      CONTAINS

C*
      SUBROUTINE DCBSAV(TITLE ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1                  STNAME,CLFRTO,NALLSAT,ALLSATNUM)
CC
CC NAME       :  DCBSAV
CC
CC PURPOSE    :  SAVE DIFFERENTIAL CODE BIASES FOR SATELLITES AND
CC               RECEIVERS
CC
CC PARAMETERS :
CC         IN :  TITLE  : GENERAL TITLE                       CH*80
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8(*)
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: INVERSE OF   R*8(*)
CC                        NORMAL EQUATION MATRIX
CC               RMS    : RMS ERROR OF UNIT WEIGHT            R*8
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16(*)
CC               CLFRTO(K,I),K=1,2: TIME LIMITS FOR REQUEST I R*8
CC               NALLSAT: NUMBER OF ALL SATELLITES            I*4
CC               ALLSATNUM(I), I=1,NALLSAT: LIST OF PRNS      I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC CREATED    :  21-OCT-97
CC
CC CHANGES    :  23-OCT-97 : SS: ORDER RECEIVER LIST
CC               31-OCT-97 : SS: DO NOT SAVE UNOBSERVED DCBS
CC               12-NOV-97 : SS: BUT SAVE A PRIORI SATELLITE DCBS
CC               26-JAN-98 : SS: "MAXPAR" FROM 1000 TO 3000
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               21-APR-99 : SS: "MAXREC" FROM 100 TO 200
CC               07-APR-00 : SS: SET "ICBTYP"
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               06-SEP-01 : SS: "MAXPAR" FROM 3000 TO 4000
CC               07-MAY-02 : SS: DCB UPDATE
CC               17-SEP-02 : SS: CHECK ALSO "XXX" TO DETECT UNOBSERVED DCBS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               09-MAY-09 : RD: WRITE FREQ-DEP. CODE BIAS INTO DCB-FILE
CC               18-AUG-11 : LP: DCBSYS 'E' added for Galileo
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC               06-JUN-12 : LP: USE g_svnsys for filling up DCBSYS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: r8b, LFNERR
      USE m_maxdim, ONLY: MAXSAT, MAXREC, MAXSTA
      USE m_global, ONLY: g_svnsys
      USE d_const,  ONLY: C
      USE d_satfil, ONLY: typeMWTR
      USE f_ikf
      USE s_dcbcor
      USE s_maxtst
      USE s_wtcbfl
      USE s_exitrc
      USE s_gtflna
      USE s_alcerr
      USE s_gtsensor
      USE s_svn2chr
      USE f_svnsys
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IP    , IRC1  , IRC2  , IRCINP, IRCOUT, ITYP  ,
     1          MXCLCQ, NPAR  , NUMREC, NUMSAT, NUMIFB, SATNUM,
     2          ISTA0 , NALLSAT, ISAT , MINFRQ, MAXFRQ, II    ,
     3          IAC   , FRQNUM
C
      REAL*8    DIST  , RMS   , RMSDCB, XXXDCB, XXX0  , TIMPAR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*80  TITLE
      CHARACTER*32  DCBINP,DCBOUT
      CHARACTER*16  STNAME(*),DCBID2(MAXREC)
      CHARACTER*16  DCBID3(2,MAXSAT*MAXSTA)
      CHARACTER*6   MXNLCQ
      CHARACTER*1   DCBSYS(MAXREC)
C
      REAL*8        XXX(*),ANOR(*)
      REAL*8        DCBVA1(2,MAXSAT),DCBVA2(2,MAXREC)
      REAL*8        DCBVA3(4,MAXSAT*MAXSTA),CLFRTO(2,*)
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: FREQ
C
      INTEGER*4     LOCQ(MXCLCQ,*),DCBID1(MAXSAT),DCBIN1(MAXSAT)
      INTEGER*4     DCBIN2(MAXREC),DCBIN3(MAXSAT*MAXSTA)
      INTEGER*4     ALLSATNUM(*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
      DATA ITYP/0/
C
C RETURN, IF NO CODE BIAS OUTPUT FILE SPECIFIED
C ---------------------------------------------
      CALL GTFLNA(0,'DCBOUT ',DCBOUT,IRCOUT)
      IF (IRCOUT.EQ.1) RETURN
C
      CALL GTFLNA(0,'DCBINP ',DCBINP,IRCINP)
C
      DO IP=1,NPAR
        IF (LOCQ(1,IP).EQ.8 .AND.
     1      LOCQ(2,IP).EQ.2 .AND.
     2      LOCQ(6,IP).LT.0) THEN
          WRITE(LFNERR,903)
903       FORMAT(/,' ### SR DCBSAV: MULTIPLIERS NOT SAVED',/)
          RETURN
        ENDIF
      ENDDO
C
C SAVE CODE BIAS INFO
C -------------------
      NUMSAT=0
      NUMREC=0
      NUMIFB=0
      ISTA0 =0
C
      DO IP=1,NPAR
C
C INTER-FREQUENCY CODE BIASES
        IF (LOCQ(1,IP).EQ.2) THEN
          RMSDCB=1.0D9*RMS*DSQRT(ANOR(IKF(IP,IP)))/C
          XXXDCB=1.0D9*XXX(IP)/C
          TIMPAR=(CLFRTO(1,LOCQ(3,IP))+CLFRTO(2,LOCQ(3,IP)))/2d0
C
C STATION-SPECIFIC
          IF (LOCQ(6,IP).EQ.0.AND.ISTA0.EQ.0) THEN
            ISTA0 = LOCQ(2,IP)
            WRITE(LFNERR,904)
904         FORMAT(/,' ### SR DCBSAV: STATION-SPECIFIC RECEIVER ',
     1             /,16X,'CLOCK OFFSETS ARE NOT SAVED',/)
          ENDIF
C
C FREQUENCY-SPECIFIC
          IF (LOCQ(6,IP).EQ.1) THEN
            IF (ISTA0.NE.LOCQ(2,IP)) THEN
              NUMIFB=NUMIFB+1
              DCBID3(1,NUMIFB) = 'G'
              DCBID3(2,NUMIFB) = STNAME(LOCQ(2,IP))
              DCBVA3(:,NUMIFB) = 0d0
            ENDIF
            DO ISAT = 1,NALLSAT
              CALL gtsensor(ALLSATNUM(ISAT),TIMPAR,
     1                      type1=typeMWTR,IFRQ=FRQNUM)
              IF (SVNSYS(1,1,(/ ALLSATNUM(ISAT) /)) .AND.
     1            LOCQ(4,IP).EQ.FRQNUM) THEN
                CALL dcbcor(2,0,0,STNAME(LOCQ(2,IP)),'',FRQNUM,
     1                      5,TIMPAR,XXX0)
                NUMIFB=NUMIFB+1
                DCBID3(1,NUMIFB) = 'R'
                WRITE(DCBID3(1,NUMIFB)(2:3),'(I2.2)')ALLSATNUM(ISAT)-100
                DCBID3(2,NUMIFB) = STNAME(LOCQ(2,IP))
                DCBVA3(1,NUMIFB) = XXXDCB+XXX0
                DCBVA3(2,NUMIFB) = RMSDCB
                DCBVA3(3,NUMIFB) = CLFRTO(1,LOCQ(3,IP))
                DCBVA3(4,NUMIFB) = CLFRTO(2,LOCQ(3,IP))
              ENDIF
            ENDDO
            ISTA0 = LOCQ(2,IP)
          ENDIF
C
C SATELLITE-SPECIFIC
          IF (LOCQ(6,IP).EQ.2) THEN
            IF (ISTA0.NE.LOCQ(2,IP).AND.
     1          LOCQ(7,IP).EQ.1.AND.LOCQ(4,IP).GE.100) THEN
              NUMIFB=NUMIFB+1
              DCBID3(1,NUMIFB) = 'G'
              DCBID3(2,NUMIFB) = STNAME(LOCQ(2,IP))
              DCBVA3(:,NUMIFB) = 0d0
            ENDIF
            CALL DCBCOR(2,0,LOCQ(4,IP),STNAME(LOCQ(2,IP)),'',0,
     1                  5,TIMPAR,XXX0)
            NUMIFB=NUMIFB+1
            CALL SVN2CHR(LOCQ(4,IP),SATNUM,DCBID3(1,NUMIFB))
            WRITE(DCBID3(1,NUMIFB)(2:3),'(I2.2)') SATNUM
            DCBID3(2,NUMIFB) = STNAME(LOCQ(2,IP))
            DCBVA3(1,NUMIFB) = XXXDCB+XXX0
            DCBVA3(2,NUMIFB) = RMSDCB
            DCBVA3(3,NUMIFB) = CLFRTO(1,LOCQ(3,IP))
            DCBVA3(4,NUMIFB) = CLFRTO(2,LOCQ(3,IP))
            ISTA0 = LOCQ(2,IP)
          ENDIF
C
C FREQUENCY-SPECIFIC, POLYNOMIAL
          IF (LOCQ(6,IP).EQ.4) THEN
            MINFRQ=999999
            MAXFRQ=999999
            DO ISAT=1,NALLSAT
              CALL gtsensor(ALLSATNUM(ISAT),TIMPAR,
     1                      type1=typeMWTR,IFRQ=FRQNUM)
              IF ( SVNSYS(1,1,(/ ALLSATNUM(ISAT) /)) ) THEN
                IF (MINFRQ.EQ.999999.OR.MINFRQ.GT.FRQNUM) MINFRQ=FRQNUM
                IF (MAXFRQ.EQ.999999.OR.MAXFRQ.LT.FRQNUM) MAXFRQ=FRQNUM
              ENDIF
            ENDDO
C
            IF (MINFRQ.NE.999999) THEN
              ALLOCATE(FREQ(2,MAXFRQ-MINFRQ+1),STAT=IAC)
              CALL ALCERR(IAC,'FREQ',(/2,MAXFRQ-MINFRQ+1/),'DCBSAV')
C
              IF (ISTA0.NE.LOCQ(2,IP)) THEN
                NUMIFB=NUMIFB+1
                DCBID3(1,NUMIFB) = 'G'
                DCBID3(2,NUMIFB) = STNAME(LOCQ(2,IP))
                DCBVA3(:,NUMIFB) = 0d0
                FREQ   = 0d0
              ENDIF
              DO II=MINFRQ,MAXFRQ
                FREQ(1,II-MINFRQ+1)=FREQ(1,II-MINFRQ+1)+
     1                     XXXDCB*(II-6)**(LOCQ(4,IP)-1)
                FREQ(2,II-MINFRQ+1)=FREQ(2,II-MINFRQ+1)+
     1                    (RMSDCB*(II-6)**(LOCQ(4,IP)-1))**2
              ENDDO
              IF (LOCQ(4,IP).EQ.LOCQ(5,IP)) THEN
                DO ISAT = 1,NALLSAT
                  IF (SVNSYS(1,1,(/ ALLSATNUM(ISAT) /))) THEN
                    CALL gtsensor(ALLSATNUM(ISAT),TIMPAR,
     1                            type1=typeMWTR,IFRQ=FRQNUM)
                    NUMIFB=NUMIFB+1
                    DCBID3(1,NUMIFB) = 'R'
                    WRITE(DCBID3(1,NUMIFB)(2:3),'(I2.2)')
     1                    ALLSATNUM(ISAT)-100
                    DCBID3(2,NUMIFB) = STNAME(LOCQ(2,IP))
                    DCBVA3(1:2,NUMIFB)=FREQ(1:2,FRQNUM-MINFRQ+1)
                    DCBVA3(2,NUMIFB) = DSQRT(DCBVA3(2,NUMIFB))
                    DCBVA3(3,NUMIFB) = CLFRTO(1,LOCQ(3,IP))
                    DCBVA3(4,NUMIFB) = CLFRTO(2,LOCQ(3,IP))
                  ENDIF
                ENDDO
              ENDIF
              DEALLOCATE(FREQ)
              ISTA0 = LOCQ(2,IP)
            ENDIF
          ENDIF
C
        ELSE IF (LOCQ(1,IP).EQ.8) THEN
          RMSDCB=RMS*DSQRT(ANOR(IKF(IP,IP)))
C
          IF (LOCQ(2,IP).EQ.1) THEN
            IF (RMSDCB.NE.0.D0 .AND. XXX(IP).NE.0.D0) THEN
              NUMSAT=NUMSAT+1
C
              CALL MAXTST(1,'DCBSAV','MAXSAT',MAXSAT,NUMSAT,IRC1)
              IF (IRC1.EQ.1) CALL EXITRC(2)
C
              ITYP=LOCQ(5,IP)
              IF (IRCINP.EQ.0) THEN
                CALL DCBCOR(2,0,LOCQ(3,IP),' ',' ',0,ITYP,
     1                      0.D0,DIST)
              ELSE
                DIST=0.D0
              ENDIF
C
              DCBID1(NUMSAT)=LOCQ(3,IP)
              DCBVA1(1,NUMSAT)=XXX(IP)+DIST
              DCBVA1(2,NUMSAT)=RMSDCB
            ELSE
              WRITE(LFNERR,901) LOCQ(3,IP)
901           FORMAT(/,' ### SR DCBSAV: UNOBSERVED DCB NOT SAVED',
     1          /,16X,'SVN: ',I3.2,/)
            ENDIF
          ELSE
            IF (RMSDCB.NE.0.D0 .AND. XXX(IP).NE.0.D0) THEN
              NUMREC=NUMREC+1
C
              CALL MAXTST(1,'DCBSAV','MAXREC',MAXREC,NUMREC,IRC2)
              IF (IRC2.EQ.1) CALL EXITRC(2)
C
              ITYP=MAX0(LOCQ(6,IP),0)
              IF (IRCINP.EQ.0) THEN
                CALL DCBCOR(2,0,LOCQ(5,IP),STNAME(LOCQ(3,IP)),' ',0,
     1                      ITYP,0.D0,DIST)
              ELSE
                DIST=0.D0
              ENDIF
C
              DCBID2(NUMREC)=STNAME(LOCQ(3,IP))
              DCBVA2(1,NUMREC)=XXX(IP)+DIST
              DCBVA2(2,NUMREC)=RMSDCB
c              IF (LOCQ(5,IP).EQ.1) THEN
c                DCBSYS(NUMREC)='G'
c              ELSE IF (LOCQ(5,IP).EQ.2) THEN
c                DCBSYS(NUMREC)='R'
c              ELSE IF (LOCQ(5,IP).EQ.3) THEN
c                DCBSYS(NUMREC)='E'
c              ENDIF
c              ADD_GNSS_HERE
              DCBSYS(NUMREC) = g_svnsys(LOCQ(5,IP)-1)
            ELSE
              WRITE(LFNERR,902) STNAME(LOCQ(3,IP)),LOCQ(5,IP)
902           FORMAT(/,' ### SR DCBSAV: UNOBSERVED DCB NOT SAVED',
     1          /,16X,'STATION NAME: ',A16,'-',I1/)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
      IF ((NUMSAT+NUMREC.GT.0 .AND. ITYP.NE.0) .OR. NUMIFB.GT.0) THEN
        CALL WTCBFL(DCBOUT,TITLE ,NUMSAT,NUMREC,NUMIFB,ITYP  ,
     1              DCBID1,DCBVA1,DCBID2,DCBVA2,DCBSYS,DCBID3,
     2              DCBVA3,DCBIN1,DCBIN2,DCBIN3)
      ELSE
        WRITE(LFNERR,900)
900     FORMAT(/,' ### SR DCBSAV: NO DCB-RELATED INFO FOUND',/)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
