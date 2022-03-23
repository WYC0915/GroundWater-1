      MODULE s_PRIORP
      CONTAINS

C*
      SUBROUTINE PRIORP(MODE ,NESTSAT,ESTSAT,NORB  ,SEQORB,
     1                  PREC  ,NSTCEP,FRCTYP,NSASTC,NUMSTC,NARC  ,
     2                  NSTCEF,TIMSTC,SIGSTC,NANOFF,NSAOFF,SATOFF,
     3                  PAROFF,NRQOFF,GRPOFF,SIGOFF,TIMOFF,ISGOFF,
     4                  GNROFF)
CC
CC NAME       :  PRIORP
CC
CC PURPOSE    :  PRINT ORBITAL PARAMETERS TO BE ESTIMATED AND A
CC               PRIORI SIGMAS
CC
CC PARAMETERS :
CC         IN :  MODE   : 1: GNSS SATELLITES                  I*4
CC                        2: LEOS
CC               NESTSAT: #SAT FOR ORBIT DETERMINATION        I*4
CC               ESTSAT : PRN FOR THAT ORBITS ARE REQU.       I*4(*)
CC               NORB   : NUMBER OF ORBITAL PARAMETERS        I*4
CC               SEQORB(I),I=1,..,NORB: ORBIT PARAMETER NR.   I*4
CC                        TO BE ESTIMATED
CC               PREC(I): A PRIORI SIGMAS FOR ALL ELEMENTS    R*8
CC               NSTCEP : NUMBER OF STOCHASTIC EPOCHS         I*4
CC               FRCTYP : STOCHASTIC FORCE TYPES FOR MAX      I*4(*)
CC                        3 FORCES PER EPOCH
CC                     (L)=1 FORCE = R : RADIAL
CC                     (L)=2 FORCE = S : NORMAL TO R IN ORB. PLANE
CC                     (L)=3 FORCE = W : NORMAL TO ORB PLANE
CC                     (L)=4 FORCE = DIRECTION SUN --> SATELLITE
CC                     (L)=5 FORCE = Y DIRECTION OF SPACE CRAFT
CC                     (L)=6 FORCE = X DIRECTION OF SPACE CRAFT
CC               NSASTC : NUMBER OF SATS WITH STOCH ORBITS    I*4
CC               NUMSTC : CORRESPONDING SAT NUMBERS           I*4(*)
CC               NARC   : NUMBER OF ARCS                      I*4
CC               NSTCEF : NUMBER OF STOCHASTIC EPOCHS/ARC     I*4(*)
CC               TIMSTC : STOCHASTIC EPOCHS (=INT BOUNDARIES) R*8(*,*,*,*)
CC               SIGSTC : A PRIORI SIGMAS FOR STOCH REQUESTS  R*8(*,*)
CC               NANOFF : NUMBER OF SATELLITE ANTENNA OFFSET  I*4
CC                        GROUPS TO BE ESTIMATED
CC               NSAOFF(I),I=1,..,NANOFF: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO GROUP I
CC               SATOFF(J,I),J=1,..,NSAOFF(I),I=1,..,NANOFF:  I*4
CC                        SATELLITE NUMBERS OF EACH ANTENNA
CC                        GROUP
CC               PAROFF(K),K=1,2,3: ANTENNA OFFSET COMPONENTS I*4
CC                        TO BE ESTIMATED (X,Y,Z IN SATELLITE
CC                        REFERENCE FRAME
CC                        =1: ESTIMATED
CC                        =0: NOT ESTIMATED
CC               NRQOFF : NUMBER OF SATELL. ANTENNA REQUESTS  I*4
CC               GRPOFF(I),I=1,..,NRQOFF: ANTENNA GROUP FOR   I*4
CC                        REQUEST NUMBER I
CC               SIGOFF(J,I),J=1,2,3,I=1,..,NRQOFF: A PRIORI  R*8
CC                        SIGMAS FOR COMP. J AND ANT. REQ. I
CC               TIMOFF(J,I),J=1,2,I=1,..,NRQOFF: TIME INTER- R*8
CC                        VAL FOR ANTENNA REQUEST I
CC               ISGOFF(I),I=1,..,NRQOFF: TYPE OF SIGMA       I*4
CC                        =0: ABSOLUTE SIGMA
CC                        =1: SIGMA RELATIVE TO THE PREVIOUS
CC                            PARAMETER OF THE SAME GROUP
CC               GNROFF(I),I=1,..,NANOFF: USER-DEFINED NUMBER I*4
CC                        FOR SATELLITE ANTENNA OFFSET GROUP I
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/20 11:06
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               22-MAR-93 : ??: STOCHASTIC ORBIT PARAMETERS
CC               03-APR-93 : ??: SATELLITE ANTENNA OFFSET PARAMETERS
CC               28-DEC-93 : MR: TIME WINDOWS FOR SAT. ANT. OFFSETS
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               14-AUG-97 : SS: "IGM" AND "SIGGM" REMOVED
CC               23-JAN-02 : CU: USE PRIORP FOR ADDNEQ2
CC               07-FEB-02 : CU: PRINT SAT.ANT.OFFSET ONLY IF NSAOFF > 0
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               10-DEC-02 : CU: DON'T USE PRIORP FOR ADDNEQ2,
CC                               DON'T USE LNFPLT (SKELETON FILE)
CC               10-NOV-03 : RS: ADD GNROFF
CC               10-DEC-03 : AJ: ADDITIONAL COMPONENT FOR TIMSTC
CC               26-MAY-05 : RD: USE DIMENSION MAXSGR FOR SATOFF AND SATSPV
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-OCT-06 : RD: MANUAL SELECTION OF SAT. FOR ORBIT DETERM.
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               12-NOV-08 : DT: SET ORBIT MODEL DESCRIPTION
CC                               (FROM ORBDSC%ORBMOD); FORMAT CHANGE FOR OUTPUT
CC               03-DEC-10 : HB: REPLACE MXCVAR WITH MAXELE=15
CC               03-NOV-11 : RD: TIMSTR->TIMST2; MORE DIGITS FOR ANTENNA SIGMAS
CC               12-MAR-12 : HB: LAYOUT UNIFICATION FOR STOCHASTIC PARAMETERS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const,  ONLY: ars
      USE P_ORBGEN, ONLY: orbdsc

      USE s_timst2
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IAROLD, IELE  , IOFF  , IOFR  , IRQOLD,
     1          ISAT  , ISEQ  , ISTC  , IT    , MXCSAT, MXCSGR, MAXELE,
     2          MXCSTC, NANOFF, NARC  , NORB  , NRQOFF, NSASTC,
     3          NSTCEP, MODE  ,NESTSAT, JSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MAXELE=15)

      CHARACTER*132 TEXT
      CHARACTER*40  TSTRN2
      CHARACTER*19  TSTRNG
      CHARACTER*6   MXNSAT,MXNSTC,MXNSGR
      CHARACTER*3   FRCTXT(6)
      CHARACTER*6   TYPTXT(3)
      CHARACTER*3   ABSREL(2)
      CHARACTER*1   YESNO(2)
      REAL*8        PREC(*),TIMSTC(3,MXCSTC,MXCSAT,*),SIGSTC(3,*)
      REAL*8        SIGOFF(3,*),TIMOFF(2,*)
      INTEGER*4     SEQORB(*),FRCTYP(*)
      INTEGER*4     NUMSTC(*),NSTCEF(MXCSAT,*)
      INTEGER*4     NSAOFF(*),SATOFF(MXCSGR,*),PAROFF(3)
      INTEGER*4     GRPOFF(*),GNROFF(*),ISGOFF(*)
      INTEGER*4     FRCHLP(6),TYPHLP
      INTEGER*4     ESTSAT(*), ii
C
      CHARACTER*4       :: empiri
      CHARACTER*41      :: DYXTXT(9), RSWTXT(9)
      CHARACTER*7       :: UNITTXT(4)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMSGR/MXCSGR,MXNSGR
C
      DATA FRCTXT/'rad','alo','out',
     1            'Sun','y  ','x  '/
      DATA TYPTXT/'epoch ','const ','linear'/
      DATA YESNO /'N','Y'/
      DATA ABSREL/'ABS','REL'/
C
      DATA DYXTXT /' Direct rad. pressure coefficient     d0 ',
     1             ' "Y-bias" rad. pressure coeff.        y0 ',
     2             ' "X-bias" rad. pressure coeff.        x0 ',
     3             ' Direct rpr (cos-term)                dc ',
     4             ' "Y-bias" (cos-term)                  yc ',
     5             ' "X-bias" (cos-term)                  xc ',
     6             ' Direct rpr (sin-term)                ds ',
     7             ' "Y-bias" (sin-term)                  ys ',
     8             ' "X-bias" (sin-term)                  xs ' /
C
      DATA RSWTXT /' Acceleration in radial               r0 ',
     1             ' Acceleration in along-track          s0 ',
     2             ' Acceleration in out-of-plane         w0 ',
     3             ' Radial once-per-rev (cos-term)       rc ',
     4             ' Along-track once-per-rev (cos-term)  sc ',
     5             ' Out-of-plane once-per-rev (cos-term) wc ',
     6             ' Radial once-per-rev (sin-term)       rs ',
     7             ' Along-track once-per-rev (sin-term)  ss ',
     8             ' Out-of-plane once-per-rev (sin-term) ws ' /
C
      DATA UNITTXT / ' m     ',
     1               '       ',
     2               ' "     ',
     3               ' m/s**2' /
C
C Get system of dynamic orbit parameters
C --------------------------------------
      DO 13 I=1,orbdsc%nlin
        IF (orbdsc%orbmod(i)(1:7)=='EMPIRI:') THEN
           empiri = orbdsc%orbmod(i)(9:12)
        ENDIF
13    CONTINUE
C
C WRITE TITLE LINES
C -----------------
      IF(NORB.GT.0) THEN

        ! GNSS Satellites
        IF (MODE.EQ.1.AND.NESTSAT.GT.0) THEN
          WRITE(LFNPRT,'(//,A,/,A)')
     1    ' List of GNSS satellites selected for orbit determination:',
     2    ' --------------------------------------------------------'
          ISAT = 1
          JSAT = 1
          DO WHILE (ISAT <= NESTSAT)
            JSAT = ISAT+25
            IF (JSAT.GT.NESTSAT) JSAT=NESTSAT
            WRITE(LFNPRT,'(26I5)') ESTSAT(ISAT:JSAT)
            ISAT=JSAT+1
          ENDDO
        ENDIF

        ! LEOs Satellites
        IF (MODE.EQ.2) WRITE(LFNPRT,'(//,A,/,A)')
     1    ' Orbit determination for LEOs:',
     2    ' ----------------------------'

        WRITE(LFNPRT,'(//,2(A,/))')
     1    ' Orbital parameters and a priori sigmas:',
     2    ' -------------------------------------- ' //
     2    '      a priori sigma'
      ENDIF
C
C ELEMENTS AND A PRIORI SIGMAS
C ----------------------------
      DO 30 IELE=1,MAXELE
        IF (IELE == 1) THEN
          TEXT = ' Semi-major axis                      a   ' //
     1           '                  m     '
        ELSE IF (IELE ==  2) THEN
          TEXT = ' Eccentricity                         e   ' //
     1           '                        '
        ELSE IF (IELE ==  3) THEN
          TEXT = ' Inclination                          i   ' //
     1           '                  "     '
        ELSE IF (IELE ==  4) THEN
          TEXT = ' Ascending node                       node' //
     1           '                  "     '
        ELSE IF (IELE ==  5) THEN
          TEXT = ' Perigee                              per ' //
     1           '                  "     '
        ELSE IF (IELE ==  6) THEN
          TEXT = ' Argument of latitude                 u0  ' //
     1           '                  "     '
C
        ELSE IF (IELE ==  7) THEN
          IF ( empiri=='DYX ' .OR. empiri=='DRSW' ) THEN
            WRITE(TEXT(1:41),11) DYXTXT(IELE-6)
          ELSEIF ( empiri=='RSW ' ) THEN
            WRITE(TEXT(1:41),11) RSWTXT(IELE-6)
          ENDIF
          WRITE(TEXT(60:66),12) UNITTXT(4)
C
11        FORMAT(A40)
12        FORMAT(A7)
C
        ELSE IF (IELE >=  8) THEN
          IF ( empiri=='DYX ' ) THEN
            WRITE(TEXT(1:41),11) DYXTXT(IELE-6)
          ELSEIF ( empiri=='RSW ' .OR. empiri=='DRSW' ) THEN
            WRITE(TEXT(1:41),11) RSWTXT(IELE-6)
          ENDIF
          WRITE(TEXT(60:66),12) UNITTXT(4)
C
        ENDIF
C
        DO 25 ISEQ=1,NORB
          IF(SEQORB(ISEQ).NE.IELE) GOTO 25
          IF(IELE.EQ.1) THEN
            WRITE(TEXT(43:58),2) PREC(ISEQ)
2           FORMAT(F16.4)
          ELSE IF(IELE.EQ.2) THEN
            WRITE(TEXT(43:58),3) PREC(ISEQ)
3           FORMAT(F16.8)
          ELSE IF(IELE.LT.7) THEN
            WRITE(TEXT(43:58),4) PREC(ISEQ)*ars
4           FORMAT(F16.4)
          ELSE
            WRITE(TEXT(43:58),5) PREC(ISEQ)/1.D9
5           FORMAT(D16.3)
          ENDIF
          WRITE(LFNPRT,1) TEXT(1:LENGT1(TEXT))
1         FORMAT(A)
          GOTO 30
25      CONTINUE
C
30    CONTINUE
C
C STOCHASTIC ORBIT PARAMETERS
C ---------------------------
      IF (NSASTC.GT.0) THEN
        WRITE(LFNPRT,'(/,5(/,A))')
     1    ' Stochastic orbit parameters:',
     2    ' ---------------------------',
     3    '                 perturbation epoch    accel.        ' //
     3    '           a priori sigmas (m/s**2)',
     4    ' arc  sat. set   or  interval start    type   ' //
     4    ' component(s)     for component(s)',
     5    ' ----------------------------------------------------' //
     5    '-----------------------------------------------------' //
     5    '--------------------------'
C
        TEXT=' '
        IAROLD=0
        DO 60 IT=1,NSTCEP
          FRCHLP(IT) = mod(FRCTYP(IT),10)
          TYPHLP = 1+(FRCTYP(IT)-mod(FRCTYP(IT),10))/10
60      CONTINUE
        DO 90 IARC=1,NARC
          DO 80 ISAT=1,NSASTC
            DO 70 ISTC=1,NSTCEF(ISAT,IARC)
              IF (IAROLD.NE.IARC) WRITE(LFNPRT,'( )')
              IAROLD=IARC
              IF(FRCTYP(1).LT.20) THEN
                CALL TIMST2(1,1,TIMSTC(1,ISTC,ISAT,IARC),TSTRNG)
              ELSE
                CALL TIMST2(1,1,TIMSTC(2,ISTC,ISAT,IARC),TSTRNG)
              END IF
              WRITE(TEXT,7) IARC,NUMSTC(ISAT),ISTC,TSTRNG
7             FORMAT(I4,2I5,3X,A19)
!!              WRITE(TEXT(40:63),8) (FRCTXT(FRCHLP(IT)),IT=1,NSTCEP)
              WRITE(TEXT(40:62),8) TYPTXT(TYPHLP),(FRCTXT(FRCHLP(IT))
     1                              ,IT=1,NSTCEP)
8             FORMAT(A6,2X,3(A3,2X))
!!              WRITE(TEXT(64:96),9) (SIGSTC(IT,ISAT),IT=1,NSTCEP)
              WRITE(TEXT(63:95),9) (SIGSTC(IT,ISAT),IT=1,NSTCEP)
9             FORMAT(3D11.3)
              WRITE(LFNPRT,10) TEXT(1:LENGT1(TEXT))
10            FORMAT(A)
70          CONTINUE
80        CONTINUE
90      CONTINUE
      ENDIF
C
C SATELLITE ANTENNA OFFSET PARAMETERS
C -----------------------------------
      IF (NRQOFF.GT.0) THEN
C
C GROUPS
        WRITE(lfnprt,'(//,2(A,/),3(/,A),/)')
     1    ' Satellite antenna offset parameters:',
     2    ' -----------------------------------',
     3    '               Comp.',
     4    ' No. Group   x  y  z   #Sat  Satellite numbers',
     5    ' ------------------------------------------------' //
     5    '------------------------------------------------'  //
     5    '-----------------------------------'
C
        DO 120 IOFF=1,NANOFF
          IF (NSAOFF(IOFF).GT.0) THEN
            WRITE(LFNPRT,111) IOFF,GNROFF(IOFF),
     1                        (YESNO(PAROFF(I)+1),I=1,3),
     2                        NSAOFF(IOFF),
     3                        (SATOFF(I,IOFF),I=1,NSAOFF(IOFF))
111         FORMAT(I3,2X,I4,4X,3(A1,2X),I4,2X,30I4)
          ENDIF
120     CONTINUE
C
C REQUESTS
        WRITE(lfnprt,'(/,3(/,A))')
     1    '                                                  ' //
     1    '             a priori sigmas (m)',
     2    ' Request  Group  from                 to          ' //
     2    '             sigmax      sigmay      sigmaz    abs' //
     2    '/rel',
     3    ' -------------------------------------------------' //
     3    '--------------------------------------------------' //
     3    '--------------------------------'
C
        IRQOLD=0
        DO 140 IOFR=1,NRQOFF
C          IF (TIMOFF(1,IOFR).NE.0D0) THEN
C            IF (GRPOFF(IOFR).NE.IRQOLD) WRITE(LFNPRT,'( )')
            IRQOLD=GRPOFF(IOFR)
            CALL TIMST2(1,2,TIMOFF(1:2,IOFR),TSTRN2)
            IF (GRPOFF(IOFR) /= 0) THEN
              WRITE(LFNPRT,131) IOFR,GNROFF(GRPOFF(IOFR)),TSTRN2,
     1                          (SIGOFF(I,IOFR),I=1,3),
     2                          ABSREL(ISGOFF(IOFR)+1)
            ELSE
              WRITE(LFNPRT,131) IOFR,GNROFF(IOFR),TSTRN2,
     1                          (SIGOFF(I,IOFR),I=1,3),
     2                          ABSREL(ISGOFF(IOFR)+1)
            ENDIF
131         FORMAT(I6,I8,3X,A40,2X,3F12.8,4X,A3)
C          ENDIF
140     CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
