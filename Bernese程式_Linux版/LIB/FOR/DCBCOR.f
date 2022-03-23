      MODULE s_DCBCOR
      CONTAINS

C*
      SUBROUTINE DCBCOR(MEATYP,ICARR ,ISVN  ,STANAM,RECTYP,FRQNUM,
     1                  ITYP  ,TOBS  ,DIST  )
CC
CC NAME       :  DCBCOR
CC
CC PURPOSE    :  ADD DIFFERENTIAL (P1-P2) OR (P1-C1) CODE BIAS
CC               CORRECTION TO DISTANCE
CC
CC PARAMETERS :
CC         IN :  MEATYP : MEASUREMENT TYPE                    I*4
CC               ICARR  : FREQUENCY                           I*4
CC                        =0: "DIST" REDEFINED
CC                        =1: L1
CC                        =2: L2
CC                        =3: L3 (IONOSPHERE-FREE)
CC                        =4: L4 (GEOMETRY-FREE)
CC                        =5: L5 (WIDELANE)
CC                        =6: L6 (MELBOURNE-WUEBBENA)
CC               ISVN   : SATELLITE NUMBER (OR SATELLITE      I*4
CC                        SYSTEM)
CC                        =1: GPS
CC                        =2: GLONASS
CC                        =3: GALILEO
CC                        ADD_GNSS_HERE (TAKE INFO FROM g_svnsys)
CC               STANAM : STATION NAME                        CH*(*)
CC                        =' ': SATELLITE NUMBER ONLY
CC               RECTYP : RECEIVER TYPE                       CH*(*)
CC                        =' ': NO RECEIVER TYPE
CC               FRQNUM : FREQUENCY NUMBER,                   I*4
CC                        ONLY USED ID ISVN==0
CC               ITYP   : REQUESTED CODE BIAS TYPE            I*4
CC                        =0: ALL
CC                        =1: P1-P2
CC                        =2: P1-C1
CC                        =3: LC
CC                        =4: P2-C2
CC                        =5: INTER-FREQ. CODE BIAS
CC               TOBS   : EPOCH IN MJD                        R*8
CC                        =0: N/A
CC     IN/OUT :  DIST   : RESULTING DISTANCE IN M (OR CODE    R*8
CC                        BIAS VALUE IN NS)
CC
CC REMARKS    :  SEE ALSO IGS MAIL 2744
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  20-OCT-97
CC
CC CHANGES    :  04-MAY-98 : SS: INCLUDE 'COMFREQ.inc' FOR GLONASS
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               07-APR-00 : SS: SET "ICBTYP"
CC               11-APR-00 : SS: CHECK "NREC0"
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               02-MAY-00 : SS: CHECK WHETHER RECEIVER MANAGEABLE
CC               10-MAY-00 : SS: CHECK STATEMENT MOVED
CC               31-MAY-00 : SS: "MAXREC" FROM 100 TO 200
CC               14-DEC-00 : SS: DO NOT STOP IN CASE OF INCOMPATIBLE
CC                               RECEIVERS
CC               20-MAR-01 : RD: ADD TRIMBLE 4700 (SEE IGS-MAIL 3236)
CC               27-NOV-01 : SS: ADD TRIMBLE 5700
CC               18-DEC-01 : MM: USE RECEIVER FILE TO IDENTIFY "SPECIAL"
CC                               RECEIVERS (C1-P2, C1-C2)
CC               07-MAY-02 : SS: DCB UPDATE
CC               21-AUG-02 : SS: ALLOW MORE THAN ONE DCB SET IN FILE
CC               17-FEB-03 : LM: Use m_maxdim
CC               08-SEP-03 : HU: RECEIVER NAME, STATION NAME DECLARED OPEN
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               21-JUN-07 : SS: CALL SR GETRCV WITH ISYST
CC               04-FEB-09 : SS: HANDLE AS-FREE PERIODS
CC               09-MAY-09 : RD: TAKE FREQ-DEP CODE BIASES FROM DCB-FILES
CC               08-DEC-10 : SS: APPLY LC DCB TO NON-CODE MEASUREMENTS
CC               09-DEC-10 : SS: APPLY LC DCB TO L1/L2/L3/L5 LC
CC               20-MAY-11 : LP: ISVN=3 or 2XX added for GALILEO
CC               06-JUN-12 : LP: USE g_svnsys for setting FLAG
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT, MAXREC, MAXSTA
      USE m_global, ONLY: g_svnsys
      USE d_const,  ONLY: C
      USE d_satfil, ONLY: typeMWTR
      USE s_getrcv
      USE s_rdcbfl
      USE s_exitrc
      USE s_gtflna
      USE s_chr2svn
      USE s_gtsensor
      USE f_asmode
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICARR , ICBTYP, ICLS  , IFIRST, IRCDCB, IREC  , ISAT  ,
     1          ISET  , ISVN  , ITYP  , MEATYP, NFREQ , NUMREC, NUMSAT,
     2          ISYST , NUMIFB, IFB   , FRQNUM, IOS
C
      REAL*8    DCBCO1, DCBCO2, DISCOR, DIST  , TOBS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*(*) STANAM,RECTYP
C
      CHARACTER*32  DCBFIL
      CHARACTER*16  DCBID2(MAXREC),ALLID2(4,MAXREC)
      CHARACTER*16  DCBID3(2,MAXSAT*MAXSTA)
      CHARACTER*1   DCBSYS(MAXREC),ALLSYS(3,MAXREC)
      CHARACTER*1   FLAG
C
      REAL*8        DCBVA1(2,MAXSAT),ALLVA1(4,2,MAXSAT)
      REAL*8        DCBVA2(2,MAXREC),ALLVA2(4,2,MAXREC)
      REAL*8        DCBVA3(4,MAXSAT*MAXSTA)
      REAL*8        FACDCB(6,3,3)
C
      INTEGER*4     DCBID1(MAXSAT),ALLID1(4,MAXSAT)
      INTEGER*4     ICODE(2),IWLFAC(2)
      INTEGER*4     ALLTYP(5),ALLNUM(5,2),DCBIN3(2,MAXSAT*MAXSTA)
C
      INCLUDE 'COMFREQ.inc'
C
      DATA IFIRST/1/
C
C EXECUTE INITIALIZATION PROCEDURE
C --------------------------------
      IF (IFIRST.EQ.1) THEN
C
C READ A PRIORI CODE BIAS FILE, IF SPECIFIED
        CALL GTFLNA(0,'DCBINP ',DCBFIL,IRCDCB)
C
        IF (IRCDCB.EQ.0) THEN
          DO ISET=1,5
            ICBTYP=ISET
            CALL RDCBFL(DCBFIL,MAXSAT,MAXREC,MAXSAT*MAXSTA,ICBTYP,
     1                  NUMSAT,NUMREC,NUMIFB,DCBID1,DCBVA1,DCBID2,
     2                  DCBVA2,DCBSYS,DCBID3,DCBVA3)
C
            IF (ICBTYP.EQ.0) THEN
              ALLTYP(ISET)=0
            ELSE
              ALLTYP(ISET)=1
            ENDIF
C
            IF (ISET.LT.5) THEN
              ALLNUM(ISET,1)=NUMSAT
              ALLNUM(ISET,2)=NUMREC
C
              DO ISAT=1,NUMSAT
                ALLID1(ISET,ISAT)=DCBID1(ISAT)
                ALLVA1(ISET,1,ISAT)=DCBVA1(1,ISAT)
                ALLVA1(ISET,2,ISAT)=DCBVA1(2,ISAT)
              ENDDO
              DO IREC=1,NUMREC
                ALLID2(ISET,IREC)=DCBID2(IREC)
                ALLVA2(ISET,1,IREC)=DCBVA2(1,IREC)
                ALLVA2(ISET,2,IREC)=DCBVA2(2,IREC)
                ALLSYS(ISET,IREC)=DCBSYS(IREC)
              ENDDO
            ELSE
              DO IFB=1,NUMIFB
                READ(DCBID3(1,IFB)(2:3),*,IOSTAT=IOS) DCBIN3(2,IFB)
                IF (IOS.NE.0.OR.DCBID3(1,IFB)(1:3).EQ.'G  ') THEN
                  DCBIN3(1:2,IFB) = (/ 99,0 /)
                ELSE
                  CALL chr2svn(DCBIN3(2,IFB),DCBID3(1,IFB)(1:1),
     1                         DCBIN3(1,IFB))
                  CALL gtsensor(prn=DCBIN3(1,IFB),epo=DCBVA3(4,IFB),
     1                         type1=typeMWTR,ifrq=DCBIN3(2,IFB))
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C
        IFIRST=0
      ENDIF
C
C RETURN, IF NO A PRIORI CODE BIAS FILE SPECIFIED
C -----------------------------------------------
      IF (IRCDCB.EQ.1) THEN
        IF (ICARR.EQ.0) DIST=0.D0
        RETURN
      ENDIF
C
C RETURN, IF NON-CODE MEASUREMENT PROCESSED
C -----------------------------------------
CC      IF (MEATYP.NE.2) RETURN
C
C CHECK WHETHER REQUESTED CODE BIAS INFORMATION UNAVAILABLE
C ---------------------------------------------------------
      IF (ITYP.NE.0) THEN
        IF (ALLTYP(ITYP).EQ.0) THEN
          WRITE(LFNERR,'(/,A,I1,A,A,/)')
     1      ' ### SR DCBCOR: TYPE-',ITYP,' CODE BIAS INFORMATION ',
     2      'UNAVAILABLE'
C
          ALLTYP(ITYP)=-1
        ENDIF
      ENDIF
C
      IF (ICARR.EQ.0) THEN
C
        IF (ITYP.NE.0) THEN
          IF (ALLTYP(ITYP).NE.1) THEN
            DIST=0.D0
            RETURN
          ENDIF
        ENDIF
C
c        IF (STANAM.NE.' '.AND.ITYP.LT.5) THEN
c          IF (ISVN.EQ.1) THEN
c            FLAG='G'
c          ELSEIF (ISVN.EQ.2) THEN
c            FLAG='R'
c          ELSEIF (ISVN.EQ.3) THEN
c            FLAG='E'
c          ADD_GNSS_HERE
c          ELSE
c            WRITE(LFNERR,'(/,A,/)')
c     1        ' *** SR DCBCOR: ILLEGAL SATELLITE SYSTEM'
c            CALL EXITRC(2)
c          ENDIF
c        ENDIF
        IF (STANAM.NE.' '.AND.ITYP.LT.5) FLAG = g_svnsys(ISVN-1)
C
      ELSE
C
C GET RECEIVER INFORMATION FROM RECEIVER FILE
C -------------------------------------------
        CALL GETRCV(RECTYP,NFREQ,ICODE,IWLFAC,ICLS,ISYST)
C
        IF (ICLS.EQ.0) THEN
          IF (ICARR.EQ.1) THEN
            ICLS=ICODE(1)
          ELSE
            WRITE(LFNERR,'(/,A,/,16X,A,A,/)')
     1        ' *** SR DCBCOR: SINGLE-FREQUENCY RECEIVER NOT EXPECTED',
     2        'RECEIVER TYPE: ',RECTYP
            CALL EXITRC(2)
          ENDIF
        ENDIF
C
C DEFINE FREQUENCY-DEPENDENT FACTORS
C ----------------------------------
        FACDCB=0D0
C P1-P2:
        FACDCB(1,1,1:3) =
     1    -FRQ(2,ISVN)**2/(FRQ(1,ISVN)**2-FRQ(2,ISVN)**2)
        FACDCB(2,1,1:3) =
     1    -FRQ(1,ISVN)**2/(FRQ(1,ISVN)**2-FRQ(2,ISVN)**2)
        FACDCB(3,1,1:3) =  0D0
        FACDCB(4,1,1:3) =  1D0
        FACDCB(5,1,1:3) =
     1     FRQ(1,ISVN)*FRQ(2,ISVN)/(FRQ(1,ISVN)**2-FRQ(2,ISVN)**2)
        FACDCB(6,1,1:3) =  0D0
C P1-C1:
        FACDCB(1,2,2)   = -1D0
        FACDCB(2,2,2)   = -1D0
        FACDCB(3,2,2)   = -1D0
        FACDCB(4,2,2)   =  0D0
        FACDCB(5,2,2)   = -1D0
        FACDCB(6,2,2)   =  1D0
C
        FACDCB(1,2,3)   = -1D0
        FACDCB(2,2,3)   =  0D0
        FACDCB(3,2,3)   =
     1    -FRQ(1,ISVN)**2/(FRQ(1,ISVN)**2-FRQ(2,ISVN)**2)
        FACDCB(4,2,3)   = -1D0
        FACDCB(5,2,3)   =
     1    -FRQ(1,ISVN)/(FRQ(1,ISVN)-FRQ(2,ISVN))
        FACDCB(6,2,3)   =
     1     FRQ(1,ISVN)/(FRQ(1,ISVN)+FRQ(2,ISVN))
C LC:
        FACDCB(1,3,1:3) =  1D0
        FACDCB(2,3,1:3) =  1D0
        FACDCB(3,3,1:3) =  1D0
        FACDCB(5,3,1:3) =  1D0
C
c        IF (ISVN.LT.100) THEN
c          FLAG='G'
c        ELSEIF (ISVN.GE.100.AND.ISVN.LT.200) THEN
c          FLAG='R'
c        ELSEIF (ISVN.GE.200.AND.ISVN.LT.300) THEN
c          FLAG='E'
c        ENDIF
c       ADD_GNSS_HERE
        FLAG = g_svnsys(INT(ISVN/100))
C
      ENDIF
C
C LOOP OVER P1-P2, P1-C1, LC CODE BIAS TYPES
C ------------------------------------------
      DISCOR=0.D0
      DO ISET=1,3
C
C LOOK FOR A PRIORI CODE BIAS VALUES
C ----------------------------------
C
C - FOR CURRENT SATELLITE
        DCBCO1=0.D0
        IF (ICARR.NE.0 .OR. STANAM.EQ.' ') THEN
          IF (ALLTYP(ISET).EQ.1 .AND.
     1      (ITYP.EQ.0 .OR. ITYP.EQ.ISET) .AND.
     2      (MEATYP.EQ.2 .OR. ISET.EQ.3)) THEN
            DO ISAT=1,ALLNUM(ISET,1)
              IF (ISVN.EQ.ALLID1(ISET,ISAT)) GOTO 111
            ENDDO
C
            ALLNUM(ISET,1)=ALLNUM(ISET,1)+1
            IF (ALLNUM(ISET,1).GT.MAXSAT) THEN
              WRITE(LFNERR,911)
911           FORMAT(/,' *** SR DCBCOR: INTERNAL BUFFER FOR ',
     1          'SATELLITE-SPECIFIC INFO FULL',/)
              CALL EXITRC(2)
            ELSE
              WRITE(LFNERR,912) ISET,ISVN
912           FORMAT(/,' ### SR DCBCOR: TYPE-',I1,' CODE BIAS VALUE ',
     1          'FOR SATELLITE NOT FOUND',
     2          /,16X,'PRN: ',I3.2,/)
C
              ISAT=ALLNUM(ISET,1)
              ALLID1(ISET,ISAT)=ISVN
              ALLVA1(ISET,1,ISAT)=0.D0
              ALLVA1(ISET,2,ISAT)=0.D0
C              ALLVA1(ISET,3,ISAT)=0.D0
c             ADD_GNSS_HERE ?
            ENDIF
C
111         CONTINUE
            DCBCO1=ALLVA1(ISET,1,ISAT)
          ENDIF
        ENDIF
C
C - FOR CURRENT RECEIVER
        DCBCO2=0.D0
        IF (ICARR.NE.0 .OR. STANAM.NE.' ') THEN
          IF (ALLNUM(ISET,2).GT.0 .AND.
     1      (ITYP.EQ.0 .OR. ITYP.EQ.ISET) .AND.
     2      (MEATYP.EQ.2 .OR. ISET.EQ.3)) THEN
            DO IREC=1,ALLNUM(ISET,2)
              IF (STANAM.EQ.ALLID2(ISET,IREC) .AND.
     1          FLAG.EQ.ALLSYS(ISET,IREC)) GOTO 121
            ENDDO
C
            ALLNUM(ISET,2)=ALLNUM(ISET,2)+1
            IF (ALLNUM(ISET,2).GT.MAXREC) THEN
              WRITE(LFNERR,921)
921           FORMAT(/,' *** SR DCBCOR: INTERNAL BUFFER FOR ',
     1          'RECEIVER-SPECIFIC INFO FULL',/)
              CALL EXITRC(2)
            ELSE
              WRITE(LFNERR,922) ISET,STANAM,FLAG
922           FORMAT(/,' ### SR DCBCOR: TYPE-',I1,' CODE BIAS VALUE ',
     1          'FOR RECEIVER NOT FOUND',
     2          /,16X,'STATION NAME: ',A16,'-',A1,/)
C
              IREC=ALLNUM(ISET,2)
              ALLID2(ISET,IREC)=STANAM
              ALLSYS(ISET,IREC)=FLAG
              ALLVA2(ISET,1,IREC)=0.D0
              ALLVA2(ISET,2,IREC)=0.D0
            ENDIF
C
121         CONTINUE
            DCBCO2=ALLVA2(ISET,1,IREC)
          ENDIF
        ENDIF
C
        IF (ICARR.EQ.0) THEN
          DISCOR=DISCOR+DCBCO1+DCBCO2
        ELSE
C
C HANDLE AS-FREE PERIODS IN CASE OF P1-C1 BIAS AND C1/X2 RECEIVER
C ---------------------------------------------------------------
          IF (ASMODE(TOBS,ISVN).EQ.0) THEN
            IF (ISET.EQ.2 .AND. ICLS.EQ.2) THEN
              DCBCO1=0.D0
              DCBCO2=0.D0
            ENDIF
          ENDIF
C
          DISCOR=DISCOR+
     1      1.D-9*C*FACDCB(ICARR,ISET,ICLS)*(DCBCO1+DCBCO2)
        ENDIF
C
      ENDDO
C
C INTERFREQUENCY CODE BIASES
C --------------------------
      IF (MEATYP.EQ.2.AND.NUMIFB.GT.0.AND.(ITYP.EQ.0.OR.ITYP.EQ.5)) THEN
        DO IFB=1,NUMIFB
          IF ( (ISVN.NE.0 .AND. ISVN  .EQ.DCBIN3(1,IFB)) .OR.
     1         (ISVN.EQ.0 .AND. FRQNUM.EQ.DCBIN3(2,IFB)) ) THEN
            IF( STANAM.EQ.DCBID3(2,IFB).AND.
     2          TOBS  .GE.DCBVA3(3,IFB).AND.
     3          TOBS  .LE.DCBVA3(4,IFB)) THEN
              IF(ICARR.EQ.0) DISCOR=DISCOR+DCBVA3(1,IFB)
              IF(ICARR.EQ.3) DISCOR=DISCOR-DCBVA3(1,IFB)*C/1D9
              EXIT
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C COMPUTE ACTUAL CODE BIAS CORRECTION AND ADD IT TO DISTANCE
C ----------------------------------------------------------
      IF (ICARR.EQ.0) THEN
        DIST=DISCOR
      ELSE
        DIST=DIST+DISCOR
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
