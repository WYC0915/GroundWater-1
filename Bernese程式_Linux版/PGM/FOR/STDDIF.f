C*
      PROGRAM STDDIF
CC
CC NAME       :  STDDIF
CC
CC PURPOSE    :  COMPUTE DIFFERENCES BETWEEN TWO STANDARD ORBITS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, G.BEUTLER
CC
CC VERSION    :  3.5  (JAN 93)
CC
CC CREATED    :  88/01/06 15:12
CC
CC CHANGES    :  23-APR-92 : ??: NEW INTERNAL FILE NAME "PLOTRS"
CC               04-JUN-92 : ??: NEW CALLS: GETORB,ORBINF; NEW SR "GETORF"
CC               30-JUN-92 : ??: NO ANTENNA OFFSETS
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-SEP-97 : DI: USE INCLUDE 'I:MAXSAT'
CC               11-JUN-00 : HU: GENERATE RESIDUALS IN VELOCITY
CC               23-JUN-00 : HU: SEVERAL IMPROVEMENTS
CC               26-JUN-01 : JD: CHECK FOR 'NO DATA' TO GENERATE HELMERT,
CC                               INITIALIZE HELMERT FOR EACH ITERATION
CC               16-DEC-01 : HU: USE D_CONST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               27-FEB-03 : HU: REPLACE CHAR BY CHR
CC               16-MAY-03 : HB: INITIALIZE STRUCTURE
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               01-DEC-03 : HU: RETURN RMS ALSO IN CASE OF NO HELMERT
CC               15-JAN-04 : HU: COMPARE MANEUVERING SATELLITES
CC               03-AUG-04 : HU: DISTM 200 M --> 2000 M
CC               13-AUG-04 : SS: VELMN FROM 0.05 TO 0.01 M/S
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               15-AUG-06 : HU: DEFAULT WINDOW FROM ORBIT OVERLAP
CC               28-AUG-06 : HU: WRITE TOTAL RMS FOR RSW
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               16-SEP-08 : DT: USE F14.4 FOR RESIDUALS IN OUTPUT
CC               11-NOV-08 : LP: DEFINE UNITS AFTER DEFCON CALL
CC               12-AUG-10 : RD: ADD SATELLITE NUMBER WHEN PRINTING RESID.
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               11-DEC-10 : SS: VELMN REDUCED FROM 0.01 TO 0.005
CC                               (FOR PRN32 REPOSINIONING ON 10-DEC-2010)
CC               03-SEP-11 : RD: MORE DIGITS FOR SOFTWARE-TESTS
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: fileNameLength, lfnPrt, lfnPlt, lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: date,time,ars
      USE d_stdorb, ONLY: t_stdhead, init_stdHead
      USE s_iordup
      USE s_opnfil
      USE s_rdstdh
      USE s_pritit
      USE s_stdodf
      USE s_readinpf
      USE s_opnerr
      USE s_timst2
      USE s_stdinp
      USE s_syminvg
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_jmt
      USE s_gtflna
      USE f_modsvn
      USE f_ikf
      USE f_listi4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IBAD  , IDAY1 , IDAY2 , IEPO  , IFILE ,
     1          IFIRST, IMANO , IMANV , IOSTAT, IPLT  , IPOS  ,
     2          IRC   , IRCODE, ISAT  , ISING , ISUB  , ITER  , ITYP  ,
     3          J     , JAHR1 , JAHR2 , K     , MAXARC, MONTH1,
     4          MONTH2, MXCARC, MXCSAT, NARC  , NITER , NOBS  , NSAT  ,
     5          NSAT1 , NSMTOT, NSUB
C
      REAL*8    DAY   , DELTAT, DIFF  , DIFFP , DIFFV , DISTM , DT    ,
     1          EDIT  , EPO1  , EPO2  , EPO3  , EPS   , HOUR1 , HOUR2 ,
     2          POSMN , POSMX , RMS   , RMSTOT, STEP1 , TA    , TAU   ,
     3          TB    , TEND  , TEPO  , TPRINT, TSTART, VELMN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXARC=20)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXARC: MAXIMUM NUMBER OF SATELLITE ARCS
C
C DECLARATIONS
C ------------
      CHARACTER*80 TITLE
      CHARACTER*(fileNameLength) FILNAM,FILSTD,FILST1
      CHARACTER*6  MXNSAT,MXNARC
      REAL*8       DXV(6),DTRAN(6),TBOUND(2,MAXARC)
      REAL*8       SUM(6,MAXSAT),SUM2(6,MAXSAT),SUM0(6,MAXSAT)
      REAL*8       SUMT(6),SUM2T(6),SUM0T(6)
      REAL*8       DIFMIN(2,MAXSAT),DIFMAX(2,MAXSAT),EPOMIN(MAXSAT)
      REAL*8       ANOR(36),BNOR(8),COEF(6,8),HELM(8),ERR(8),UNIT(8)
      REAL*8       SCAL(8),RMSSAT(MAXSAT)
      REAL*8       POS1(6),POS2(6),POS3(6),DPOS(3)
      INTEGER*4    NSUM(MAXSAT),SVNNUM(MAXSAT),INDX(MAXSAT),NSATS(2)
      INTEGER*4    NSUMT,SVN,SVNM,NUMSAT(MAXARC)
      INTEGER*4    NHELM,IHELM(8),IDTRAN(3,MAXSAT),IRMS(3,3,MAXSAT)
      INTEGER*4    MARK(MAXSAT),NSMSAT(MAXSAT),SATLST(MAXSAT,2),FLAG
      CHARACTER*18 CHR(8)
      CHARACTER*8  CUNIT(8)
      CHARACTER*6  CMARK(2)
      CHARACTER*19 TSTRNG
C
      TYPE(t_stdhead),DIMENSION(2)  :: stdHdr
C
C COMMON BLOCKS
C -------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMARC/MXCARC,MXNARC
C
      DATA CHR/'X-TRANSLATION     ',
     2         'Y-TRANSLATION     ',
     3         'Z-TRANSLATION     ',
     4         'X-ROTATION        ',
     5         'Y-ROTATION        ',
     6         'Z-ROTATION        ',
     7         'RATE OF Z-ROTATION',
     8         'SCALE             '/
      DATA CUNIT/'M       ','M       ','M       ',
     2           'ARCS    ','ARCS    ','ARCS    ',
     3           'ARCS/DAY','1E-6    '/
      DATA CMARK/'      ','Marked'/
      DATA IFIRST /1/
      DIFMIN=1D20
      DIFMAX=0D0
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXCARC=MAXARC
      MXNSAT='MAXSAT'
      MXNARC='MAXARC'
C --------------------------------------------------------------------
C
C NULLIFY POINTERS
C ----------------
      CALL INIT_STDHEAD(stdHdr(1))
      CALL INIT_STDHEAD(stdHdr(2))
      CALL INIT_INPKEY(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C UNITS (m, arcs, arcs/day, 1e-6)
      UNIT     =1D0
      UNIT(4:7)=ars
      UNIT(8)  =1E6
C
C WRITE TITLE
C -----------
      CALL PRITIT('STDDIF','Compare standard orbits')
C
C GET FILENAME OF SECOND STANDARD ORBIT FILE
C ------------------------------------------
      CALL GTFLNA(1,'STDORB ',FILSTD,IRC)
      CALL GTFLNA(1,'STDOR1 ',FILST1,IRC)
C
C SAME FILENAMES: COMPARE MANEUVERING SATELLITES
C ----------------------------------------------
      IMANV=0
      IF (FILSTD==FILST1) IMANV=1
C
C GET ORBIT INFORMATION
C ---------------------
      CALL rdstdh(filstd,stdHdr(1),irCode)
      CALL rdstdh(filst1,stdHdr(2),irCode)
C
      narc=stdHdr(1)%narc
      DO iarc=1,narc
        numsat(iarc)     = stdHdr(1)%arc(iarc)%nsat
        tbound(1:2,iarc) = stdHdr(1)%arc(iarc)%tbound(1:2)
      ENDDO
C
C OVERLAPPING WINDOW
C ------------------
      tstart=MAX(stdHdr(1)%arc(1)%tbound(1),
     1           stdHdr(2)%arc(1)%tbound(1))
      tend  =MIN(stdHdr(1)%arc(stdHdr(1)%narc)%tbound(2),
     1           stdHdr(2)%arc(stdHdr(2)%narc)%tbound(2))
C
C GET LIST OF COMMON SATELLITES IN ALL ARCS
C -----------------------------------------
      DO ifile=1,2
        nsats(ifile)    = 0
        satlst(:,ifile) = 0
        DO iarc=1,stdHdr(ifile)%narc
          DO isat=1,stdHdr(ifile)%arc(iarc)%nsat
            ipos=listi4(1,maxsat,satlst(:,ifile),
     1                  stdHdr(ifile)%arc(iarc)%sat(isat)%svn,
     2                  nsats(ifile))
          ENDDO
        ENDDO
      ENDDO
      NSAT=0
      DO isat=1,nsats(1)
        ipos=listi4(0,maxsat,satlst(:,2),satlst(isat,1),nsats(2))
        IF (ipos > 0) ipos=listi4(1,maxsat,svnnum,satlst(isat,1),nsat)
      ENDDO
      CALL IORDUP(SVNNUM,NSAT,INDX)
C
C SELECT ARC, SATELLITE AND TIME INTERVAL
C ---------------------------------------
10    CALL STDINP(NARC,NUMSAT,TBOUND,NSAT,SVNNUM,IARC,SVN,TSTART,
     1            TEND,DELTAT,NSUB,IPLT,ITYP,IHELM,NITER,EDIT,TITLE,IRC)
      IF(IRC.NE.0) GOTO 200
      NHELM=0
      DO I=1,8
        IF(IHELM(I).NE.0)NHELM=NHELM+1
      ENDDO
cccc maneuver detection, parameters hard-wired
      imano=1                 ! enable
cc      posmx=2000D0            ! largest position difference larger than (m)
      posmx=1000D0            ! largest position difference larger than (m)
      posmn=3000D0            ! smallest position difference smaller than (m)
      distm=2000D0           ! smallest position after regula falsi (m)
cc      velmn=0.01D0            ! velocity difference at tman larger than (m/s)
      velmn=0.005D0            ! velocity difference at tman larger than (m/s)
      step1=10d0/1440d0       ! step size to initialize regula falsi
      eps  =0.1D0/86400D0     ! termination condition for regula falsi
cccc
C
C SINGLE SATELLITE SELECTED
      IF(ITYP.EQ.1.AND.SVN.NE.0)THEN
        NSAT=1
        SVNNUM(1)=SVN
        INDX(1)  =1
      ENDIF
C --------------------------------------------------------------------
C
C DETERMINE HELMERT PARAMETERS
C ----------------------------
      MARK=1
      FLAG=0
      IF(NHELM.EQ.0)NITER=1
C
C ITERATION TO REMOVE BAD SATELLITES
      DO ITER=1,NITER
C
C INITIALIZE ESTIMATION
        NOBS=0
        RMS =0D0
        BNOR=0D0
        ANOR=0D0
        HELM=0D0
C
        NSMTOT=0
        RMSTOT=0D0
        NSMSAT=0
        RMSSAT=0D0
C
C LOOP OVER ALL EPOCHS
C --------------------
        DO IEPO=1,1000000
          DT  =(IEPO-1)*DELTAT
          TEPO=TSTART+DT
          IF (TEPO.GT.TEND) EXIT
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO ISAT=1,NSAT
            IF (MARK(ISAT).EQ.2) CYCLE
            SVN=SVNNUM(INDX(ISAT))
C
C GET SATELLITE POSITIONS AND VELOCITIES
C --------------------------------------
            CALL STDODF(FILST1,TEPO,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                     DXV,DTRAN,COEF,IRC)
            IF(IRC.NE.0)CYCLE
            FLAG=1
C
C COMPUTE SATELLITE-SPECIFIC AND TOTAL RMS
C ----------------------------------------
            RMSSAT(ISAT)=RMSSAT(ISAT)+DTRAN(1)**2+
     1                                DTRAN(2)**2+DTRAN(3)**2
            NSMSAT(ISAT)=NSMSAT(ISAT)+1
C
C SET UP NORMAL EQUATION FOR HELMERT PARAMETER ESTIMATION
C -------------------------------------------------------
            DO K=1,3
              NOBS=NOBS+1
              RMS=RMS+DXV(K)*DXV(K)
              DO I=1,8
                BNOR(I)=BNOR(I)+COEF(K,I)*DXV(K)
                DO J=1,I
                  ANOR(IKF(I,J))=ANOR(IKF(I,J))+COEF(K,I)*COEF(K,J)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C
C SINGLE SVN WITHOUT DATA
C -----------------------
        IF(FLAG.EQ.0) EXIT
C
C PARAMETERS NOT TO BE DETERMINED
C -------------------------------
        IF (NHELM.GT.0) THEN
          DO I=1,8
            IF(IHELM(I).EQ.0)THEN
              ANOR(IKF(I,I))=1D0
              BNOR(I)       =0D0
            ENDIF
          ENDDO
C
C SOLVE NORMAL EQUATION SYSTEM
C ----------------------------
          DO I=1,8
            SCAL(I)=DSQRT(ANOR(IKF(I,I)))
          ENDDO
          DO I=1,8
            DO J=1,I
              ANOR(IKF(I,J))=ANOR(IKF(I,J))/(SCAL(I)*SCAL(J))
            ENDDO
          ENDDO
          CALL SYMINVG(8,ANOR,0,ISING)
          DO I=1,8
            DO J=1,I
              ANOR(IKF(I,J))=ANOR(IKF(I,J))/(SCAL(I)*SCAL(J))
            ENDDO
          ENDDO
C
          DO I=1,8
            DO J=1,8
              HELM(I)=HELM(I)+ANOR(IKF(I,J))*BNOR(J)
            ENDDO
          ENDDO
C
          DO I=1,8
            RMS=RMS-HELM(I)*BNOR(I)
          ENDDO
          RMS=DSQRT(RMS/(NOBS-NHELM))
C
          DO I=1,8
            ERR(I)=RMS*DSQRT(ANOR(IKF(I,I)))
          ENDDO
C
C LOOK FOR BAD SATELLITES
C -----------------------
          IF(NITER.GT.1)THEN
            DO ISAT=1,NSAT
              IF(NSMSAT(ISAT).GT.1)THEN
                RMSSAT(ISAT)=DSQRT(RMSSAT(ISAT)/(3*(NSMSAT(ISAT)-1)))
                RMSTOT=RMSTOT+RMSSAT(ISAT)
                NSMTOT=NSMTOT+1
              ENDIF
            ENDDO
            RMSTOT=RMSTOT/NSMTOT
            IBAD=0
            DO ISAT=1,NSAT
              IF(NSMSAT(ISAT).EQ.1.OR.
     1           NSMSAT(ISAT).GT.1.AND.RMSSAT(ISAT).GT.EDIT*RMSTOT)THEN
                MARK(ISAT)=2
                IBAD=1
              ENDIF
            ENDDO
C NO BAD SATELLITES: EXIT ITERATION
            IF(IBAD.EQ.0)EXIT
          ENDIF
        ELSE
          RMS=DSQRT(RMS/NOBS)
        ENDIF
      ENDDO
C ------------------------------------------------------------------
C
C WRITE TITLE LINES
C -----------------
      CALL JMT(TSTART,JAHR1,MONTH1,DAY)
      IDAY1=DAY
      HOUR1=(DAY-IDAY1)*24.D0
      CALL JMT(TEND,JAHR2,MONTH2,DAY)
      IDAY2=DAY
      HOUR2=(DAY-IDAY2)*24.D0
      IF(ITYP.EQ.1)THEN
        WRITE(LFNPRT,1) IARC,SVN,JAHR1,MONTH1,IDAY1,HOUR1,
     1                           JAHR2,MONTH2,IDAY2,HOUR2
1       FORMAT(' ORBIT DIFFERENCES:',/,
     1         ' -----------------',//,
     2         ' ARC        :',I5,/,
     3         ' SATELLITE  :',I5,/,
     4         ' START EPOCH:',I5,2I3,F5.1,/,
     4         ' END EPOCH  :',I5,2I3,F5.1)
      ELSEIF(ITYP.EQ.2)THEN
        WRITE(LFNPRT,2) IARC,JAHR1,MONTH1,IDAY1,HOUR1,
     1                       JAHR2,MONTH2,IDAY2,HOUR2
2       FORMAT(' ORBIT DIFFERENCES:',/,
     1         ' -----------------',//,
     2         ' ARC        :',I5,/,
     4         ' START EPOCH:',I5,2I3,F5.1,/,
     4         ' END EPOCH  :',I5,2I3,F5.1,//,
     5  '      SVN           RADIAL     ALONG TRACK   OUT OF PLANE')
      ENDIF
C
C OPEN PLOT FILE; TITLE LINES
C ---------------------------
      IF(IPLT.EQ.1) THEN
        CALL GTFLNA(1,'PLOTRS ',FILNAM,IRC)
        CALL OPNFIL(LFNPLT,FILNAM,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILNAM,'STDDIF')
C .. HEADER
        WRITE(LFNPLT,12)DATE,TIME,TRIM(TITLE),TRIM(FILSTD),TRIM(FILST1)
12      FORMAT('#',
     1       /,'# DIFFERENCE OF TWO STANDARD ORBITS',20X,A9,1X,A7,
     2       /,'# ---------------------------------',
     3       /,'#',
     4       /,'# Title : ',A,
     5       /,'# File 1: ',A,
     6       /,'# File 2: ',A)
      ENDIF
C
C LOOP OVER SUB-INTERVALS
C -----------------------
      NSUMT=0
      SUMT =0D0
      SUM2T=0D0
      DO ISUB=1,NSUB
        WRITE(LFNPRT,*)
        TA=TSTART+(ISUB-1)*(TEND-TSTART)/NSUB
        TB=TSTART+    ISUB*(TEND-TSTART)/NSUB
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ITYP = 1: RESIDUALS FOR A SINGLE SATELLITE
C ------------------------------------------
        IF (ITYP.EQ.1) THEN
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO ISAT=1,NSAT
            SVN=SVNNUM(INDX(ISAT))
C
C PRINT HEADER OF THE TABLE
            WRITE(LFNPRT,'(//,A,/)')
     1            ' TIME (MJD)          RADIAL       ALONG TRACK' //
     2            '     OUT OF PLANE     REMARK'
C
C PRING
            IF (IPLT.EQ.1) THEN
C BLANK LINE TO SUPPORT GNUPLOT
              IF (IFIRST.EQ.1) THEN
                IFIRST=0
              ELSE
                WRITE(LFNPLT,*)
              ENDIF
C
C SUBTITLE
              WRITE(LFNPLT,113)ISUB,SVN,CMARK(MARK(ISAT)),IARC,TA,TB
113           FORMAT('#',
     1             /,'# SUBINTERVAL         : ',I8,
     1             /,'# SATELLITE NUMBER    : ',I8,4X,A,
     1             /,'# ARC NUMBER          : ',I8,
     2             /,'# STARTING EPOCH (MJD): ',F15.6,
     2             /,'# ENDING EPOCH (MJD)  : ',F15.6,
     3             /,'#',
     4             /,'#  EPOCH    ______POSITION DIFFERENCE_____ ',
     4                           '______VELOCITY DIFFERENCE_____',
     5             /,'#',
     6             /,'#               Radial    Along     Cross  ',
     6                           '    Radial    Along     Cross',
     7             /,'#  [MJD]         [m]       [m]       [m]   ',
     7                           '    [mm/s]    [mm/s]    [mm/s]',
     8             /,'#')
            ENDIF
C
C LOOP OVER ALL EPOCHS
C --------------------
            NSUM=0
            SUM =0D0
            SUM2=0D0
            DO IEPO=1,1000000
              DT  =(IEPO-1)*DELTAT
              TEPO=TA+DT
              IF (TEPO.GT.TB) EXIT
C             TPRINT=(TEPO-TA)*1440.D0
              TPRINT=TEPO
C
C GET SATELLITE POSITIONS AND VELOCITIES
C --------------------------------------
              CALL STDODF(FILST1,TEPO,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                       DXV,DTRAN,COEF,IRC)
              IF (IRC.NE.0) CYCLE
C
C MINIMUM, MAXIMUM
C ----------------
              DIFF=DSQRT(DTRAN(1)**2+DTRAN(2)**2+DTRAN(3)**2)
              IF (DIFF.GT.DIFMAX(1,ISAT)) THEN
                DIFMAX(1,ISAT)=DIFF
                DIFMAX(2,ISAT)=DSQRT(DTRAN(4)**2+DTRAN(5)**2+
     1                                           DTRAN(6)**2)
              ENDIF
              IF (DIFF.LT.DIFMIN(1,ISAT)) THEN
                DIFMIN(1,ISAT)=DIFF
                DIFMIN(2,ISAT)=DSQRT(DTRAN(4)**2+DTRAN(5)**2+
     1                                           DTRAN(6)**2)
                EPOMIN(ISAT)=TEPO
              ENDIF
C
C SUMS
C ----
              NSUM(1)=NSUM(1)+1
              SUM(1:6,1) =SUM(1:6,1) +DTRAN(1:6)
              SUM2(1:6,1)=SUM2(1:6,1)+DTRAN(1:6)**2
              NSUMT=NSUMT+1
              SUMT(1:6) =SUMT(1:6) +DTRAN(1:6)
              SUM2T(1:6)=SUM2T(1:6)+DTRAN(1:6)**2
C
C PRINT ORBIT DIFFERENCES
C -----------------------
              WRITE(LFNPRT,141) TPRINT,(DTRAN(K),K=1,3),SVN
141           FORMAT(1X,F11.5,3F16.5,7X,'Sat:',I4)
              IF(IPLT.EQ.1) THEN
                WRITE(LFNPLT,142)TPRINT,(DTRAN(K),K=1,3),
     1                                  (DTRAN(K)*1D3,K=4,6)
142             FORMAT(F11.5,2(1X,3F10.4))
              ENDIF
            ENDDO
C
C RMS
C ---
            IF(NSUM(1).GT.1)THEN
              DO I=1,6
                SUM(I,1) =SUM(I,1)/NSUM(1)
                SUM0(I,1)=DSQRT(SUM2(I,1)/(NSUM(1)-1))
                SUM2(I,1)=SUM2(I,1)-NSUM(1)*SUM(I,1)**2
                SUM2(I,1)=DSQRT(SUM2(I,1)/(NSUM(1)-1))
              ENDDO
            ELSE
              SUM0=0
              SUM2=0
            ENDIF
C
C PRINT
C -----
            WRITE(LFNPRT,155)SUM(1:3,1),SVN,
     1            SUM2(1:3,1),SVN,SUM0(1:3,1),SVN
155         FORMAT(/,' Mean:   ',3X,3F16.5,7X,'Sat:',I4,
     1             /,' RMS :   ',3X,3F16.5,7X,'Sat:',I4,
     2             /,' RMS0:   ',3X,3F16.5,7X,'Sat:',I4)
C
            IF (IPLT.EQ.1) THEN
              WRITE(LFNPLT,151)SUM(1:3,1), SUM(4:6,1)*1D3,
     1                         SUM2(1:3,1),SUM2(4:6,1)*1D3,
     2                         SUM0(1:3,1),SUM0(4:6,1)*1D3
151           FORMAT('#',
     1             /,'# MEAN:    ',2(1X,3F10.4),
     2             /,'# RMS :    ',2(1X,3F10.4),
     3             /,'# RMS0:    ',2(1X,3F10.4))
            ENDIF
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ITYP = 2: RMS FOR ALL SATELLITES
C --------------------------------
        ELSEIF (ITYP.EQ.2) THEN
C
C PRINT
          IF (IPLT.EQ.1) THEN
C BLANK LINE TO SUPPORT GNUPLOT
            IF (IFIRST.EQ.1) THEN
              IFIRST=0
            ELSE
              WRITE(LFNPLT,*)
            ENDIF
C
C SUBTITLE
            WRITE(LFNPLT,214)ISUB,IARC,TA,TB,DELTAT*1440D0
214         FORMAT('#',
     1           /,'# SUBINTERVAL         : ',I8,
     1           /,'# ARC NUMBER          : ',I8,
     1           /,'# STARTING EPOCH (MJD): ',F15.6,
     1           /,'# ENDING EPOCH (MJD)  : ',F15.6,
     1           /,'# STEP (min)          : ',F15.1,
     2           /,'#',
     3           /,'#    SVN    _________POSITION RMS_________ ',
     3                         '_________VELOCITY RMS_________',
     4           /,'#',
     5           /,'#               Radial    Along     Cross  ',
     5                         '    Radial    Along     Cross',
     6           /,'#                [m]       [m]       [m]   ',
     6                         '    [mm/s]    [mm/s]    [mm/s]',
     7           /,'#')
          ENDIF
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO ISAT=1,NSAT
            SVN=SVNNUM(INDX(ISAT))
C
C LOOP OVER ALL EPOCHS
C --------------------
            NSUM=0
            SUM =0D0
            SUM2=0D0
            DO IEPO=1,1000000
              DT  =(IEPO-1)*DELTAT
              TEPO=TA+DT
              IF (TEPO.GT.TB) EXIT
C             TPRINT=(TEPO-TA)*1440.D0
              TPRINT=TEPO
C
C GET SATELLITE POSITIONS AND VELOCITIES
C --------------------------------------
              CALL STDODF(FILST1,TEPO,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                       DXV,DTRAN,COEF,IRC)
              IF (IRC.NE.0) CYCLE
C
C MINIMUM, MAXIMUM
C ----------------
              DIFF=DSQRT(DTRAN(1)**2+DTRAN(2)**2+DTRAN(3)**2)
              IF (DIFF.GT.DIFMAX(1,ISAT)) THEN
                DIFMAX(1,ISAT)=DIFF
                DIFMAX(2,ISAT)=DSQRT(DTRAN(4)**2+DTRAN(5)**2+
     1                                           DTRAN(6)**2)
              ENDIF
              IF (DIFF.LT.DIFMIN(1,ISAT)) THEN
                DIFMIN(1,ISAT)=DIFF
                DIFMIN(2,ISAT)=DSQRT(DTRAN(4)**2+DTRAN(5)**2+
     1                                           DTRAN(6)**2)
                EPOMIN(ISAT)=TEPO
              ENDIF
C
C SUMS
C ----
              NSUM(ISAT)=NSUM(ISAT)+1
              SUM(1:6,ISAT) =SUM(1:6,ISAT) +DTRAN(1:6)
              SUM2(1:6,ISAT)=SUM2(1:6,ISAT)+DTRAN(1:6)**2
              NSUMT=NSUMT+1
              SUMT(1:6) =SUMT(1:6) +DTRAN(1:6)
              SUM2T(1:6)=SUM2T(1:6)+DTRAN(1:6)**2
C
C PRINT ORBIT DIFFERENCES
C -----------------------
            ENDDO
C
C RMS
C ---
            IF(NSUM(ISAT).GT.1)THEN
              DO I=1,6
                SUM(I,ISAT) =SUM(I,ISAT)/NSUM(ISAT)
                SUM0(I,ISAT)=DSQRT(SUM2(I,ISAT)/(NSUM(ISAT)-1))
                SUM2(I,ISAT)=SUM2(I,ISAT)-NSUM(ISAT)*SUM(I,ISAT)**2
                SUM2(I,ISAT)=DSQRT(SUM2(I,ISAT)/(NSUM(ISAT)-1))
              ENDDO
            ELSE
              SUM0=0
              SUM2=0
            ENDIF
C
C PRINT
            WRITE(LFNPRT,256)SVN,SUM0(1:3,ISAT)
256         FORMAT(1X,I8,3X,3F14.3)
C
            IF (IPLT.EQ.1) THEN
              WRITE(LFNPLT,252)SVN,SUM0(1:3,ISAT),SUM0(4:6,ISAT)*1D3,
     1                         CMARK(MARK(ISAT))(1:1)
252           FORMAT(I8,3X,2(1X,3F10.4),1X,1A)
            ENDIF
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ITYP = 3: RESIDUALS FOR ALL SATELLITES
C --------------------------------------
        ELSEIF (ITYP.EQ.3) THEN
C
C PRINT
          WRITE(LFNPRT,"(' SATELLITE:',I6,/)") SVNNUM(INDX(1))
          IF (IPLT.EQ.1) THEN
C BLANK LINE TO SUPPORT GNUPLOT
            IF (IFIRST.EQ.1) THEN
              IFIRST=0
            ELSE
              WRITE(LFNPLT,*)
            ENDIF
C
C SUBTITLE
            WRITE(LFNPLT,15)ISUB,IARC,TA,TB,DELTAT*1440D0
15          FORMAT('#',
     1           /,'# SUBINTERVAL         : ',I8,
     1           /,'# ARC NUMBER          : ',I8,
     1           /,'# STARTING EPOCH (MJD): ',F15.6,
     1           /,'# ENDING EPOCH (MJD)  : ',F15.6,
     1           /,'# STEP (min)          : ',F15.1,
     2           /,'#')
            WRITE(LFNPLT,16)(SVNNUM(INDX(I)),CMARK(MARK(I)),I=1,NSAT)
16          FORMAT('#        SVN',99(I10,1X,A6,1X))
            WRITE(LFNPLT,17)(' ---------------- ',I=1,NSAT)
17          FORMAT('#           ',99A)
            WRITE(LFNPLT,18)('  Rad Along Cross ',I=1,NSAT)
18          FORMAT('# MJD   [cm]',99A)
            WRITE(LFNPLT,"('#')")
          ENDIF
C
C LOOP OVER ALL EPOCHS
C --------------------
          NSUM=0
          SUM =0D0
          SUM2=0D0
          DO IEPO=1,1000000
            DT  =(IEPO-1)*DELTAT
            TEPO=TA+DT
            IF (TEPO.GT.TB) EXIT
C           TPRINT=(TEPO-TA)*1440.D0
            TPRINT=TEPO
C
C LOOP OVER ALL SATELLITES
C ------------------------
            NSAT1=0
            DO ISAT=NSAT,1,-1
              SVN=SVNNUM(INDX(ISAT))
C
C GET SATELLITE POSITIONS AND VELOCITIES
C --------------------------------------
              CALL STDODF(FILST1,TEPO,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                     DXV,DTRAN,COEF,IRC)
              IF (IRC.NE.0) CYCLE
              NSAT1=NSAT1+1
C
C MINIMUM, MAXIMUM
C ----------------
              DIFF=DSQRT(DTRAN(1)**2+DTRAN(2)**2+DTRAN(3)**2)
              IF (DIFF.GT.DIFMAX(1,ISAT)) THEN
                DIFMAX(1,ISAT)=DIFF
                DIFMAX(2,ISAT)=DSQRT(DTRAN(4)**2+DTRAN(5)**2+
     1                                           DTRAN(6)**2)
              ENDIF
              IF (DIFF.LT.DIFMIN(1,ISAT)) THEN
                DIFMIN(1,ISAT)=DIFF
                DIFMIN(2,ISAT)=DSQRT(DTRAN(4)**2+DTRAN(5)**2+
     1                                           DTRAN(6)**2)
                EPOMIN(ISAT)=TEPO
              ENDIF
C
C SUMS
C ----
              NSUM(ISAT)=NSUM(ISAT)+1
              SUM(1:6,ISAT) =SUM(1:6,ISAT) +DTRAN(1:6)
              SUM2(1:6,ISAT)=SUM2(1:6,ISAT)+DTRAN(1:6)**2
cc              do i=1,6
cc              SUM(i,ISAT) =SUM(i,ISAT) +DTRAN(i)
cc              SUM2(i,ISAT)=SUM2(i,ISAT)+DTRAN(i)**2
cc              enddo
              NSUMT=NSUMT+1
              SUMT(1:6) =SUMT(1:6) +DTRAN(1:6)
              SUM2T(1:6)=SUM2T(1:6)+DTRAN(1:6)**2
C
C SAVE DTRAN, CHECK FORMAT OVERFLOW
              DO I=1,3
                IDTRAN(I,ISAT)=IDNINT(DTRAN(I)*100D0)
                IF (IDTRAN(I,ISAT).GT. 9999) IDTRAN(I,ISAT)= 9999
                IF (IDTRAN(I,ISAT).LT.-9999) IDTRAN(I,ISAT)=-9999
              ENDDO
            ENDDO
            IF (NSAT1.EQ.0) CYCLE
C
C PRINT ORBIT DIFFERENCES
C -----------------------
            WRITE(LFNPRT,341) TPRINT,(DTRAN(K),K=1,3)
341         FORMAT(1X,F11.5,3F14.4)
            IF(IPLT.EQ.1) THEN
              WRITE(LFNPLT,342)TPRINT,((IDTRAN(K,I),K=1,3),I=1,NSAT)
342           FORMAT(F11.5,36(1X,I5,1X,I5,1X,I5))
            ENDIF
          ENDDO
C
C RMS
C ---
          DO ISAT=1,NSAT
            IF(NSUM(ISAT).GT.1)THEN
              DO I=1,6
                SUM(I,ISAT) =SUM(I,ISAT)/NSUM(ISAT)
                SUM0(I,ISAT)=DSQRT(SUM2(I,ISAT)/(NSUM(ISAT)-1))
                SUM2(I,ISAT)=SUM2(I,ISAT)-NSUM(ISAT)*SUM(I,ISAT)**2
                SUM2(I,ISAT)=DSQRT(SUM2(I,ISAT)/(NSUM(ISAT)-1))
              ENDDO
            ELSE
              SUM0(:,ISAT)=0
              SUM2(:,ISAT)=0
            ENDIF
          ENDDO
C
C PRINT
C -----
          WRITE(LFNPRT,355)SUM(1:3,1),SUM2(1:3,1),SUM0(1:3,1)
355       FORMAT(/,' Mean:   ',3X,3F14.4,
     1           /,' RMS :   ',3X,3F14.4,
     2           /,' RMS0:   ',3X,3F14.4)
C
          IF (IPLT.EQ.1) THEN
C
C CHECK OVERFLOW
            DO J=1,NSAT
              DO I=1,3
                IRMS(1,I,J)=IDNINT(SUM(I,J)*100D0)
                IRMS(2,I,J)=IDNINT(SUM2(I,J)*100D0)
                IRMS(3,I,J)=IDNINT(SUM0(I,J)*100D0)
                DO K=1,3
                  IF (IRMS(K,I,J).GT. 9999) IRMS(K,I,J)= 9999
                  IF (IRMS(K,I,J).LT.-9999) IRMS(K,I,J)=-9999
                ENDDO
              ENDDO
            ENDDO
C
            WRITE(LFNPLT,"('#')")
            WRITE(LFNPLT,351)'MEAN',(IRMS(1,1:3,I),I=1,NSAT)
            WRITE(LFNPLT,351)'RMS ',(IRMS(2,1:3,I),I=1,NSAT)
            WRITE(LFNPLT,351)'RMS0',(IRMS(3,1:3,I),I=1,NSAT)
351         FORMAT('# ',A4,':    ',36(1X,I5,1X,I5,1X,I5))
          ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ENDIF
      ENDDO
C
C TOTAL RMS
C ---------
      IF(NSUMT.GT.1)THEN
        DO I=1,6
          SUMT(I) =SUMT(I)/NSUMT
          SUM0T(I)=DSQRT(SUM2T(I)/(NSUMT-1))
          SUM2T(I)=SUM2T(I)-NSUMT*SUMT(I)**2
          SUM2T(I)=DSQRT(SUM2T(I)/(NSUMT-1))
        ENDDO
      ELSE
        SUM0T=0
        SUM2T=0
      ENDIF
      IF (IPLT.EQ.1) THEN
        WRITE(LFNPLT,451)SUMT(1:3), SUMT(4:6)*1D3,
     1                   SUM2T(1:3),SUM2T(4:6)*1D3,
     2                   SUM0T(1:3),SUM0T(4:6)*1D3
451           FORMAT('#',
     1             /,'# TOTAL:',
     2             /,'# MEAN ALL:',2(1X,3F10.4),
     3             /,'# RMS  ALL:',2(1X,3F10.4),
     4             /,'# RMS0 ALL:',2(1X,3F10.4))
      ENDIF
C
C WRITE HELMERT PARAMETERS
C ------------------------
      IF(NHELM.GT.0)THEN
        WRITE(LFNPRT,31)RMS
31      FORMAT(/,' HELMERT PARAMETERS:               RMS',F11.6)
        IF(IPLT.EQ.1)WRITE(LFNPLT,32)RMS
32      FORMAT('#',/,'# HELMERT PARAMETERS:               RMS',F11.6)
        DO I=1,8
          IF(IHELM(I).NE.0)THEN
            WRITE(LFNPRT,35)CHR(I),HELM(I)*UNIT(I),
     1                             ERR(I)*UNIT(I),CUNIT(I)
35          FORMAT(1X,A,': ',F12.6,'  +-',F12.6,2X,A)
            IF(IPLT.EQ.1)WRITE(LFNPLT,36)CHR(I),HELM(I)*UNIT(I),
     1                                   ERR(I)*UNIT(I),CUNIT(I)
36          FORMAT('# ',A,': ',F12.6,'  +-',F12.6,2X,A)
          ENDIF
        ENDDO
        WRITE(LFNPRT,*)
        IF(IPLT.EQ.1)WRITE(LFNPLT,"('#')")
      ELSE
        WRITE(LFNPRT,37)RMS
37      FORMAT(/,' RMS              ',F11.6)
        IF(IPLT.EQ.1)WRITE(LFNPLT,38)RMS
38      FORMAT('#',/,'# RMS        ',F11.6)
      ENDIF
C
C IDENTIFY MANEUVERS
C ------------------
      IF (IMANO.EQ.1.OR.IMANV.EQ.1) THEN
        SATLOOP: DO ISAT=1,NSAT
          IF (DIFMAX(1,ISAT).GT.POSMX.AND.
     1        DIFMIN(1,ISAT).LT.POSMN.AND.DIFMIN(2,ISAT).GT.VELMN) THEN
            SVN=SVNNUM(INDX(ISAT))
C PREPARE REGULA FALSI
            EPO1=EPOMIN(ISAT)
            CALL STDODF(FILST1,EPO1,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                     DXV,POS1,COEF,IRC)
            EPO2=EPOMIN(ISAT)+STEP1
            CALL STDODF(FILST1,EPO2,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                     DXV,POS2,COEF,IRC)
C REGULA FALSI
            NITER=0
            DO
              NITER=NITER+1
              DPOS=POS2(1:3)-POS1(1:3)
              TAU =-DOT_PRODUCT(DPOS,POS1(1:3))/DOT_PRODUCT(DPOS,DPOS)
              EPO3=EPO1+TAU*(EPO2-EPO1)
              IF (EPO3.LT.TSTART.OR.EPO3.GT.TEND) CYCLE SATLOOP
              CALL STDODF(FILST1,EPO3,DT,SVN,NHELM,IHELM,HELM,IMANV,
     1                                       DXV,POS3,COEF,IRC)
              IF (TAU.LT.0.5D0) THEN
                EPO2=EPO3
                POS2=POS3
              ELSE
                EPO1=EPO3
                POS1=POS3
              ENDIF
              IF (DABS(EPO2-EPO1).LT.EPS.OR.NITER.GT.10) EXIT
            ENDDO
C CLOSEST APPROACH
            DIFFP=DSQRT(POS3(1)**2+POS3(2)**2+POS3(3)**2)
            IF (DIFFP.GT.DISTM) CYCLE SATLOOP
            DIFFV=DSQRT(POS3(4)**2+POS3(5)**2+POS3(6)**2)
            CALL TIMST2(1,1,EPO3,TSTRNG)
            SVNM=MODSVN(SVN)
            WRITE(LFNPRT,"(/,' Maneuver:   ',A,'    PRN ',I3,
     1                          F9.4,' m',F9.1,' mm/s ',3F8.1,' mm/s')")
     2                         TSTRNG,SVNM,DIFFP,DIFFV*1D3,POS3(4:6)*1D3
            IF (NITER.GT.10)
     1            WRITE(LFNPRT,"(' *** Regula falsi: No convergence')")
            IF (IPLT.EQ.1) THEN
              WRITE(LFNPLT,"(/,'#',/,'# Maneuver:   ',A,'    PRN ',I3,
     1                          F9.4,' m',F9.1,' mm/s ',3F8.1,' mm/s')")
     2                         TSTRNG,SVNM,DIFFP,DIFFV*1D3,POS3(4:6)*1D3
              IF (NITER.GT.10)
     1            WRITE(LFNPLT,"('# *** Regula falsi: No convergence')")
            ENDIF
          ENDIF
        ENDDO SATLOOP
      ENDIF
C
C CLOSE ORBIT FILES
C -----------------
200   IF(IPLT.EQ.1)CLOSE(UNIT=LFNPLT)
C
      CALL EXITRC(0)
      END
