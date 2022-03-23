      MODULE s_ADJBIM
      CONTAINS

C*
      SUBROUTINE ADJBIM(IADJ  ,OPTPG1,OPTPG2,INXSP1,TECMAP,MAXVAL,
     1                  BIMCO0,BIMCOE,BIMRMS,XRMS0 ,XWRMS ,XMAX  ,
     2                  IWAR  )
CC
CC NAME       :  ADJBIM
CC
CC PURPOSE    :  DETERMINE BROADCAST IONOSPHERE MODEL (BIM)
CC               PARAMETERS BEST FITTING IONEX TEC MAPS
CC
CC PARAMETERS :
CC         IN :  IADJ   : ADJUSTMENT NUMBER                   I*4
CC               OPTPG1 : OPTIONS 1                           I*4(*)
CC                        (1): DETERMINE BIM PARAMETERS FOR
CC                             N TEC MAPS EACH
CC                             =0: ALL
CC                        (2): IGNORE TEC DATA AT POLAR CAPS
CC                        (3): CONSIDER RMS MAPS
CC                        (4): TAKE OVER ADJUSTED BIM
CC                             PARAMETERS AS A PRIORI INFO
CC                        (5): RESTRICT ANALYSIS TO PRE-
CC                             DEFINED AREA
CC                        (6): MAXIMUM NUMBER OF ITERATIONS
CC                        (7): FIND BEST FITTING SET OF
CC                             KLOBUCHAR CONSTANS
CC               OPTPG2 : OPTIONS 2                           R*8(*)
CC                        (1): FROM LATITUDE
CC                        (2): TO LATITUDE
CC                        (3): FROM LONGITUDE
CC                        (4): TO LONGITUDE
CC                        (5): REDUCE TEC DATA WITH FACTOR
CC                        (6): ADD CONSTANT
CC               INXSP1 : SPECIFICATIONS 1                    R*8(*)
CC                        ( 1): EPOCH OF FIRST MAP (IN MJD)
CC                        ( 2): EPOCH OF LAST MAP (IN MJD)
CC                        ( 3): INTERVAL (IN SEC)
CC                        ( 4): FROM LATITUDE
CC                        ( 5): TO LATITUDE
CC                        ( 6): WITH INCREMENT (IN DEG)
CC                        ( 7): FROM LONGITUDE
CC                        ( 8): TO LONGITUDE
CC                        ( 9): WITH INCREMENT (IN DEG)
CC                        (10): ELEVATION CUTOFF (IN DEG)
CC                        (11): BASE RADIUS (IN KM)
CC                        (12): FROM HEIGHT
CC                        (13): TO HEIGHT
CC                        (14): WITH INCREMENT (IN KM)
CC               TECMAP(I,J),I=1,..,MAXVAL,J=1,2: TEC/RMS MAP R*8(*,*)
CC                        (IN TECU)
CC                        =999.9: UNDEFINED
CC               MAXVAL : MAXIMUM NUMBER OF TEC/RMS VALUES    I*4
CC               BIMCO0(I=1,2,J=1,..,4): A PRIORI BIM         R*8(2,4)
CC                        COEFFICIENTS
CC                        I=1: ION ALPHAS
CC                        I=2: ION BETAS
CC        OUT :  BIMCO0(I=1,2,J=1,..,4): ADJUSTED BIM         R*8(2,4)
CC                        COEFFICIENTS
CC                        I=1: ION ALPHAS
CC                        I=2: ION BETAS
CC               BIMRMS(I=1,2,J=1,..,4): ASSOCIATED RMS INFO  R*8(2,4)
CC                        I=1: ION ALPHAS
CC                        I=2: ION BETAS
CC               XRMS0  : RMS OF UNIT WEIGHT                  R*8
CC               XWRMS  : WEIGHTED RMS ERROR (TECU)           R*8
CC               XMAX(I=1,2): MAXIMUM TEC (TECU)              R*8(2)
CC                        I=1: EQUATORIAL REGION
CC                        I=2: POLAR REGION
CC               IWAR   : WARNING WRT PROBLEMS AT POLAR CAPS  I*4
CC                        =-1: BOTH POLAR CAPS
CC                        = 0: OK
CC                        = 1: NORTHERN POLAR CAP
CC                        = 2: SOUTHERN POLAR CAP
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.3
CC
CC CREATED    :  23-JUN-00
CC
CC CHANGES    :  12-JUL-00 : SS: DETECT PROBLEMS AT POLAR CAPS
CC               09-AUG-00 : SS: MANAGE NUMERICAL INSTABILITIES
CC               06-JUL-04 : RD: FORMAT STATEMENT CORRECTED
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC               04-MAY-12 : RD: REMOVE UNUSED VARIABLES
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2000     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt, lfnerr
      USE d_const,  ONLY: PI
      USE l_basfun, ONLY: dmod
      USE f_ikf
      USE s_gtixps
      USE s_solve
      USE s_syminvg
      USE s_bimder
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IADJ  , ICOE  , IERR1 , IERR2 , ILAT  ,
     1          ILON  , IMAP  , IMAP1 , IMAP2 , INTD  , IPAR  , IPAR1 ,
     2          IPAR2 , IPOS  , IRED1 , IRED2 , IRUN  , ISING , IWAR  ,
     3          IWAR0 , IWAR1 , IWAR2 , MAXPAR, MAXVAL, NDOF  , NLAT  ,
     4          NLON  , NMAP  , NOBS  , NOSC  , NOSC0 , NRUN
C
      REAL*8    XEPO  , XINT  , XION0 , XLAT  , XLAT0 , XLON  ,
     1          XOMC  , XPER  , XPER0 , XRMS0 , XSIG  , XSIG0 , XSUM  ,
     2          XTEC  , XWGT  , XWGT0 , XWRMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXPAR=8)
C
      REAL*8        OPTPG2(*),INXSP1(*),TECMAP(MAXVAL,*)
      REAL*8        BIMCO0(2,4),BIMCOE(2,4),BIMRMS(2,4)
      REAL*8        BIMARG(3),XTEC0(2),XDER(2,4)
      REAL*8        ANOR(MAXPAR*(MAXPAR+1)/2),BNOR(MAXPAR)
      REAL*8        AOBS(MAXPAR)
      REAL*8        XSOL0(MAXPAR),XSOL(MAXPAR)
      REAL*8        INPARG(4),OUTARG(4)
      REAL*8        BIMSCA(2)
      REAL*8        XMAX(*)
C
      INTEGER*4     OPTPG1(*)
C
C
      DATA XSIG0/1.D0/,BIMSCA/1.D-8,1.D4/,XLAT0/74.88D0/
      DATA XPER0/72000.D0/,XION0/5.D-9/
      DATA NOSC0/3/
C
      IRED1=0
      IRED2=0
C
      IWAR=0
      IWAR0=3
      NOSC=-1
C
C DEFINE A PRIORI SOLUTION VECTOR
C -------------------------------
      DO ICOE=1,4
        XSOL0(ICOE  )=BIMCO0(1,ICOE)
        XSOL0(ICOE+4)=BIMCO0(2,ICOE)
      ENDDO
      DO IPAR=1,MAXPAR
        XSOL(IPAR)=1.D0
      ENDDO
C
C GET IONEX SPECIFICATIONS
C ------------------------
      XINT=INXSP1(3)/86400.D0
      NMAP=IDNINT((INXSP1(2)-INXSP1(1))/XINT)+1
C
      NLAT=IDNINT((INXSP1(5)-INXSP1(4))/INXSP1(6))+1
      NLON=IDNINT((INXSP1(8)-INXSP1(7))/INXSP1(9))+1
C
      IF (OPTPG1(1).EQ.0) THEN
        IMAP1=1
        IMAP2=NMAP
      ELSE
        IMAP1=(IADJ-1)*OPTPG1(1)+1
        IMAP2=IADJ*OPTPG1(1)
      ENDIF
C
      DO 100 IRUN=1,OPTPG1(6)
        DO ICOE=1,4
          BIMCOE(1,ICOE)=XSOL0(ICOE)
          BIMCOE(2,ICOE)=XSOL0(ICOE+4)
        ENDDO
C
C CHECK WHETHER ANOTHER ITERATION NECESSARY
C -----------------------------------------
        XSUM=0.D0
        DO IPAR=1,MAXPAR
          XSUM=XSUM+DABS(XSOL(IPAR))
        ENDDO
        IF (XSUM.LT.1.D-3) GOTO 900
C
        NOBS=0
        XMAX(1)=0.D0
        XMAX(2)=0.D0
        XPER=XPER0
        DO IPAR1=1,MAXPAR
          DO IPAR2=IPAR1,MAXPAR
            ANOR(IKF(IPAR1,IPAR2))=0.D0
          ENDDO
          BNOR(IPAR1)=0.D0
        ENDDO
        XRMS0=0.D0
        XWGT0=0.D0
        IWAR1=0
        IWAR2=0
C
        IF (NOSC.GT.NOSC0 .AND. IRED1.EQ.0) THEN
          WRITE(LFNERR,911)
911       FORMAT(/,' ### SR ADJBIM: NUMERICAL INSTABILITY DETECTED',
     1      /,16X,'ALPHA_3 RESET TO ZERO',/)
          IRED1=1
C
          XSOL0(2)=0.D0
          XSOL0(3)=-XSOL0(1)/0.416D0**2
          XSOL0(4)=0.D0
          DO ICOE=2,4
            BIMCOE(1,ICOE)=XSOL0(ICOE)
          ENDDO
        ENDIF
C
        IERR1=0
        IF (BIMCOE(2,1).LT.0.D0) IERR1=1
        IERR2=0
        IF (BIMCOE(2,1).LT.XPER0 .AND. IRUN.GT.OPTPG1(6)/2) IERR2=1
        IF (IERR1+IERR2.GT.0 .AND. IRED2.EQ.0) THEN
          IF (IERR1.EQ.1) THEN
            WRITE(LFNERR,912)
912         FORMAT(/,' ### SR ADJBIM: BETA_0 NEGATIVE',
     1        /,16X,'A PRIORI BETA COEFFICIENTS REDEFINED',/)
          ENDIF
          IF (IERR2.EQ.1) THEN
            WRITE(LFNERR,913)
913         FORMAT(/,' ### SR ADJBIM: BETA_0 SMALLER THAN 20 HOURS',
     1        /,16X,'A PRIORI BETA COEFFICIENTS REDEFINED',/)
          ENDIF
          IRED2=1
C
          XSOL0(5)=35.D0*2.D0**11
          BIMCOE(2,1)=XSOL0(5)
          DO ICOE=2,4
            XSOL0(ICOE+4)=0.D0
            BIMCOE(2,ICOE)=XSOL0(ICOE+4)
          ENDDO
        ENDIF
C
        DO 110 IMAP=IMAP1,IMAP2
          XEPO=INXSP1(1)+(IMAP-1)*XINT
C
          DO 120 ILAT=1,NLAT
            XLAT=(INXSP1(4)+(ILAT-1)*INXSP1(6))
            IF (OPTPG1(5).EQ.1 .AND. XLAT.LT.OPTPG2(1)) GOTO 120
            IF (OPTPG1(5).EQ.1 .AND. XLAT.GT.OPTPG2(2)) GOTO 120
C
            DO 130 ILON=1,NLON
              XLON=(INXSP1(7)+(ILON-1)*INXSP1(9))
              IF (OPTPG1(5).EQ.1 .AND. XLON.LT.OPTPG2(3)) GOTO 130
              IF (OPTPG1(5).EQ.1 .AND. XLON.GT.OPTPG2(4)) GOTO 130
C
              INPARG(1)=XEPO
              INPARG(2)=XLAT
              INPARG(3)=XLON
              INPARG(4)=0.D0
              CALL GTIXPS(INXSP1,1,INPARG,OUTARG,IPOS)
              XTEC=TECMAP(IPOS,1)
C
              BIMARG(1)=DMOD(XEPO,1.D0)
              BIMARG(2)=XLAT*PI/180.D0
              BIMARG(3)=XLON*PI/180.D0
              CALL BIMDER(BIMCOE,BIMARG,XTEC0,INTD,XDER)
C
C CHECK WHETHER PROBLEMS WRT POLAR CAPS
C -------------------------------------
              IF (DABS(XLAT).GT.XLAT0) THEN
                IF (INTD.EQ.0) THEN
                  IF (XLAT.GT. XLAT0) IWAR1=1
                  IF (XLAT.LT.-XLAT0) IWAR2=2
                ENDIF
                IF (OPTPG1(2).EQ.1) GOTO 130
              ENDIF
              IF (XTEC.GE.999.D0) GOTO 130
C
              NOBS=NOBS+1
              IF (DABS(XLAT).LT.XLAT0) THEN
                XMAX(1)=DMAX1(XTEC0(1),XMAX(1))
              ELSE
                XMAX(2)=DMAX1(XTEC0(1),XMAX(2))
              ENDIF
              XPER=DMAX1(XTEC0(2),XPER)
C
              DO ICOE=1,4
                AOBS(ICOE  )=XDER(1,ICOE)*BIMSCA(1)
                AOBS(ICOE+4)=XDER(2,ICOE)*BIMSCA(2)
              ENDDO
              IF (IRED1.EQ.1) AOBS(4)=0.D0
C
              XOMC=(XTEC*OPTPG2(5)+OPTPG2(6))-XTEC0(1)
C
              XSIG=1.D0/DCOS(XLAT)
              IF (OPTPG1(3).EQ.1) XSIG=XSIG*TECMAP(IPOS,2)
C
              XWGT=(XSIG0/XSIG)**2
              XWGT0=XWGT0+XWGT
C
              DO IPAR1=1,MAXPAR
                DO IPAR2=IPAR1,MAXPAR
                  ANOR(IKF(IPAR1,IPAR2))=ANOR(IKF(IPAR1,IPAR2))+
     1              AOBS(IPAR1)*XWGT*AOBS(IPAR2)
                ENDDO
                BNOR(IPAR1)=BNOR(IPAR1)+AOBS(IPAR1)*XWGT*XOMC
              ENDDO
              XRMS0=XRMS0+XWGT*XOMC**2
130         CONTINUE
120       CONTINUE
110     CONTINUE
C
        IWAR=MAX0(IWAR1,IWAR2)
        IF (MIN0(IWAR1,IWAR2).GT.0) IWAR=-1
C
        IF (IWAR.NE.IWAR0) NOSC=NOSC+1
        IWAR0=IWAR
C
C STOP IF NO TEC OBSERVATIONS FOUND
C ---------------------------------
        IF (NOBS.EQ.0) THEN
          WRITE(LFNERR,921)
921       FORMAT(/,' *** SR ADJBIM: NO TEC OBSERVATIONS FOUND',/)
          CALL EXITRC(2)
        ENDIF
C
C CONSTRAIN ALPHA_3 OR/AND BETA COEFFICIENTS IF REQUIRED
C ------------------------------------------------------
        IF (IRED1.EQ.1) ANOR(IKF(4,4))=ANOR(IKF(4,4))+1.D0
        IF (IRED2.EQ.1) THEN
          DO IPAR=5,8
            ANOR(IKF(IPAR,IPAR))=ANOR(IKF(IPAR,IPAR))+1.D0
          ENDDO
        ENDIF
C
C INVERT NORMAL EQUATION MATRIX
C -----------------------------
        CALL SYMINVG(MAXPAR,ANOR,0,ISING)
C
C CHECK WHETHER NORMAL EQUATION MATRIX REGULAR
C --------------------------------------------
        IF (ISING.GT.0) THEN
          DO ICOE=1,4
            BIMCOE(1,ICOE)=0.D0
            BIMCOE(2,ICOE)=0.D0
            BIMRMS(1,ICOE)=0.D0
            BIMRMS(2,ICOE)=0.D0
          ENDDO
          XRMS0=0.D0
          XWRMS=0.D0
C
          WRITE(LFNERR,922)
922       FORMAT(/,' ### SR ADJBIM: BIM PARAMETERS NOT DETERMINED',/)
          RETURN
        ENDIF
C
C SOLVE NORMAL EQUATION SYSTEM
C ----------------------------
        CALL SOLVE(MAXPAR,ANOR,BNOR,XSOL)
C
        DO ICOE=1,4
          XSOL0(ICOE  )=XSOL0(ICOE  )+XSOL(ICOE  )*BIMSCA(1)
          XSOL0(ICOE+4)=XSOL0(ICOE+4)+XSOL(ICOE+4)*BIMSCA(2)
        ENDDO
        DO IPAR=1,MAXPAR
          XRMS0=XRMS0-XSOL(IPAR)*BNOR(IPAR)
        ENDDO
C
        XWRMS=DSQRT(XRMS0/XWGT0)
C
        NDOF=NOBS-MAXPAR
        XRMS0=DSQRT(XRMS0/NDOF)
C
        WRITE(*,'(2F12.6,2I6,2F8.2)') XRMS0,XWRMS,IRUN,IWAR,
     1    (XMAX(I),I=1,2)
        WRITE(*,'(1P,4D12.4)') (XSOL0(I),I=1,4)
        WRITE(*,'(1P,4D12.4)') (XSOL0(I),I=5,8)
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(*,'(A8,F10.0)') 'PERIOD: ',XPER
        WRITE(LFNPRT,'(2F12.6,2I6,2F8.2)') XRMS0,XWRMS,IRUN,IWAR,
     1    (XMAX(I),I=1,2)
        WRITE(LFNPRT,'(1P,4D12.4)') (XSOL0(I),I=1,4)
        WRITE(LFNPRT,'(1P,4D12.4)') (XSOL0(I),I=5,8)
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(LFNPRT,'(A8,F10.0)') 'PERIOD: ',XPER
C
100   CONTINUE
C
900   CONTINUE
      NRUN=IRUN-1
C
      IF (IRED1.EQ.1) BIMCOE(1,4)=0.D0
      IF (IRED2.EQ.1) BIMCOE(2,1)=XPER0
C
      IERR1=0
      IERR2=0
      DO ICOE=1,4
        BIMRMS(1,ICOE)=XRMS0*DSQRT(ANOR(IKF(ICOE  ,ICOE  )))*BIMSCA(1)
        BIMRMS(2,ICOE)=XRMS0*DSQRT(ANOR(IKF(ICOE+4,ICOE+4)))*BIMSCA(2)
        IF (BIMRMS(1,ICOE).EQ.0.D0) THEN
          WRITE(LFNERR,931) ICOE-1
931       FORMAT(/,' *** SR ADJBIM: ALPHA_',I1,' NOT DETERMINED',/)
          IERR1=1
        ENDIF
        IF (BIMRMS(2,ICOE).EQ.0.D0) THEN
          WRITE(LFNERR,932) ICOE-1
932       FORMAT(/,' ### SR ADJBIM: BETA_',I1,' NOT DETERMINED',/)
          IERR2=1
        ENDIF
      ENDDO
      IF (IERR1.EQ.1) CALL EXITRC(2)
      IF (IERR2.EQ.1) THEN
CCC ???        WRITE(LFNERR,933) ICOE-1
        WRITE(LFNERR,933)
933     FORMAT(/,' ### SR ADJBIM: BETA COEFFICIENTS RESET',/)
C
        IF (BIMCOE(2,1).NE.0.D0) BIMCOE(2,1)=XPER0
        DO ICOE=2,4
          BIMCOE(2,ICOE)=0.D0
        ENDDO
      ENDIF
C
      IF (BIMCOE(2,1).GT.172800.D0 .AND. OPTPG1(5).NE.1) THEN
        WRITE(LFNERR,941) BIMCOE(2,1)
941     FORMAT(/,' ### SR ADJBIM: BETA_0 GREATER THAN 48 HOURS',
     1    /,16X,'BETA_0: ',1P,D12.4,' SEC',/)
      ENDIF
      IF (BIMCOE(2,1).LT.XPER0 .AND. OPTPG1(5).NE.1) THEN
        WRITE(LFNERR,942) BIMCOE(2,1)
942     FORMAT(/,' *** SR ADJBIM: BETA_0 SMALLER THAN 20 HOURS',
     1    /,16X,'BETA_0: ',1P,D12.4,' SEC',/)
        CALL EXITRC(2)
      ENDIF
      IF (BIMCOE(2,1).LT.0.D0 .AND. OPTPG1(5).NE.1) THEN
        WRITE(LFNERR,943) BIMCOE(2,1)
943     FORMAT(/,' *** SR ADJBIM: BETA_0 NEGATIVE',
     1    /,16X,'BETA_0: ',1P,D12.4,' SEC',/)
        CALL EXITRC(2)
      ENDIF
      IF (XPER.GT.172800.D0) THEN
        WRITE(LFNERR,944) XPER
944     FORMAT(/,' ### SR ADJBIM: PERIOD GREATER THAN 48 HOURS',
     1    /,16X,'PERIOD: ',1P,D12.4,' SEC',/)
      ENDIF
C
      IF (BIMCOE(1,1).LT.0.D0 .AND. OPTPG1(5).NE.1) THEN
        WRITE(LFNERR,951) BIMCOE(1,1)
951     FORMAT(/,' *** SR ADJBIM: ALPHA_0 NEGATIVE',
     1    /,16X,'ALPHA_0: ',1P,D12.4,' SEC',/)
        CALL EXITRC(2)
      ENDIF
      IF (BIMCOE(1,3).GT.0.D0 .AND. OPTPG1(5).NE.1) THEN
        WRITE(LFNERR,952) BIMCOE(1,3)
952     FORMAT(/,' *** SR ADJBIM: ALPHA_2 POSITIVE',
     1    /,16X,'ALPHA_2: ',1P,D12.4,' SEC',/)
        CALL EXITRC(2)
      ENDIF
      IF (BIMCOE(1,1).LT.XION0 .AND. OPTPG1(5).NE.1) THEN
        WRITE(LFNERR,953) BIMCOE(1,1)
953     FORMAT(/,' ### SR ADJBIM: ALPHA_0 SMALLER THAN NIGHT-TIME ',
     1    'CORRECTION',
     2    /,16X,'ALPHA_0: ',1P,D12.4,' SEC',/)
      ENDIF
C
      IF (XMAX(2).GT.XMAX(1)) THEN
        WRITE(LFNERR,961) XMAX(2)
961     FORMAT(/,' *** SR ADJBIM: TOO HIGH TEC AT POLAR REGION',
     1    /,16X,'TEC: ',F6.1,' TECU',/)
CC        CALL EXITRC(2)
      ENDIF
      IF (XMAX(2).GT.2.D0*9.2D0) THEN
        WRITE(LFNERR,962) XMAX(2)
962     FORMAT(/,' ### SR ADJBIM: HIGH TEC AT POLAR REGION',
     1    /,16X,'TEC: ',F6.1,' TECU',/)
      ENDIF
C
      WRITE(*,'(A11,2F12.6,2I6)')
     1  'KEY VALUES: ',XRMS0,XWRMS,NRUN,IWAR
      WRITE(*,'(1P,4D12.4)') (BIMRMS(1,I),I=1,4)
      WRITE(*,'(1P,4D12.4)') (BIMRMS(2,I),I=1,4)
C
      WRITE(LFNPRT,'(A11,2F12.6,2I6)')
     1  'KEY VALUES: ',XRMS0,XWRMS,NRUN,IWAR
      WRITE(LFNPRT,'(1P,4D12.4)') (BIMCOE(1,I),I=1,4)
      WRITE(LFNPRT,'(1P,4D12.4)') (BIMCOE(2,I),I=1,4)
      WRITE(LFNPRT,'(1P,4D12.4)') (BIMRMS(1,I),I=1,4)
      WRITE(LFNPRT,'(1P,4D12.4)') (BIMRMS(2,I),I=1,4)
      WRITE(LFNPRT,*)
C
      RETURN
      END SUBROUTINE

      END MODULE
