      MODULE s_ELESAV
      CONTAINS

C*
      SUBROUTINE ELESAV(TITLE,NFIL,ORBFIL,NSEFF,SVNEFF,ELE,RPRESS,
     1                  TOSC,NPAR,LOCQ,SOL,SCLORB,SCLSTO,RMS,ANOR,
     2                  NSTOCH,TSTOCH,KMAT,LMAT,MMAT,DRHO,DRHOT,
     3                  NUMDYN,SVNDYN,NSTCEP,HINT,ARCNUM,MANOVR)
CC
CC NAME       :  ELESAV
CC
CC PURPOSE    : GENERATE THE *.ELE FILE
CC
CC PARAMETERS :
CC        IN  : TITLE   : TITLE LINE                                CH*80
CC              NFIL    : NUMBER OF INPUT FILES                     I*4
CC              ORBFIL  : FILE NAMES                                CH*32(*,*)
CC              NSEFF   : NUMBER OF SATELLITES DEALT WITH           I*4
CC              SVNEFF  : SATELLITE NUMBERS                         I*4(*)
CC              ELE     : ELEMENTS OF SATELLITES                    R*8(*,*,*)
CC              RPRESS  : RADIATION PRESSURE PARAMETERS             R*8(*,*)
CC              TOSC    : OSCULATION EPOCH OF COMBINED ARC          R*8
CC              NPAR    : NUMBER OF PARAMETERS IN ADJUSTMENT        I*4
CC              LOCQ    : PARAMETER CHARACTERIZATION                I*4(*,*)
CC              SOL     : SOLUTION VECTOR                           R*8(*)
CC              SCLORB  : SCALING FACTORS FOR ORBITAL ELEMENTS      R*8(*)
CC              SCLSTO  : SCALING FACTORS FOR STOCHASTIC ELEMENTS   R*8(*)
CC              RMS     : MEAN ERROR OF WEIGHT UNIT                 R*8
CC              ANOR    : INVERTED NEQ MATRIX (UPPER TRIANGULAR)    R*8(*)
CC              NSTOCH  : NUMBER OF STOCHASTIC PARAMETERS           I*4
CC              TSTOCH  : EPOCHS FOR STOCHASTIC PULSES              R*8(*)
CC              KMAT    : K-MATRICES FOR TRAFO OF ELE-SETS          R*8(*,*,*,*)
CC              LMAT    : L-MATRICES FOR TRAFO OF ELE-SETS          R*8(*,*,*,*)
CC              MMAT    : K-MATRICES FOR TRAFO OF ELE-SETS          R*8(*,*,*,*,*)
CC              DRHO    : DRHO ARRAYS FOR TRAFO OF ELE-SETS         R*8(*,*,*)
CC              DRHOT   : DRHOT ARRAYS FOR TRAFO OF ELE-SETS        R*8(*,*,*)
CC              NSTCEP  : NUMBER OF STOCHASTIC PULSES PER EPOCH     I*4
CC              HINT    : PARTIAL INTERVAL LENGTH FOR INTEGRATION   R*8
CC              ARCNUM  : ARRAY CONTAINING THE ARCNUMBERS FOR       I*4(*,*)
CC                        EACH FILE/SATELLITE
CC              MANOVR  : INDICATES WHETHER THE SATELLITE "ISAT"    I*4(*)
CC                        HAD A MANOEUVRE
CC                        MANOVR(ISAT)=0 : NO MANEOUVRE SO FAR
CC                              (ISAT)=KFIL : THERE WAS A MANOEUVRE
CC                                            IN FILE KFIL.
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  08-APR-95 : MR: WRITE ALWAYS ARC NUMBER 1 FOR STOCHASTIC
CC                               PARAMETERS
CC               29-APR-95 : MR: WRITE TITLE FOR STOCH. ONLY IF NECESSARY
CC               10-JAN-96 : GB: CHANGES FOR NEW ORBIT MODEL
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               25-SEP-97 : DI: REMOVE UNUSED PARAMETER 'MAXSAT'
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               22-DEC-01 : HU: H EDIT DESCRIPTOR REMOVED
CC               22-JAN-03 : HU: WRITE TOSC WITH HIGHER PRECISION
CC               12-AUG-03 : HU: HANDLE SINGULAR STOCHASTIC PARAMETERS
CC               18-AUG-03 : RD: CLOSE OUTPUT FILE
CC               26-JAN-04 : HU: NO IF STATEMENT WITH UNDEFINED INDEX
CC               31-JAN-05 : HU: ORBID MODEL DESCRIPTION
CC               16-JUN-05 : MM: UNUSED COMCONST.INC REMOVED
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               16-JAN-08 : HB: ADD ACCELERATIONS FOR LEOS
CC               25-AUG-08 : DT: SYSTEM FOR DYNAMICAL PARAMETERS (DYX,RSW,DRSW)
CC               03-SEP-08 : DT: CORRECT WRITING IF MORE SETS OF STOCH.
CC                               PARAMETERS
CC               19-SEP-08 : DT: CHECK FOR ONE OR MORE ELE-FILES
CC                               CORRECTED/EXTENDED
CC               24-SEP-08 : DT: USE MAXDYN, MAXSTD FROM P_ADDNEQ
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               13-NOV-08 : DT: USE ORBDSC DIRECTLY FROM P_ORBGEN (NO CALL
CC                               GTORBMD)
CC               05-AUG-09 : DT: CORRECT WRITING OF STOCHASTIC PULSES
CC               18-NOV-09 : RD: SPECIAL INDEXING FOR EARLY MANEUVERS
CC               31-AUG-10 : HB: REPLACE MAXVAR WITH MAXELE
CC               03-DEC-10 : HB: ADD PARAMETER FOR SR PRTDER
CC               27-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM ELETRN
CC               27-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR,LFNLOC
      USE P_ORBGEN, ONLY: orbdsc
      USE P_ADDNEQ, ONLY: opt, maxDyn, maxSTD
      USE d_satcrx, ONLY: gtsatm2
      USE s_prtder
      USE s_opnfil
      USE f_modsvn
      USE s_inlist
      USE s_opnerr
      USE s_curarc
      USE s_eletrn
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , ICR   , IELE  , IFIL  , IFMTEL, INTER ,
     1          IORSYS, IOSTAT, IPAR  , IPRTH , IRCRPR, ISAT  , iIST  ,
     2          ISOL  , IST   , ISTOCH, KRPR  , KSAT  ,
     3          MXCDYN, MXCLCQ, MXCSAT, MXCSTD, MAXMAN, MAXELE,
     4          NFIL  , NFLOUT, NORB  , NPAR  , NRAD1 , NSEFF , NSTCEP,
     5          NSTOCH, NUMDYN, NUMSAT, NUMSTD, NVAR1 , SVNOLD, ARCOLD,
     6          NMA2  , IMAN  , NSTOC , ISTOC , SVNNEW
C
      REAL*8    EN    , EO    , HINT  , RMS   , RMSII , TBEG  , TLAST  ,
     1          TMIN  , TEND
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER    (MAXMAN=2000,MAXELE=15)
C
      CHARACTER*80 TITLE
      CHARACTER*32 ORBFIL(4,*),FILNAM
      CHARACTER*21 TXTELE(2+MAXELE)
      CHARACTER*8  ANLTYP
      CHARACTER*6  MXNLCQ,MXNDYN,MXNSAT,MXNSTD
      CHARACTER*1  MARK
      CHARACTER*4  empiri
C
      INTEGER*4   SVNEFF(*),LOCQ(MXCLCQ,*),SVNDYN(*)
      INTEGER*4   ARCNUM(MXCSAT,*),MANOVR(*)
      INTEGER*4   FILNUM(MAXSTD),EXTELE(MAXELE)
      INTEGER*4   ISTL(6),INTR(6),INDX(6)
      INTEGER*4   SATMA2(MAXMAN)
C
      REAL*8      ELE(7,MXCSAT,*),RPRESS(MXCDYN,MXCSAT,*)
      REAL*8      SOL(*),SCLORB(*),SCLSTO(*)
      REAL*8      TOSC(*),ANOR(*),TSTOCH(*),KMAT(6,6,MXCSAT,*)
      REAL*8      LMAT(6,MXCDYN,MXCSAT,*)
      REAL*8      MMAT(6,MXCDYN,MXCSAT,MXCSTD,*)
      REAL*8      DRHO(6,MXCSAT,*),DRHOT(6,MXCSAT,*)
      REAL*8      ELEOLD(MAXELE),ELENEW(MAXELE),RMSI(MAXELE)
      REAL*8      HELP(3),ELESAT(7),RPRPAR(MAXELE)
      REAL*8      TSTC(6),STOC(6),RMSJ(6)
      REAL*8      TIMMA2(MAXMAN),RLIM
C
c
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMDYN/MXCDYN,MXNDYN
      COMMON/MCMSTD/MXCSTD,MXNSTD
C
      DATA        RLIM/.042D0/
C
C MAXIMUM DIMENSION
C -----------------
      IF(MAXSTD.LT.MXCSTD)THEN
        WRITE(LFNERR,13)MAXSTD
13      FORMAT(//,' ** SR ELESAV : LOCAL DIM. MAXSTD TOO SMALL :',I3,//)
        CALL EXITRC(2)
      END IF
C
C ONE FILE OR N FILES TO BE GENERATED ?
C -------------------------------------
      IF(ORBFIL(4,1)(1:4).EQ.'    ')GOTO 999
      IF(opt%splitDyn.EQ.0 .AND.
     1   ORBFIL(4,1).EQ.ORBFIL(4,NFIL) )THEN
        NFLOUT=1
      ELSE
        NFLOUT=NFIL
      END IF
C
C TEXT TO BE WRITTEN INTO THE OUTPUT FILE:
C ---------------------------------------
      TXTELE(1) ='ARC-NUMBER          ='
      TXTELE(2) ='A                   ='
      TXTELE(3) ='E                   ='
      TXTELE(4) ='I                   ='
      TXTELE(5) ='NODE                ='
      TXTELE(6) ='PERIGEE             ='
      TXTELE(7) ='ARG. OF LAT (START) ='
C
C LOOP OVER ALL OUTPUT FILES
C --------------------------
      DO 1000 IFIL=1,NFLOUT
        IF (IFIL == 1) CALL GTSATM2(MAXMAN,NMA2,SATMA2,TIMMA2)
C
C GET TYPE OF A PRIORI ORBIT FILE
        CALL PRTDER(ORBFIL(2,IFIL),SVNEFF(1),1,0,0,TOSC(IFIL),1,ICR,
     1              IORSYS,NVAR1,NRAD1,HELP,ELESAT,RPRPAR,ANLTYP,
     2              IRCRPR)
C
C OUTPUT FILE NAME
        FILNAM=ORBFIL(4,IFIL)
C
C OPEN OUTPUT FILE
        CALL OPNFIL(LFNLOC,FILNAM,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'ELESAV')
C
C PRINT TITLE
        WRITE(LFNLOC,20)TITLE
20      FORMAT(A80)
        IF (ORBDSC%NLIN > 0) THEN
          IFMTEL=1
          WRITE(LFNLOC,"(80('-'))")
          WRITE(LFNLOC,"('FORMAT: ',2I6)") IFMTEL,ORBDSC%NLIN
          DO I=1,ORBDSC%NLIN

            IF (orbdsc%orbmod(I)(1:6).EQ.'EMPIRI') THEN
              empiri=orbdsc%orbmod(I)(9:12)
            END IF

            WRITE(LFNLOC,"(A80)") orbdsc%orbmod(I)

          ENDDO
          WRITE(LFNLOC,"(80('-'))")
        ENDIF
C
C Which dynamical parameters?
C ---------------------------
        IF (empiri.EQ.'DYX ')THEN
          TXTELE(8) ='D0                  ='
          TXTELE(9) ='Y0                  ='
          TXTELE(10)='X0                  ='
          TXTELE(11)='DC                  ='
          TXTELE(12)='YC                  ='
          TXTELE(13)='XC                  ='
          TXTELE(14)='DS                  ='
          TXTELE(15)='YS                  ='
          TXTELE(16)='XS                  ='
        ELSEIF(empiri.EQ.'RSW ')THEN
          TXTELE(8) ='R0                  ='
          TXTELE(9) ='S0                  ='
          TXTELE(10)='W0                  ='
          TXTELE(11)='RC                  ='
          TXTELE(12)='SC                  ='
          TXTELE(13)='WC                  ='
          TXTELE(14)='RS                  ='
          TXTELE(15)='SS                  ='
          TXTELE(16)='WS                  ='
        ELSEIF(empiri.EQ.'DRSW')THEN
          TXTELE(8) ='D0                  ='
          TXTELE(9) ='S0                  ='
          TXTELE(10)='W0                  ='
          TXTELE(11)='RC                  ='
          TXTELE(12)='SC                  ='
          TXTELE(13)='WC                  ='
          TXTELE(14)='RS                  ='
          TXTELE(15)='SS                  ='
          TXTELE(16)='WS                  ='
        ENDIF
C
C LOOP OVER ALL SATELLITES (PRINT ELEMENTS+DYNAMIC PARMAMETERS)
C -------------------------------------------------------------
        DO 200 ISAT=1,NSEFF
C
C COLLECT ELEMENTS AND RADIATION PRESSURE PARAMETERS FOR CURRENT FILE
C -------------------------------------------------------------------
          CALL ELETRN(IFIL,ISAT,NSEFF,SVNEFF,NPAR,LOCQ,ANOR,SOL,
     1                RMS,SCLORB,SCLSTO,ELE,RPRESS,NUMDYN,SVNDYN,
     2                KMAT,LMAT,MMAT(1,1,1,1,IFIL),DRHO,DRHOT,
     3                TOSC,TSTOCH,ARCNUM,NORB,ELEOLD,ELENEW,
     4                RMSI,EXTELE)
C
C IF SATELLITE WAS NOT IN ARC, SKIP SATELLITE
C -------------------------------------------
          IF(NORB.EQ.0)GO TO 200
C
C HANDLE MANOEUVRE SATELLITE
C --------------------------
          KSAT=0
          IF(SVNEFF(ISAT).NE.MODSVN(SVNEFF(ISAT)))THEN
            CALL INLIST(MODSVN(SVNEFF(ISAT)),NSEFF,SVNEFF,KSAT)
            IF (KSAT.EQ.0) THEN
              CALL GTSATM2(MAXMAN,NMA2,SATMA2,TIMMA2)
              DO IMAN=1,NMA2
                IF (DABS(TOSC(IFIL)-TIMMA2(IMAN)).LT.RLIM.AND.
     1              MODSVN(SVNEFF(ISAT)).EQ.SATMA2(IMAN)) THEN
                  CALL INLIST(SVNEFF(ISAT),NSEFF,SVNEFF,KSAT)
                  EXIT
                ENDIF
              ENDDO
            ENDIF
          END IF
          IF (KSAT.GT.0) THEN
            IF(IFIL.EQ.MANOVR(KSAT))THEN
              NUMSAT=SVNEFF(ISAT)
            ELSE
              NUMSAT=MODSVN(SVNEFF(ISAT))
            END IF
          ELSE
            NUMSAT=MODSVN(SVNEFF(ISAT))
          END IF
C
C WRITE TITLE LINE FOR SATELLITE
C ------------------------------
CCC  Arc number ist fest verdrahtet auf 1 gesetzt !!!
          IF (NUMSAT < 900 .OR. NUMSAT > 950) THEN
            WRITE(LFNLOC,30)TXTELE(1),1,NUMSAT,TOSC(IFIL)
30          FORMAT(A21,I3,' SATELLITE           =',I3,' TOSC=',F20.12,
     1                  /,75('-'))
          ELSEIF (NUMSAT >= 900 .AND. NUMSAT < 951) THEN
            WRITE(LFNLOC,31)TXTELE(1),1,NUMSAT,TOSC(IFIL)
31          FORMAT(A21,I3,' LEO                 =',I3,' TOSC=',F20.12,
     1                  /,75('-'))
          ENDIF
          MARK=' '
          DO 100 IELE=1,6
            IF(IELE.EQ.1)THEN
              WRITE(LFNLOC,50)
     1        TXTELE(1+IELE),ELEOLD(IELE),ELENEW(IELE),RMSI(IELE),
     2        MARK,IORSYS
50            FORMAT(A21,2F16.5,' +-',F12.3,2X,A1,' ORBSYS',I2)
            ELSE IF(IELE.EQ.2)THEN
              WRITE(LFNLOC,51)
     1        TXTELE(1+IELE),ELEOLD(IELE),ELENEW(IELE),RMSI(IELE),
     2        MARK,IORSYS
51            FORMAT(A21,2F16.10,' +-',F12.9,2X,A1,' ORBSYS',I2)
            ELSE IF(IELE.GE.3.AND.IELE.LE.6)THEN
              WRITE(LFNLOC,52)
     1        TXTELE(1+IELE),ELEOLD(IELE),ELENEW(IELE),RMSI(IELE),
     2        MARK,IORSYS
52            FORMAT(A21,2F16.9,' +-',F12.9,2X,A1,' ORBSYS',I2)
            END IF
100       CONTINUE
C
C WRITE RADIATION PRESSURE PARAMETERS
C -----------------------------------
          DO 190 IELE=7,MAXELE
C
C LOOK FOR CORRESPONDING ARRAY ELEMENT IN SOLUTION
            DO 110 KRPR=7,NORB
              IF(EXTELE(KRPR).EQ.IELE)THEN
                ISOL=KRPR
                EO=ELEOLD(ISOL)
                EN=ELENEW(ISOL)
                RMSII=RMSI(ISOL)
                MARK=' '
                GO TO 120
              END IF
110         CONTINUE
            EO=0.D0
            EN=0.D0
            RMSII=0.D0
            MARK='*'
120         CONTINUE
            WRITE(LFNLOC,53)
     1            TXTELE(1+IELE),EO,EN,RMSII,MARK,ANLTYP
53          FORMAT(A21,2D16.9,' +-',D12.5,2X,A1,1X,A8)
190       CONTINUE
200     CONTINUE
C
C STOCHASTIC PARAMETERS
C ---------------------
        IPRTH=1
        SVNOLD=-1
        ARCOLD=-1
C
C LOOP OVER ALL SATELLITES
        DO 300 ISAT=1,NSEFF
          CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
C
C LOOP OVER ALL (STOCHASTIC) PARAMETERS
          TLAST=0D0
          DO
            TMIN=1D20
            NSTOC=0
C
C FIND PARAMETERS FOR SMALLEST EPOCH
            DO IPAR=1,NPAR
              IF(LOCQ(1,IPAR).EQ.11.AND.
     1           LOCQ(3,IPAR).EQ.SVNEFF(ISAT).AND.
     2           LOCQ(2,IPAR).EQ.IARC)THEN
                ISTOCH= LOCQ(4,IPAR)
                IF (TSTOCH(ISTOCH).GT.TLAST+1D-6) THEN
                  IF (DABS(TSTOCH(ISTOCH)-TMIN).LT.1D-6) THEN
                    NSTOC=NSTOC+1
                    IF (NSTOC.GT.6) THEN
                      WRITE(LFNERR,"(/,' *** SR ELESAV: TOO MANY ',
     1                  'STOCHASTIC PARAMETERS FOR EPOCH',F15.8,/)")
     2                  TSTOCH(ISTOCH)
                      CALL EXITRC(2)
                    ENDIF
                    INDX(NSTOC)=IPAR
                  ELSEIF (TSTOCH(ISTOCH).LT.TMIN) THEN
                    TMIN=TSTOCH(ISTOCH)
                    NSTOC=1
                    INDX(NSTOC)=IPAR
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
C
C NO STOCHASTIC PARAMETER FOUND: NEXT SATELLITE
            IF (NSTOC.EQ.0) EXIT
C
C SAVE EPOCH FOR NEXT LOOP
            TLAST=TSTOCH(LOCQ(4,INDX(1)))
C
C COLLECT INFORMATION FOR PULSES OF SELECTED EPOCH
            ISTL(1:6)=0
            STOC(1:6)=0D0
            RMSJ(1:6)=0D0
            DO ISTOC=1,NSTOC
              IPAR=INDX(ISTOC)
              SVNNEW=LOCQ(3,IPAR)
C
C HANDLE MANOEUVRE SATELLITE
              IF(SVNEFF(ISAT).NE.MODSVN(SVNEFF(ISAT))) THEN
                CALL INLIST(MODSVN(SVNEFF(ISAT)),NSEFF,SVNEFF,KSAT)
                IF (KSAT.EQ.0) THEN
                  CALL GTSATM2(MAXMAN,NMA2,SATMA2,TIMMA2)
                  DO IMAN=1,NMA2
                    IF (DABS(TOSC(IFIL)-TIMMA2(IMAN)).LT.RLIM.AND.
     1                  MODSVN(SVNEFF(ISAT)).EQ.SATMA2(IMAN)) THEN
                      CALL INLIST(SVNEFF(ISAT),NSEFF,SVNEFF,KSAT)
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF (KSAT.GT.0) THEN
                  IF(IFIL.EQ.MANOVR(KSAT))THEN
                    NUMSAT=SVNEFF(ISAT)
                  ELSE
                    NUMSAT=MODSVN(SVNEFF(ISAT))
                  END IF
                ELSE
                  NUMSAT=MODSVN(SVNEFF(ISAT))
                END IF
              ELSE
                NUMSAT=SVNEFF(ISAT)
              END IF
C
C COLLECT STOCHASTIC PULSES FOR SATELLITE
              IST = LOCQ(5,IPAR)
              IF (IST>10) THEN
                iIST=IST-10
              ELSE
                iIst=IST
              ENDIF
              RMSII = RMS*DSQRT(ANOR(IPAR*(IPAR+1)/2))/SCLSTO(iIST)
              ISTOCH= LOCQ(4,IPAR)
              INTER=DNINT((TSTOCH(ISTOCH)-TOSC(IFIL))/(HINT/24.D0))+1
              IF(NFLOUT.EQ.1)THEN
                ISTL(iIST)=IST
                INTR(iIST)=INTER
                TSTC(iIST)=TSTOCH(ISTOCH)
                STOC(iIST)=SOL(IPAR)/SCLSTO(iIST)
                RMSJ(iIST)=RMSII
              ELSE
                TBEG=TOSC(IFIL)+5/1440.D0
                IF(IFIL.LT.NFLOUT)THEN
                  TEND=TOSC(IFIL+1)
                ELSE
                  TEND=10000000000.D0
                END IF

                IF(TSTOCH(ISTOCH).GE.TBEG.AND.
     1             TSTOCH(ISTOCH).LT.TEND)THEN
                  ISTL(iIST)=IST
                  INTR(iIST)=INTER
                  TSTC(iIST)=TSTOCH(ISTOCH)
                  STOC(iIST)=SOL(IPAR)/SCLSTO(iIST)
                  RMSJ(iIST)=RMSII
C
C Cycle parameter loop if not fitting into interval
                ELSE
                  CYCLE
                END IF
              END IF
C
C ..WRITE GENERAL TITLE
              IF (IPRTH.EQ.1) THEN
                WRITE(LFNLOC,210)
210             FORMAT('*** STOCHASTIC ORBIT PARAMETERS ***',
     1                /,80('-'))
                IPRTH=0
              END IF
C
C ..SATELLITE / ARC HEADER
              IF(SVNEFF(ISAT).NE.SVNOLD .OR. IARC.NE.ARCOLD) THEN
                IF (NUMSAT <900 .OR. NUMSAT > 950) THEN
                  WRITE(LFNLOC,30)TXTELE(1),1,NUMSAT,TOSC(IFIL)
                ELSEIF (NUMSAT >= 900 .AND. NUMSAT < 951) THEN
                  WRITE(LFNLOC,31)TXTELE(1),1,NUMSAT,TOSC(IFIL)
                ENDIF
                SVNOLD=SVNEFF(ISAT)
                ARCOLD=IARC
              END IF
C
C ..STOCHASTIC PULSES
              WRITE(LFNLOC,220)ISTL(IST),NSTCEP,INTR(IST),TSTC(IST),
     1                         0.D0,STOC(IST),RMSJ(IST)
220           FORMAT(2I2,I4,F12.5,1X,2D16.9,D15.5)
C
            END DO
!!          END IF
        END DO
!!280       CONTINUE
C
CC --- wrong!!! must be within loop of parameters -> 05-Aug-09 corrected ----
CCC
CCC WRITE LINES FOR STOCHASTIC PARAMETERS
CCC
CCC ..COUNT NUMBER OF PULSES
CC          NSTOC=0
CC          DO I=1,6
CC            IF (ISTL(I).NE.0) NSTOC=NSTOC+1
CC          ENDDO
CCC ..NO PULSES: DO NOT WRITE
CC          IF (NSTOC.EQ.0) CYCLE
CCC
CCC ..WRITE GENERAL TITLE
CC          IF (IPRTH.EQ.1) THEN
CC            WRITE(LFNLOC,210)
CC210         FORMAT('*** STOCHASTIC ORBIT PARAMETERS ***',
CC     1            /,80('-'))
CC            IPRTH=0
CC          END IF
CCC ..SATELLITE HEADER
CC          WRITE(LFNLOC,30)TXTELE(1),1,NUMSAT,TOSC(IFIL)
CCC ..PULSES
CC          DO I=1,6
CC            IF (ISTL(I).NE.0) THEN
CC              WRITE(LFNLOC,220)ISTL(I),NSTOC,INTR(I),TSTC(I),
CC     1                         0.D0,STOC(I),RMSJ(I)
CC220           FORMAT(2I2,I4,F12.5,1X,2D16.9,D15.5)
CC            ENDIF
CC          ENDDO
CCC
300     CONTINUE
        CLOSE(LFNLOC)
1000  CONTINUE
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
