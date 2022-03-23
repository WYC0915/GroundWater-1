      MODULE s_ELETRN
      CONTAINS

C*
      SUBROUTINE ELETRN(IFIL,ISAT,NSEFF,SVNEFF,NPAR,LOCQ,ANOR,SOL,
     1                  RMS,SCLORB,SCLSTO,ELE,RPRESS,NUMDYN,SVNDYN,
     2                  KMAT,LMAT,MMAT,DRHO,DRHOT,TOSC,TSTOCH,ARCNUM,
     3                  NORB,ELEOLD,ELENEW,RMSI,EXTELE)
CC
CC NAME       :  ELETRN
CC
CC PURPOSE    : TRANSFORM ELEMENTS OF FILE 1 TO ELEMENTS OF FILE IFIL
CC              PREPARE OUTPUT OF ELEMENTS.
CC
CC PARAMETERS :
CC        IN  : IFIL    : FILE NUMBER                               I*4
CC              ISAT    : SATELLITE INDEX                           I*4
CC              SVNEFF  : ARRAY WITH SATELLITE NUMBERS              I*4(*)
CC              NPAR    : NUMBER OF PARAMETERS                      I*4
CC              LOCQ    : PARAMETER DESCRIPTION                     I*4(*,*)
CC              ANOR    : NEQ MATRIX                                R*8(*)
CC              SOL     : SOLUTION VECTOR                           R*8(*)
CC              RMS     : MEAN ERROR OF WEIGHT UNIT                 R*8
CC              SCLORB  : SCALE PARAMETERS FOR ORBIT PARAMETERS     R*8(*)
CC              SCLSTO  : SCALING FOR STOCHASTIC PARAMETERS         R*8(*)
CC              ELE     : A PRIORI OSCULATING ELEMENTS              R*8(*,*,*)
CC              RPRESS  : A PRIORI VALUES FOR THE RADIATION         R*8(*,*,*)
CC                        PRESSURE PARAMETERS
CC              NUMDYN  : NUMBER OF ARC-SPECIFIC RPR PAARAMETERS    I*4
CC              SVNDYN  : CORRESPONDING SVN-NUMBERS                 I*4(*)
CC              KMAT    : K-MATRICES                                R*8(*,*,*,*)
CC              LMAT    : L-MATRICES                                R*8(*,*,*,*)
CC              MMAT    : M-MATRICES                                R*8(*,*,*,*)
CC              DRHO    : DRHO ARRAYS FOR TRAFO OF ELE-SETS         R*8(*,*,*)
CC              DRHOT   : DRHOT ARRAYS FOR TRAFO OF ELE-SETS        R*8(*,*,*)
CC              TOSC    : OSCULATION EPOCHS FOR FILES               R*8(*)
CC              TSTOCH  : EPOCHS FOR STOCHASTIC PULSES              R*8(*)
CC              ARCNUM  : ARRAY CONTAINING THE ARCNUMBERS FOR       I*4(*,*)
CC                        EACH FILE/SATELLITE
CC        OUT : NORB    : NUMBER OF ORBIT PARAMETERS                R*8(*)
CC              ELEOLD  : OLD PARAMETER VALUES                      R*8(*)
CC              ELENEW  : NEW PARAMETER VALUES                      R*8(*)
CC              RMSI    : CORRESPONDING MEAN ERRORS                 R*8(*)
CC              EXTELE  : EXTERNAL ELEMENT NUMBER FOR               I*4(*)
CC                        SEQUENCE OF EST. ORBIT ELEMENTS
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  18-OCT-94 : EB: HANDLING OF STOCHASTIC PARA AND NEW ARC
CC               29-NOV-95 : EB: INIT. OF NCLAST AND COE
CC               26-JAN-96 : GB: NEW ORBIT MODEL
CC               27-JAN-96 : GB: "SMALL" CORRECTION
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               16-JUN-05 : MM: COMCONST.INC REPLACED BY D_CONST
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               22-AUG-08 : DT: USE MAXDYN, MAXSTD FROM P_ADDNEQ
CC               25-AUG-08 : DT: USE CORRECT SCLORB
CC               24-SEP-08 : DT: REMOVE UNUSED VARIABLE I000
CC                               INCREASE MAXCOL 80->500
CC               27-MAR-12 : RD: USE TRAFO4 AS MODULE NOW
CC               27-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC               17-AUG-12 : RD/SL: ORBFIL MUST BE AN ARRAY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR
      USE d_const,  ONLY: GM, PI
      USE p_addneq, ONLY: MAXSTD, MAXDYN, opt
      USE f_modsvn
      USE s_inlist
      USE s_curarc
      USE s_vcovlk
      USE s_trafo4
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IA    , IARC  , IDUM  , IDUMM , IELE  , IF    ,
     1          IFDYN , IFF   , IFIL  , II    , INCR  , IOOO  , IORB  ,
     2          IPAR  , IPIP  , IRSW  , ISAT  , ISTOCH, K     , KORB  ,
     3          LSAT  , MAXCOL, MXCDYN, MXCLCQ,
     4          MXCSAT, MXCSTD, MXCVAR, NCLAST, NEWOLD, NORB  , NPAR  ,
     5          NRPR  , NSEFF , NSTOCH, NUMDYN, NUMSAT, NUMSTD, NUMTST
C
      REAL*8    DUMMY , E0    , HELP  , RMS   , V0    , XM0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
CCC      PARAMETER (MAXDYN=12,MAXSTD=20)
CCC      PARAMETER (MAXCOL=80)
      PARAMETER (MAXCOL=500)
C
C MAXCOL=MAXIMUM NUMBER OF ORBIT PARAMETERS PER SATELLITE
C (OSCULATING ELEMENTS + # DYN. PARMS + STOCHASTIC PARMS)
C new: (# DYN. PARMS)*(#Files) is needed
C
      CHARACTER*6 MXNSAT,MXNLCQ,MXNDYN,MXNSTD,MXNVAR
      CHARACTER*32 ORBFIL(4,1)
C
      INTEGER*4   SVNEFF(*),LOCQ(MXCLCQ,*),SVNDYN(*),ARCNUM(MXCSAT,*)
      INTEGER*4   EXTELE(*),IDUM1(1)
      INTEGER*4   INDEX(MAXCOL),FILNUM(MAXSTD),FILNM2(MAXSTD)
C
      REAL*8      ANOR(*),SOL(*),SCLORB(*),SCLSTO(*),ELE(7,MXCSAT,*),
     1            RPRESS(MXCDYN,MXCSAT,*),KMAT(6,6,MXCSAT,*),
     2            LMAT(6,MXCDYN,MXCSAT,*),MMAT(6,MXCDYN,MXCSAT,*),
     3            RDUM1(1),DRHO(6,MXCSAT,*),DRHOT(6,MXCSAT,*),
     4            ELEOLD(*),ELENEW(*),RMSI(*),TSTOCH(*),TOSC(*)
      REAL*8      DQI(MAXDYN,MAXSTD),DELE(6),DELET(6)
      REAL*8      ELEPAR(8)
      REAL*8      COE(6,MAXCOL),COV(21)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMDYN/MXCDYN,MXNDYN
      COMMON/MCMSTD/MXCSTD,MXNSTD
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
C
C CHECK MAXIMUM DIMENSIONS
C ------------------------
      IF(MAXDYN.LT.MXCDYN)THEN
        WRITE(LFNERR,11)MAXDYN
11      FORMAT(//,' ** SR ELETRN : LOCAL MAXDYN TOO SMALL:',I3,//)
        CALL EXITRC(2)
      END IF
C
      IF(MAXSTD.LT.MXCSTD)THEN
        WRITE(LFNERR,12)MAXSTD
12      FORMAT(//,' ** SR ELETRN : LOCAL MAXSTD TOO SMALL:',I3,//)
        CALL EXITRC(2)
      END IF
C
C CURRENT ARC, NUMBER OF FILES, FILE NUMBERS
C ------------------------------------------
C
      CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
      IF(NUMSTD.EQ.0)THEN
        NORB=0
        GO TO 999
      END IF
C
C COLLECT ALL ORBITAL ELEMENTS FOR SATELLITE WITH INDEX ISAT :
C ----------------------------------------------------------
      NUMSAT=SVNEFF(ISAT)
      CALL INLIST(NUMSAT,NUMDYN,SVNDYN,IELE)
      NUMTST=MODSVN(NUMSAT)
      CALL INLIST(NUMTST,NSEFF,SVNEFF,LSAT)
C
      IF(IELE.NE.0)THEN
        IFDYN=IFIL
      ELSE
        IFDYN=FILNUM(1)
      END IF
      NORB=0
      DO 20 IPAR=1,NPAR
        IF(LOCQ(1,IPAR).EQ.3.AND.
     1     LOCQ(3,IPAR).EQ.NUMSAT.AND.
     2     LOCQ(2,IPAR).EQ.IARC)THEN
          IOOO=IOOO+1
          IORB=LOCQ(4,IPAR)
          IPIP=IPAR*(IPAR+1)/2
          IF(IORB.LE.6)THEN
            NORB=NORB+1
            INDEX(NORB)=IPAR
            IF(ELE(1,ISAT,IFIL).EQ.0.D0)THEN
              NORB=0
              WRITE(LFNERR,55)NUMSAT
55            FORMAT(//,' *** SR ELETRN: WARNING: NO ELEMENTS FOR'
     1                 ,' SATELLITE: ',I4,/,
     2               16X,'NOTHING IS STORED FOR THIS SATELLITE',/)
              GO TO 999
            END IF
            EXTELE(NORB)=IORB
            RMSI(NORB)=RMS*DSQRT(ANOR(IPIP))/SCLORB(IORB)
            ELEOLD(NORB)=ELE(IORB,ISAT,IFIL)
            DELE(NORB)=SOL(IPAR)
            NRPR=LOCQ(5,IPAR)-6
            IF(IORB.EQ.6.AND.ELEOLD(6).EQ.0.D0)THEN
              XM0=DSQRT(GM/ELEOLD(1)**3)*(-ELE(7,ISAT,IFIL))
              E0=XM0
              DO 138 I=1,10
                E0=XM0+ELEOLD(2)*DSIN(E0)
138           CONTINUE
              HELP=DSQRT((1+ELEOLD(2))/(1-ELEOLD(2)))
              V0=2*DATAN(HELP*DTAN(E0/2))
              ELEOLD(6)=V0+ELEOLD(5)
            END IF
          ELSE
            IF(LOCQ(7,IPAR).EQ.IFDYN)THEN
              NORB=NORB+1
              EXTELE(NORB)=IORB
              RMSI(NORB)=RMS*DSQRT(ANOR(IPIP))/SCLORB(IORB)
              ELEOLD(NORB)=RPRESS(IORB-6,ISAT,IFDYN)
            END IF
            IFF=LOCQ(7,IPAR)
            CALL CURARC(ISAT,IFF,ARCNUM,IA,IF,FILNM2)
            IF(IA.EQ.IARC)THEN
              KORB=LOCQ(6,IPAR)
              IF(KORB.EQ.0)KORB=LOCQ(4,IPAR)

C No sub-arcs (only one set of RPR per satellite)
              IF (opt%splitDyn.EQ.0) THEN
                DQI(KORB-6,IF)=SOL(IPAR)
                INCR=(IF-1)*NRPR+KORB-6
C
C Sub-arcs for dynamic parameters
              ELSE
                DQI(NORB-6,IF)=SOL(IPAR)
                INCR=(IF-1)*NRPR + NORB-6
              END IF

              INDEX(6+INCR)=IPAR

            END IF
          END IF
        END IF
20    CONTINUE
C
C PARAMETER TRANSFORMATION TO ELEMENT SET NUMBER IFIL
C ---------------------------------------------------
      NCLAST=6
      DO 30 I=1,6
        DELET(I)=DELE(I)
        DO 35 K=1,NCLAST
          IF (I.EQ.K) THEN
            COE(I,K)=1.D0
          ELSE
            COE(I,K)=0.D0
          ENDIF
35      ENDDO
30    CONTINUE
C
      IF(IFIL.GT.FILNUM(1))THEN
        DO 100 I=1,6
          IF(IELE.EQ.0)THEN
            DELET(I)=DRHO(I,ISAT,IFIL)
          ELSE
            DELET(I)=DRHOT(I,ISAT,IFIL)
          END IF
          DO 40 K=1,6
            NCLAST=K
            DELET(I)=DELET(I)+KMAT(I,K,ISAT,IFIL)*DELE(K)
            COE(I,NCLAST)=KMAT(I,K,ISAT,IFIL)
40        CONTINUE
          IF(IELE.EQ.0)THEN
            DO 50 K=1,NRPR
              DELET(I)=DELET(I)+LMAT(I,K,ISAT,IFIL)*DQI(K,1)
              NCLAST=6+K
              IF(NCLAST.GT.MAXCOL)THEN
                WRITE(LFNERR,45)MAXCOL
45              FORMAT(//,' ** SR ELETRN : INT. DIM MAXCOL=',I4,
     1                    ' TOO SMALL')
                CALL EXITRC(2)
              END IF
              COE(I,NCLAST)=LMAT(I,K,ISAT,IFIL)
50          CONTINUE
          ELSE
            DO 70 IFF=1,NUMSTD
              IF=FILNUM(IFF)
              DO 60 K=1,NRPR
                DELET(I)=DELET(I)+MMAT(I,K,ISAT,IF)*DQI(K,IFF)
                NCLAST=6+NRPR*(IFF-1)+K
                IF(NCLAST.GT.MAXCOL)THEN
                  WRITE(LFNERR,45)MAXCOL
                  CALL EXITRC(2)
                END IF
                COE(I,NCLAST)=MMAT(I,K,ISAT,IF)
60            CONTINUE
70          CONTINUE
          END IF
100     CONTINUE
      END IF
C
C COLLECT ALL RELEVANT STOCHASTIC PULSES
C --------------------------------------
      NSTOCH=0
      DO 130 IPAR=1,NPAR
        IF(LOCQ(1,IPAR).EQ.11.AND.
     1     LOCQ(3,IPAR).EQ.NUMSAT.AND.
     2     LOCQ(2,IPAR).EQ.IARC)THEN
          ISTOCH=LOCQ(4,IPAR)
          IRSW=LOCQ(5,IPAR)
          IF(TSTOCH(ISTOCH).LE.TOSC(IFIL)+5.D0/1440.D0)THEN
            NSTOCH=NSTOCH+1
            CALL TRAFO4(2,IFIL,IDUMM,IRSW,TSTOCH(ISTOCH),DUMMY,NUMSAT,
     1                  RDUM1,ORBFIL,IDUM,IDUM1,IDUM,IDUM1,
     2                  RDUM1,NEWOLD,ELEPAR)
            NCLAST=NCLAST+1
            IF(NCLAST.GT.MAXCOL)THEN
              WRITE(LFNERR,45)MAXCOL
              CALL EXITRC(2)
            END IF
            INDEX(NCLAST)=IPAR
            DO 140 I=1,6
              DELET(I)=DELET(I)+ELEPAR(I)*SCLORB(I)/SCLSTO(IRSW)*
     1                 SOL(IPAR)
              COE(I,NCLAST)=ELEPAR(I)*SCLORB(I)/SCLSTO(IRSW)
140         CONTINUE
          END IF
        END IF
130   CONTINUE
C
C FOR IFIL NE 1 COMPUTE NEW RMS ERRORS
C ------------------------------------
      IF(IFIL.NE.1)THEN
        CALL VCOVLK(NCLAST,6,INDEX,ANOR,COE,COV)
        DO 305 I=1,6
          II=I*(I+1)/2
          RMSI(I)=RMS*DSQRT(COV(II))/SCLORB(I)
305     CONTINUE
      END IF
C
C COMPUTE NEW OSCULATING ELEMENTS
C -------------------------------
      DO 310 I=1,6
        ELENEW(I)=ELEOLD(I)+DELET(I)/SCLORB(I)
        IF(I.GT.2.AND.I.LE.6)THEN
          ELEOLD(I)=ELEOLD(I)*180/PI
          ELENEW(I)=ELENEW(I)*180/PI
          RMSI(I)  =RMSI(I)  *180/PI
        END IF
310   CONTINUE
C
C NEW RADIATION PRESSURE PARAMETERS
C ---------------------------------
      CALL CURARC(ISAT,IFDYN,ARCNUM,IA,IF,FILNM2)
C
      IF (opt%splitDyn.EQ.0) THEN
        IPAR = NRPR
      ELSE
        IPAR = NORB-6
      ENDIF
C
      DO 350 I=1,IPAR
C
CCC Wrong???
CCC 25-Aug-2008 corrected: SCLORB contains ALL orbit parameters (not only estimated)
CCC        ELENEW(6+I)=ELEOLD(6+I)+DQI(I,IF)/SCLORB(6+I)
        ELENEW(6+I)=ELEOLD(6+I)+DQI(I,IF)/SCLORB(EXTELE(6+I))
C
350   CONTINUE
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
