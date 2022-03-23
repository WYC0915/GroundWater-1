C*
      PROGRAM ERPEST
CC
CC NAME       :  ERPEST
CC
CC PURPOSE    :  ESTIMATE AMPLITUDES OF SPECIFIC FREQUENCIES FROM ERP
CC               ESTIMATES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. ROTHACHER
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  24-FEB-98 : MR: ADD SAMPLING OPTIONS AND
CC                               EPOCH ADJUSTMENT
CC               16-AUG-99 : JJ: RM UNUSED VARS LINE, HLPSTR
CC               20-MAY-03 : PS: SWITCH TO VERSION MR_190303
CC               28-MAY-03 : PS: CHANGED CALL TO SR FMTPOL
CC                               ALLOCATE LFLAG
CC               30-MAY-03 : PS: ALLOCATE AMAT
CC                               DEALLOCATE MEMORY
CC                               USE F90-SR WTNUTM
CC               25-JUL-03 : MR: ADD RELATIVE SAGNAC FREQUENCY
CC               01-NOV-03 : HU: ADDITIONAL PARAMETERS FOR FMTPOL, RDPOL
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               09-FEB-04 : HU: NUTNAM RENAMED TO MODNAM, CALL RDPOLH CORR.
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               21-OCT-08 : HB: ADD USE S_SUBMOD
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL ADDED
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*

! Modules
! -------
      USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfnErr, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const , ONLY : pi
      USE s_opnfil
      USE s_eeresi
      USE s_nutsav
      USE s_ortho_eop
      USE s_syminvg
      USE s_rdsubm
      USE s_eeaddn
      USE s_defcon
      USE s_pritit
      USE s_opnsys
      USE s_submod
      USE s_alcerr
      USE s_subsav
      USE s_eeamat
      USE s_eeinpt
      USE s_gtfile
      USE s_rdpol
      USE s_readinpf
      USE s_opnerr
      USE s_eeinit
      USE s_rdpolh
      USE s_solve
      USE s_rdpoli
      USE s_fmtpol
      USE s_exitrc
      USE s_eeprnt
      USE s_linsav
      USE f_lengt1
      USE f_djul
      USE f_iyear4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IADEPO , ICMP   , ICOR   , IDAY1  , IEND   , IEPO   ,
     1          IFIL   , IFILIN , IFORMI , IHOUR1 , ILIN   , IMONT1 ,
     2          IOBS   , IOK    , ISYS   , ITYP   , IUSSIG , IYEAR  ,
     3          IYEAR1 , K      , MIN1   , NEND   ,
     4          NEPO   , NFIL   , NFILIN , NFLCOL , NOBS   , NPAR   ,
     5          NSING  , NSUB   , NWIN
C
      REAL*8    DAY    , DAY1   , DAYYEAR, DTERP  , DTSAMP ,
     1          DUMMY1 , DUMMY2 , DUMMY3 , DUMMY4 , DUMMY5 , DUMMY6 ,
     2          ERPEPO , ERPMOD , ERPOB1 , ERPOMC , ERPSI1 , GPSUTC ,
     3          HEIGHT , OMC    , POLTIM , RESIDU , RMS    , SIGMA0 ,
     4          SUMPER , WEIGHT , XLAT   , XLONG  , XMJD0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)


C
C MAXIMUM DIMENSIONS
C ------------------
      INTEGER(i4b)                ::MAXFIL,MAXREC,MAXWIN
      INTEGER(i4b)                ::MAXTYP,MAXPER,MAXFIN
      PARAMETER (MAXFIL=1,MAXREC=3,MAXWIN=1,MAXTYP=11,
     1           MAXPER= 200,MAXFIN=10)
C
C MAXFIL : MAXIMUM NUMBER OF FILES
C MAXREC : MAXIMUM NUMBER OF RECORDS TO BE READ FROM ONE FILE
C MAXWIN : MAXIMUM NUMBER OF WINDOWS
C MAXTYP : MAXIMUM NUMBER OF MEASUREMENT TYPES
C MAXPER : MAXIMUM NUMBER OF PERIODS IN SUBDAILY ERP MODEL
C MAXFIN : MAXIMUM NUMBER OF FILE TYPES ENTERED

C DECLARATIONS
C ------------
      CHARACTER*156 STRING
      CHARACTER*80  TITNEW,TITLE(MAXFIL),TITLEH,SUBTIT
      CHARACTER(LEN=fileNameLength) :: FILNAM(2,MAXFIL),FILERP(MAXFIN)
      CHARACTER(LEN=20),DIMENSION(:),POINTER ::  parNam
      CHARACTER*16  MODNAM,SUBNAM
      CHARACTER*3   REM
      CHARACTER*2   OBSTYP(MAXTYP)
      CHARACTER*1   OBSSYS
C
      REAL*8        ERPTIM(MAXREC),ERPVAL(5,MAXREC),ERPRMS(5,MAXREC)
      REAL*8        ERPOBS(MAXTYP),ERPSIG(MAXTYP),REFEPO(2)
      REAL*8        POLCOO(5),POLRAT(5),RMSPOL(5),RMSRAT(5)
      REAL*8        WINTIM(2,MAXWIN),WINSIG(MAXWIN),RMSOBS(MAXTYP)
      REAL*8        WGTTYP(2,MAXTYP)
      REAL*8        SUBFAR(6,6),SUBPER(MAXPER),SUBCOE(4,MAXPER)
      REAL*8        ERPSUB(3),ERPSUR(3)
      REAL*8        XSTELL(3)
      REAL*8        ERPEP4,ERPSUB4(3)
      REAL(r8b),DIMENSION(:,:),POINTER :: parTim
      REAL(r8b),DIMENSION(:),POINTER   :: parSig
      REAL(r8b),DIMENSION(:),ALLOCATABLE :: ANor
      REAL(r8b),DIMENSION(:),ALLOCATABLE :: BNor
      REAL(r8b),DIMENSION(:),ALLOCATABLE :: AHelp
      REAL(r8b),DIMENSION(:),ALLOCATABLE :: XXXAPR
      REAL(r8b),DIMENSION(:),ALLOCATABLE :: XXX
      REAL(r8b),DIMENSION(:),ALLOCATABLE :: AMat
C
      INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: parFlg

      INTEGER*4     POLTYP(2),POLTYH(2),NUMOBS(MAXTYP)
      INTEGER*4     IRECNR(MAXREC),IERPFL(5,MAXREC),IERFLG(MAXTYP)
      INTEGER*4     IUSTYP(2,MAXTYP),ISAMPL(3)
      INTEGER*4     SUBMLT(6,MAXPER),IDATA(MAXFIN)

      INTEGER(i4b)                          :: iac,iostat,iformh
      INTEGER(i4b)                          :: iread,ipar,iwin,irec
      INTEGER(i4b)                          :: ivers
      INTEGER(i4b),DIMENSION(11)            :: icol

C
C DATA
C ----
      DATA OBSTYP/'X_','Y_','U_','E_','P_',
     1            'DX','DY','DU','DE','DP','SF'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(parTim)
      NULLIFY(parNam)
      NULLIFY(parSig)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
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
C PRINT TITLE
C -----------
      CALL PRITIT('ERPEST',
     1  'Estimate Ampl. of specific Frequ. from ERP Estimates')
C
C READ OPTION INPUT FILE
C ----------------------
      CALL EEINPT(MAXTYP,TITNEW,MODNAM,NPAR,PARNAM,
     1            PARTIM,PARSIG,SIGMA0,WGTTYP,IUSTYP,IUSSIG,SUMPER,
     2            NWIN,WINTIM,WINSIG,ISAMPL,IADEPO,nFilIn,iData,filErp)

! Allocate Memory
! --------------
      ALLOCATE(parFlg(npar),stat=iac)
      CALL alcerr(iac,'parFlg',(/nPar/),'erpest')
      ALLOCATE(BNor(nPar),stat=iac)
      CALL alcerr(iac,'BNor',(/nPar/),'erpest')
      ALLOCATE(AHelp(nPar),stat=iac)
      CALL alcerr(iac,'AHelp',(/nPar/),'erpest')
      ALLOCATE(XXXAPR(nPar),stat=iac)
      CALL alcerr(iac,'XXXAPR',(/nPar/),'erpest')
      ALLOCATE(XXX(nPar),stat=iac)
      CALL alcerr(iac,'XXX',(/nPar/),'erpest')
      ALLOCATE(ANor(nPar*(nPar+1)/2),stat=iac)
      CALL alcerr(iac,'ANor',(/nPar*(nPar+1)/2/),'erpest')
      ALLOCATE(AMat(nPar),stat=iac)
      CALL alcerr(iac,'AMat',(/nPar/),'erpest')
C
C INITIALIZE NORMAL EQUATION SYSTEM AND ADD A PRIORI CONSTRAINTS
C --------------------------------------------------------------
      CALL EEINIT(NPAR,PARNAM,PARTIM,PARSIG,NOBS,
     1            RMS,ANOR,BNOR,XXXAPR)

C
C LOOP OVER ALL DIFFERENT FILE TYPES AVAILABLE
C --------------------------------------------
      DO IFILIN=1,NFILIN
        IF (IDATA(IFILIN).EQ.0) IDATA(IFILIN)=10
C
        IF (IDATA(IFILIN).LE.3) THEN
C
C ALL DATA IN ONE FILE (POLE EXTRACTION)
          CALL OPNFIL(LFN001,FILERP(IFILIN),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFN001,IOSTAT,FILERP(IFILIN),'ERPEST')
          IF (IDATA(IFILIN).EQ.1) READ(LFN001,'(/)')
          IF (IDATA(IFILIN).EQ.2) READ(LFN001,'(///)')
          IF (IDATA(IFILIN).EQ.3) READ(LFN001,'(///)')
C
        ELSEIF (IDATA(IFILIN).EQ.4) THEN
C
C GET POLE FILE FORMAT
          CALL OPNFIL(LFN001,FILERP(IFILIN),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFN001,IOSTAT,FILERP(IFILIN),'ERPEST')
C          IFORMH=FMTPOL(LFN001,0)
          CALL FMTPOL(lfn001,0,iformh,ivers,icol,modnam,subnam)
          IF (IFORMH.EQ.0) GOTO 920
C
        ELSEIF (IDATA(IFILIN).EQ.5) THEN
C
C GET SUBDAILY ERP MODEL
          CALL RDSUBM(MAXPER,FILERP(IFILIN),SUBTIT,SUBNAM,SUBFAR,NSUB,
     1                SUBPER,SUBMLT,SUBCOE)
C
C CHECK WINDOW AND SAMPLING RATE
          IF (ISAMPL(3).LE.0) GOTO 930
          IF (NWIN.NE.1) GOTO 940
C
          DTSAMP=ISAMPL(3)/1440.D0
          NEPO=IDNINT((WINTIM(2,1)-WINTIM(1,1))/DTSAMP)+1
C
        ELSEIF (IDATA(IFILIN).EQ.6) THEN
C
C DATA FROM FILE IN VLBI FORMAT
          CALL OPNFIL(LFN001,FILERP(IFILIN),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFN001,IOSTAT,FILERP(IFILIN),'ERPEST')
C
        ELSEIF (IDATA(IFILIN).EQ.7. OR. IDATA(IFILIN).EQ.8) THEN
C
C DATA FROM FILE WITH RINGLASER DATA (G1=7 OR UG=8)
          CALL OPNFIL(LFN001,FILERP(IFILIN),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFN001,IOSTAT,FILERP(IFILIN),'ERPEST')
C
C FIRST LINE CONTAINS ELLIPSOIDAL COORDINATES OF RINGLASER
          READ(LFN001,*) XLAT,XLONG,HEIGHT
          XSTELL(1)=XLAT/180.D0*PI
          XSTELL(2)=XLONG/180.D0*PI
          XSTELL(3)=HEIGHT
C
        ELSE
C
C READ INPUT FILE NAMES FROM F-FILE
          NFLCOL=2
          CALL GTFILE('ERPFIL ',NFLCOL,MAXFIL,NFIL,FILNAM)
        ENDIF
C
C LOOP OVER OF OBSERVATION EPOCHS (OR ERP FILES IF IRCERP.NE.0)
C -------------------------------------------------------------
        IFIL=0
C
        DO 1000 IEPO=1,20000000
          OBSSYS=' '
          ISYS=1
C
C READ DATA FROM POLE EXTRACTION FILE
C -----------------------------------
          IF (IDATA(IFILIN).EQ.1) THEN
C
C READ NEXT EPOCH FROM DATA FILE
            READ(LFN001,101,END=1010) IYEAR1,IMONT1,IDAY1,IHOUR1,MIN1,
     1                      DAY1,
     2                      ERPOBS(10),ERPSIG(10),ERPOBS(9),ERPSIG(9),
     3                      ERPOBS(8),ERPSIG(8)
101         FORMAT(I5,4I3,F10.3,6F15.8)
C
C CONVERT DATA
            DAY=IDAY1+IHOUR1/24.D0+MIN1/1440.D0
            ERPEPO=DJUL(IYEAR1,IMONT1,DAY)
C
            DO ICMP=1,MAXTYP
              IF (ICMP.GE.8 .AND. ICMP.LE.10) THEN
                IERFLG(ICMP)=1
              ELSE
                IERFLG(ICMP)=0
              ENDIF
            ENDDO
C
C READ DATA FROM RESIDUAL FILE OF PROGRAM ERPEST
C ----------------------------------------------
          ELSEIF (IDATA(IFILIN).EQ.2) THEN
C
C READ NEXT EPOCH FROM DATA FILE
            READ(LFN001,102,END=1010) IOBS,ITYP,ERPEPO,ERPOB1,ERPMOD,
     1                                ERPOMC,RESIDU,ERPSI1
102         FORMAT(I6,I4,F13.5,5F13.7)
C
C CONVERT DATA
            DO ICMP=1,MAXTYP
              IERFLG(ICMP)=0
            ENDDO
            IERFLG(ITYP)=1
            ERPOBS(ITYP)=ERPMOD/1000.D0
            ERPSIG(ITYP)=1.D0
C
C READ DATA FROM RESIDUAL FILE BUT SET ALL OBSERVATIONS TO ZERO
C -------------------------------------------------------------
          ELSEIF (IDATA(IFILIN).EQ.3) THEN
C
C READ NEXT EPOCH FROM DATA FILE
            READ(LFN001,102,END=1010) IOBS,ITYP,ERPEPO,ERPOB1,ERPMOD,
     1                                ERPOMC,RESIDU,ERPSI1
C
C CONVERT DATA
            DO ICMP=1,MAXTYP
              IERFLG(ICMP)=0
            ENDDO
            IERFLG(ITYP)=1
            ERPOBS(ITYP)=0.D0
            ERPSIG(ITYP)=1.D0
C
C READ DATA FROM POLXTR FILE
C --------------------------
          ELSEIF (IDATA(IFILIN).EQ.4) THEN
C
C READ POLE RECORD
            CALL RDPOL(LFN001,1     ,IFORMH,TITLEH,POLTYH,POLTIM,POLCOO,
     1                 POLRAT,GPSUTC,REM   ,RMSPOL,RMSRAT,IEND,
     2                 MODNAM,SUBNAM)
            IF (IEND.NE.0) GOTO 1010
C
C CONVERT DATA
            DO ICMP=1,MAXTYP
              IERFLG(ICMP)=0
            ENDDO
C
            ERPEPO=POLTIM
            DO ICMP=1,5
              IERFLG(ICMP)=1
              ERPOBS(ICMP)=POLCOO(ICMP)
              ERPSIG(ICMP)=RMSPOL(ICMP)
            ENDDO
            DO ICMP=1,5
              IERFLG(ICMP+5)=1
              ERPOBS(ICMP+5)=POLRAT(ICMP)
              ERPSIG(ICMP+5)=RMSRAT(ICMP)
            ENDDO
C
            OBSSYS='G'
C
C SIMULATE SUBDAILY ERP MODEL OBSERVATIONS
C ----------------------------------------
          ELSEIF (IDATA(IFILIN).EQ.5) THEN
C
            IF (IEPO.GT.NEPO) GOTO 1010
C
            ERPEPO=WINTIM(1,1)+(IEPO-1)*DTSAMP
C
C Use subroutine of IERS Conventions 2000, if SUBNAM='IERS2000'
            IF (SUBNAM(1:8).EQ.'IERS2000') THEN
              ERPEP4=ERPEPO
              CALL ORTHO_EOP(ERPEP4,ERPSUB4)
              ERPSUB=ERPSUB4*1.D-6
              ERPSUR=0.D0
            ELSE
              CALL SUBMOD(ERPEPO,SUBFAR,NSUB,SUBMLT,SUBCOE,
     1                    ERPSUB,ERPSUR)
            ENDIF
C
            DO ICMP=1,MAXTYP
              IERFLG(ICMP)=0
            ENDDO
C
            DO ICMP=1,3
              IERFLG(ICMP)=1
              ERPOBS(ICMP)=ERPSUB(ICMP)
              ERPSIG(ICMP)=1.D0
            ENDDO
            DO ICMP=1,3
              IERFLG(ICMP+5)=1
              ERPOBS(ICMP+5)=ERPSUR(ICMP)
              ERPSIG(ICMP+5)=1.D0
            ENDDO
C
C READ DATA FROM FILE IN VLBI FORMAT
C ----------------------------------
          ELSEIF (IDATA(IFILIN).EQ.6) THEN
C
C READ NEXT EPOCH FROM DATA FILE
            READ(LFN001,'(A)',END=1010) STRING
            IF (STRING(1:1).EQ.'#') GOTO 1000
C
C CORRECTIONS RELATIVE TO A SMOOTH POLE
            READ(STRING,601,ERR=950) IYEAR1,IMONT1,IDAY1,IHOUR1,MIN1,
     1                      ERPOBS(1),ERPSIG(1),DUMMY1,DUMMY2,
     2                      ERPOBS(2),ERPSIG(2),DUMMY3,DUMMY4,
     3                      ERPOBS(3),ERPSIG(3),DUMMY5,DUMMY6
C
C CORRECT FOR A PRIORI HIGH FREQUENCY MODEL
            ERPOBS(1)=ERPOBS(1)+DUMMY2-DUMMY1
            ERPOBS(2)=ERPOBS(2)+DUMMY4-DUMMY3
            ERPOBS(3)=ERPOBS(3)+DUMMY6-DUMMY5
C
C FULL ERP VALUES
C           READ(STRING,601,ERR=950) IYEAR1,IMONT1,IDAY1,IHOUR1,MIN1,
C     1                      DUMMY1,ERPSIG(1),DUMMY2,ERPOBS(1),
C     2                      DUMMY3,ERPSIG(2),DUMMY4,ERPOBS(2),
C     3                      DUMMY5,ERPSIG(3),DUMMY6,ERPOBS(3)
601         FORMAT(5I3,12F11.3)
C
C DATA EDITING TO AVOID BLUNDER !!!
            IF (ERPSIG(1).GT.5.D0) GOTO 1000
C
C CONVERT DATA
            IYEAR=IYEAR4(IYEAR1)
            DAY=IDAY1+IHOUR1/24.D0+MIN1/1440.D0
            ERPEPO=DJUL(IYEAR,IMONT1,DAY)
C
            DO ICMP=1,3
              IERFLG(ICMP)=1
              ERPOBS(ICMP)=ERPOBS(ICMP)/1000.D0
              ERPSIG(ICMP)=ERPSIG(ICMP)/1000.D0
            ENDDO
C
            DO ICMP=4,MAXTYP
              IERFLG(ICMP)=0
            ENDDO
C
            OBSSYS='V'
            IF (NFILIN.EQ.1) THEN
              ISYS=1
            ELSE
              ISYS=2
            ENDIF
C
C READ DATA FROM RINGLASER "G1"- OR "UG"- FILE
C (RELATIVE SAGNAC FREQUENCY: DOMEGA/OMEGA)
C ---------------------------------------------------------------
          ELSEIF (IDATA(IFILIN).EQ.7 .OR. IDATA(IFILIN).EQ.8) THEN
C
C READ NEXT EPOCH FROM DATA FILE
            READ(LFN001,*,END=1010) IYEAR,DAYYEAR,ERPOB1
C
C CONVERT DATA
            XMJD0=DJUL(IYEAR,1,1.D0)-1.D0
            ERPEPO=XMJD0+DAYYEAR
            DO ICMP=1,MAXTYP
              IERFLG(ICMP)=0
            ENDDO
            ICMP=11
            IERFLG(ICMP)=1
            ERPOBS(ICMP)=ERPOB1*1.D3
            ERPSIG(ICMP)=1.D0
            IF (IDATA(IFILIN).EQ.7) THEN
              OBSSYS='R'
            ELSE
              OBSSYS='U'
            ENDIF
C
C READ DATA DIRECTLY FROM ERP OUTPUT FILES
C ----------------------------------------
          ELSE
            IFIL=IFIL+1
            IF (IFIL.GT.NFIL) GOTO 1010
C
C READ LINE NUMBERS TO BE READ FROM FILNAM(2,IFIL)
            READ(FILNAM(2,IFIL),11) (IRECNR(K),K=1,MAXREC)
11          FORMAT(100I3)
C
C OPEN POLE RESULT FILE
            CALL OPNFIL(LFN001,FILNAM(1,IFIL),'OLD','FORMATTED',
     1                  'READONLY',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(1,IFIL),'ERPEST')
C
C READ POLE HEADER
            CALL RDPOLH(LFN001,1,TITLE(IFIL),POLTYP,IFORMH,IEND,
     1                  MODNAM,SUBNAM)
            IF (IEND.EQ.1) GOTO 910
C
C READ THE THREE POLE COORDINATES GIVEN IN EACH RESULT FILE
            IREAD=0
            DO ILIN=1,100000
C
C READ POLE RECORD
              CALL RDPOLI(LFN001,POLTIM,POLCOO,GPSUTC,REM,RMSPOL,IFORMI,
     1                    IEND)
              IF (IEND.NE.0) GOTO 910
C
C PUT ERP VALUES INTO CORRECT ARRAY ELEMENTS
              DO IREC=1,MAXREC
                IF (IRECNR(IREC).EQ.ILIN) THEN
                  ERPTIM(IREC)=POLTIM
                  DO ICOR=1,5
                    IERPFL(ICOR,IREC)=1
                    ERPVAL(ICOR,IREC)=POLCOO(ICOR)
                    ERPRMS(ICOR,IREC)=RMSPOL(ICOR)
                  ENDDO
                  IF (IFORMI.NE.4) THEN
                    IERPFL(4,IREC)=0
                    IERPFL(5,IREC)=0
                  ENDIF
                  IREAD=IREAD+1
                ENDIF
                IF (IREAD.EQ.MAXREC) GOTO 10
              ENDDO
            ENDDO
C
            GOTO 910
C
C CLOSE FILE
10          CLOSE(UNIT=LFN001)
C
C FILL IN OBSERVATIONS ACCORDING TO OBSERVATION TYPE (ARCSEC OR SEC)
            ERPEPO=ERPTIM(2)
            DO ICMP=1,5
              IERFLG(ICMP)=IERPFL(ICMP,2)
              ERPOBS(ICMP)=ERPVAL(ICMP,2)
              ERPSIG(ICMP)=ERPRMS(ICMP,2)
            ENDDO
C
C FILL IN OBSERVATION RATES (ARCSEC/DAY OR SEC/DAY)
            DTERP = ERPTIM(3)-ERPTIM(1)
            ERPEPO=(ERPTIM(1)+ERPTIM(3))/2.D0
            DO ICMP=1,5
              IERFLG(ICMP+5)=IERPFL(ICMP,1)*IERPFL(ICMP,3)
              ERPOBS(ICMP+5)=(ERPVAL(ICMP,3)-ERPVAL(ICMP,1))/DTERP
              IF (ICMP.LE.2) THEN
                ERPSIG(ICMP+5)=
     1                   DSQRT(ERPRMS(ICMP,1)**2+ERPRMS(ICMP,3)**2)
              ELSE
                ERPSIG(ICMP+5)=(ERPRMS(ICMP,3)-ERPRMS(ICMP,1))/DTERP
              ENDIF
            ENDDO
C
C END READING ONE EPOCH OF OBSERVATIONS
          ENDIF
C
C ONLY USE ACTIVATED OBSERVATION TYPES
C ------------------------------------
          DO ICMP=1,MAXTYP
            IF (IUSTYP(ISYS,ICMP).EQ.0) IERFLG(ICMP)=0
          ENDDO
C
C ADJUST EPOCH TO NEXT FULL "IADEPO" MINUTES
C ------------------------------------------
          IF (IADEPO.NE.0) THEN
            ERPEPO=DNINT(ERPEPO*1440.D0/IADEPO)*IADEPO/1440.D0
          ENDIF
C
C SAMPLING OPTION (INDEPENDENT OF WINDOW)
C ---------------------------------------
          IF (MOD(IEPO-ISAMPL(2),ISAMPL(1)).NE.0) GOTO 1000
C
C CHECK BAD DATA WINDOWS
C ----------------------
          IOK=0
          NEND=0
          DO IWIN=1,NWIN
            IF (ERPEPO.GE.WINTIM(1,IWIN) .AND.
     1          ERPEPO.LE.WINTIM(2,IWIN))  IOK=1
            IF (ERPEPO.GT.WINTIM(2,IWIN)) NEND=NEND+1
          ENDDO
          IF (NEND.EQ.NWIN .AND. NWIN.GT.0) GOTO 1010
          IF (NWIN.NE.0 .AND. IOK.EQ.0) GOTO 1000
C
C SET NORMAL EQUATION MATRIX
C --------------------------
          DO ICMP=1,MAXTYP
C
C CHECK IF OBSERVATION PRESENT (ACTIVATED)
            IF (IERFLG(ICMP).NE.0) THEN
C
C FIRST DESIGN MATRIX AND OBSERVED-COMPUTED
C -----------------------------------------
              CALL EEAMAT(ICMP,OBSSYS,OBSTYP(ICMP),ERPEPO,
     1                    ERPOBS(ICMP),
     2                    ERPSIG(ICMP),SIGMA0,WGTTYP(ISYS,ICMP),IUSSIG,
     3                    NWIN,WINTIM,WINSIG,XSTELL,
     4                    NPAR,PARNAM,PARTIM,AMAT,OMC,WEIGHT,REFEPO)

C
C UPDATE NORMAL EQUATION SYSTEM
C -----------------------------
              CALL EEADDN(NPAR,AMAT,OMC,WEIGHT,NOBS,RMS,ANOR,BNOR,AHELP)
C
            ENDIF
          ENDDO
C
C END OF LOOP OVER IEP FILES
C --------------------------
1000    CONTINUE
1010    CONTINUE
C
C END LOOP OVER DIFFERENT FILE TYPES
        CLOSE(UNIT=LFN001)
      ENDDO

! Check if there are more observations than parameters
      IF (nPar.gt.nObs) THEN
         WRITE(LFNERR,'(/,A,/,16X,A,I8,/,16X,A,I8,/)')
     1    ' *** PG ERPEST: Too many Parameters','Nr of Parameters  :',
     2     nPar, 'Nr of Observations:', nObs
         CALL EXITRC(2)
       ENDIF

C
C INVERT NORMAL EQUATION MATRIX
C -----------------------------
      CALL SYMINVG(NPAR,ANOR,0,NSING,PARFLG)
C
C SOLVE NORMAL EQUATIONS
C ----------------------
      CALL SOLVE(NPAR,ANOR,BNOR,XXX)
C
C COMPUTE RMS
C -----------
      DO IPAR=1,NPAR
        RMS=RMS-BNOR(IPAR)*XXX(IPAR)
      ENDDO
      IF (RMS.GT.0 .AND. NOBS-NPAR.GT.0) THEN
        RMS=DSQRT(RMS/(NOBS-NPAR))
      ELSE
        RMS=999.D0
      ENDIF
C
C COMPUTE RESIDUALS
C -----------------
      CALL EERESI(MAXTYP,TITNEW,NOBS,NPAR,XXX,ANOR,
     1            NUMOBS,RMSOBS,RMS,AMAT)
C
C PRINT A PRIORI INFO AND RESULTS
C -------------------------------
      CALL EEPRNT(MAXTYP,TITNEW,MODNAM,SIGMA0,WGTTYP,IUSSIG,SUMPER,
     1            NWIN,WINTIM,WINSIG,ISAMPL,IADEPO,REFEPO,NOBS,NUMOBS,
     2            NPAR,PARNAM,PARTIM,RMS,RMSOBS,XXXAPR,XXX,ANOR)
C
C SAVE LINEAR PARAMETERS (RESIDUAL FILE FORMAT)
C ---------------------------------------------
      CALL LINSAV(TITNEW,MODNAM,MAXTYP,OBSTYP,NPAR,PARNAM,
     1            PARTIM,RMS,XXXAPR,XXX,ANOR)
C
C SAVE NUTATION MODEL
C -------------------
      CALL NUTSAV(TITNEW,MODNAM,REFEPO(1),NPAR,PARNAM,PARTIM,RMS,
     1            XXXAPR,XXX,ANOR)
C
C SAVE SUBDAILY ERP MODEL
C -----------------------
      CALL SUBSAV(TITNEW,MODNAM,REFEPO(1),NPAR,PARNAM,PARTIM,RMS,
     1            XXXAPR,XXX,ANOR)
C
      GOTO 999
C
C ERROR READING POLE FILE
C -----------------------
910   WRITE(LFNERR,911) MAXREC,FILNAM(1,IFIL)
911   FORMAT(/,' *** PG ERPEST: END OF FILE REACHED BEFORE HAVING',/,
     1                     16X,'READ',I4,' POLE EPOCHS',/,
     2                     16X,'POLE FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C ERROR READING POLE FILE
C -----------------------
920   WRITE(LFNERR,921) FILERP(IFILIN)
921   FORMAT(/,' *** PG ERPEST: UNKNOWN ERP FILE FORMAT',/,
     1                     16X,'POLE FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C ERROR IN SAMPLING INTERVAL FOR SIMULATION
C -----------------------------------------
930   WRITE(LFNERR,931) ISAMPL(3)
931   FORMAT(/,' *** PG ERPEST: INVALID SAMPLING INTERVAL FOR',
     1         ' SIMULATION OF OBSERVATIONS',/,
     2                     16X,'SAMPLING INTERVAL (MIN):',I10,/)
      CALL EXITRC(2)
C
C ERROR: NO WINDOW GIVEN FOR SIMULATION
C -------------------------------------
940   WRITE(LFNERR,941) NWIN
941   FORMAT(/,' *** PG ERPEST: INVALID NUMBER OF TIME WINDOWS',
     1         ' FOR SIMULATION OF OBSERVATIONS',/,
     2                     16X,'NUMBER OF WINDOWS:',I3,/,
     3                     16X,'EXACTLY ONE WINDOW HAS TO BE ',
     4         'SPECIFIED',/)
      CALL EXITRC(2)
C
C ERROR: BAD DATA RECORD IN VLBI FILE
C -----------------------------------
950   WRITE(LFNERR,951) IEPO,STRING(1:LENGT1(STRING))
951   FORMAT(/,' *** PG ERPEST: BAD DATA RECORD IN VLBI FILE',
     1                     16X,'RECORD NUMBER:',I8,/,
     2                     16X,'RECORD       : ',A,/)
      CALL EXITRC(2)

! Deallocate Memory
! -----------------
      DEALLOCATE (parFlg,stat=iac)
      DEALLOCATE (BNor,stat=iac)
      DEALLOCATE (AHelp,stat=iac)
      DEALLOCATE (XXXAPR,stat=iac)
      DEALLOCATE (XXX,stat=iac)
      DEALLOCATE (ANor,stat=iac)
      DEALLOCATE (AMat,stat=iac)

999   CONTINUE
      CALL EXITRC(0)
      END
