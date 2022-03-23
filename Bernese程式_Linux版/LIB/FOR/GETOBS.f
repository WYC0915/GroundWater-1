      MODULE s_getobs
      CONTAINS
C*
      SUBROUTINE GETOBS(ISVN,ITYPFL,RMSRAT,RMSMIN,NSAT,SATNUM,NEPSAT,
     1                  OBSEPO,PSDOBS,CLOCKC,RMSOBS,DTMOBS,OBSTYP,
     2                  SENSOR,DELTAT)
CC
CC NAME       :  GETOBS
CC
CC PURPOSE    : GET PSEUDO-OBSERVATIONS FOR ONE SATELLITE OR
CC              ALL SATELLITES IN A SEQUENCE OF *.SP3-,*.TAB
CC              OR *.EPH-FILES. ON RETURN ALL POSITIONS REFER
CC              TO THE J2000 INERTIAL SYSTEM
CC
CC PARAMETERS :
CC        IN  :  ISVN   : SATELLITE NUMBER REQUESTED (-99=ALL)      I*4
CC               ITYPFL : TYPE OF INPUT FILE                        I*4
CC                        =1: SP3-FILE
CC                        =2: TAB-FILE
CC                        =4: PPD-FILE
CC               RMSRAT : RATIO OF RMS VALUES POS:POS.DIFF          R*8
CC               RMSMIN : MINIMUM RMS VALUES FOR POS, POS.DIFF      R*8
CC        OUT :  NSAT   : ACTUAL NUMBER OF SATELLITES               I*4
CC               SATNUM : SATELLITE NUMBERS                         I*4
CC               NEPSAT : NUMBER OF EPOCHS FOR EACH SATELLITE       I*4
CC               OBSEPO : OBSERVATION EPOCHS                        R*8
CC               PSDOBS : PSEUDO-OBSERVATIONS                       R*8
CC               RMSOBS : RMS OF POSITIONS / POS.DIFFERENCES        R*8
CC               DTMOBS : TIME INTERVAL FOR POS.DIFFERENCES         R*8
CC               OBSTYP : OBSERVATION TYPE (1=POS/2=POSDIF)         I*4
CC               SENSOR : SENSOR NAME (FOR *.PPD-FILES)             CH*16
CC               DELTAT : SPACING BETWEEN OBSERVATIONS (SEC)        R*8
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  2000/06/01 (GEORGETOWN)
CC
CC CHANGES    :  14-APR-2005 HU: USE INTERFACE TO RDPREH, RDPREI
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-2005 HB: ADD BIAS TO PARAMETER LIST OF
CC                               SR NUTEFF, USE GSTIME
CC               01-AUG-2005 HU: MJD FROM RDPREI AS STRUCTURE
CC               30-JAN-2007 CU: USE GSTIME FOR COMPUTATION OF SIDERIAL TIME SZ
CC               19-JUL-2010 SL: TAB CHARACTERS REMOVED
CC               24-FEB-2011 RD: NEW CALL OF SETXYZ
CC               05-MAR-2012 RD: USE GETOBS AS MODULE
CC               05-MAR-2012 RD: NULLIFY/DEALLOCTE KEYVALUE
CC               05-MAR-2012 RD: REMOVE UNUSED VARIABLES
CC               08-NOV-2012 RD: NO CLOCKS WITHOUT POSITION ARE TAKEN
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2000     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C Modules for readKeys
C --------------------
      USE m_bern,   ONLY: i4b, lfnerr, lfn001,
     1                    fileNameLength, keyValueLength
      USE m_maxdim, ONLY: MAXSAT
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.),
     1            OPERATOR(.realToEpoch.)

      USE s_nuteff
      USE s_dmlmtv
      USE s_setxyz
      USE s_opnfil
      USE s_readkeys
      USE s_exitrc
      USE s_gtfile
      USE f_gstime
      USE s_poldf1
      USE s_prceff
      USE s_gtflna
      USE s_opnerr
      USE s_dmlmav
      USE s_ddreh
      USE s_rdpreh
      USE s_rdprei
      USE f_gstime
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPO    , IERPSPEC, IFIL    , IFILT   , IFRMAT  ,
     1          IOBST   , IORSYS  , IOSTAT  , IRCTOT  , IREADE  ,
     2          IREC    , ISAT    , ISVN    , ISVNF   , ITYPFL  ,
     3          K       , KSAT    , MAXFIL  , MXCEPO  , MXCFIL  ,
     4          MXCSAT  , NEPO    , NFIL    , NFLCOL  , NSAT    ,
     5          NSATF   , NSATFL
C
      REAL*8    DELTAT  , DT1     , DT2     , DTABD   , DTTAB   ,
     1          GPSUTC  , RMSSAV  , SQEQUI  , SZ      , T0ARC   ,
     2          TB1     , TB2     , TDT     , TEPOCH  , TEST    ,
     3          TFIRST  , TIMREF  , TOBS    , TUT1    ,
     4          UT1UTC  , XPOLE   , YPOLE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER(MAXFIL=100)
      CHARACTER*1  SOURCE(10)
      CHARACTER*6  MXNSAT,MXNEPO,MXNFIL
      CHARACTER(LEN=fileNameLength),DIMENSION(2,MAXFIL) :: FILNAM
      CHARACTER*57 TITLE(4)
      CHARACTER*32 FILPDF(1)
      CHARACTER*5  COOSYS,DATDES
      CHARACTER*4  AGENCY
      CHARACTER*3  ORBTYP
      CHARACTER*16 SENSOR

      CHARACTER*3  TIMSYS
      CHARACTER*2  FILTYP
C
      INTEGER*4 SATNMF(MAXSAT),SATWGT(MAXSAT),INDSAT(MAXSAT)
      INTEGER*4 NEPSAT(*),SATNUM(*),OBSTYP(*)
      REAL*8    OBSEPO(MXCEPO,MXCSAT),PSDOBS(3,MXCEPO,MXCSAT)
      REAL*8    CLOCKC(MXCEPO,MXCSAT)
      REAL*8    POS(3,MAXSAT),VEL(3,MAXSAT),DTSATC(MAXSAT),
     1          DDTSAT(MAXSAT),RMSOBS(*),DTMOBS(*)
      REAL*8    POLE(3,3),PRE(3,3),NUT(3,3),THET(3,3),XTF(3),
     1          BIAS(3,3)
      REAL*8    RMSRAT(*),RMSMIN(*)
      REAL*8    BASPOS, BASCLK
      INTEGER*4    ACCPOS(4,maxsat),ACCVEL(4,maxsat)
      CHARACTER*1  EVTFLG(4,maxsat)
      INTEGER*4    IEREC(2)
      REAL*8       SDEVP(4,maxsat),SDEVV(4,maxsat)
      REAL*8       CORRP(6,maxsat),CORRV(6,maxsat),NSAMPL(2)

      TYPE(t_epoch) :: tmjd,ttut1,ttdt
C
C Declaration for readKeys
C ------------------------
      CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
      INTEGER(i4b) :: irc
C
C COMMON BLOCKS
C -------------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPO/MXCEPO,MXNEPO
C
      COMMON/TORIGO/T0ARC
      COMMON/ORBSYS/IORSYS
C
C WRITE MAXIMUM NUMBER OF FILES INTO COMMON
C -----------------------------------------
      MXCFIL= MAXFIL
      MXNFIL='MAXFIL'
C
C DEFINE "LOCAL" TIME ORIGIN, INERTIAL REFERENCE FRAME
      T0ARC=0.D0
      IORSYS=2
C
      NULLIFY(keyValue)
C
C Read File Names
C ----------------
      NFLCOL=2
      IF(itypfl.EQ.2)THEN
C
        CALL readKeys('SATTYP', keyValue, irc)
        IF (keyValue(1).EQ.'LEO')THEN
          CALL GTFILE('TABFILL',NFLCOL,MAXFIL,NFIL,FILNAM(:,1))
        ELSE
          CALL GTFILE('TABFIL ',NFLCOL,MAXFIL,NFIL,FILNAM(:,1))
        END IF
        DEALLOCATE(keyValue)
C
      ELSE IF(itypfl.EQ.1)THEN
        CALL GTFILE('PREFIL ',NFLCOL,MAXFIL,NFIL,FILNAM(:,1))
      ELSE
        CALL GTFILE('PPDFIL ',1,MAXFIL,NFIL,FILPDF)
        CALL GTFLNA(1,'PPDFIL  ',FILNAM(1,1),IRC)
        CALL OPNFIL(LFN001,FILNAM(1,1),'UNKNOWN','FORMATTED',' ',' ',
     &              IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(1,1),'GETOBS')
      END IF
C
C This should be improved:
C ------------------------
C      CALL readKeys('ERPSPEC', keyValue, irc)
C      READ (keyValue(1),*) iErpSpec
      IERPSPEC=0
      IF (iErpSpec /= 1) THEN
        CALL readKeys('POLE', keyValue, irc)
        FILNAM(2,1:nfil) = keyValue(1)
        DEALLOCATE(keyValue)
      END IF
C
C INITIALIZE NUMBER OF SATELLITES:
C -------------------------------
      NSAT=0
C
C HANDLE SP3-FILES
C ----------------
      IF(ITYPFL.EQ.1)THEN
        DO 100 IFIL=1,NFIL
          CALL RDPREH(FILNAM(1,IFIL),LFN001,IFRMAT,NSATFL,SATNMF,SATWGT,
     1                TFIRST,NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,
     2                AGENCY, FILTYP,TIMSYS,BASPOS,BASCLK)
C
C FIND REQUIRED SATELLITE NUMBERS
C -------------------------------
          IF(ISVN.GT.0)THEN
            DO 10 ISAT=1,NSATFL
              IF(ISVN.EQ.SATNMF(ISAT))THEN
                IF(NSAT.EQ.0)THEN
                  NSAT=1
                  SATNUM(1)=SATNMF(ISAT)
                  NEPSAT(1)=0
                END IF
                INDSAT(1)=ISAT
                GO TO 50
              END IF
10          CONTINUE
            GO TO 95
          ELSE IF(ISVN.EQ.-99)THEN
C
C LOOK FOR NEW SATELLITES
C -----------------------
            DO 25 KSAT=1,NSAT
              INDSAT(KSAT)=0
25          CONTINUE
            DO 30 ISAT=1,NSATFL
              DO 20 KSAT=1,NSAT
                IF(SATNMF(ISAT).EQ.SATNUM(KSAT))THEN
                  INDSAT(KSAT)=ISAT
                  GO TO 30
                END IF
20            CONTINUE
              NSAT=NSAT+1
              SATNUM(NSAT)=SATNMF(ISAT)
              INDSAT(NSAT)=ISAT
              NEPSAT(NSAT)=0
30          CONTINUE
          ELSE
            WRITE(LFNERR,40)ISVN
40          FORMAT(//,' *** SR GETOBS: ILLEGAL SVN:',I6,//)
            CALL EXITRC(1)
          END IF
50        CONTINUE
C
C READ THE POLE FILE FOR THE FIRST TIME
          CALL POLDF1(FILNAM(2,IFIL),TFIRST,1,XPOLE,YPOLE,UT1UTC,GPSUTC)
C
C INITIALIZE WOBBLE MATRIX
          POLE(1,1)=1.D0
          POLE(2,2)=1.D0
          POLE(3,3)=1.D0
          POLE(1,2)=0.D0
          POLE(2,1)=0.D0
C
C READ ALL EPOCHS OF SP3-FILE
          DO 90 IEPO=1,NEPO
C
C READ OBSERVATION CONCERNING ONE EPOCH
            ireade = 0
            CALL RDPREI(LFN001,IFRMAT,IREADE,NSATFL,SATNMF,TMJD,
     1                  POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,
     2                  IEREC,SDEVP,SDEVV,CORRP,CORRV,IRC)
            IF(IRC.EQ.1)GO TO 95
            TEPOCH=.epochToReal.TMJD
C
C POLE-INFORMATION (TIME ARGUMENT: UTC)
C -------------------------------------
            CALL POLDF1(FILNAM(2,IFIL),TEPOCH,1,XPOLE,YPOLE,
     1                  UT1UTC,GPSUTC)
            POLE(1,3)=-XPOLE
            POLE(2,3)= YPOLE
            POLE(3,1)= XPOLE
            POLE(3,2)=-YPOLE
C
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
            TDT=TEPOCH+(19.D0+32.184D0)/86400.D0
            CALL PRCEFF(IORSYS,5.D0,TDT,PRE)
            CALL NUTEFF(IORSYS,2.D0,TDT,NUT,SQEQUI,BIAS)
            PRE = matmul(PRE,BIAS)
C
C SIDERIAL TIME (TIME ARGUMENT: UT1)
            TUT1=TEPOCH-GPSUTC+UT1UTC
            ttut1 =.realToEpoch.TUT1
            ttdt  =.realToEpoch.tdt
            sz = gstime(0,ttut1,ttdt,nut(2,1),sqequi)
            CALL DDREH(3,SZ,THET)
C
C STORE ALL REQUIRED SATELLITES
            DO 70 ISAT=1,NSAT
              KSAT=INDSAT(ISAT)
              test=sqrt(pos(1,ksat)**2+pos(2,ksat)**2+pos(3,ksat)**2)
              IF(KSAT.NE.0.AND.TEST.NE.0.D0)THEN
                NEPSAT(ISAT)=NEPSAT(ISAT)+1
                OBSEPO(NEPSAT(ISAT),ISAT)=TEPOCH
C
C TRANSFORM EARTH-FIXED INTO J2000.0 SYSTEM
                CALL DMLMAV(POS(1,KSAT),POLE,POS(1,KSAT))
                CALL DMLMTV(POS(1,KSAT),THET,POS(1,KSAT))
                CALL DMLMTV(POS(1,KSAT),NUT,POS(1,KSAT))
                CALL DMLMTV(POS(1,KSAT),PRE,POS(1,KSAT))

                DO 60 K=1,3
                  PSDOBS(K,NEPSAT(ISAT),ISAT)=POS(K,KSAT)
60              CONTINUE
                CLOCKC(NEPSAT(ISAT),ISAT)=DTSATC(KSAT)
              END IF
70          CONTINUE
90        CONTINUE
95        CLOSE (UNIT=LFN001)
100     CONTINUE
C
C read tabular files in Bernese format
C ************************************
      ELSE IF(ITYPFL.EQ.2)THEN
C
C READ TABULAR FILES, COPY RESULT INTO AN AUXILIARY FILES (DISK)
C --------------------------------------------------------------
        TB1=0.D0
        TB2=1.D33
        CALL SETXYZ(NFIL,FILNAM(1,1:NFIL),1,1,TB1,TB2,DTABD,
     1              NSATF,SATNMF,IRCTOT,SOURCE,NSAMPL)
C
C ONE SATELLITE OR ALL SATELLITES?
C -------------------------------
        IF(ISVN.EQ.-99)THEN
          NSAT=NSATF
          DO ISAT=1,NSAT
            NEPSAT(ISAT)=0
            SATNUM(ISAT)=SATNMF(ISAT)
          ENDDO
        ELSE
          NSAT=1
          ISAT=1
          SATNUM(ISAT)=ISVN
          NEPSAT(ISAT)=0
        ENDIF
C
C READ AUXILIARY FILE, GENERATE CORRECT OBSERVATION MATRIX
C --------------------------------------------------------
        REWIND LFN001
        DO IREC=1,10000000
C
C READ ONE RECORD
          READ(LFN001,END=190) IFILT,ISVNF,TOBS,(XTF(K),K=1,3)
C
C IS THE SATELLITE OK?
          IF(ISVN.NE.-99)THEN
            IF(ISVNF.NE.ISVN)CYCLE
C
C GET INDEX OF SATELLITE
          ELSE
            DO ISAT=1,NSAT
              IF(ISVNF.EQ.SATNUM(ISAT))GO TO 120
            ENDDO
            GO TO 190
          ENDIF
120       CONTINUE
C
C SATELLITE FOUND
          NEPSAT(ISAT)=NEPSAT(ISAT)+1
          OBSEPO(NEPSAT(ISAT),ISAT)=TOBS

! ONLY FOR TIMS KKP: CORRECT RADIAL ANTENNA OFFSET
! Unit vector ER (radial)
! -----------------------
!          rsat=DSQRT(XTF(1)**2+XTF(2)**2+XTF(3)**2)
!          ersw(1:3)=XTF(1:3)/rsat
!!          XTF(1:3)=XTF(1:3)-ersw(1:3)*0.3928D0  ! CHAMP
!          XTF(1:3)=XTF(1:3)-ersw(1:3)*4.6316D0   ! TOPEX
          DO K=1,3
            PSDOBS(K,NEPSAT(ISAT),ISAT)=XTF(K)
          ENDDO
        ENDDO
190     CONTINUE
        CLOSE (UNIT=LFN001)
      ELSE
C
C POSITION AND POSITION DIFFERENCES FILE
C --------------------------------------
        NSAT=1
cc        sclfct=(7087000.+.773)/7087000.
        READ(LFN001,210)SATNUM(1),SENSOR,TIMREF
210     FORMAT(////,22X,I4,12X,A16,/,21X,F10.1,///)
        WRITE(*,*)'SATNUM,TIMREF=',SATNUM(1),TIMREF
        DO IREC=1,10000000
          READ(LFN001,220,END=230)IOBST,DT1,DT2,
     1                           (PSDOBS(K,IREC,1),K=1,3),RMSOBS(IREC)
220       FORMAT(I3,1X,2(F17.12,1X),3(F14.4,1X),F8.4)
cc          do k=1,3
cc            psdobs(k,irec,1)=psdobs(k,irec,1)/sclfct
cc          enddo
          RMSSAV=RMSOBS(IREC)
          OBSTYP(IREC)=IOBST
          OBSEPO(IREC,1)=TIMREF+DT1
          DTMOBS(IREC)=(DT2-DT1)*86400.D0
C
          IF(RMSRAT(1).EQ.0.D0.AND.RMSRAT(2).EQ.0.D0)THEN
C
C IGNORE RMS-VALUES IN FILE
            IF(IOBST.EQ.1)THEN
              IF(RMSMIN(1).NE.0.D0)THEN
                RMSOBS(IREC)=1.D0
              ELSE
                RMSOBS(IREC)=0.D0
              ENDIF
            ELSE
              IF(RMSMIN(1).NE.0.D0)THEN
                RMSOBS(IREC)=RMSMIN(2)/RMSMIN(1)
              ELSE
                RMSOBS(IREC)=RMSMIN(2)
              ENDIF
            ENDIF
          ELSE
C
C USE RMS-VALUES IN FILE
            IF(IOBST.EQ.1)THEN
              IF(RMSRAT(1).EQ.0)THEN
                RMSOBS(IREC)=0.D0
              ELSE IF(RMSOBS(IREC).LT.RMSMIN(1))THEN
                RMSOBS(IREC)=RMSMIN(1)
              ENDIF
            ELSE
              IF(RMSRAT(2).EQ.0.D0)THEN
                RMSOBS(IREC)=0.D0
              ELSE IF(RMSOBS(IREC).LT.RMSMIN(2))THEN
                RMSOBS(IREC)=RMSMIN(2)
              ENDIF
            ENDIF
          ENDIF
          if(rmssav.eq.0.d0)rmsobs(irec)=0.d0
          test=abs(rmsobs(irec)-0.1d0)
          if(iobst.eq.2.and.test.lt.1.0d-10)rmsobs(irec)=0.d0
cc          IF(IOBST.EQ.2.AND.RMSSAV.GE.0.1D0)
CC          write(*,*)'irec,ind,rms=',irec,obstyp(irec),rmsobs(irec)
        ENDDO
230     NEPSAT(1)=IREC-1
        CLOSE (UNIT=LFN001)
      END IF
C
C SPACING BETWEEN OBSERVATION EPOCHS
C ----------------------------------
      DELTAT=9.99D+33
      DO ISAT=1,NSAT
        DO IEPO=2,NEPSAT(ISAT)
          TEST=OBSEPO(IEPO,ISAT)-OBSEPO(IEPO-1,ISAT)
          IF(TEST.LT.DELTAT.AND.TEST.NE.0.D0)DELTAT=TEST
        ENDDO
      ENDDO
      DELTAT=86400.D0*DELTAT
C
      RETURN
      END SUBROUTINE

      END MODULE
