      MODULE s_SUMRES
      CONTAINS

C*
      SUBROUTINE SUMRES(IFIL,FROMTO,RESSIZ,ICARR,IWLFAC,ICYCLE,
     1                  IUNIT,NSATOT,NUMTOT,NSAMPL,MININT,
     2                  NUMCLS,HUNIT,NHISTO,XPOINT,RMS,
     3                  NEPMED,RMSMED,NDEL,LSTDEL,IRCODE)
CC
CC NAME       :  SUMRES
CC
CC PURPOSE    :  SUM UP RESIDUALS FOR ALL SATELLITES
CC               RESHED%DSC%ITYP=1: ONLY RESIDUALS ACTUALLY GIVEN IN
CC                                  FILE LFNRES MAY BE COMPUTED
CC               RESHED%DSC%ITYP=2: IT IS ASSUMED, THAT L1 AND L2
CC                                  RESIDUALS ARE ON THE INPUT FILE,
CC                                  ALLOWING TO COMPUTE ANY LINEAR COMB.
CC                                  OF L1 AND L2. L1 AND L2 RESIDUALS MUST
CC                                  FOLLOW EACH OTHER, IF EXISTENT.
CC
CC PARAMETERS :
CC         IN :  IFIL   : FILE NUMBER TO BE PROCESSED         I*4
CC               FROMTO(I),I=1,2: START/END EPOCH             I*4
CC               RESSIZ : RESIDUALS LARGER THAN RESSIZ ARE    R*8
CC                        LISTED AS OUTLIERS
CC               ICARR  : CARRIER                             I*4
CC               IWLFAC : WAVELENGTH FACTOR                   I*4
CC               ICYCLE : TYPE OF CYCLE TO BE USED AS UNITS   I*4
CC                        =1: L1 CYCLES
CC                        =2: L2 CYCLES
CC                        =3: NARROW LANE ("APPENZELLER")
CC               IUNIT  : UNITS OF RESIDUALS                  I*4
CC                        =1: CYCLES (ACCORDING TO "IWLFAC"
CC                            AND "ICYCLE")
CC                        =2: MILLIMETERS
CC                        =3: METERS
CC               NSATOT : TOTAL NUMBER OF SATELLITES          I*4
CC               NUMTOT(I),I=1,NSATOT: SATELLITE NUMBERS      I*4
CC               NSAMPL : SAMPLING INTERVAL IN SECONDS        I*4
CC               MININT : MINIMUM TIME INTERVAL FOR SMALL PIECES I*4
CC                        OF DATA (SECONDS)
CC               NUMCLS : SIZE OF HISTOGRAM                   I*4
CC               HUNIT  : BIN WIDTH IN NHISTO                 R*8
CC               NHISTO : HISTOGRAM(-NUMCLS...NUMCLS)         I*4(*)
CC        OUT :  XPOINT(I),I=1,NSATOT: NUMBER OF RESIDUALS    R*8
CC                        FOR EACH SATELLITE
CC               RMS(I),I=1,NSATOT: SQUARED SUM OF RESIDUALS  R*8
CC                        FOR EACH SATELLITE
CC               NEPMED : NUMBER OF ELEMENTS IN RMSMED        I*4
CC               RMSMED : EPOCH-WISE MEDIAN OF THE RESIDUALS  R*8(*)
CC               NDEL   : NUMBER OF OUTLIER AREAS             I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL          I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): TYPE OF MARING
CC               IRCODE : RETURN CODE                         I*4
CC                        =0: OK
CC                        =1: FILE NOT FOUND IN RESIDUAL FILE
CC                        =2: INVALID FREQUENCY SELECTED
CC                        =3: MAXIMUM NUMBER OF EPOCHS EXCEEDED
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  09-NOV-93
CC
CC CHANGES    :  30-JUN-94 : MR: CREATE LIST OF RESIDUALS ABOVE
CC                               A GIVEN RESIDUAL SIZE
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               23-MAR-95 : MR: MAXFIL=200 (INSTEAD OF 100)
CC               30-OCT-95 : MR: CHECK FOR NOBS=0
CC               06-JUN-96 : MR: REMOVED UNUSED VARIABLES
CC               19-AUG-97 : TS: USE WGTCOD,WGTPHS FOR SIMULTANEOUS
CC                               SCREENING OF CODE AND PHASE (CLOCKS)
CC               24-SEP-97 : DI: USE MAXSAT.inc, RENAME COMMON
CC                               BLOCK 'CDSPRS' TO 'CSUMRE'
CC               01-ARP-98 : TS: IMPROVED HANDLING OF ZD-RESIDUALS
CC               28-JUL-98 : MR: ADD SATELLITE TO CALL LINCOM
CC               15-AUG-99 : JJ: RM UNSED VAR FACT
CC               11-OCT-99 : TS: INCREASE FROMTO(2) TO 99999 (FOR SLR)
CC               27-JAN-00 : TS: ABUSE SATELLITE NUMBER FOR AZI AND ELE
CC               28-AUG-02 : RD: HANDLE NEW FORMATTED RESIDUAL FILES
CC               02-OCT-02 : RD: RESSIZ MAY BE ZERO
CC               15-JAN-03 : RD: STATION OBSERV. SIGMA FACTORS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-MAR-03 : HU: INTERFACE FOR ALCERR ADDED
CC               15-MAY-03 : HB: INITIALIZE STRUCTURE
CC               09-JUL-03 : RD: IEPSMP MUST BE AT LEAST ONE!
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               23-NOV-04 : RD: USE ICARR TO CALL MRKOBS (INSTEAD OF IFRQ)
CC               16-JUN-05 : MM: COMCONST.INC REPLACED BY D_CONST
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-JUL-08 : DT: ALLOW NOBS=1 (NEEDED FOR SLR)
CC               20-MAY-09 : DT: ICARR=0 IF SCREEN FREQU. AS AVAILABLE
CC               07-JUN-11 : AS/RD/LP: APPLY RESIDUAL FACTORS RESFAC FOR GNSS
CC               26-JAN-12 : LP: Flexibilization of RESFAC dimension
CC               28-MAR-12 : LP: Change RESFAC for GAL 2.0 -> 1.5
CC               30-JUL-12 : RD: SR STATIS WITH OPTIONAL ARGUMENTS
CC               30-JUL-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnres
      USE m_global, ONLY: maxsys
      USE m_maxdim, ONLY: maxsat
      USE d_resFil, ONLY: t_resHead,init_reshead
      USE d_const,  ONLY: WGTCOD, WGTPHA
C
      USE s_alcerr
      USE s_mrkobs
      USE s_rdresh2
      USE s_lincom
      USE s_setflg
      USE s_cksizer1
      USE s_statis
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICARR , ICLS  , ICYCLE, IDEL  , IDSTYP, IENT  , IEPO  ,
     1          IEPO2 , IEPOCH, IEPOLD, IEPSMP, IFIL  , IFILE , IFIRST,
     2          IFREQ , IFRQ  , IOB1  , IOB2  , IOBS  , IRC   , IRCODE,
     3          ISADEL, ISAT  , ISUM  , ISVN  , IUNIT , IWLFAC, K     ,
     4          MAXEPO, MDIFF , MINEPO, MININT, NDEL  , NDELNW, NEPMED,
     5          NOBS  , NSAMPL, NSATOT, NUMCLS
C
      REAL*8    HUNIT , RES   , RESHLP, RESSIZ, WAVE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXEPO=3000)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXEPO: MAXIMUM NUMBER OF EPOCH FOR MEDIAN (USED AS UNIT FOR ALLOCATION)
C
      TYPE(t_resHead) :: resHed
C
      INTEGER*4    FROMTO(*)
      INTEGER*4    SVNHLP(2)
      INTEGER*4    SVNHL2(2,2),IFIL2(2)
      INTEGER*4    ICYC(2),NUMTOT(*)
      INTEGER*4    SVN(2),SVNEPO(2,MAXSAT),LSTDEL(5,*)
      INTEGER*4    NHISTO(*)
C
      REAL(r8b),   DIMENSION(:), POINTER :: RMSMED
      REAL*8       RESHL2(2),WGTGEN(3)
      REAL*8       RMS(*),XPOINT(*),RESEPO(MAXSAT),XMEAN(MAXSAT)
      REAL*8       RESEP2(MAXSAT)
      REAL(r8b),   DIMENSION(MAXSYS):: RESFAC
C
      CHARACTER    FLG(3)*1
      CHARACTER    RESFLG*1,RESFL1*1,RESFL2*1
C
C      COMMON/CSUMRE/SVNFIL
      INCLUDE 'COMFREQ.inc'
      DATA IFIRST/1/
C
C INITIALIZE FACTORS FOR DIFFERENT GNSS
C -------------------------------------
      RESFAC    = 1.d0
c     GALILEO: allow larger residuals (low number of obs, new system)
      RESFAC(3) = 1.5
c
c     ADD_GNSS_HERE
C
C
C NULLIFGY POINTER
C ----------------
      CALL init_reshead(resHed)

      IF(IFIRST.EQ.1) THEN
C
C SET WEIGHT FACTORS
C ------------------
        WGTGEN(1)=1.D0
        WGTGEN(2)=WGTCOD/WGTPHA
        WGTGEN(3)=1.D0
        IFIRST=0
      ENDIF
C
C INITIALIZATION OF THE NUMBER OF RESIDUALS AND THE SQUARED SUM
C -------------------------------------------------------------
      DO K=1,NSATOT
        RMS(K)=0.D0
        XPOINT(K)=0.D0
      ENDDO
      IEPOLD=0
C
      NEPMED=0
C
C COMPUTE UNITS FOR DISPLAY IN CYCLES
C -----------------------------------
      IF(IUNIT.EQ.1) THEN
        DO 10 IFRQ=1,2
          IF(ICYCLE.EQ.IFRQ.OR.ICYCLE.EQ.3) THEN
            ICYC(IFRQ)=1
          ELSE
            ICYC(IFRQ)=0
          ENDIF
10      CONTINUE
      ENDIF
C
C ALLOCATE RESIDUAL MEDIAN ARRAY
C ------------------------------
      ALLOCATE(RMSMED(MAXEPO),STAT=IRC)
      CALL ALCERR(IRC,'RESMED',(/MAXEPO/),'SUMRES')
C
C READ HEADER OF RESIDUAL FILE
C ----------------------------
      REWIND LFNRES
C
      CALL rdresh2(lfnres,resHed)
C
      mDiff = resHed%dsc%nResat+1
C
C CHECK REQUESTED FILE NUMBER
C ---------------------------
      IF(IFIL.LT.1.OR.IFIL.GT.resHed%nFil) THEN
        WRITE(LFNERR,62)IFIL,resHed%nFil
62      FORMAT(/,' *** SR SUMRES: FILE NUMBER OUT OF RANGE',/,
     1                       16X,'REQUESTED FILE NUMBER        :',I4,/,
     2                       16X,'LARGEST FILE NUMBER AVAILABLE:',I4,/)
        IRCODE=1
C
        DO iFile=1,resHed%nFil
          DEALLOCATE(resHed%filHead(iFile)%numSat,stat=irc)
        ENDDO
        DEALLOCATE(resHed%filHead,stat=irc)
C
        RETURN
      END IF
C
C DEFINE FIRST AND LAST EPOCH, IF 0,0 IS ENTERED
C ----------------------------------------------
      IF(FROMTO(1).EQ.0) FROMTO(1)=1
      IF(FROMTO(2).EQ.0) FROMTO(2)=99999
C
C EPOCH SPACING DUE TO SAMPLING
C -----------------------------
      IEPSMP=NSAMPL/resHed%filHead(iFil)%ideltt
      MINEPO=MININT/resHed%filHead(iFil)%ideltt
C
      IF (IEPSMP.LT.1) IEPSMP=1
C
C CHECK WHETHER REQUESTED FREQUENCY IS ALLOWED
C --------------------------------------------
      DO 70 IFRQ=1,resHed%filHead(iFil)%nFrfil
        IF( (ICARR.EQ.resHed%filHead(iFil)%iCarr(iFrq) .AND.
     1       ICARR>0 ) .OR. ICARR==0 ) THEN
          IDSTYP=1
          GOTO 100
        ENDIF
70    CONTINUE
      IF(resHed%dsc%iTyp.EQ.2               .AND.
     1   resHed%filHead(iFil)%nFrfil.EQ.2   .AND.
     2   resHed%filHead(iFil)%iCarr(1).EQ.1 .AND.
     3   resHed%filHead(iFil)%iCarr(2).EQ.2) THEN
        IF (resHed%dsc%nResat == 0) THEN
          WRITE(lfnerr,'(/,A,2(/,16X,A),/)')
     1    ' *** SR SUMRES: The handling of more than one ' //
     2                    'satellite is not yet ',
     3                    'implemented. Only the original' //
     4                    ' frequencies from the',
     5                    'residual file may be analyzed.'
          CALL exitrc(2)
        ENDIF
        CALL SETFLG(FLG(1),0)
        CALL SETFLG(FLG(2),0)
        IDSTYP=2
        GO TO 100
      END IF
      WRITE(LFNERR,71)ICARR
71    FORMAT(/,' *** SR SUMRES: INVALID FREQUENCY'/,
     1                     16X,'FREQUENCY:',I3,/)
      IRCODE=2
C
      DO iFile=1,resHed%nFil
        DEALLOCATE(resHed%filHead(iFile)%numSat,stat=irc)
      ENDDO
      DEALLOCATE(resHed%filHead,stat=irc)
C
      RETURN
100   CONTINUE
C
C  DISPLAY OF TYPE 1
C  -----------------
      IF (IDSTYP.EQ.1) THEN
          NOBS=0
170       READ(LFNRES,END=180) IFILE,IEPOCH,IFREQ,
     1                         (SVNHLP(K),K=1,2),RESHLP,RESFLG
C
          IF (IFILE.NE.IFIL)       GOTO 170
          IF (IEPOCH.LT.FROMTO(1)) GOTO 170
          IF (IEPOCH.GT.FROMTO(2)) GOTO 180
          IF (IFREQ.NE.ICARR .AND. ICARR>0) GOTO 170
C
C APPLY WEIGHTS
C -------------
          RESHLP=RESHLP*DSQRT(WGTGEN(resHed%filHead(iFil)%meaTyp))
C
          IF(IUNIT.EQ.1) THEN
            ISVN=SVNHLP(1)
            WAVE=DABS((ICYC(1)*WLGT(1,ISVN)*FACLIN(ICARR,1,ISVN)+
     1                 ICYC(2)*WLGT(2,ISVN)*FACLIN(ICARR,2,ISVN))
     2                 /IWLFAC)
            RESHLP=RESHLP/WAVE
          ELSE IF(IUNIT.EQ.2) THEN
            RESHLP=RESHLP*1D3
          ENDIF
C
C UPDATE HISTOGRAM
C ----------------
          IF (DBLE(NUMCLS).LT.DNINT(RESHLP/HUNIT)) THEN
            ICLS=2*NUMCLS+1
          ELSE IF (-DBLE(NUMCLS).GT.DNINT(RESHLP/HUNIT)) THEN
            ICLS=1
          ELSE
            ICLS=NINT(RESHLP/HUNIT)+NUMCLS+1
          ENDIF
C
          NHISTO(ICLS) = NHISTO(ICLS)+1
C
C PROCESS PREVIOUS EPOCH
C ----------------------
          IF (IEPOCH.NE.IEPOLD) THEN
            IF (IEPOLD.NE.0) THEN
C
              DO ISAT=1,NSATOT
                XMEAN(ISAT)=0.D0
              ENDDO
C
C DOUBLE-DIFFERENCE RESIDUALS
C ---------------------------
              IF (MDIFF.EQ.2) THEN
                DO IOB1=1,NOBS
                  DO IOB2=IOB1,NOBS
                    SVN(1)=SVNEPO(1,IOB1)
                    SVN(2)=SVNEPO(2,IOB2)
                    RES=0.D0
                    DO ISUM=IOB1,IOB2
                      RES=RES+RESEPO(ISUM)
                    ENDDO
C
C ADD RESIDUAL TO SUM
                    DO ISVN=1,2
                      DO ISAT=1,NSATOT
                        IF (NUMTOT(ISAT).EQ.SVN(ISVN)) GOTO 172
                      ENDDO
172                   RMS(ISAT)=RMS(ISAT)+RES**2/NOBS
                      XMEAN(ISAT)=XMEAN(ISAT)+RES*(-1)**(ISVN-1)
                      XPOINT(ISAT)=XPOINT(ISAT)+1.D0/NOBS
                    ENDDO
                  ENDDO
                ENDDO
              ELSE
C
C ZERO-DIFFERENCE RESIDUALS
C -------------------------
                DO IOBS=1,NOBS
                  DO ISAT=1,NSATOT
                    IF (NUMTOT(ISAT).EQ.SVNEPO(1,IOBS)) THEN
                      XMEAN(ISAT)=RESEPO(IOBS)
                      XPOINT(ISAT)=XPOINT(ISAT)+1.D0
                      RMS(ISAT)=RMS(ISAT)+XMEAN(ISAT)**2
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
C
C DETECT OUTLIERS
              DO ISAT=1,NSATOT
                IF (NOBS.NE.0 .AND. MDIFF.EQ.2) THEN
                  XMEAN(ISAT)=XMEAN(ISAT)/NOBS
                ENDIF
                IF (RESSIZ.NE.0.AND.
     1              DABS(XMEAN(ISAT)).GT.RESSIZ*1.0D3*
     2                             RESFAC(NUMTOT(ISAT)/100+1)) THEN
                  DO IEPO=IEPOLD,IEPOLD+IEPSMP-1
                    CALL MRKOBS(1,NUMTOT(ISAT),IEPO,ICARR,NSATOT,NUMTOT,
     1                          NDEL,LSTDEL)

                    IF (ICARR.EQ.0) LSTDEL(4,NDEL)=IFREQ

                  ENDDO
                ENDIF
              ENDDO
C
C GET THE MEDIAN OF ALL RESIDUALS OF THE EPOCH
C --------------------------------------------
CCC              IF (NOBS.GT.1) THEN
              IF (NOBS.GE.1) THEN
                NEPMED=NEPMED+1
                DO IOBS=1,NOBS
                  RESEP2(IOBS)=DABS(RESEPO(IOBS))
                ENDDO
                CALL CKSIZER1(RMSMED,NEPMED,MAXEPO)
                CALL STATIS(NOBS,RESEP2,xMed=RMSMED(NEPMED))
              ENDIF
            ENDIF
            NOBS=0
            IEPOLD=IEPOCH
          ENDIF
C
          NOBS=NOBS+1
          DO ISVN=1,MDIFF
            SVNEPO(ISVN,NOBS)=SVNHLP(ISVN)
          ENDDO
          RESEPO(NOBS)=RESHLP
        GOTO 170
180     CONTINUE
C
C ADD LAST EPOCH
C --------------
        DO ISAT=1,NSATOT
          XMEAN(ISAT)=0.D0
        ENDDO
C
C DOUBLE-DIFFERENCE RESIDUALS
C ---------------------------
        IF (MDIFF.EQ.2) THEN
          DO IOB1=1,NOBS
            DO IOB2=IOB1,NOBS
              SVN(1)=SVNEPO(1,IOB1)
              SVN(2)=SVNEPO(2,IOB2)
              RES=0.D0
              DO ISUM=IOB1,IOB2
                RES=RES+RESEPO(ISUM)
              ENDDO
C
C ADD RESIDUAL TO SUM
              DO ISVN=1,2
                DO ISAT=1,NSATOT
                  IF (NUMTOT(ISAT).EQ.SVN(ISVN)) GOTO 182
                ENDDO
182             RMS(ISAT)=RMS(ISAT)+RES**2/NOBS
                XMEAN(ISAT)=XMEAN(ISAT)+RES*(-1)**(ISVN-1)
                XPOINT(ISAT)=XPOINT(ISAT)+1.D0/NOBS
              ENDDO
            ENDDO
          ENDDO
        ELSE
C
C ZERO-DIFFERENCE RESIDUALS
C -------------------------
          DO IOBS=1,NOBS
            DO ISAT=1,NSATOT
              IF (NUMTOT(ISAT).EQ.SVNEPO(1,IOBS)) THEN
                XMEAN(ISAT)=RESEPO(IOBS)
                XPOINT(ISAT)=XPOINT(ISAT)+1.D0
                RMS(ISAT)=RMS(ISAT)+XMEAN(ISAT)**2
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
C DETECT OUTLIERS LAST EPOCH
        DO ISAT=1,NSATOT
          IF (NOBS.NE.0 .AND. MDIFF.EQ.2) THEN
            XMEAN(ISAT)=XMEAN(ISAT)/NOBS
          ENDIF
          IF (RESSIZ.NE.0.AND.
     1        DABS(XMEAN(ISAT)).GT.RESSIZ*1.0D3*
     2                             RESFAC(NUMTOT(ISAT)/100+1)) THEN
            DO IEPO=IEPOLD,IEPOLD+IEPSMP-1
              CALL MRKOBS(1,NUMTOT(ISAT),IEPO,ICARR,NSATOT,NUMTOT,
     1                    NDEL,LSTDEL)

              IF (ICARR.EQ.0) LSTDEL(4,NDEL)=IFREQ

            ENDDO
          ENDIF
        ENDDO
C
C GET THE MEDIAN OF ALL RESIDUALS OF THE EPOCH
C --------------------------------------------
CCC        IF (NOBS.GT.1) THEN
        IF (NOBS.GE.1) THEN
          NEPMED=NEPMED+1
          DO IOBS=1,NOBS
            RESEP2(IOBS)=DABS(RESEPO(IOBS))
          ENDDO
          CALL CKSIZER1(RMSMED,NEPMED,MAXEPO)
          CALL STATIS(NOBS,RESEP2,xMed=RMSMED(NEPMED))
        ENDIF
C
C DISPLAY OF TYPE 2
C -----------------
      ELSE
C
C LOOP OVER RESIDUALS
C READ L1 RESIDUAL
270       READ(LFNRES,END=280) IFIL2(1),IEPOCH,IFREQ,
     1                         (SVNHL2(K,1),K=1,2),RESHL2(1),RESFL1
          IF(IFREQ.NE.1) GOTO 270
C
C READ L2 RESIDUAL
190       READ(LFNRES,END=280) IFIL2(2),IEPO2,IFREQ,
     1                         (SVNHL2(K,2),K=1,2),RESHL2(2),RESFL2
          IF(IFREQ.EQ.1) THEN
            IFIL2(1)=IFIL2(2)
            IEPOCH=IEPO2
            SVNHL2(1,1)=SVNHL2(1,2)
            SVNHL2(2,1)=SVNHL2(2,2)
            RESHL2(1)=RESHL2(2)
            RESFL1=RESFL2
            GOTO 190
          ENDIF
C
C L2 CORRESPONDS TO L1 ?
          IF(IFREQ      .NE.2          .OR.
     1       IFIL2(1)   .NE.IFIL2(2)   .OR.
     2       IEPOCH     .NE.IEPO2      .OR.
     3       SVNHL2(1,1).NE.SVNHL2(1,2).OR.
     4       SVNHL2(2,1).NE.SVNHL2(2,2)) GOTO 270
C
          IF(IFIL2(1).NE.IFIL)    GOTO 270
          IF(IEPOCH.LT.FROMTO(1)) GOTO 270
          IF(IEPOCH.GT.FROMTO(2)) GOTO 280
C
          IF(ICARR.EQ.1) THEN
            RESFLG=RESFL1
          ELSE IF(ICARR.EQ.2) THEN
            RESFLG=RESFL2
          ELSE
            RESFLG=RESFL1
          ENDIF
          IF(RESFLG.EQ.' '.AND.RESFL2.NE.' ') RESFLG=RESFL2
          CALL LINCOM(ICARR,SVNHL2(1,1),RESHL2(1),RESHL2(2),
     1                FLG(1),FLG(2),RESHLP,FLG(3))
C
          IF(IUNIT.EQ.1) THEN
            ISVN=SVNHL2(1,1)
            WAVE=DABS((ICYC(1)*WLGT(1,ISVN)*FACLIN(ICARR,1,ISVN)+
     1                 ICYC(2)*WLGT(2,ISVN)*FACLIN(ICARR,2,ISVN))
     2                 /IWLFAC)
            RESHLP=RESHLP/WAVE
          ELSE IF(IUNIT.EQ.2) THEN
            RESHLP=RESHLP*1000
          ENDIF
C
C UPDATE HISTOGRAM
C ----------------
          IF (DBLE(NUMCLS).LT.DNINT(RESHLP/HUNIT)) THEN
            ICLS=2*NUMCLS+1
          ELSE IF (-DBLE(NUMCLS).GT.DNINT(RESHLP/HUNIT)) THEN
            ICLS=1
          ELSE
            ICLS=NINT(RESHLP/HUNIT)+NUMCLS+1
          ENDIF
C
          NHISTO(ICLS) = NHISTO(ICLS)+1
C
C PROCESS PREVIOUS EPOCH
          IF (IEPOCH.NE.IEPOLD) THEN
            IF (IEPOLD.NE.0) THEN
C
              DO ISAT=1,NSATOT
                XMEAN(ISAT)=0.D0
              ENDDO
C
              DO IOB1=1,NOBS
                DO IOB2=IOB1,NOBS
                  SVN(1)=SVNEPO(1,IOB1)
                  SVN(2)=SVNEPO(2,IOB2)
                  RES=0.D0
                  DO ISUM=IOB1,IOB2
                    RES=RES+RESEPO(ISUM)
                  ENDDO
C
C ADD RESIDUAL TO SUM
                  DO ISVN=1,2
                    DO ISAT=1,NSATOT
                      IF (NUMTOT(ISAT).EQ.SVN(ISVN)) GOTO 272
                    ENDDO
272                 RMS(ISAT)=RMS(ISAT)+RES**2/NOBS
                    XMEAN(ISAT)=XMEAN(ISAT)+RES*(-1)**(ISVN-1)
                    XPOINT(ISAT)=XPOINT(ISAT)+1.D0/NOBS
                  ENDDO
                ENDDO
              ENDDO
C
C DETECT OUTLIERS
              DO ISAT=1,NSATOT
                XMEAN(ISAT)=XMEAN(ISAT)/NOBS
                IF (RESSIZ.NE.0.AND.
     1              DABS(XMEAN(ISAT)).GT.RESSIZ*1000.D0*
     2                             RESFAC(NUMTOT(ISAT)/100+1)) THEN
                  DO IEPO=IEPOLD,IEPOLD+IEPSMP-1
                    CALL MRKOBS(1,NUMTOT(ISAT),IEPO,ICARR,NSATOT,NUMTOT,
     1                          NDEL,LSTDEL)

                    IF (ICARR.EQ.0) LSTDEL(4,NDEL)=IFREQ

                  ENDDO
                ENDIF
              ENDDO
C
C GET THE MEDIAN OF ALL RESIDUALS OF THE EPOCH
C --------------------------------------------
CCC              IF (NOBS.GT.1) THEN
              IF (NOBS.GE.1) THEN
                NEPMED=NEPMED+1
                DO IOBS=1,NOBS
                  RESEP2(IOBS)=DABS(RESEPO(IOBS))
                ENDDO
                CALL CKSIZER1(RMSMED,NEPMED,MAXEPO)
                CALL STATIS(NOBS,RESEP2,xMed=RMSMED(NEPMED))
              ENDIF
            ENDIF
            NOBS=0
            IEPOLD=IEPOCH
          ENDIF
          NOBS=NOBS+1
          DO ISVN=1,2
            SVNEPO(ISVN,NOBS)=SVNHL2(ISVN,1)
          ENDDO
          RESEPO(NOBS)=RESHLP
        GOTO 270
280     CONTINUE
C
C ADD LAST EPOCH
        DO ISAT=1,NSATOT
          XMEAN(ISAT)=0.D0
        ENDDO
C
        DO IOB1=1,NOBS
          DO IOB2=IOB1,NOBS
            SVN(1)=SVNEPO(1,IOB1)
            SVN(2)=SVNEPO(2,IOB2)
            RES=0.D0
            DO ISUM=IOB1,IOB2
              RES=RES+RESEPO(ISUM)
            ENDDO
C
C ADD RESIDUAL TO SUM
            DO ISVN=1,2
              DO ISAT=1,NSATOT
                IF (NUMTOT(ISAT).EQ.SVN(ISVN)) GOTO 282
              ENDDO
282           RMS(ISAT)=RMS(ISAT)+RES**2/NOBS
              XMEAN(ISAT)=XMEAN(ISAT)+RES*(-1)**(ISVN-1)
              XPOINT(ISAT)=XPOINT(ISAT)+1.D0/NOBS
            ENDDO
          ENDDO
        ENDDO
C
C DETECT OUTLIERS LAST EPOCH
        DO ISAT=1,NSATOT
          XMEAN(ISAT)=XMEAN(ISAT)/NOBS
          IF (RESSIZ.NE.0.AND.
     1        DABS(XMEAN(ISAT)).GT.RESSIZ*1000.D0*
     2                             RESFAC(NUMTOT(ISAT)/100+1)) THEN
            DO IEPO=IEPOLD,IEPOLD+IEPSMP-1
              CALL MRKOBS(1,NUMTOT(ISAT),IEPO,ICARR,NSATOT,NUMTOT,
     1                    NDEL,LSTDEL)

              IF (ICARR.EQ.0) LSTDEL(4,NDEL)=IFREQ

            ENDDO
          ENDIF
        ENDDO
C
C GET THE MEDIAN OF THE SQUARED EPOCH RESIDUALS
C ---------------------------------------------
CCC        IF (NOBS.GT.1) THEN
        IF (NOBS.GE.1) THEN
          NEPMED=NEPMED+1
          DO IOBS=1,NOBS
            RESEP2(IOBS)=DABS(RESEPO(IOBS))
          ENDDO
          CALL CKSIZER1(RMSMED,NEPMED,MAXEPO)
          CALL STATIS(NOBS,RESEP2,xMed=RMSMED(NEPMED))
        ENDIF
      ENDIF
C
C ADD EPOCHS AT THE BEGINNING OF A MARKED AREA, IF SAMPLED RESIDUALS
      DO IDEL=1,NDEL
        LSTDEL(2,IDEL)=LSTDEL(2,IDEL)-IEPSMP+1
        IF (LSTDEL(2,IDEL).LT.0) LSTDEL(2,IDEL)=1
      ENDDO
C
C COMBINE AREAS IF JUST ONE EPOCH IS MISSING INBETWEEN
      DO ISAT=1,NSATOT
        ISADEL=0
        DO IDEL=1,NDEL
          IF (LSTDEL(1,IDEL).EQ.NUMTOT(ISAT)) THEN
            IF (ISADEL.EQ.0) THEN
              ISADEL=IDEL
            ELSE IF (LSTDEL(3,ISADEL)+MINEPO+1.GE.LSTDEL(2,IDEL)) THEN
              LSTDEL(3,ISADEL)=LSTDEL(3,IDEL)
              LSTDEL(1,IDEL)=-1
            ELSE
              ISADEL=IDEL
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      NDELNW=0
      DO IDEL=1,NDEL
        IF (LSTDEL(1,IDEL).NE.-1) THEN
          NDELNW=NDELNW+1
          DO IENT=1,5
            LSTDEL(IENT,NDELNW)=LSTDEL(IENT,IDEL)
          ENDDO
        ENDIF
      ENDDO
      NDEL=NDELNW
C
      DO iFile=1,resHed%nFil
        DEALLOCATE(resHed%filHead(iFile)%numSat,stat=irc)
      ENDDO
      DEALLOCATE(resHed%filHead,stat=irc)
C
      RETURN
      END SUBROUTINE

      END MODULE
