C*
      PROGRAM SUBDIF
CC
CC NAME       :  SUBDIF
CC
CC PURPOSE    :  COMPARE DIFFERENT SUB-DAILY ERP MODELS. COMPUTE
CC               AMPLITUDE DIFFERENCES AND RMS DIFFERENCES BETWEEN
CC               MODELS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  27-MAR-98
CC
CC CHANGES    :  16-AUG-99 : JJ: RM UNUSED VAR MEAADD
CC               24-OCT-01 : HB: SWITCH TO NEW MENU
CC               20-MAY-03 : PS: SWITCH TO VERSION 5
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-JUN-05 : MM: LFNUM.INC REMOVED (LFNUMS IN M_BERN)
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               21-OCT-08 : HB: ADD USE S_SUBMOD
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               27-APR-12 : RD: NULLIFY POINTERS, USE M_BERN WITH ONLY
CC               27-APR-12 : RD: REMOVE UNUSED LABELS
CC               27-APR-12 : RD: FORMAT STATEMENT CHANGED (LINE TOO LONG)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: r8b, i4b, lfnprt, lfnerr, lfn001,
     1                    fileNameLength
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_submod
      USE s_alcerr
      USE s_opnfil
      USE s_prflna
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_subinp
      USE s_timst2
      USE s_rdsubm
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE f_ikf

      IMPLICIT NONE


C
C MAXIMUM DIMENSIONS
C ------------------
      INTEGER(i4b)                :: MAXPER
      PARAMETER (MAXPER=1000)
C
C DECLARATIONS
C ------------
      CHARACTER*80 TITLE1,TITLE2
      CHARACTER(LEN=80),DIMENSION(:),ALLOCATABLE :: TITLE
      CHARACTER*40 TSTRNG
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam
      CHARACTER*20 PTXTTT(2),PTXTT2(2),PARTXT(2,4)
      CHARACTER*16 SUBNA1,SUBNA2
      CHARACTER(LEN=16),DIMENSION(:),ALLOCATABLE :: SUBNAM
      CHARACTER*5  UNITS(2)
      CHARACTER*3  SINCOS(2),YESNO(2)
      CHARACTER*2  ERPTYP(2)
      CHARACTER(LEN=8)                               :: datenow
      CHARACTER(LEN=10)                              :: timenow


C
      REAL*8       SUBFA1(6,6),SUBPE1(MAXPER),SUBCO1(4,MAXPER)
      REAL*8       SUBFA2(6,6),SUBPE2(MAXPER),SUBCO2(4,MAXPER)
      REAL*8       WINDOW(2),ERPSU1(3),ERPSR1(3),ERPSU2(3),ERPSR2(3)
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE :: RMSDIF
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE :: MEADIF
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE   :: RMSDTT
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE   :: MEADTT
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: RM2DIF
C
      INTEGER(i4b),DIMENSION(:,:),POINTER  :: ioemlt
      INTEGER(i4b),DIMENSION(:),POINTER    :: ioeTyp
      INTEGER*4    IDFOPT(2),IFRTYP(2,4)
      INTEGER*4    SUBML1(6,MAXPER),SUBML2(6,MAXPER),SUBMLT(6)
      INTEGER(i4b),DIMENSION(:,:,:),ALLOCATABLE :: NTERM
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: NTERMT
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: NTERM2

! Local Variables
! ---------------
      REAL(r8b)                   :: SUBCM1,SUBCM2
      REAL(r8b)                   :: PERIOD,TEPO
      REAL(r8b)                   :: DTSAMP                              ! Sampling rate in min

      INTEGER(i4b)                :: NIOE,INCEXC
      INTEGER(i4b)                :: iac,IOSTAT
      INTEGER(i4b)                :: IFIL,IFIL1,IFIL2,nFil
      INTEGER(i4b)                :: ITRM,IIOE
      INTEGER(i4b)                :: IMLT,NEPO,IEPO
      INTEGER(i4b)                :: ITYP,IADD
      INTEGER(i4b)                :: NSU1,NSU2
      INTEGER(i4b)                :: ISU1,ISU2
      INTEGER(i4b)                :: ICMP,IND
      INTEGER(i4b)                :: ITERM,I12
      INTEGER(i4b)                :: II
C
C DATA
C ----
      DATA UNITS/'MUAS ','MUS  '/
      DATA PTXTTT/'PM TOTAL            ','UT TOTAL            '/
      DATA PTXTT2/'PM TOTAL INTERVAL   ','UT TOTAL INTERVAL   '/
      DATA PARTXT/'PM DIURNAL PROGRADE ','UT DIURNAL PROGRADE ',
     1            'PM DIURNAL RETROGR. ','UT DIURNAL RETROGR. ',
     2            'PM SEMI-D. PROGRADE ','UT SEMI-D. PROGRADE ',
     3            'PM SEMI-D. RETROGR. ','UT SEMI-D. RETROGR. '/
      DATA ERPTYP/'PM','UT'/
      DATA SINCOS/'SIN','COS'/
      DATA YESNO /'NO ','YES'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(ioeTyp)
      NULLIFY(ioemlt)
      NULLIFY(filnam)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)

! Print titles and File Names
! ---------------------------
      CALL pritit('SUBDIF',
     1            'Comparison Between Different Sub-daily ERP Models')
      CALL prflna
C
C GET INPUT OPTIONS
C -----------------
      CALL SUBINP(filNam,IDFOPT,IFRTYP,INCEXC,NIOE,IOETYP,IOEMLT,
     1            DTSAMP,WINDOW)

      nFil=SIZE(filNam,2)

! Allocate Memory
! ---------------
      ALLOCATE (title(nFil),stat=iac)
      CALL alcerr(iac,'title',(/nFil/),'subdif')
      ALLOCATE (subnam(nFil),stat=iac)
      CALL alcerr(iac,'subnam',(/nFil/),'subdif')
      ALLOCATE (rmsdif(2,4,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'rmsdif',(/2,4,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (meadif(2,4,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'meadif',(/2,4,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (rmsdtt(2,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'rmsdtt',(/2,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (meadtt(2,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'meadtt',(/2,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (rm2dif(2,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'rm2dif',(/2,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (nterm(2,4,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'nterm',(/2,4,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (ntermt(2,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'ntermt',(/2,nFil*(nFil+1)/2/),'subdif')
      ALLOCATE (nterm2(2,nFil*(nFil+1)/2),stat=iac)
      CALL alcerr(iac,'nterm2',(/2,nFil*(nFil+1)/2/),'subdif')



      DO IFIL=1,NFIL
        WRITE(LFNPRT,1002) IFIL,FILNAM(1,IFIL),FILNAM(2,IFIL)
1002    FORMAT(I4,3X,A32,2X,A32)
      ENDDO
C
      WRITE(LFNPRT,'(/)')
C
C PRINT OPTIONS
C -------------
      WRITE(LFNPRT,1031) YESNO(IDFOPT(1)+1)
1031  FORMAT(1X,'PROCESSING OPTIONS:',/,1X,18('-'),//,
     1       1X,'TERMS PRESENT IN ONE MODEL ONLY INCLUDED: ',A,/)
C
      DO ITYP=1,2
        DO ITRM=1,4
          WRITE(LFNPRT,1032) PARTXT(ITYP,ITRM),
     1                       YESNO(IFRTYP(ITYP,ITRM)+1)
1032      FORMAT(1X,A20,' INCLUDED           : ',A)
        ENDDO
      ENDDO
      WRITE(LFNPRT,'( )')
C
C TERMS INCLUDED/EXCLUDED
      IF (INCEXC.NE.0 .AND. NIOE.NE.0) THEN
C
        IF (INCEXC.EQ.1) THEN
          WRITE(LFNPRT,1033)
1033      FORMAT(1X,'TERMS INCLUDED IN RMS COMPUTATION:',I5,/)
        ELSE
          WRITE(LFNPRT,1034)
1034      FORMAT(1X,'TERMS EXCLUDED FROM RMS COMPUTATION:',I5,/)
        ENDIF
C
        DO IIOE=1,NIOE
          WRITE(LFNPRT,1035) ERPTYP(IOETYP(IIOE)),
     1                       (IOEMLT(IMLT,IIOE),IMLT=1,6)
1035      FORMAT(3X,A2,6I3)
        ENDDO
C
        WRITE(LFNPRT,'( )')
      ENDIF
C
C NUMBER OF EPOCHS
      IF (DTSAMP.NE.0.D0) THEN
        NEPO=IDNINT((WINDOW(2)-WINDOW(1))/DTSAMP)+1
        CALL TIMST2(1,2,WINDOW,TSTRNG)
        WRITE(LFNPRT,1036) NEPO,IDNINT(DTSAMP*1440.D0),TSTRNG
1036    FORMAT(1X,'NUMBER OF EPOCHS COMPARED:',I10,/,
     1         1X,'SAMPLING INTERVAL (MIN)  :',I10,/,
     2         1X,'TIME INTERVAL CONSIDERED : ',A,/)
      ENDIF
C
      WRITE(LFNPRT,'( )')
C
C TITLE FOR MODEL DIFFERENCES
C ---------------------------
      IF (IDFOPT(2).EQ.1) THEN
        WRITE(LFNPRT,1011)
1011    FORMAT(1X,'DIFFERENCES BETWEEN MODELS:',/,
     1         1X,'--------------------------',/)
      ENDIF
C
C 1ST LOOP OVER MODELS
C --------------------
      DO IFIL1=1,NFIL
C
C READ SUB-DAILY MODEL 1
        CALL RDSUBM(MAXPER,FILNAM(1,IFIL1),TITLE1,SUBNA1,SUBFA1,NSU1,
     1              SUBPE1,SUBML1,SUBCO1)
        TITLE(IFIL1) =TITLE1
        SUBNAM(IFIL1)=SUBNA1
C
C GENERATE SERIES OF SUBDAILY ERP VALUES (RESIDUAL FILE FORMAT OF ERPEST)
C -----------------------------------------------------------------------
        IF (DTSAMP.NE.0 .AND. FILNAM(2,IFIL1).NE.' ') THEN
          CALL OPNFIL(LFN001,FILNAM(2,IFIL1),'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(1,IFIL1),'SUBDIF')


          CALL DATE_AND_TIME(datenow,timenow)
          WRITE(LFN001,2001) SUBNA1,TSTRNG,
     1    IDNINT(DTSAMP*1440.D0),datenow(1:4),datenow(5:6),datenow(7:8),
     2    timenow(1:2),timenow(3:4)
2001      FORMAT(A16,':  ',A40,',',I6,' MIN, ',A,'-',A,'-',A,' ',A,
     1       ':',A, /,88('-'),/,
     2       ' OBS.  TYP  EPOCH(MJD)   OBSERV.(mas)  MODEL(mas)   ',
     3       ' O-C(mas)    RESIDUAL(mas)   SIGMA(mas)',/)
C
          DO IEPO=1,NEPO
            TEPO=WINDOW(1)+(IEPO-1)*DTSAMP
            CALL SUBMOD(TEPO,SUBFA1,NSU1,SUBML1,SUBCO1,ERPSU1,ERPSR1)
            DO ICMP=1,3
              WRITE(LFN001,2002) IEPO,ICMP,TEPO,0.D0,ERPSU1(ICMP)*1.D3,
     1                           0.D0,0.D0,0.D0
2002          FORMAT(I6,I4,F13.5,5F13.7)
            ENDDO
          ENDDO
C
          CLOSE(UNIT=LFN001)
        ENDIF
C
C 2ND LOOP OVER MODELS
C --------------------
        DO IFIL2=IFIL1+1,NFIL
C
C READ SUB-DAILY MODEL 2
          CALL RDSUBM(MAXPER,FILNAM(1,IFIL2),TITLE2,SUBNA2,SUBFA2,NSU2,
     1                SUBPE2,SUBML2,SUBCO2)
C
C INDEX
          I12=IKF(IFIL1,IFIL2)
C
C INITIALIZE RMS OF DIFFERENCES
C -----------------------------
          DO ITYP=1,2
            NTERM2(ITYP,I12)=0
            RM2DIF(ITYP,I12)=0.D0
            DO ITRM=1,4
              NTERM(ITYP,ITRM,I12)=0
              MEADIF(ITYP,ITRM,I12)=0.D0
              RMSDIF(ITYP,ITRM,I12)=0.D0
            ENDDO
          ENDDO
C
C PRINT MODELS COMPARED
C ---------------------
          IF (IDFOPT(2).EQ.1) THEN
            WRITE(LFNPRT,1012) SUBNA1,NSU1,SUBNA2,NSU2
1012        FORMAT(1X,'MODEL 1: ',A16,'  NUMBER OF TERMS:',I4,/,
     1             1X,'MODEL 2: ',A16,'  NUMBER OF TERMS:',I4,//,
     2             1X,' TERM  COMP.   MULTIPLIERS        PERI(H)',
     3                '  UNITS     MOD 1    MOD 2   DIFF1-2',/)
            ITERM=0
          ENDIF
C
C COMPUTE RMS DIFFERENCES
C -----------------------
          DO 100 ISU1=1,NSU1
            IADD=0
C
C CHECK IF TERM IN EXCLUSION LIST
            IF (INCEXC.EQ.-1) THEN
              DO 80 IIOE=1,NIOE
                DO IMLT=1,6
                  IF (IOEMLT(IMLT,IIOE).NE.SUBML1(IMLT,ISU1)) GOTO 80
                ENDDO
C TERM TO BE EXCLUDED
                GOTO 100
80            CONTINUE
            ENDIF
C
C CHECK IF TERM IN INCLUSION LIST
            IF (INCEXC.EQ.1) THEN
              DO 85 IIOE=1,NIOE
                DO IMLT=1,6
                  IF (IOEMLT(IMLT,IIOE).NE.SUBML1(IMLT,ISU1)) GOTO 85
                ENDDO
C
C TERM TO BE INCLUDED
                GOTO 87
85            CONTINUE
C TERM NOT FOUND IN LIST: EXCLUDE
              GOTO 100
            ENDIF
C
87          CONTINUE
C
            DO 90 ISU2=1,NSU2
              DO IMLT=1,6
                IF (SUBML2(IMLT,ISU2).NE.SUBML1(IMLT,ISU1)) GOTO 90
              ENDDO
C
C TERM FOUND IN BOTH MODELS
              IADD=1
              GOTO 95
90          CONTINUE
95          CONTINUE
C
C ADD TERMS ONLY PRESENT IN ONE SERIES
            IF (IADD.EQ.0 .AND. IDFOPT(1).EQ.1) IADD=2
C
C ADD TERM TO STATISTICS
            IF (IADD.NE.0) THEN
C
C DIURNAL OR SEMI-DIURNAL, PROGRADE OR RETROGRADE
              IF (SUBML1(6,ISU1).EQ. 1) THEN
                ITRM=1
              ELSEIF (SUBML1(6,ISU1).EQ.-1) THEN
                ITRM=2
              ELSEIF (SUBML1(6,ISU1).EQ. 2) THEN
                ITRM=3
              ELSE
                ITRM=4
              ENDIF
C
              PERIOD=SUBPE1(ISU1)*24.D0
              DO IMLT=1,6
                SUBMLT(IMLT)=SUBML1(IMLT,ISU1)
              ENDDO
C
              DO ITYP=1,2
C
C COMPUTE/PRINT ONLY IF THESE FREQUENCY BAND TO BE INCLUDED
                IF (IFRTYP(ITYP,ITRM).NE.0) THEN
C
C SINE AND COSINE TERM
                  DO ICMP=1,2
C
                    IND=(ITYP-1)*2+ICMP
                    SUBCM1=SUBCO1(IND,ISU1)*1.D6
                    IF (IADD.EQ.1) THEN
                      SUBCM2=SUBCO2(IND,ISU2)*1.D6
                    ELSE
                      SUBCM2=0.D0
                    ENDIF
C
C COMPUTE STATISTICS
                    NTERM(ITYP,ITRM,I12)=NTERM(ITYP,ITRM,I12)+1
C
                    MEADIF(ITYP,ITRM,I12)=MEADIF(ITYP,ITRM,I12)+
     1                                    DABS(SUBCM1-SUBCM2)
                    RMSDIF(ITYP,ITRM,I12)=RMSDIF(ITYP,ITRM,I12)+
     1                                    (SUBCM1-SUBCM2)**2
C
C PRINT DIFFERENCES
                    IF (IDFOPT(2).EQ.1) THEN
                      ITERM=ITERM+1
                      WRITE(LFNPRT,1013) ITERM,ERPTYP(ITYP),
     1                                   SINCOS(ICMP),
     2                                   (SUBMLT(II),II=1,6),PERIOD,
     3                                   UNITS(ITYP),
     4                                   SUBCM1,SUBCM2,SUBCM1-SUBCM2
1013                  FORMAT(I5,3X,A2,1X,A3,6I3,3X,F6.3,3X,A5,1X,3F9.2)
                    ENDIF
C
                  ENDDO
C
                ENDIF
              ENDDO
            ENDIF
100       CONTINUE
C
C ADD TERM ONLY PRESENT IN SECOND MODEL (IF OPTION SET ACCORDINGLY)
          DO 200 ISU2=1,NSU2
            IADD=0
C
C CHECK IF TERM IN EXCLUSION LIST
            IF (INCEXC.EQ.-1) THEN
              DO 180 IIOE=1,NIOE
                DO IMLT=1,6
                  IF (IOEMLT(IMLT,IIOE).NE.SUBML2(IMLT,ISU2)) GOTO 180
                ENDDO
C TERM TO BE EXCLUDED
                GOTO 200
180           CONTINUE
            ENDIF
C
C CHECK IF TERM IN INCLUSION LIST
            IF (INCEXC.EQ.1) THEN
              DO 185 IIOE=1,NIOE
                DO IMLT=1,6
                  IF (IOEMLT(IMLT,IIOE).NE.SUBML2(IMLT,ISU2)) GOTO 185
                ENDDO
C
C TERM TO BE INCLUDED
                GOTO 187
185           CONTINUE
C TERM NOT FOUND IN LIST: EXCLUDE
              GOTO 200
            ENDIF
C
187         CONTINUE
C
            DO 190 ISU1=1,NSU1
              DO IMLT=1,6
                IF (SUBML1(IMLT,ISU1).NE.SUBML2(IMLT,ISU2)) GOTO 190
              ENDDO
              IADD=1
190         CONTINUE
C
C ADD TERMS ONLY PRESENT IN ONE SERIES
            IF (IADD.EQ.0 .AND. IDFOPT(1).EQ.1) IADD=2
C
C ADD TERM TO STATISTICS
            IF (IADD.EQ.2) THEN
C
C DIURNAL OR SEMI-DIURNAL, PROGRADE OR RETROGRADE
              IF (SUBML2(6,ISU2).EQ. 1) THEN
                ITRM=1
              ELSEIF (SUBML2(6,ISU2).EQ.-1) THEN
                ITRM=2
              ELSEIF (SUBML2(6,ISU2).EQ. 2) THEN
                ITRM=3
              ELSE
                ITRM=4
              ENDIF
C
              PERIOD=SUBPE1(ISU2)*24.D0
              DO IMLT=1,6
                SUBMLT(IMLT)=SUBML2(IMLT,ISU2)
              ENDDO
C
              DO ITYP=1,2
C
C COMPUTE/PRINT ONLY IF THESE FREQUENCY BAND TO BE INCLUDED
                IF (IFRTYP(ITYP,ITRM).NE.0) THEN
C
C SINE AND COSINE TERM
                  DO ICMP=1,2
C
                    IND=(ITYP-1)*2+ICMP
                    SUBCM1=0.D0
                    SUBCM2=SUBCO2(IND,ISU2)*1.D5
C
C COMPUTE STATISTICS
                    NTERM(ITYP,ITRM,I12)=NTERM(ITYP,ITRM,I12)+1
C
                    MEADIF(ITYP,ITRM,I12)=MEADIF(ITYP,ITRM,I12)+
     1                                    DABS(SUBCM1-SUBCM2)
                    RMSDIF(ITYP,ITRM,I12)=RMSDIF(ITYP,ITRM,I12)+
     1                                    (SUBCM1-SUBCM2)**2
C
C PRINT DIFFERENCES
                    IF (IDFOPT(2).EQ.1) THEN
                      ITERM=ITERM+1
                      WRITE(LFNPRT,1013) ITERM,ERPTYP(ITYP),
     1                                   SINCOS(ICMP),
     2                                   (SUBMLT(II),II=1,6),PERIOD,
     3                                   UNITS(ITYP),
     4                                   SUBCM1,SUBCM2,SUBCM1-SUBCM2
                    ENDIF
C
                  ENDDO
C
                ENDIF
              ENDDO
            ENDIF
200       CONTINUE
C
          IF (IDFOPT(2).EQ.1) WRITE(LFNPRT,'(/)')
C
C COMPUTE TOTAL STATISTICS
C ------------------------
          DO ITYP=1,2
            NTERMT(ITYP,I12)=0
            MEADTT(ITYP,I12)=0.D0
            RMSDTT(ITYP,I12)=0.D0
            DO ITRM=1,4
              NTERMT(ITYP,I12)=NTERMT(ITYP,I12)+ NTERM(ITYP,ITRM,I12)
              MEADTT(ITYP,I12)=MEADTT(ITYP,I12)+MEADIF(ITYP,ITRM,I12)
              RMSDTT(ITYP,I12)=RMSDTT(ITYP,I12)+RMSDIF(ITYP,ITRM,I12)
            ENDDO
          ENDDO
C
          DO ITYP=1,2
            IF (NTERMT(ITYP,I12).NE.0) THEN
              MEADTT(ITYP,I12)=MEADTT(ITYP,I12)/NTERMT(ITYP,I12)
              RMSDTT(ITYP,I12)=DSQRT(RMSDTT(ITYP,I12)/NTERMT(ITYP,I12))
            ENDIF
C
            DO ITRM=1,4
              IF (NTERM(ITYP,ITRM,I12).NE.0) THEN
                MEADIF(ITYP,ITRM,I12)=
     1            MEADIF(ITYP,ITRM,I12)/NTERM(ITYP,ITRM,I12)
                RMSDIF(ITYP,ITRM,I12)=
     1            DSQRT(RMSDIF(ITYP,ITRM,I12)/NTERM(ITYP,ITRM,I12))
              ENDIF
            ENDDO
          ENDDO
C
C COMPUTE RMS DIFFERENCES OVER A SPECIFIED TIME INTERVAL
C ------------------------------------------------------
          IF (DTSAMP.NE.0) THEN
            DO IEPO=1,NEPO
              TEPO=WINDOW(1)+(IEPO-1)*DTSAMP
              CALL SUBMOD(TEPO,SUBFA1,NSU1,SUBML1,SUBCO1,ERPSU1,ERPSR1)
              CALL SUBMOD(TEPO,SUBFA2,NSU2,SUBML2,SUBCO2,ERPSU2,ERPSR2)
C X-POLE AND Y-POLE
              NTERM2(1,I12)=NTERM2(1,I12)+2
              RM2DIF(1,I12)=RM2DIF(1,I12)+(ERPSU1(1)-ERPSU2(1))**2
     1                                   +(ERPSU1(2)-ERPSU2(2))**2
C UT
              NTERM2(2,I12)=NTERM2(2,I12)+1
              RM2DIF(2,I12)=RM2DIF(2,I12)+(ERPSU1(3)-ERPSU2(3))**2
            ENDDO
C
            DO ITYP=1,2
              RM2DIF(ITYP,I12)=
     1          DSQRT(RM2DIF(ITYP,I12)/NTERM2(ITYP,I12))*1.D6
            ENDDO
          ENDIF
C
C END FIRST AND SECOND LOOP
        ENDDO
      ENDDO
C
C PRINT MODEL NAMES:
C -----------------
      WRITE(LFNPRT,1021) NFIL
1021  FORMAT(1X,'LIST OF MODELS USED:',/,
     1       1X,'-------------------',//,
     2       1X,'NUMBER OF MODELS:',I4,//,
     3       1X,'FILE  MODEL NAME        TITLE',/)
C
      DO IFIL=1,NFIL
        WRITE(LFNPRT,1022) IFIL,SUBNAM(IFIL),TITLE(IFIL)
1022    FORMAT(I4,3X,A16,2X,A)
      ENDDO
C
      WRITE(LFNPRT,'(/)')
C
C PRINT RMS DIFFERENCES
C ---------------------
      WRITE(LFNPRT,1003)
1003  FORMAT(1X,'TOTAL STATISTICS:',/,
     1       1X,'----------------',//,
     2       1X,'MODEL 1           MODEL 2           PARAMETERS',
     3          '           #FREQ   UNITS     MEAN ABS',
     4          '     UNWEIGHTED',/)
C
      DO ITYP=1,2
        DO IFIL1=1,NFIL
          DO IFIL2=IFIL1+1,NFIL
            I12=IKF(IFIL1,IFIL2)
C
            IF (DTSAMP.NE.0 .AND. NTERM2(ITYP,I12).NE.0) THEN
              WRITE(LFNPRT,1004) SUBNAM(IFIL1),SUBNAM(IFIL2),
     1                           PTXTT2(ITYP),MIN(NSU1,NSU2),
     2                           UNITS(ITYP),0.D0,
     3                           RM2DIF(ITYP,I12)
1004          FORMAT(1X,A16,2X,A16,2X,A20,I5,5X,A5,F10.2,F14.2)
            ENDIF
          ENDDO
        ENDDO
C
        IF (NFIL.GT.2) WRITE(LFNPRT,'( )')
      ENDDO
C
      IF (DTSAMP.NE.0) WRITE(LFNPRT,'( )')
C
      DO ITYP=1,2
        DO IFIL1=1,NFIL
          DO IFIL2=IFIL1+1,NFIL
            I12=IKF(IFIL1,IFIL2)
C
            IF (NTERMT(ITYP,I12).NE.0) THEN
              WRITE(LFNPRT,1004) SUBNAM(IFIL1),SUBNAM(IFIL2),
     1                           PTXTTT(ITYP),NTERMT(ITYP,I12),
     2                           UNITS(ITYP),MEADTT(ITYP,I12),
     3                           RMSDTT(ITYP,I12)
            ENDIF
          ENDDO
        ENDDO
C
        IF (NFIL.GT.2) WRITE(LFNPRT,'( )')
      ENDDO
C
      WRITE(LFNPRT,'( )')
C
C INDIVIDUAL BANDS
C ----------------
      DO ITYP=1,2
        DO ITRM=1,4
          DO IFIL1=1,NFIL
            DO IFIL2=IFIL1+1,NFIL
              I12=IKF(IFIL1,IFIL2)
              IF (NTERM(ITYP,ITRM,I12).NE.0) THEN
                WRITE(LFNPRT,1004) SUBNAM(IFIL1),SUBNAM(IFIL2),
     1                             PARTXT(ITYP,ITRM),
     2                             NTERM(ITYP,ITRM,I12),
     3                             UNITS(ITYP),MEADIF(ITYP,ITRM,I12),
     4                             RMSDIF(ITYP,ITRM,I12)
              ENDIF
            ENDDO
          ENDDO
          IF (NFIL.GT.2) WRITE(LFNPRT,'( )')
        ENDDO
      ENDDO
C
      WRITE(LFNPRT,'(/)')

! Deallocate Memory
! -----------------
      DEALLOCATE (title,stat=iac)
      DEALLOCATE (subnam,stat=iac)
      DEALLOCATE (rmsdif,stat=iac)
      DEALLOCATE (meadif,stat=iac)
      DEALLOCATE (rmsdtt,stat=iac)
      DEALLOCATE (meadtt,stat=iac)
      DEALLOCATE (rm2dif,stat=iac)
      DEALLOCATE (nterm,stat=iac)
      DEALLOCATE (ntermt,stat=iac)
      DEALLOCATE (nterm2,stat=iac)
C END
C ---
      CALL EXITRC(0)
      END
