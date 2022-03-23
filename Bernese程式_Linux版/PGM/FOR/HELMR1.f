C*
      PROGRAM HELMR1
CC
CC NAME       :  HELMR1
CC
CC PURPOSE    :  HELMERT TRANSFORMATION BETWEEN TWO SETS OF STATION
CC               COORDINATES (SITES TO BE EXCLUDED CAN BE INDICATED)
CC
CC REMARKS    :  --> INTERACTIVE PANEL VERSION <--
CC
CC AUTHOR     :  M.ROTHACHER / W. GURTNER / E. BROCKMANN
CC
CC VERSION    :  3.5  (JAN 93)
CC
CC CREATED    :  88/01/13 09:00
CC
CC CHANGES    :  16-JUL-92 : ALLOW NON-INTERACTIVE MODE
CC               19-MAR-95 : MR: PROGRAM NAME "HELMR1"
CC                3-JAN-96 : EB: OPTIONS: VELOC., USE STATION LIST,
CC                                        TRANSFORMATION OF CRD.
CC                               AND NEW CALL HELMTR
CC                5-NOV-98 : TS: WRITE TRANSFORMATION OUTPUT (FOR IGS COMBO)
CC               04-AUG-99 : PF: USE NEW SR WTSTAT AND GETCOO
CC               15-MAR-02 : SS: WRITE MESSAGE IN CASE OF UNCREATED
CC                               COORDINATE OUTPUT FILE
CC               16-MAR-03 : HU: OUTLIER REJECTION
CC               24-APR-03 : RD: CORRECTED FORMAT STATEMENT
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               19-FEB-04 : HU: STOP IF NO REDUNDANCY
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-FEB-06 : HU: CALL TO HELMTR CHANGED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON,
CC                               CALL DEFCON WITH PARAMETER
CC               21-JUL-09 : DT: ADD RMS/COMPONENT IN CALL TO HELMTR
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               24-JAN-11 : RD: USE GETCO3 INSTEAD OF GETCOO
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               22-FEB-11 : SS: REDEFINED TITLE1/TITLE2
CC               26-JUL-11 : SS: USE PURE FILENAME FOR TITLE1/TITLE2
CC               28-FEB-13 : SS: REVISED WARNING MESSAGE CONCERNING
CC                               DIFFERENT GEODETIC DATES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: filTitle, ars
      USE s_wtstat
      USE s_opnfil
      USE s_crdvel
      USE s_gtvelo
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_hminpi
      USE s_getco3
      USE s_getdat
      USE s_helmtr
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_jmt
      USE s_gtflna
      USE s_alcerr
      USE s_fparse
      USE f_djul
      USE f_lengt1

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDOF  , IM    , INUV1A, IOPT  , IOSTAT, IOUTL ,
     1          IRC   , IRCTRN, IRCVEL, IRETRN, ISTA  , ISTAT1, ISTAT2,
     2          IUNIT , IY    , J     , NFLAG , NFLGVE, NSTAT ,
     3          NSTAT1, NSTAT2, IAC   , MAXSTA
C
      REAL*8    AELL  , BELL  , D     , DAYY  , RMSHLM, SCELL ,
     1          TIMCO1, TIMCO2, YEAR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
C
C MAXSTA: MAXIMUM NUMBER OF STATIONS
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=6),              PARAMETER :: pgName = 'HELMR1'
      CHARACTER(LEN=shortLineLength)           :: TITLE1
      CHARACTER(LEN=shortLineLength)           :: TITLE2
      CHARACTER(LEN=fileNameLength)            :: FILNA1
      CHARACTER(LEN=fileNameLength)            :: FILNA2
      CHARACTER(LEN=fileNameLength)            :: FILCOR
      CHARACTER(LEN=fileNameLength)            :: FILTRN
      CHARACTER(LEN=fileNameLength)            :: NODE
      CHARACTER(LEN=fileNameLength)            :: DEVICE
      CHARACTER(LEN=fileNameLength)            :: DIR
      CHARACTER(LEN=fileNameLength)            :: NAME
      CHARACTER(LEN=fileNameLength)            :: EXT
      CHARACTER(LEN=fileNameLength)            :: VER
      CHARACTER(LEN=16)                        :: DATUM
      CHARACTER(LEN=16)                        :: DATUM2
      CHARACTER(LEN=6)                         :: FILKEY = 'VELAPR'
      CHARACTER(LEN=staNameLength),POINTER,    DIMENSION(:)   :: STNAM1
      CHARACTER(LEN=staNameLength),POINTER,    DIMENSION(:)   :: STNAM2
      CHARACTER(LEN=staNameLength),ALLOCATABLE,DIMENSION(:)   :: STNAME
      CHARACTER(LEN=staFlagLength),POINTER,    DIMENSION(:)   :: STFLG1
      CHARACTER(LEN=staFlagLength),POINTER,    DIMENSION(:)   :: STFLG2
      CHARACTER(LEN=staFlagLength),ALLOCATABLE,DIMENSION(:,:) :: STAFLG
      CHARACTER(LEN=staFlagLength),ALLOCATABLE,DIMENSION(:)   :: VELFLG
      CHARACTER(LEN=staFlagLength),            DIMENSION(1)   :: FLAGS
      INTEGER(i4b), POINTER,     DIMENSION(:)   :: STANU1
      INTEGER(i4b), POINTER,     DIMENSION(:)   :: STANU2
      INTEGER(i4b), ALLOCATABLE, DIMENSION(:)   :: STANUM
      INTEGER(i4b), ALLOCATABLE, DIMENSION(:)   :: ITYP
      INTEGER(i4b),              DIMENSION(7)   :: IP
      REAL(r8b),    POINTER,     DIMENSION(:,:) :: XSTAT1
      REAL(r8b),    POINTER,     DIMENSION(:,:) :: XSTAT2
      REAL(r8b),    ALLOCATABLE, DIMENSION(:,:) :: XVEL
      REAL(r8b),    ALLOCATABLE, DIMENSION(:,:) :: XSTA1
      REAL(r8b),    ALLOCATABLE, DIMENSION(:,:) :: XSTA2
      REAL(r8b),    ALLOCATABLE, DIMENSION(:,:) :: XSTNEW
      REAL(r8b),                 DIMENSION(3)   :: DXELL
      REAL(r8b),                 DIMENSION(3)   :: DRELL
      REAL(r8b),                 DIMENSION(7)   :: PAR
      REAL(r8b),                 DIMENSION(7)   :: RMSPAR
      REAL(r8b),                 DIMENSION(7)   :: Z1
      REAL(r8b),                 DIMENSION(7)   :: ZS
      REAL(r8b),                 DIMENSION(3,3) :: ROT
      REAL(r8b),                 DIMENSION(3)   :: RESMX
      REAL(r8b),                 DIMENSION(3)   :: VEL
      REAL(r8b),                 DIMENSION(3)   :: RMSS
      LOGICAL,      ALLOCATABLE, DIMENSION(:)   :: VFOUND
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(STNAM1)
      NULLIFY(STNAM2)
      NULLIFY(STFLG1)
      NULLIFY(STFLG2)
      NULLIFY(STANU1)
      NULLIFY(STANU2)
      NULLIFY(XSTAT1)
      NULLIFY(XSTAT2)
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
C CONSTANTS
C ---------
      CALL DEFCON(1)
C
C TITLE
C -----
      CALL pritit ('HELMR1', 'Helmert Transformation')
C
C COORDINATE FILE NAMES
C ---------------------
      CALL GTFLNA(1,'COORD1 ',FILNA1,IRC)
      CALL GTFLNA(1,'COORD2 ',FILNA2,IRC)
C
C READ ALL STATIONS WITH FLAG "NON-BLANK" FROM COORDINATE FILE 1 AND 2
C --------------------------------------------------------------------
      NFLAG=1
      FLAGS(1)='#'
C
C FILE2: "TRANSFORMATION FILE"
      CALL GETCO3(FILNA2,NFLAG,FLAGS,NSTAT2,STNAM2,
     1            stanum=STANU2,staflg=STFLG2,xstat=XSTAT2,
     2            datum=DATUM2,title=TITLE2,timcrd=TIMCO2)
C FILE1: "REFERENCE"
      CALL GETCO3(FILNA1,NFLAG,FLAGS,NSTAT1,STNAM1,
     1            stanum=STANU1,staflg=STFLG1,xstat=XSTAT1,
     2            datum=DATUM,title=TITLE1,timcrd=TIMCO1)
C
      IF (DATUM.NE.DATUM2) THEN
        WRITE(LFNERR,'(/,A,2(/,16X,A,A),/)')
     1    ' ### PG HELMR1: DIFFERENT GEODETIC DATES FOUND',
     2                    'FIRST  DATUM: ',TRIM(DATUM),
     3                    'SECOND DATUM: ',TRIM(DATUM2)
      ENDIF
C
      CALL GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C REDEFINE TITLE1/TITLE2
      CALL FPARSE(0,FILNA1,NODE,DEVICE,DIR,NAME,EXT,VER,IRC)
      IF (IRC.EQ.0) THEN
        NAME=TRIM(NAME)//'.'//TRIM(EXT)
        TITLE1=NAME(1:LENGT1(NAME))//': '//TITLE1(1:78-LENGT1(NAME))
      ENDIF
      CALL FPARSE(0,FILNA2,NODE,DEVICE,DIR,NAME,EXT,VER,IRC)
      IF (IRC.EQ.0) THEN
        NAME=TRIM(NAME)//'.'//TRIM(EXT)
        TITLE2=NAME(1:LENGT1(NAME))//': '//TITLE2(1:78-LENGT1(NAME))
      ENDIF
C
C GET VELOCITIES /VELOCITY MODEL IF SPECIFIED IN N-FILE WITH
C FIXVEL (VELOCITY FILE)
C ---------------------------------------------------------------
      NFLGVE=1
      FLAGS(1)='@'
      INUV1A=0
C
      ALLOCATE(XVEL(3,NSTAT1),STAT=IAC)
      CALL ALCERR(IAC,'XVEL',(/3,NSTAT1/),PGNAME)
      XVEL=0D0
      ALLOCATE(VELFLG(NSTAT1),STAT=IAC)
      CALL ALCERR(IAC,'VELFLG',(/NSTAT1/),PGNAME)
      VELFLG=''
      ALLOCATE(VFOUND(NSTAT1),STAT=IAC)
      CALL ALCERR(IAC,'VFOUND',(/NSTAT1/),PGNAME)
      VFOUND=.FALSE.
C
      CALL GTVELO(FILKEY,INUV1A,NFLGVE,FLAGS,NSTAT1,XSTAT1,STNAM1,
     1            XVEL,VELFLG,VFOUND,IRCVEL)
C
C APPLY VELOCITIES TO FILE 1
C --------------------------
      IF (IRCVEL.EQ.0) THEN
        CALL CRDVEL(NSTAT1,XSTAT1,XVEL,VFOUND,TIMCO2,TIMCO1)
      ENDIF
C
C ALLOCATE THE RESULT ARRAYS
C --------------------------
      MAXSTA=MAX(NSTAT1,NSTAT2)
C
      ALLOCATE(STNAME(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'STNAME',(/MAXSTA/),PGNAME)
      STNAME=''
C
      ALLOCATE(STANUM(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'STANUM',(/MAXSTA/),PGNAME)
      STANUM=0
C
      ALLOCATE(STAFLG(2,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'STAFLG',(/2,MAXSTA/),PGNAME)
      STAFLG=''
C
      ALLOCATE(XSTA1(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'XSTA1',(/3,MAXSTA/),PGNAME)
      XSTA1=0d0
C
      ALLOCATE(XSTA2(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'XSTA2',(/3,MAXSTA/),PGNAME)
      XSTA2=0d0
C
      ALLOCATE(ITYP(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'ITYP',(/MAXSTA/),PGNAME)
      ITYP=0
C
      ALLOCATE(XSTNEW(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'XSTNEW',(/3,MAXSTA/),PGNAME)
      XSTNEW=0d0
C
C
C FIND THE INTERSECTION SET OF THE TWO STATION SETS
C -------------------------------------------------
      NSTAT=0
      DO 100 ISTAT1=1,NSTAT1
C
C FIND STATION OF FILE 1 IN FILE 2
        DO 10 ISTAT2=1,NSTAT2
          IF(STNAM2(ISTAT2).EQ.STNAM1(ISTAT1)) GOTO 20
10      CONTINUE
        GOTO 100
C
C STATION IN BOTH FILES
20      NSTAT=NSTAT+1
        STNAME(NSTAT)=STNAM1(ISTAT1)
        STANUM(NSTAT)=STANU1(ISTAT1)
        STAFLG(1,NSTAT)=STFLG1(ISTAT1)
        STAFLG(2,NSTAT)=STFLG2(ISTAT2)
        DO 30 I=1,3
          XSTA1(I,NSTAT)=XSTAT1(I,ISTAT1)
          XSTA2(I,NSTAT)=XSTAT2(I,ISTAT2)
30      CONTINUE
100   CONTINUE
C
C INPUT TRANSFORMATION OPTIONS; MARK OR EXCLUDE STATIONS
C ------------------------------------------------------
C
C  ROUTINE TOGETHER WITH MENU SYSTEM
      CALL HMINPI(NSTAT,STNAME,STANUM,STAFLG,
     1            ITYP,IOPT,IP,IUNIT,IOUTL,RESMX,IRETRN)
      IF(IRETRN.NE.0) GOTO 999
cc    CALL OPNSYS
C
C HELMERT TRANSFORMATION
C ----------------------
      VEL=0D0
      CALL HELMTR(TITLE1,TITLE2,NSTAT,STNAME,STANUM,STAFLG,XSTA2,XSTA1,
     1            VEL,ITYP,IP,IOPT,IUNIT,DATUM,AELL,BELL,DXELL,DRELL,
     2            SCELL,LFNPRT,2,IOUTL,RESMX,PAR,RMSPAR,RMSHLM,IDOF,
     3            RMSS)
C
C Write IGSCOMBO transformation file
C ----------------------------------
      CALL GTFLNA(0,'TRNFIL ',FILTRN,IRCTRN)
      IF (irctrn .eq. 0) THEN
        CALL OPNFIL(LFNRES,FILTRN,'UNKNOWN','FORMATTED',
     1  ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILTRN,'HELMR1')
C
        write(lfnres,301)filna1(17:22),filna2(17:19)
301     format('APRIO',9X,A6,2X,A3)
        do i=1,3
          z1(i)=par(i)*1d2
          zs(i)=rmspar(i)*1d2
          z1(i+4)=par(I+3)*ars*1.D3
          zs(i+4)=rmspar(i+3)*ars*1.d3
        enddo
        z1(4)=(par(7)-1d0)*1d6
        zs(4)=rmspar(7)*1d9
        call jmt(timco2,iy,im,d)
        dayy=timco2-djul(iy,1,1d0)+1d0
        year=iy+dayy/365.25d0
c
        write(lfnres,302)year,z1
        do i=1,7
          z1(i)=0d0
        enddo
        write(lfnres,303)z1
        write(lfnres,304)zs
302     format(8f10.5,'    cm(T_XYZ),ppb(SC),mas(R_XYZ)')
303     format(10x,7f10.5,
     1        '    cm(T_XYZ_rates),ppb(SC_rate),mas/y(R_XYZ_rates)')
304     format(10x,7f10.5,
     1        '    cm(T_XYZ_sig),ppb(SC_sig),mas(R_XYZ_sig)')
        close(lfnres)
      END IF
C
C ROTATION-MATRIX (SMALL ANGLES ONLY)
C -----------------------------------
      IF (IOPT.EQ.2) THEN
        ROT(1,1)=1.D0
        ROT(1,2)= PAR(6)
        ROT(1,3)=-PAR(5)
        ROT(2,1)=-PAR(6)
        ROT(2,2)=1.D0
        ROT(2,3)= PAR(4)
        ROT(3,1)= PAR(5)
        ROT(3,2)=-PAR(4)
        ROT(3,3)=1.D0
C
C TRANSFORMATION INTO NEW SYSTEM
C ------------------------------
        DO 130 ISTA=1,NSTAT2
          DO 120 I=1,3
            XSTNEW(I,ISTA)=0.D0
            DO 110 J=1,3
              XSTNEW(I,ISTA)=XSTNEW(I,ISTA)+ROT(I,J)*XSTAT2(J,ISTA)
110         CONTINUE
            XSTNEW(I,ISTA)=XSTNEW(I,ISTA)*PAR(7)+PAR(I)
120       CONTINUE
130     CONTINUE
C
        FILCOR=' '
C
      ENDIF
C
C WRITE NEW COORDINATE FILE
      IF (IOPT.EQ.2) THEN
        CALL WTSTAT(1,FILCOR,TITLE2,DATUM,NSTAT2,STNAM2,XSTNEW,
     1              STANU2,STFLG2,TIMCO2)
      ELSE
        CALL GTFLNA(0,'COORDRS',FILCOR,IRC)
        IF(IRC.EQ.0) THEN
          WRITE(LFNERR,901)
901       FORMAT(/,' ### PG HELMR1: COORDINATE OUTPUT FILE NOT ',
     1             'CREATED',/,
     2         16X,'TRANSFORMATION MUST BE DONE IN X/Y/Z, NOT N/E/U ',
     3             'SYSTEM',/)
        ENDIF
      ENDIF
C
C WRITE NEW FIX FILE
C ------------------
      CALL GTFLNA(0,'OKSTA',FILTRN,IRC)
      IF (IRC .EQ. 0) THEN
C STOP IN CASE NO REDUNDANCY IS AVAILABLE
        IF (IDOF.LE.0) THEN
          WRITE(LFNERR,"(/,' *** PGM HELMR1: NO REDUNDANCY. ',
     1                     'NO VERIFICATION OF SITES POSSIBLE',/)")
          CALL EXITRC(2)
        ENDIF
        CALL OPNFIL(LFNRES,FILTRN,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILTRN,'HELMR1')
        WRITE(LFNRES,"(A,
     1               /,80('-'),
     2              //,'Station name',
     3               /,'****************')") filTitle
        DO ISTA=1,NSTAT
          IF (ITYP(ISTA).EQ.0) THEN
            WRITE(LFNRES,"(A)") STNAME(ISTA)
          ENDIF
        ENDDO
        WRITE(LFNRES,"(1X)")
      ENDIF
C
999   CALL EXITRC(0)
      END
