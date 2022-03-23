C*
      PROGRAM ETRS89
CC
CC PURPOSE    :  TRANSFORM COORDINATES GIVEN IN ITRFyy AT EPOCH TC
CC               INTO ETRFxx
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  19-NOV-96
CC
CC CHANGES    :  04-AUG-99 : PF: ADAPTED CALL TO SR GETCOO (TIMCRD ADDED)
CC               15-OCT-01 : HB: SWITCH TO NEW MENU
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               04-MAR-03 : MR: CORRECT CALL FOR WTCOOR
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               02-JAN-04 : HU: MAXSTA INCREASED FROM 200 TO 400
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               14-JUN-05 : SS: USE TIMCRD (EPOCH VARIABLE REMOVED)
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-AUG-05 : SS: INVERSE TRANSFORMATION
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               04-FEB-11 : RD: USE GETCO3/WTSTAT INSTEAD OF GETCOO/WTCOOR
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               14-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               14-DEC-11 : SL: USE ITRF2[IE]TRF
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, shortLineLength, fileNameLength,
     1                    staNameLength, staFla2Length, staFlagLength,
     1                    lfnPrt
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_prflna
      USE s_wtstat
      USE s_defcon
      USE s_exitrc
      USE s_pritit
      USE s_opnsys
      USE s_etrsin
      USE s_gtflna
      USE s_readinpf
      USE s_getco3
      USE s_alcerr
      USE s_itrf2itrf
      USE s_itrf2etrf
      USE f_djul
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IRC   , ISTA  , J     , NFLAG , NSTAT,
     1          IAC
C
      REAL*8    DT    , TIMCRD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXIMUM DIMENSIONS
C ------------------
C
C MAXSTA : MAXIMUM NUMBER OF STATIONS
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=6),          PARAMETER      :: PGNAME = 'ETRS89'
C
      CHARACTER(LEN=shortLineLength)            :: TITNEW
      CHARACTER(LEN=fileNameLength)             :: FILINP,FILCOR
      CHARACTER(LEN=staNameLength),
     1              POINTER,     DIMENSION(:)   :: stName
      CHARACTER(LEN=16)                         :: DATUM,DATNEW
      CHARACTER(LEN=staFla2Length),
     1              POINTER,     DIMENSION(:)   :: STAFLG
      CHARACTER(LEN=staFlagLength),
     1                           DIMENSION(1)   :: FLAGS
C
      INTEGER(i4b), POINTER,     DIMENSION(:)   :: STANUM
C
      REAL(r8b),    POINTER,     DIMENSION(:,:) :: XSTAT
      REAL(r8b),    ALLOCATABLE, DIMENSION(:,:) :: XSTNEW
      REAL(r8b),                 DIMENSION(3)   :: XSTHLP
      REAL(r8b),                 DIMENSION(3,3) :: ROT
      REAL(r8b),                 DIMENSION(3)   :: DXYZ
      REAL(r8b),                 DIMENSION(3)   :: DROT
      INTEGER(i4b)                              :: TR
C
C NULLIFY THE POINTERS
C --------------------
      NULLIFY(STNAME)
      NULLIFY(STANUM)
      NULLIFY(XSTAT)
      NULLIFY(STAFLG)
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
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
C Write title and file list
C -------------------------
      CALL pritit('ETRS89','Transform coordinates to ETRS89 frame')
      CALL prflna
C
C READ INPUT OPTIONS (TRANSFORMATION PARAMETERS ...)
C --------------------------------------------------
      DXYZ(:) = 0d0
      DROT(:) = 0d0
      CALL ETRSIN(TITNEW,DATNEW,DXYZ,DROT)
C
C GET INPUT FILE NAME
C -------------------
      CALL GTFLNA(1,'COORD  ',FILINP,IRC)
C
C READ ALL STATIONS FROM INPUT COORDINATE FILE
C --------------------------------------------
      NFLAG=1
      FLAGS(1)='@'
      CALL GETCO3(FILINP,NFLAG,FLAGS,NSTAT,STNAME,
     1            STANUM=STANUM,STAFLG5=STAFLG,XSTAT=XSTAT,
     1            DATUM=DATUM,TIMCRD=TIMCRD)
C
C CHECK DATUM STRINGS
C -------------------
      IF(DATUM(1:1) == 'I') THEN
        DATNEW = 'E'//DATNEW
        TR = 1
      ELSEIF(DATUM(1:1) == 'E') THEN
        DATNEW = 'I'//DATNEW
        TR = -1
      ENDIF
      WRITE(lfnPrt,*) TRIM(DATUM),' -> ',TRIM(DATNEW)
C
C ALLOCATE THE RESULT ARRAY
C -------------------------
      ALLOCATE(XSTNEW(3,NSTAT),STAT=IAC)
      CALL ALCERR(IAC,'XSTNEW',(/3,NSTAT/),PGNAME)
C
C ROTATION-MATRIX (SMALL ANGLES ONLY)
C -----------------------------------
      ROT(1,1)=0.D0
      ROT(1,2)=-DROT(3)
      ROT(1,3)= DROT(2)
      ROT(2,1)= DROT(3)
      ROT(2,2)=0.D0
      ROT(2,3)=-DROT(1)
      ROT(3,1)=-DROT(2)
      ROT(3,2)= DROT(1)
      ROT(3,3)=0.D0
C
C TIME FACTOR (EPOCH - 1989.0 IN YEARS)
C -------------------------------------
      DT=(TIMCRD-DJUL(1989,1,0.D0))/365.2422
C
C TRANSFORMATION INTO NEW SYSTEM
C ------------------------------
      DO ISTA=1,NSTAT
        IF(SQRT(SUM(DXYZ*DROT)).NE.0d0) THEN
C USE PARAMETERS FROM PANEL
          DO 20 I=1,3
            XSTNEW(I,ISTA)=XSTAT(I,ISTA)
            DO 10 J=1,3
              XSTNEW(I,ISTA)=XSTNEW(I,ISTA)+ROT(I,J)*DT*XSTAT(J,ISTA)
10          CONTINUE
            XSTNEW(I,ISTA)=XSTNEW(I,ISTA)+DXYZ(I)
20        CONTINUE
        ELSE
C USE PARAMETERS FROM MEMO V8
          IF(TR.EQ.1) THEN
            CALL ITRF2ITRF(TIMCRD,DATUM,DATNEW,XSTAT(:,ISTA),XSTHLP)
            CALL ITRF2ETRF(TIMCRD,DATNEW,TR,XSTHLP,XSTNEW(:,ISTA))
          ELSEIF(TR.EQ.-1) THEN
            CALL ITRF2ETRF(TIMCRD,DATUM,TR,XSTAT(:,ISTA),XSTHLP)
            CALL ITRF2ITRF(TIMCRD,DATUM,DATNEW,XSTHLP,XSTNEW(:,ISTA))
          ENDIF
        ENDIF
      ENDDO
C
C WRITE NEW COORDINATE FILE
C -------------------------
      CALL GTFLNA(1,'COORDRS',FILCOR,IRC)
      CALL WTSTAT(0,FILCOR,TITNEW,DATNEW,NSTAT,STNAME,XSTNEW,
     1            STANUM,STAFLG,TIMCRD)
C
      CALL EXITRC(0)
C
      END PROGRAM ETRS89
