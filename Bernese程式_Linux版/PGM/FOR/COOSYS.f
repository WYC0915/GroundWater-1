C*
      PROGRAM COOSYS
CC
CC NAME       :  COOSYS
CC
CC PURPOSE    :  TRANSFORM COORDINATES ACCORDING TO TRANSFORMATION
CC               PARAMETERS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  89/02/02 15:21
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               06-JUN-96 : MR: REMOVED UNUSED VARIABLES
CC               06-JUN-99 : PF: CALL GETCOO MODIFIED (TIMCRD added)
CC               18-SEP-01 : MM: SWITCH TO NEW MENU
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               19-JAN-11 : RD: CHANGE FROM GETCOO TO GETCO3
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, staNameLength
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
C
      USE s_wtstat
      USE s_cosyin
      USE s_stripdir
      USE s_defcon
      USE s_pritit
      USE s_exitrc
      USE s_opnsys
      USE s_gtflna
      USE s_readinpf
      USE s_getco3
      USE s_alcerr

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IRC   , ISTA  , J     , NFLAG , NSTAT, IAC
C
      REAL*8    TIMCRD
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
      CHARACTER*80 TITNEW
      CHARACTER*32 FILINP, FILCOR
      CHARACTER*16 DATUM
      CHARACTER(LEN=staNameLength), POINTER, DIMENSION(:) :: STNAME
      CHARACTER(LEN=1),             POINTER, DIMENSION(:) :: STAFLG
      CHARACTER*1  FLAGS(1)
C
      INTEGER(i4b), POINTER, DIMENSION(:) :: STANUM
C
      REAL(r8b), POINTER, DIMENSION(:,:)     :: XSTAT
      REAL(r8b), ALLOCATABLE, DIMENSION(:,:) :: XSTNEW
      REAL*8 ROT(3,3),DXYZ(3),DROT(3), DSCALE
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(STNAME)
      NULLIFY(STAFLG)
      NULLIFY(STANUM)
      NULLIFY(XSTAT)
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
      CALL pritit('COOSYS','Coordiante transformation')
C
C READ INPUT OPTIONS
C ------------------
      CALL cosyin(DXYZ, DROT, DSCALE)
C
C READ ALL STATIONS FROM INPUT COORDINATE FILE
C --------------------------------------------
      CALL GTFLNA(1,'COORD  ',FILINP,IRC)
      NFLAG=1
      FLAGS(1)='@'
      CALL GETCO3(FILINP,NFLAG,FLAGS,NSTAT,STNAME,
     1            STANUM=STANUM,STAFLG=STAFLG,XSTAT=XSTAT,
     2            DATUM=DATUM,TIMCRD=TIMCRD)
C
C PREPARE THE RESULT COORDINATE ARRAY
C -----------------------------------
      ALLOCATE(XSTNEW(3,NSTAT),STAT=IAC)
      CALL alcerr(iac,'XSTNEW',(/3,NSTAT/),'COOSYS')
C
C ROTATION-MATRIX (SMALL ANGLES ONLY)
C -----------------------------------
      ROT(1,1)=1.D0
      ROT(1,2)= DROT(3)
      ROT(1,3)=-DROT(2)
      ROT(2,1)=-DROT(3)
      ROT(2,2)=1.D0
      ROT(2,3)= DROT(1)
      ROT(3,1)= DROT(2)
      ROT(3,2)=-DROT(1)
      ROT(3,3)=1.D0
C
C TRANSFORMATION INTO NEW SYSTEM
C ------------------------------
      DO 30 ISTA=1,NSTAT
        DO 20 I=1,3
          XSTNEW(I,ISTA)=0.D0
          DO 10 J=1,3
            XSTNEW(I,ISTA)=XSTNEW(I,ISTA)+ROT(I,J)*XSTAT(J,ISTA)
10        CONTINUE
          XSTNEW(I,ISTA)=XSTNEW(I,ISTA)*(1.D0+DSCALE)+DXYZ(I)
20      CONTINUE
30    CONTINUE
C
C WRITE NEW COORDINATE FILE
C -------------------------
      CALL stripdir(FILINP)
      TITNEW = 'COMPUTED FROM FILE: '//FILINP
      FILCOR=''
C
      CALL WTSTAT(0,FILCOR,TITNEW,DATUM,NSTAT,STNAME,XSTNEW,
     1            STANUM,STAFLG, TIMCRD)

C
      CALL EXITRC(0)
      END
