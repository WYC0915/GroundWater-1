C*
      PROGRAM NUVELO
CC
CC NAME       :  NUVELO
CC
CC PURPOSE    :  COMPUTES NUVEL1 OR NUVEL1A VELOCITY FIELD FOR A GIVEN
CC               COORDINATE FILE
CC               NECESSARY ONLY A DESCRIPTION FILE WITH THE PLATE DEFINITION
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC CREATED    :  18-FEB-96
CC
CC CHANGES    :  04-AUG-99 : PF: USE NEW VERSION OF GETCOO
CC               30-AUG-01 : MM: SWITCH TO NEW MENU
CC               31-AUG-01 : MM: ADD SOME OUTPUT
CC               08-MAY-02 : RD: ADD PLATE TO THE OUTPUT FILE
CC               30-JUL-02 : HU: USE INTERFACE FOR ALCERR
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               06-OCT-10 : RD: EXITRC ADDED AT THE END
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               11-FEB-11 : RD: EXCHANGE GETCOO->GETCO3 AND WTVELO->WTSTAT
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               27-APR-12 : RD: NULLIFY POINTERS
CC               27-APR-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, keyValueLength, shortLineLength,
     1                    fileNameLength, staNameLength,
     1                    staFlagLength, lfnPrt
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_alcerr
      USE s_prflna
      USE s_gtvelo
      USE s_pritit
      USE s_readinpf
      USE s_gtstanum
      USE s_getco3
      USE s_readkeys
      USE s_defcon
      USE s_wtstat
      USE s_opnsys
      USE s_gtflna
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAC   , INUV1A, IRCVEL, NFLAG ,
     1          NSTAT1
C
      REAL*8    TIMCRD
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
      CHARACTER(LEN=keyValueLength),
     1              DIMENSION(:),   POINTER     :: keyValue
      CHARACTER(LEN=shortLineLength)            :: TITLE,TITSAV
      CHARACTER(LEN=fileNameLength)             :: FILNAM,FILVEL,PLTFIL
      CHARACTER(LEN=staNameLength),
     1              DIMENSION(:),   POINTER     :: STANAM
      CHARACTER(LEN=staNameLength),
     1              DIMENSION(:),   POINTER     :: STANAM_HLP
      CHARACTER(LEN=16)                         :: DATUM
      CHARACTER(LEN= 7)                         :: MODEL
      CHARACTER(LEN= 4),
     1              DIMENSION(:),   POINTER     :: PLATE_HLP
      CHARACTER(LEN= 4),
     1              DIMENSION(:),   ALLOCATABLE :: PLATE
      CHARACTER(LEN=staFlagLength),
     1              DIMENSION(:),   POINTER     :: STAFLG
      CHARACTER(LEN=staFlagLength),
     1              DIMENSION(:),   POINTER     :: VELFLG
      CHARACTER(LEN=staFlagLength), DIMENSION(1):: FLAGS
C
      INTEGER(i4b), DIMENSION(:),   POINTER     :: STANUM
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: STANUM_HLP
      INTEGER(i4b)                              :: nDummy
      INTEGER(i4b)                              :: nStat,nStat_Hlp
      INTEGER(i4b)                              :: iSta, jSta
      INTEGER(i4b)                              :: irc
C
      REAL(r8b),    DIMENSION(:,:), POINTER     :: XSTAT
      REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: XVEL
      REAL(r8b),    DIMENSION(:,:), POINTER     :: XDUMMY
C
      LOGICAL,      DIMENSION(:),   ALLOCATABLE :: VFOUND
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(STANAM)
      NULLIFY(STANAM_HLP)
      NULLIFY(PLATE_HLP)
      NULLIFY(STAFLG)
      NULLIFY(VELFLG)
      NULLIFY(STANUM)
      NULLIFY(XSTAT)
      NULLIFY(XDUMMY)
      NULLIFY(keyValue)
      CALL init_inpkey(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
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
C PRINT TITLE AND FILES
C ---------------------
      CALL pritit('NUVELO', 'Compute NUVEL velocities')
      CALL prflna
C
C GET MODEL TO USE
C ----------------
      CALL readkeys('MODEL', keyValue, irc)
      IF (keyValue(1) == 'NUVEL1') THEN
        INUV1A = 0
        MODEL = 'NUVEL1'
      ELSEIF (keyValue(1) == 'NUVEL1A') THEN
        INUV1A = 1
        MODEL = 'NUVEL1A'
      ENDIF
C
C COORDINATE FILE NAMES
C ---------------------
      CALL GTFLNA(1,'COORD  ',FILNAM,IRC)
C
C READ ALL STATIONS WITH FLAG "NON-BLANK" FROM COORDINATE FILE
C ------------------------------------------------------------
      NFLAG=1
      FLAGS(1)='@'
      CALL GETCO3(FILNAM,NFLAG,FLAGS,NSTAT,STANAM,
     1            stanum=STANUM,staflg=STAFLG,xstat=XSTAT,
     2            datum=DATUM,title=TITLE,timcrd=TIMCRD)
C
C GET LIST OF STATIONS TO PROCESS
C -------------------------------
      CALL readkeys('STASEL', keyValue, irc)

C
      IF (keyValue(1) == 'MANUAL') THEN
C
        ALLOCATE(STANUM_HLP(NSTAT),STAT=IAC)
        CALL ALCERR(IAC,'STANUM_HLP',(/NSTAT/),'NUVELO')
        STANUM_HLP = 0
        ALLOCATE(STANAM_HLP(NSTAT),STAT=IAC)
        CALL ALCERR(IAC,'STANAM_HLP',(/NSTAT/),'NUVELO')
        STANAM_HLP = ''
C
        CALL GTSTANUM(NSTAT,STANUM,STANAM,'STASEL','VELSTA',' ',' ',
     1  NSTAT_HLP,STANUM_HLP,STANAM_HLP,NDUMMY,XDUMMY)
C
        NSTAT = NSTAT_HLP
        STANUM(1:NSTAT) = STANUM_HLP(1:NSTAT)
        STANAM(1:NSTAT) = STANAM_HLP(1:NSTAT)
C
        DEALLOCATE(STANUM_HLP,STAT=IAC)
        DEALLOCATE(STANAM_HLP,STAT=IAC)
C
C END OF READING COORDINATE FILE
      ENDIF
C
      ALLOCATE(VELFLG(NSTAT),STAT=IAC)
      CALL ALCERR(IAC,'VELFLG',(/NSTAT/),'NUVELO')
      VELFLG = ''
C
      ALLOCATE(XVEL(3,NSTAT),STAT=IAC)
      CALL ALCERR(IAC,'XVEL',(/3,NSTAT/),'NUVELO')
      XVEL = 0D0
C
      ALLOCATE(VFOUND(NSTAT),STAT=IAC)
      CALL ALCERR(IAC,'VFOUND',(/NSTAT/),'NUVELO')
      VFOUND = .FALSE.
C
      ALLOCATE(PLATE(NSTAT),STAT=IAC)
      CALL ALCERR(IAC,'PLATE',(/NSTAT/),'NUVELO')
      PLATE = ''
C
C GET VELOCITIES /VELOCITY MODEL IF SPECIFIED IN N-FILE WITH
C FIXVEL (VELOCITY FILE) OR VELTRN(NUVEL MODEL) TO FIX ON SPECIAL
C VELOCITIES OR TO INTRODUCE IN NORMALEQUATIONS
C ---------------------------------------------------------------
      NFLAG=1
      FLAGS(1)='@'
      CALL GTVELO('',INUV1A,NFLAG,FLAGS,NSTAT,XSTAT,STANAM,
     1            XVEL,VELFLG,VFOUND,IRCVEL)
C
C REMOVE STATIONS WITHOUT PLATE INFROMATION IN GTVELO
C ---------------------------------------------------
      NSTAT1 = 0
      DO ISTA=1,NSTAT
        IF (VFOUND(ISTA)) THEN
          NSTAT1=NSTAT1+1
          IF (NSTAT1.LT.NSTAT) THEN
            STANAM(NSTAT1)=STANAM(ISTA)
            STANUM(NSTAT1)=STANUM(ISTA)
            XVEL(1:3,NSTAT1)=XVEL(1:3,ISTA)
            VELFLG(NSTAT1)=VELFLG(ISTA)
          ENDIF
        ENDIF
      ENDDO
      NSTAT_HLP=NSTAT-NSTAT1
      NSTAT=NSTAT1
C
C GET THE CORRESPONDING PLATE NAMES:
C ----------------------------------
      CALL GTFLNA(1,'VELTRN',PLTFIL,IRC)
C
      IF (IRC.EQ.0) THEN
        CALL GETCO3(PLTFIL,NFLAG,FLAGS,
     1              NSTAT1,STANAM_HLP,PLATE=PLATE_HLP)
C
        DO ISTA=1,NSTAT
          DO JSTA=1,NSTAT1
            IF (STANAM(ISTA).EQ.STANAM_HLP(JSTA))
     1        PLATE(ISTA)=PLATE_HLP(JSTA)
          ENDDO
        ENDDO
        DEALLOCATE(STANAM_HLP,STAT=IAC)
        DEALLOCATE(PLATE_HLP,STAT=IAC)
      ENDIF
C
C SAVE VELOCITIES
C ---------------
      TITSAV = TRIM(MODEL)//'-NNR VELOCITIES'
      FILVEL = ' '
      CALL WTSTAT(1,FILVEL,TITSAV,DATUM,NSTAT,STANAM,XVEL,
     1            STANUM,VELFLG,plate=PLATE)
C
C GENERATE SOME MINIMUM OUTPUT
C ----------------------------
      WRITE(lfnprt,'(A,17X,A)')  " Used model:         ", MODEL
      WRITE(lfnprt,'(A,I4)')
     1      " Number of stations with velocity:    ", NSTAT
      WRITE(lfnprt,'(A,I4,/,A)')
     1      " Number of stations without velocity: ", NSTAT_HLP,
     2      " (because of missing plate information)"

      CALL EXITRC(0)

      END
