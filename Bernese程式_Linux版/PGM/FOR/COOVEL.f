C*
      PROGRAM COOVEL
CC
CC NAME       :  COOVEL
CC
CC PURPOSE    :  FROM COORDINATE FILE ARE SUBSTRACTED VELOCITY FIELD
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC CREATED    :  16-MAR-1992 11:00
CC
CC CHANGES    :  06-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               27-FEB-03 : RD: ADD TITLE SECTION
CC               08-JUN-03 : HU: NO OUTPUT TO SCREEN
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               23-JAN-11 : RD: USE GETCO3 INSTEAD OF GETCOO
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL CHANGED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: i4b, r8b, shortLineLength, fileNameLength,
     1                    timStrgLength, staNameLength, staFlagLength,
     1                    lfnPrt
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsta => maxcrd
      USE d_inpkey, ONLY: inpKey, init_inpkey
C
      USE s_alcerr
      USE s_coovei
      USE s_defcon
      USE s_exitrc
      USE s_getco3
      USE s_gtflna
      USE s_gtvelo
      USE s_opnsys
      USE s_prflna
      USE s_pritit
      USE s_readinpf
      USE s_timst2
      USE s_wtstat
      USE s_crdvel
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER(i4b), PARAMETER :: maxfil = 1
      CHARACTER(LEN=6), PARAMETER :: pgName = 'COOVEL'

      CHARACTER(LEN=shortLineLength) :: title
      CHARACTER(LEN=fileNameLength), DIMENSION(2,MAXFIL) :: filnam
      CHARACTER(LEN=timStrgLength) :: tstrng
      CHARACTER(LEN=staNameLength),  DIMENSION(:), POINTER :: stName
      CHARACTER(LEN=staFlagLength),  DIMENSION(:), POINTER :: staFlg
      CHARACTER(LEN=staFlagLength),  DIMENSION(:), ALLOCATABLE :: velFlg
      CHARACTER(LEN=16)                                  :: datum
      CHARACTER(LEN=staFlagLength),  DIMENSION(1)        :: flags

      REAL(r8b), DIMENSION(:,:), POINTER :: xStat
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: xVel
      REAL(r8b)    :: timcrd
      REAL(r8b)    :: tref
      REAL(r8b)    :: facInt

      INTEGER(i4b) :: iFil,nFil
      INTEGER(i4b) :: nFlag
      INTEGER(i4b) :: iSta
      INTEGER(i4b) :: nStat
      INTEGER(i4b), DIMENSION(:), POINTER :: staNum
      INTEGER(i4b) :: inuv1a
      INTEGER(i4b) :: ii
      INTEGER(i4b) :: ircVel
      INTEGER(i4b) :: irc, iac

      LOGICAL,  DIMENSION(:), ALLOCATABLE :: vFound

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
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C PRINT TILE SECTION
C ------------------
      CALL PRITIT('COOVEL','Extrapolate coordinates')
      CALL PRFLNA
C
C READ NAMES OF INPUT FILES (F-FILE)
C -----------------------------------
      CALL GTFLNA(1,'CRDINP',filnam(1,1),irc)
      CALL GTFLNA(1,'CRDOUT',filnam(2,1),irc)
C
      nfil = 1
C
C READ EPOCH OF REFERENCE COORDINATE SYSTEM
C -----------------------------------------
      CALL coovei(title,tref)
C
C LOOP OVER ALL INPUT COORDINATE FILES
C ------------------------------------
      NFLAG=1
      FLAGS(1)='@'
C
      DO IFIL=1,NFIL
C
C READ THE COORDINATE FILE
        CALL GETCO3(FILNAM(1,IFIL),NFLAG,FLAGS,NSTAT,STNAME,
     1              STANUM=STANUM,STAFLG=STAFLG,XSTAT=XSTAT,
     2              DATUM=DATUM,TIMCRD=TIMCRD)
C
C GET THE CORRESPONDING VELOCITIES
        ALLOCATE(XVEL(3,NSTAT), STAT=IAC)
        CALL ALCERR(IAC, 'XVEL', (/3,NSTAT/), pgName)
        ALLOCATE(VELFLG(NSTAT), STAT=IAC)
        CALL ALCERR(IAC, 'VELFLG', (/NSTAT/), pgName)
        ALLOCATE(VFOUND(NSTAT), STAT=IAC)
        CALL ALCERR(IAC, 'VFOUND', (/NSTAT/), pgName)

        INUV1A = 0
        XVEL   = 0.D0

        CALL GTVELO('VELOCI', INUV1A, NFLAG, FLAGS,
     1              NSTAT, XSTAT, STNAME, XVEL, VELFLG, VFOUND, IRCVEL)
C
C COMPUTE INTERPOLATION FACTOR
C-----------------------------
        CALL timst2(1,1,timcrd,tstrng)
        WRITE(lfnprt,*)'REFERENCE EPOCH:         ',tstrng
C
        FACINT=(timcrd-TREF)/365.25D0
        WRITE(lfnprt,*)'INTERPOLATION FACTOR:  ',FACINT
C
        DO ISTA=1,NSTAT
          IF ( .NOT. VFOUND(ISTA) ) THEN
            WRITE(LFNPRT,'(1X,A,I3,A,A)')
     1           'NO ',stanum(ista),' -VELOCITY: ',STNAME(ISTA)
          ENDIF
        ENDDO
C
C APPLY THE VELOCITIES
C --------------------
        CALL CRDVEL(NSTAT,XSTAT,XVEL,VFOUND,TREF,TIMCRD)
C
        CALL WTSTAT(0,FILNAM(2,IFIL),TITLE,DATUM,NSTAT,STNAME,
     1              XSTAT,STANUM,STAFLG,TREF)
C
      ENDDO
C
      CALL EXITRC(0)
      END
