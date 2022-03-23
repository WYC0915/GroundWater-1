C*
      PROGRAM VELDIF
CC
CC NAME       :  VELDIF
CC
CC PURPOSE    :  DIFFERENCING OF VELOCITY FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E.BROCKMANN
CCw
CC CREATED    :  16-MAR-92 11:00
CC
CC CHANGES    :  25-MAR-08 : SS: ADAPTED TO VERSION 5.0
CC               26-MAR-08 : SS: CLOSE LFNLOC
CC               28-MAR-08 : SS: ADAPTED TO VERSION 5.1
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               23-FEB-11 : RD: USE STANDARD ROUTINE FOR READ/WRITE CRD/VEL
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               20-DEC-11 : DT: CORRECT IF NO SECOND VEL FILE;
CC                               IMPROVEMENTS FOR OUTPUT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: i4b, r8b, staNameLength, staFla2Length,
     1                    lineLength, lfnprt, lfnerr
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
C
      USE s_readinpf
      USE s_opnsys
      USE s_defcon
      USE s_pritit
      USE s_prflna
      USE s_gtflna
      USE s_veldii
      USE s_getco3
      USE s_getdat
      USE s_xyzloc
      USE s_xyzell
      USE s_gtvelo
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      USE s_alcerr
      USE s_wtstat
C
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 MAXSTA, IDIF  , NFLGAP, JSTA  , IAC   ,
     1          NSTA  , IRCVEL, NVEL  , ISTA  ,
     2          IRC   , IVEL  , NVEL2
C
      REAL*8    TIMCRD, SCELL , BELL  , AELL
C
      PARAMETER (MAXSTA=500)
C
      CHARACTER(LEN=9), PARAMETER :: pgName='VELDIF'
C
C MAXSTA: MAXIMUM NUMBER STATIONS IN VELOCITY FILE
C
C DECLARATIONS
C ------------
C
C LOCAL DECLARATIONS
C ------------------
      INTEGER(i4b),DIMENSION(:),   POINTER     :: NR
      INTEGER(i4b)                             :: refTyp
      REAL*8       VELREF(3)
      REAL*8      ,DIMENSION(3)                :: dVelBLH
      REAL(r8b),   DIMENSION(:,:), POINTER     :: STACOR
      REAL(r8b),   DIMENSION(:,:), POINTER     :: VEL
      REAL(r8b),   DIMENSION(:,:), ALLOCATABLE :: XELLAP
      REAL(r8b),   DIMENSION(:,:), POINTER     :: VELXYZ
      REAL*8       TEMLBH(3),LBHREF(3),TEMVEL(3)
      REAL*8       DXELL(3),DRELL(3),VELLBH(3)
      REAL*8       REFLBH(3,MAXSTA),TSTLBH(3)
      REAL*8       HMAT(3,3)
      CHARACTER*1  FLGAPR(1)
      CHARACTER*16 DATUM,VELNAM
      CHARACTER(LEN=staNameLength), DIMENSION(:), POINTER :: STNAME
      CHARACTER(LEN=staNameLength), DIMENSION(:), POINTER :: STAVEL
      CHARACTER(LEN=staNameLength), DIMENSION(:), POINTER :: STANAM
      CHARACTER(LEN=staFla2Length), DIMENSION(:), POINTER :: FLAG
      CHARACTER(LEN= 4),            DIMENSION(:), POINTER :: PLATE
      CHARACTER(LEN=lineLength),    DIMENSION(:), POINTER :: fLine
      CHARACTER*32 FILNAM(2),FILVEL,FILCOO
      CHARACTER*80 TITAPR
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INIT POITNERS
C -------------
      NULLIFY(STNAME)
      NULLIFY(STAVEL)
      NULLIFY(STANAM)
      NULLIFY(FLAG)
      NULLIFY(PLATE)
      NULLIFY(FLINE)
      NULLIFY(STACOR)
      NULLIFY(VEL)
      NULLIFY(VELXYZ)
      NULLIFY(NR)
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
C PRINT TILE SECTION
C ------------------
      CALL PRITIT(pgName,'Velocity comparison')
      CALL PRFLNA
C
C READ NAMES OF INPUT FILES (F-FILE)
C -----------------------------------
      CALL GTFLNA(1,'VELINP',FILNAM(1),IRC)
      CALL GTFLNA(1,'VELOUT',FILNAM(2),IRC)
C
C READ CONSTANT REFERENCE VELOCITIES
C ----------------------------------
      idif = 0
      CALL veldii(refTyp,velref,velnam,idif)
c
c get coordinate file
c
      CALL GTFLNA(1,'COORD   ',FILCOO,IRC)
      NFLGAP=1
      FLGAPR(1)='@'
      CALL GETCO3(FILCOO,NFLGAP,FLGAPR,NSTA,STNAME,
     1            xstat=STACOR,datum=DATUM,title=TITAPR,timcrd=TIMCRD)
C
      CALL GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
      ALLOCATE(XELLAP(3,NSTA),STAT=IAC)
      CALL ALCERR(IAC,'XELLAP',(/3,NSTA/),'VELDIF')
      XELLAP=0D0
C
      DO ISTA=1,NSTA
        CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1              STACOR(:,ISTA),XELLAP(:,ISTA))
      ENDDO
C
C COMPUTE LBH VELOCITIES FOR VELNAM
C (only if reference station selected)
C
      IF (refTyp.EQ.2 .AND. idif.ne.0) THEN
        DO JSTA=1,NSTA
          IF (VELNAM.EQ.STNAME(JSTA)) THEN
            CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                  XELLAP(3,JSTA),
     1                  AELL,BELL,-1,VELREF,LBHREF,HMAT)
            GOTO 199
          ENDIF
        ENDDO
        WRITE(lfnerr,'(1X,A)')'NO DIFF STATION COORDINATES: ',STANAM
199     CONTINUE
C
      ENDIF
C
C READ VELOCITY OF THE STATIONS
C (if VEL file as reference)
C -----------------------------
      nvel=0
      IF (refTyp.EQ.1) THEN
        CALL GTFLNA(0,'VELOCI ',FILVEL,IRCVEL)
        NFLGAP=1
        FLGAPR(1)='@'
C
        CALL GETCO3(FILVEL,NFLGAP,FLGAPR,NVEL,STAVEL,xstat=VEL)
C
C COMPUTE VERTICAL VELOCITIES (for reference file)
        IF (idif.EQ.0) GOTO 109
C
        DO ISTA=1,NVEL
          DO JSTA=1,NSTA
            IF (STAVEL(ISTA).EQ.STNAME(JSTA)) THEN
               CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                     XELLAP(3,JSTA),
     1                     AELL,BELL,-1,VEL(:,ISTA),REFLBH(:,ISTA),HMAT)
               GOTO 99
            ENDIF
          ENDDO
          WRITE(lfnerr,'(1X,A,A)')'NO STATION COORD FOR: ',STAVEL(ISTA)
99        CONTINUE
        ENDDO
      ENDIF
109   CONTINUE
C
C READ VELOCITY OF THE STATIONS (input file)
C ------------------------------------------
      NFLGAP=1
      FLGAPR(1)='@'
      CALL GETCO3(FILNAM(1),NFLGAP,FLGAPR,NVEL2,STANAM,
     1            stanum=nr,xstat=VELXYZ,staflg5=FLAG,plate=PLATE,
     2            datum=DATUM,title=TITAPR,footer=FLINE)
C
C Get velocity of reference station
C ---------------------------------
      IF (refTyp.EQ.2) THEN
        DO ISTA=1,NVEL2
          IF (velNam.EQ.STANAM(ISTA)) THEN
            VELREF(1:3) = VELXYZ(1:3,ISTA)
            EXIT
          ENDIF
        ENDDO
      ENDIF
C
C COMPUTE INTERPOLATION FACTOR
C-----------------------------
      DO IVEL=1,NVEL2
C
C GET LBH COMPONENTS
C
        IF (IDIF.NE.0) THEN
          TEMVEL(:)=VELXYZ(:,IVEL)
          DO JSTA=1,NSTA
            IF (STANAM(IVEL).EQ.STNAME(JSTA)) THEN
              CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                    XELLAP(3,JSTA),
     1                    AELL,BELL,-1,TEMVEL,VELLBH,HMAT)
              EXIT
            ENDIF
          ENDDO
          WRITE(lfnerr,'(1X,A,A)')'NO STATION COORD FOR: ',STANAM
          CYCLE
        ENDIF
C
C VEL file as reference
C ---------------------
        IF (refTyp.EQ.1) THEN
          DO ISTA=1,NVEL
            JSTA=0
            IF (STANAM(IVEL).EQ.STAVEL(ISTA)) THEN
              IF (IDIF.EQ.0) THEN
                VELXYZ(:,IVEL)=VELXYZ(:,IVEL)-VEL(:,ISTA)
              ELSE
                TEMLBH(1)=VELLBH(1)-REFLBH(1,ISTA)
                TEMLBH(2)=VELLBH(2)-REFLBH(2,ISTA)
                TEMLBH(3)=VELLBH(3)-REFLBH(3,ISTA)
C CONVERSION BACK
                CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                      XELLAP(3,JSTA),
     1                      AELL,BELL,1,TSTLBH,TEMLBH,HMAT)
                VELXYZ(:,IVEL)=TSTLBH(:)
              ENDIF
C
              JSTA=ISTA
              EXIT
            ENDIF
          ENDDO
          IF (JSTA.EQ.0)
     1      WRITE(lfnerr,'(1X,A,I3,A,A)')
     2      'NO ',NR(IVEL),' -VELOCITY: ',STANAM(IVEL)
C
C Station or velocity vector to be subtracted
C -------------------------------------------
        ELSE
          IF (IDIF.EQ.0) THEN
            VELXYZ(:,IVEL)=VELXYZ(:,IVEL)-VELREF(:)
          ELSE
            TEMLBH(1)=VELLBH(1)-LBHREF(1)
            TEMLBH(2)=VELLBH(2)-LBHREF(2)
            TEMLBH(3)=VELLBH(3)-LBHREF(3)
C CONVERSION BACK
            CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                  XELLAP(3,JSTA),
     1                  AELL,BELL,1,TSTLBH,TEMLBH,HMAT)
            VELXYZ(:,IVEL)=TSTLBH(:)
          ENDIF
        ENDIF
C
      ENDDO
C
C No specific datum of difference velocities
C ------------------------------------------
      DATUM = ''
C
C Write velocity differences to file
C ----------------------------------
      CALL WTSTAT(0,FILNAM(2),TITAPR,DATUM,NVEL2,STANAM,VELXYZ,
     1            stanum=NR,staflg=FLAG,plate=PLATE,footer=FLINE)
C
C Write velocity differences to output
C ------------------------------------
      WRITE(lfnprt,"(/,1X,'VELOCITIES SUBTRACTED',
     1               /,1X,'---------------------',/)")

      IF (refTyp.EQ.1) THEN
        WRITE(lfnprt,"(1X,'From VEL file: ',A,/)") FILVEL

      ELSEIF (refTyp.EQ.2) THEN
        WRITE(lfnprt,"(1X,'From station: ',A,/,
     1                 15X, 'XVel = ',F10.5,' m/y',/,
     2                 15X, 'YVel = ',F10.5,' m/y',/,
     3                 15X, 'ZVel = ',F10.5,' m/y',/)")
     4               velNam, velRef(1), velRef(2), velRef(3)

      ELSEIF (refTyp.EQ.3) THEN
        WRITE(lfnprt,"(1X,'Velocity vector: dXVel = ',F10.5,' m/y',/,
     1                 18X, 'dYVel = ',F10.5,' m/y',/,
     2                 18X, 'dZVel = ',F10.5,' m/y',/)")
     3               velRef(1), velRef(2), velRef(3)

      ENDIF

      WRITE(lfnprt,"(/,1X,'VELOCITY DIFFERENCES',/,1X,20('-'),/)")

      IF (refTyp.EQ.1) THEN
        WRITE(lfnprt,"(1X,'Station name',8X,1X,
     1               8X,'Velocity differences [m/y]',
     2               18X,'Velocity differences [m/y]',/,
     3               28X,'X',13X,'Y',13X,'Z',13X,'Latitude',
     4               6X,'Longitude',6X,'Height',/,1X,106('-'),/)")

      ELSE
        WRITE(lfnprt,"(1X,'Station name',8X,1X,
     1               8X,'New station velocity [m/y]',
     2               18X,'Velocity differences [m/y]',/,
     3               28X,'X',13X,'Y',13X,'Z',13X,'Latitude',
     4               6X,'Longitude',6X,'Height',/,1X,106('-'),/)")

      ENDIF

      DO ISTA=1,NVEL2
        dVelBLH(:) = 0d0
        DO JSTA=1,NSTA
          IF (STANAM(ISTA).EQ.STNAME(JSTA)) THEN
            IF (refTyp.EQ.1) THEN
              CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                    XELLAP(3,JSTA),
     2                    AELL,BELL,-1,VELXYZ(1:3,ISTA),dVelBLH,HMAT)

            ELSE
              CALL XYZLOC(XELLAP(1,JSTA),XELLAP(2,JSTA),
     1                    XELLAP(3,JSTA),
     2                    AELL,BELL,-1,VelRef,dVelBLH,HMAT)
            ENDIF
            EXIT
          ENDIF
        ENDDO

        WRITE(lfnprt,200) STANAM(ISTA), VELXYZ(1:3,ISTA),
     1                    dVelBLH(1:3)
200     FORMAT(1X,A14,6X,1X,3(F12.5,2X),3(2X,F12.5))
      ENDDO
C
      CALL EXITRC(0)
      END
