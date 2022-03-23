      MODULE s_NUTSAV
      CONTAINS

C*
      SUBROUTINE NUTSAV(TITNEW,NUTNAM,REFEPO,NPAR,PARNAM,PARTIM,RMS,
     1                  XXXAPR,XXX,ANOR)
CC
CC NAME       :  NUTSAV
CC
CC PURPOSE    :  SAVE NUTATION MODEL IN A FILE
CC
CC PARAMETERS :
CC        IN  :  TITNEW : GENERAL TITLE OF PROGRAM RUN          CH*80
CC               NUTNAM : NUTATION MODEL NAME                   CH*16
CC               REFEPO : REFERENCE EPOCH FOR DRIFT PARAMETERS   R*8
CC               NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               PARNAM(I),I=1,..,NPAR: PARAMETER NAMES         CH*20
CC               PARTIM(2,I),I=1,..,NPAR: PARAMETER WINDOWS      R*8
CC                          FROM,TO IN MJD
CC               RMS    : SUM OF O-C**2                          R*8
CC               XXXAPR(I),I=1,..,NPAR: A PRIORI VALUES OF PARA. R*8
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR             R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: NORMAL EQUATION R*8
CC                        MATRIX
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  02-JUN-03 : PS: USE F90-SRs RDNUTM, WTNUTM
CC               12-AUG-03 : PS: CORRECTED ORDER OF NUTCOE
CC               21-OCT-08 : HB: ADD USE s_nutval
CC               21-MAY-10 : MF: CALL SR init_nutat
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*

! Modules
! -------
      USE m_bern
      USE d_nutmod, ONLY: t_nutat, init_nutat
      USE s_rdnutm
      USE s_wtnutm
      USE s_nutval
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE

! List of Parameters
! ------------------
! input:
      CHARACTER(LEN=80)              :: titNew ! General title of program run
      CHARACTER(LEN=16)              :: nutNam ! Nutation model name
      REAL(r8b)                      :: refEpo ! Reference epoch for drift parameters
      INTEGER(i4b)                   :: npar   ! Maximum number of parameters allowed
      CHARACTER*20 PARNAM(*)
      REAL(r8b),DIMENSION(2,*)       :: partim !
      REAL(r8b)                      :: rms    !
      REAL(r8b),DIMENSION(*)         :: xxxapr !
      REAL(r8b),DIMENSION(*)         :: xxx    !
      REAL(r8b),DIMENSION(*)         :: anor   !

! List of Functions
! -----------------


! Local types
! -----------
      TYPE(t_nutat)                  :: nutat        ! Nutation model parameters

! Local Parameters
! ----------------
      INTEGER(i4b)                   :: maxPer
      PARAMETER (MAXPER=1000)

! Local Variables
! ---------------
      CHARACTER(LEN=32)              :: FILNUT,FILSAV

      INTEGER(i4b),DIMENSION(14)     :: ifdarg
      INTEGER(i4b)                   :: iarg,inut
      INTEGER(i4b)                   :: ipar,ii
      INTEGER(i4b)                   :: irc,ircsav

      REAL(r8b)                      :: arg,argr

C
C INITIALIZE "IFDARG"
C -------------------
      DO IARG=1,14
        IFDARG(IARG)=0
      ENDDO
C
C GET REFERENCE NUTATION MODEL
C ----------------------------
      CALL GTFLNA(0,'NUTREF ',FILNUT,IRC)
      IF (IRC.EQ.0) THEN
!        CALL RDNUTM(MAXPER,FILNUT,TITLE ,NUTNAR,NUTPRE,NUTFAR,NNUTR ,
!     1              NUTPER,NUTMLT,NUTCOR)
         CALL init_nutat(nutat)
         CALL RDNUTM(filnut,nutat)

      ELSE
        nutat%nnut=0
      ENDIF

      nutat%title=titnew
      nutat%nutnam=nutnam

C
C COPY A PRIORI NUTATION COEFFICIENTS
C -----------------------------------
!      NNUT=NNUTR
!      DO INUT=1,NNUTR
!        DO IARG=1,14
!          NUTCOE(IARG,INUT)=nutat%nutcoe(IARG,INUT)
!        ENDDO
!      ENDDO

C
C SET CORRECT FREQUENCIES
C -----------------------
      DO INUT=1,nutat%NNUT
        CALL NUTVAL(REFEPO,nutat%NUTFAR,nutat%NUTMLT(:,INUT),
     1              ARG,ARGR,nutat%NUTPER(INUT))
      ENDDO
C
C UPDATE NUTATION COEFFICIENTS
C ----------------------------
      DO IPAR=1,NPAR
        IF (PARNAM(IPAR)(1:2).EQ.'EC' .OR.
     1      PARNAM(IPAR)(1:2).EQ.'ES' .OR.
     2      PARNAM(IPAR)(1:2).EQ.'PC' .OR.
     3      PARNAM(IPAR)(1:2).EQ.'PS') THEN
          READ(PARNAM(IPAR)(3:17),'(5I3)') (IFDARG(II),II=1,5)
          DO INUT=1,nutat%NNUT
            DO IARG=1,5
              IF (nutat%NUTMLT(IARG,INUT).NE.IFDARG(IARG)) GOTO 10
            ENDDO
C TERM FOUND
            GOTO 20
10          CONTINUE
          ENDDO
C
C TERM NOT FOUND: A PRIORI REMAINS ZERO
          nutat%NNUT=nutat%NNUT+1
C
C CHECK DIMENSION
          IF (nutat%NNUT.GT.MAXPER) THEN
            WRITE(LFNERR,910) MAXPER,FILSAV
910         FORMAT(/,' *** SR NUTSAV: TOO MANY NUTATION PERIODS TO ',
     1               'BE SAVED',
     2             /,16X,'MAXIMUM NUMBER OF PERIODS ALLOWED:',I5,
     3             /,16X,'NUTATION FILE NAME               : ',A,/)
            CALL EXITRC(2)
          ENDIF
C
          CALL NUTVAL(REFEPO,nutat%NUTFAR,IFDARG,ARG,ARGR,
     1       nutat%NUTPER(nutat%nnut))
          DO IARG=1,14
            nutat%NUTMLT(IARG,nutat%NNUT)=IFDARG(IARG)
            nutat%NUTCOE(IARG,nutat%NNUT)=0.D0
          ENDDO
C
20        CONTINUE

!          IF (PARNAM(IPAR)(1:2).EQ.'ES')
!     1      nutat%NUTCOE(6,INUT)=nutat%NUTCOE(6,INUT)+XXX(IPAR)/1000.D0
!          IF (PARNAM(IPAR)(1:2).EQ.'EC')
!     1      nutat%NUTCOE(3,INUT)=nutat%NUTCOE(3,INUT)+XXX(IPAR)/1000.D0
!          IF (PARNAM(IPAR)(1:2).EQ.'PS')
!     1      nutat%NUTCOE(1,INUT)=nutat%NUTCOE(1,INUT)+XXX(IPAR)/1000.D0
!          IF (PARNAM(IPAR)(1:2).EQ.'PC')
!     1      nutat%NUTCOE(5,INUT)=nutat%NUTCOE(5,INUT)+XXX(IPAR)/1000.D0

          IF (PARNAM(IPAR)(1:2).EQ.'ES')
     1      nutat%NUTCOE(7,INUT)=nutat%NUTCOE(7,INUT)+XXX(IPAR)/1000.D0
          IF (PARNAM(IPAR)(1:2).EQ.'EC')
     1      nutat%NUTCOE(5,INUT)=nutat%NUTCOE(5,INUT)+XXX(IPAR)/1000.D0
          IF (PARNAM(IPAR)(1:2).EQ.'PS')
     1      nutat%NUTCOE(1,INUT)=nutat%NUTCOE(1,INUT)+XXX(IPAR)/1000.D0
          IF (PARNAM(IPAR)(1:2).EQ.'PC')
     1      nutat%NUTCOE(3,INUT)=nutat%NUTCOE(3,INUT)+XXX(IPAR)/1000.D0
        ENDIF
C
      ENDDO


C
C WRITE NEW NUTATION MODEL FILE
C -----------------------------
      CALL GTFLNA(0,'NUTSAV ',FILSAV,IRCSAV)
C
      IF (IRCSAV.EQ.0) THEN
!        CALL WTNUTM(FILSAV,TITNEW,NUTNAM,NUTPRE,NUTFAR,NNUT  ,NUTPER,
!     1              NUTMLT,NUTCOE)

        CALL WTNUTM(filsav,nutat)


      ENDIF
C
C END
C ---
      RETURN
      END SUBROUTINE

      END MODULE
