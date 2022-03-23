MODULE s_REBLOCQ
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE reblocq(neq, ipar, parType, siteCode, pointCode, solID, reftime, &
                   unit, sintim, techSNX, numdesc, snxFileName, numERPpar,  &
                   mxERPt, mnERPt, flgUT1r, nut_unit, mxTRPt, mnTRPt, timCrd)

! --------------------------------------------------------------------------
! Purpose:    Rebuilt of locq, set neq%par%time%mean and timCrd, handling of
!             solution ID for station coordinates and velocities
!
! Author:     A. Gaede
!
! Created:    31-Oct-2006
!
! Changes:    20-Aug-2007     SINEX filename in warning message added
!             13-Jul-2009 DT: locq for range biases adapted to new definition
!             08-Sep-2010 RD: Merge SLR-time bias option
!             20-Oct-2010 DT: Size of numERPpar, mxERPt, mnERPt 1->5;
!                             maxdesc from p_snx2nq0;
!                             siteCode as par%name for RBIAS, TBIAS
!             05-Oct-2012 DT: Add nutation rates and Cnm/Snm; adopt numdesc
!             05-Oct-2012 DT: Set technique flag for each parameter
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq,maxStaSin
  USE d_par,    ONLY: add_techn
  USE d_satfil, ONLY: typeMWTR
  USE m_time,   ONLY: t_timint
  USE p_snx2nq0,ONLY: maxdesc

  USE s_gtsensor
  USE s_svn2prn
  USE s_sindat
  USE s_exitrc
  USE f_ut1_ut1r
  USE s_upperc
  IMPLICIT NONE

! List of Parameters
! ------------------
! Input/Output:
  TYPE(t_neq)                    :: neq         ! NEQ structure
! INPUT:
  CHARACTER(LEN=fileNameLength)  :: snxFileName ! Name of SINEX file
  INTEGER(i4b)                   :: ipar        ! parameter
  CHARACTER(LEN=6)               :: parType     ! parameter type
  CHARACTER(LEN=4)               :: siteCode    ! four character Id
  CHARACTER(LEN=2)               :: pointCode   ! point code
  CHARACTER(LEN=4)               :: solID       ! solution Id
  CHARACTER(LEN=12)              :: refTime
  CHARACTER(LEN=4)               :: unit
  TYPE(t_timint)                 :: sintim      ! Start/end epoch of SINEX
  CHARACTER(LEN=1)               :: techSNX     ! Technique (from header line)
! Output:
  INTEGER(i4b), DIMENSION(:)     :: flgUt1r
  INTEGER(i4b), DIMENSION(5)     :: numERPpar
  REAL(r8b), DIMENSION(5)        :: mxERPt
  REAL(r8b), DIMENSION(5)        :: mnERPt
  REAL(r8b)                      :: mxTRPt
  REAL(r8b)                      :: mnTRPt
  REAL(r8b)                      :: timCrd
  CHARACTER(LEN=4)               :: nut_unit


! Local Variables
! ---------------
  INTEGER(i4b),DIMENSION(maxdesc):: numdesc
  INTEGER(i4b)                   :: ios
  INTEGER(i4b)                   :: ista
  INTEGER(i4b)                   :: solNum
  INTEGER(i4b),SAVE              :: nsitelst=0
  INTEGER(i4b)                   :: ifound
  INTEGER(i4b)                   :: prn
  INTEGER(i4b)                   :: isol
  INTEGER(i4b),SAVE              :: iprt = 1

  CHARACTER(LEN=fileNameLength)  :: savFil
  CHARACTER(LEN=20)                           :: satant
  CHARACTER(LEN=4), DIMENSION(maxStaSin),SAVE :: siteLst
  CHARACTER(LEN=4), DIMENSION(maxStaSin),SAVE :: solnLst
  CHARACTER(LEN=2), DIMENSION(maxStaSin),SAVE :: codeLst
  CHARACTER(LEN=1), DIMENSION(maxStaSin),SAVE :: solChar
  CHARACTER(LEN=1)                            :: solchr
  CHARACTER(LEN=4)                            :: techFlg

  LOGICAL,SAVE                   :: iFirst=.TRUE.

  TYPE(t_timint)                 :: timint

  techFlg = ''

  IF (savFil /= snxFileName) THEN
    savFil = snxFileName
    nsitelst = 0
    iprt = 1
  ENDIF

! Set time%mean and epoch for coordinate file (timCrd)
! ----------------------------------------------------
  CALL sindat(1, neq%par(ipar)%time%mean, reftime)
  neq%par(ipar)%time%half = 0.d0
  IF (parType(1:3) == 'STA') THEN
    IF (timCrd == 1D20) THEN
      timCrd = neq%par(ipar)%time%mean
    ELSEIF (iprt==1.AND.DABS(neq%par(ipar)%time%mean-timCrd) > 1D-5) THEN
      WRITE(lfnerr,"(/,' ### SR REBLOCQ: Different epochs for ', &
                     & 'station coordinates.'                   &
                     & /,17X,'Epoch used for CRD file: ',F12.6,/)") &
           timCrd
      iprt=0
    ENDIF
  ENDIF

! Solution number handling
! ------------------------
  DO ista = 1, neq%misc%nstat_sinex

    IF ( neq%misc%sinex(ista)%stname(1:4)   == siteCode    .AND.     &
         neq%misc%sinex(ista)%stname(15:16) == pointCode ) THEN

      neq%par(ipar)%name = neq%misc%sinex(ista)%stname(1:14)
      CALL upperc(neq%par(ipar)%name)

      IF (parType(1:3) == 'STA') THEN
        neq%par(ipar)%time%mean = (neq%misc%sinex(ista)%timint%t(2) +   &
             neq%misc%sinex(ista)%timint%t(1))/2
        neq%par(ipar)%time%half = (neq%misc%sinex(ista)%timint%t(2) -   &
             neq%misc%sinex(ista)%timint%t(1))/2
      ENDIF

      ifound=0
      solChr=' '
      DO isol=1,nsitelst
        IF (siteCode == siteLst(isol) .AND.  &
             pointCode == codeLst(isol)) THEN
          ifound=ifound+1
          IF (solId == solnLst(isol)) THEN
            ifound=999
            solChr=solChar(isol)
          ENDIF
        ENDIF
      ENDDO
      IF (ifound /= 999) THEN
        nsitelst=nsitelst+1
        IF (nsitelst > maxStaSin) THEN
          WRITE(lfnerr,"(/,' *** SR REBLOCQ: Too many solutions', &
                       & /,17X,'Maximum number  :',I6, &
                       & /,17X,'Increase maxStaSin in SR D_NEQ!')") &
               maxStaSin
          CALL exitrc(2)
        ENDIF
        siteLst(nsitelst)=siteCode
        codeLst(nsitelst)=pointCode
        solnLst(nsitelst)=solId
        IF (ifound < 26) THEN
          solChar(nsiteLst)=CHAR(ifound+65)
          solChr=solChar(isol)
          IF (ifound > 0) &
          WRITE(lfnerr,"(/,' ### SR REBLOCQ: Additional solution found', &
               & /,'                 Station                     : ',A, &
               & /,'                 Site code                   : ',A, &
               & /,'                 Solution ID                 : ',A, &
               & /,'                 Sol.ID added to station name: ',A,/)") &
               TRIM(neq%par(ipar)%name),   &
               siteCode,solId,solChar(nsitelst)
        ELSE
          WRITE(lfnerr,"(/,' *** SR REBLOCQ: Too many solution numbers',&
               & /,'                 Station            : ',A, &
               & /,'                 Number of solutions:',I6, &
               & /,'                 SINEX file: ',A,/)") &
               TRIM(neq%par(ipar)%name),ifound+1,snxFileName
          CALL exitrc(2)
        ENDIF
      ENDIF
      neq%par(ipar)%name(15:15) = solChr
      EXIT
    ENDIF
  ENDDO

! locq rebuilding
! ---------------

! station coordinates
  IF ( parType == 'STAX' ) THEN
    numdesc(1) = numdesc(1) + 1
    neq%par(ipar)%locq(1) = 1
    neq%par(ipar)%locq(3) = 1
    neq%par(ipar)%locq(4) = 0
    neq%par(ipar)%scale   = 1.d0
  ELSEIF ( parType == 'STAY' ) THEN
    neq%par(ipar)%locq(1) = 1
    neq%par(ipar)%locq(3) = 2
    neq%par(ipar)%locq(4) = 0
    neq%par(ipar)%scale   = 1.d0
  ELSEIF ( parType == 'STAZ' ) THEN
    neq%par(ipar)%locq(1) = 1
    neq%par(ipar)%locq(3) = 3
    neq%par(ipar)%locq(4) = 0
    neq%par(ipar)%scale   = 1.d0

! station velocities
  ELSEIF ( parType == 'VELX' ) THEN
    numdesc(2) = numdesc(2) + 1
    neq%par(ipar)%locq(1) = 1
    neq%par(ipar)%locq(3) = 1
    neq%par(ipar)%locq(4) = 3
    neq%par(ipar)%scale   = 1.d0
  ELSEIF ( parType == 'VELY' ) THEN
    neq%par(ipar)%locq(1) = 1
    neq%par(ipar)%locq(3) = 2
    neq%par(ipar)%locq(4) = 3
    neq%par(ipar)%scale   = 1.d0
  ELSEIF ( parType == 'VELZ' ) THEN
    neq%par(ipar)%locq(1) = 1
    neq%par(ipar)%locq(3) = 3
    neq%par(ipar)%locq(4) = 3
    neq%par(ipar)%scale   = 1.d0

! earth orientation parameter
  ELSEIF ( parType == 'XPO'  ) THEN
    numdesc(3) = numdesc(3) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  1
    neq%par(ipar)%locq(5) =  1
    neq%par(ipar)%scale   = 1.d0
  ELSEIF ( parType == 'YPO'  ) THEN
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  2
    neq%par(ipar)%locq(5) =  1
    neq%par(ipar)%scale   = 1.d0

  ELSEIF ( parType == 'UT'   ) THEN
    numdesc(4) = numdesc(4) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  3
    neq%par(ipar)%locq(5) =  1
    neq%par(ipar)%scale   = 1.d0
! correct ut1_ut1r
    flgUt1r(ipar) = 1

  ELSEIF ( parType == 'XPOR' ) THEN
    numdesc(5) = numdesc(5) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  1
    neq%par(ipar)%locq(5) =  2
    neq%par(ipar)%scale   = 1.d0
    numERPpar(1) = numERPpar(1) + 1
    IF (neq%par(ipar)%time%mean > mxERPt(1)) &
      mxERPt(1) = neq%par(ipar)%time%mean
    IF (neq%par(ipar)%time%mean < mnERPt(1)) &
      mnERPt(1) = neq%par(ipar)%time%mean
  ELSEIF ( parType == 'YPOR' ) THEN
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  2
    neq%par(ipar)%locq(5) =  2
    neq%par(ipar)%scale   = 1.d0
    numERPpar(2) = numERPpar(2) + 1
    IF (neq%par(ipar)%time%mean > mxERPt(2)) &
      mxERPt(2) = neq%par(ipar)%time%mean
    IF (neq%par(ipar)%time%mean < mnERPt(2)) &
      mnERPt(2) = neq%par(ipar)%time%mean

  ELSEIF ( parType == 'LOD'  ) THEN
    numdesc(6) = numdesc(6) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  3
    neq%par(ipar)%locq(5) =  2
    neq%par(ipar)%scale   = 1.d0
    numERPpar(3) = numERPpar(3) + 1
    IF (neq%par(ipar)%time%mean > mxERPt(3)) &
      mxERPt(3) = neq%par(ipar)%time%mean
    IF (neq%par(ipar)%time%mean < mnERPt(3)) &
      mnERPt(3) = neq%par(ipar)%time%mean
! correct ut1_ut1r
    flgUt1r(ipar) = 1

  ELSEIF ( parType == 'UTR' ) THEN
    numdesc(7) = numdesc(7) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  3
    neq%par(ipar)%locq(5) =  1
    neq%par(ipar)%scale   = 1.d0

  ELSEIF ( parType == 'LODR' ) THEN
    numdesc(8) = numdesc(8) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  3
    neq%par(ipar)%locq(5) =  2
    neq%par(ipar)%scale   = 1.d0
    numERPpar(3) = numERPpar(3) + 1

  ELSEIF ( parType == 'NUT_OB' ) THEN
    numdesc(13) = numdesc(13) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  4
    neq%par(ipar)%locq(5) =  1
    neq%par(ipar)%scale   = 1.d0
    nut_unit = unit
  ELSEIF ( parType == 'NUT_LN' ) THEN
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  5
    neq%par(ipar)%locq(5) =  1
    neq%par(ipar)%scale   = 1.d0

  ELSEIF ( parType == 'NUTROB' ) THEN
    numdesc(14) = numdesc(14) + 1
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  4
    neq%par(ipar)%locq(5) =  2
    neq%par(ipar)%scale   = 1.d0
    numERPpar(4) = numERPpar(4) + 1
    IF (neq%par(ipar)%time%mean > mxERPt(4)) &
      mxERPt(4) = neq%par(ipar)%time%mean
    IF (neq%par(ipar)%time%mean < mnERPt(4)) &
      mnERPt(4) = neq%par(ipar)%time%mean
  ELSEIF ( parType == 'NUTRLN' ) THEN
    neq%par(ipar)%locq(1) = 10
    neq%par(ipar)%locq(4) =  5
    neq%par(ipar)%locq(5) =  2
    neq%par(ipar)%scale   = 1.d0
    numERPpar(5) = numERPpar(5) + 1
    IF (neq%par(ipar)%time%mean > mxERPt(5)) &
      mxERPt(5) = neq%par(ipar)%time%mean
    IF (neq%par(ipar)%time%mean < mnERPt(5)) &
      mnERPt(5) = neq%par(ipar)%time%mean

! GEO Centre coordinates
  ELSEIF ( parType(2:3) == 'GC') THEN
    IF ( parType == 'XGC') numdesc(9) = numdesc(9) + 1
    neq%par(ipar)%locq(1) = 16
    IF ( parType == 'XGC') neq%par(ipar)%locq(2) =  1
    IF ( parType == 'YGC') neq%par(ipar)%locq(2) =  2
    IF ( parType == 'ZGC') neq%par(ipar)%locq(2) =  3
    neq%par(ipar)%scale   = 1.d0
    neq%par(ipar)%time%mean = (sintim%t(2) + sintim%t(1))/2
    neq%par(ipar)%time%half = (sintim%t(2) - sintim%t(1))/2

! Satellite antenna offsets
  ELSEIF ( parType(1:5) == 'SATA_') THEN
    numdesc(19) = numdesc(19) + 1
    neq%par(ipar)%locq(1) = 12
    IF (solID /= '----')THEN
      READ (solID,'(i4)') neq%par(ipar)%locq(2)
    ELSE
      neq%par(ipar)%locq(2) = 0
    ENDIF
    IF (parType == 'SATA_X')neq%par(ipar)%locq(3) = 1
    IF (parType == 'SATA_Y')neq%par(ipar)%locq(3) = 2
    IF (parType == 'SATA_Z')neq%par(ipar)%locq(3) = 3
    neq%par(ipar)%locq(4) =  0
    READ(siteCode,"(1X,I3)")neq%par(ipar)%locq(5)
    IF (pointCode == 'L1')neq%par(ipar)%locq(6)=1
    IF (pointCode == 'L2')neq%par(ipar)%locq(6)=2
    IF (pointCode == 'LC')neq%par(ipar)%locq(6)=3
    CALL svn2prn(2,siteCode,neq%par(ipar)%time%mean,prn,timint,ios)
    CALL gtsensor(prn,neq%par(ipar)%time%mean,typeMWTR,satant)
    neq%par(ipar)%name = satant
    neq%par(ipar)%scale   = 1.d0
    neq%par(ipar)%time%half = neq%par(ipar)%time%mean - sintim%t(1)

  ELSEIF ( parType == 'RBIAS' ) THEN
    numdesc(15) = numdesc(15) + 1
    neq%par(ipar)%locq(1) = 26
    neq%par(ipar)%locq(4) = 1    !wavelength hardwired
    neq%par(ipar)%scale   = 1.d0
    neq%par(ipar)%name = siteCode  ! full station name is set in READSIN
    IF     ( pointCode == 'L1' )  THEN
      neq%par(ipar)%locq(3) = 1
      neq%par(ipar)%locq(5) = 951
    ELSEIF ( pointCode == 'L2' )  THEN
      neq%par(ipar)%locq(3) = 2
      neq%par(ipar)%locq(5) = 952
    ELSEIF ( pointCode == 'LC' )  THEN
      neq%par(ipar)%locq(5) = -10
    ELSEIF ( pointCode == 'E1' )  THEN
      neq%par(ipar)%locq(3) = 3
      neq%par(ipar)%locq(5) = 953
    ELSEIF ( pointCode == 'E2' )  THEN
      neq%par(ipar)%locq(3) = 4
      neq%par(ipar)%locq(5) = 954
    ELSEIF ( pointCode == 'EC' )  THEN
      neq%par(ipar)%locq(5) = -10
    ELSE
      WRITE(lfnerr,"(/,' *** SR READSIN: Range bias type not handled, ', &
           & /,'                 Point Code: ',A2,/)")           &
           pointCode
      CALL exitrc(2)
    ENDIF

  ELSEIF ( parType == 'TBIAS' ) THEN
    numdesc(16) = numdesc(16) + 1
    neq%par(ipar)%locq(1) = 2
    neq%par(ipar)%name = siteCode  ! full station name is set in READSIN
    IF     ( pointCode == 'L1' )  THEN
      neq%par(ipar)%locq(7) = 951
    ELSEIF ( pointCode == 'L2' )  THEN
      neq%par(ipar)%locq(7) = 952
    ELSEIF ( pointCode == 'LC' )  THEN
      neq%par(ipar)%locq(7) = 0
    ELSEIF ( pointCode == 'E1' )  THEN
      neq%par(ipar)%locq(7) = 953
    ELSEIF ( pointCode == 'E2' )  THEN
      neq%par(ipar)%locq(7) = 954
    ELSEIF ( pointCode == 'EC' )  THEN
      neq%par(ipar)%locq(7) = 0
    ELSE
      WRITE(lfnerr,"(/,' *** SR READSIN: Time bias type not handled, ', &
           & /,'                 Point Code: ',A2,/)")          &
           pointCode
      CALL exitrc(2)
    ENDIF

  ELSEIF ( parType == 'TROTOT' ) THEN
    numdesc(17) = numdesc(17) + 1
    READ(solId,'(i4)') solNum
    neq%par(ipar)%locq(1) = 6
    neq%par(ipar)%locq(2) = numdesc(17)
    neq%par(ipar)%locq(4) = 3
    neq%par(ipar)%locq(6) = solNum
    neq%par(ipar)%scale = 1.d0
    IF (neq%par(ipar)%time%mean > mxTRPt) mxTRPt=neq%par(ipar)%time%mean
    IF (neq%par(ipar)%time%mean < mnTRPt) mnTRPt=neq%par(ipar)%time%mean
  ELSEIF ( parType == 'TGNTOT' ) THEN
    numdesc(18) = numdesc(18) + 1
    READ(solId,'(i4)') solNum
    neq%par(ipar)%locq(1) = 6
    neq%par(ipar)%locq(4) = 1
    neq%par(ipar)%locq(6) = solNum
    neq%par(ipar)%scale = 1.d0
  ELSEIF ( parType == 'TGETOT' ) THEN
    READ(solId,'(i4)') solNum
    neq%par(ipar)%locq(1) = 6
    neq%par(ipar)%locq(4) = 2
    neq%par(ipar)%locq(6) = solNum
    neq%par(ipar)%scale = 1.d0


  ELSEIF ( parType == 'CN' ) THEN
    numdesc(20) = numdesc(20) + 1
    neq%par(ipar)%locq(1) = 13
    neq%par(ipar)%locq(4) = 1
    READ(solId,'(i4)')    neq%par(ipar)%locq(6)
    READ(siteCode,'(i4)') neq%par(ipar)%locq(5)
    neq%par(ipar)%locq(7) = numdesc(20)
    neq%par(ipar)%scale = 1.d0
  ELSEIF ( parType == 'SN' ) THEN
    numdesc(20) = numdesc(20) + 1
    neq%par(ipar)%locq(1) = 13
    neq%par(ipar)%locq(4) = 2
    READ(solId,'(i4)')    neq%par(ipar)%locq(6)
    READ(siteCode,'(i4)') neq%par(ipar)%locq(5)
    neq%par(ipar)%locq(7) = numdesc(20)
    neq%par(ipar)%scale = 1.d0

  ELSE
    WRITE(lfnerr,"(/,' *** SR REBLOCQ: Parameter type not handled, ', &
                 & /,'                 Parameter type:',A6,/)")       &
         parType
    CALL exitrc(2)
  ENDIF

! Set technique flag
! ------------------
  IF     ( techSNX == 'P' ) THEN
    CALL add_techn(neq%par(ipar), gnss=1, gps=1, glo=1)
    techFlg = 'GNSS'

  ELSEIF ( techSNX == 'L' ) THEN
    CALL add_techn(neq%par(ipar), slr=1)
    techFlg = 'SLR '

  ELSEIF ( techSNX == 'R' ) THEN
    CALL add_techn(neq%par(ipar), vlbi=1)
    techFlg = 'VLBI'

  ELSEIF ( techSNX == 'D' ) THEN
    CALL add_techn(neq%par(ipar), doris=1)
    techFlg = 'DORIS'

  ENDIF

  IF ( iFirst ) THEN
    WRITE(lfnprt, '(10X,A,1X,A/)') &
                  'Technique Flag for parameters:', techFlg
    iFirst = .FALSE.
  ENDIF

END SUBROUTINE reblocq

END MODULE
