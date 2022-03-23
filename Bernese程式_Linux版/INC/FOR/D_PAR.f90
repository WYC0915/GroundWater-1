! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_par

! -------------------------------------------------------------------------
! Purpose:    Parameter description
!
! Author:     R. Dach
!
! Created:    22-Sep-2005
! Last mod. : 01-Dec-2010
!
! Changes:    23-Jan-2008 RD: New version with name = chr*20
!             13-Jun-2008 RD: Add system/technique flags
!             19-Dec-2008 LM: parameter type (t_parType) added
!             07-Jan-2009 RD: initialization of typeDef is not possible here
!             27-Apr-2009 LM/SL: type and omega added to t_par and writepar,
!                                new block to read current NEQ
!             04-Jan-2010 SL: maxParTyp 26->27
!             15-Nov-2010 RD: Write omega in any case (version 7)
!             30-Nov-2010 DT: maxParTyp 27->28
!             01-Dec-2010 MM: maxParTyp 27->30
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  ! Modules
  ! -------
  USE m_bern
  USE m_time, ONLY: t_time, t_timint

  IMPLICIT NONE

  ! Dimensions
  ! ----------
  INTEGER(i4b),PARAMETER  :: maxParTyp = 30
  INTEGER(i4b),PARAMETER  :: maxlcq    =  7   ! Size of the locq in t_par

  ! Parameter Types
  ! ---------------
  INTEGER(i4b), PARAMETER :: parTypeLength = 2

  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_epochSpecific     = "ES"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_constant          = "CC"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_linear            = "L "
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_linearLeftPoint   = "LL"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_linearMiddlePoint = "LM"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_linearRightPoint  = "LR"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_periodic          = "P "
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_periodicOffset    = "PO"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_periodicDrift     = "PD"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_periodicCos       = "PC"
  CHARACTER(LEN=parTypeLength), PARAMETER :: parType_periodicSin       = "PS"

  ! Old Parameter Structure
  ! -----------------------
  TYPE t_par_v2
    INTEGER(i4b),DIMENSION(maxlcq)     :: locq    ! locq according to GPSEST
    CHARACTER(LEN=16)                  :: name    ! (usually) station name
    TYPE(t_time)                       :: time    ! validity interval (MJD)
    REAL(r8b)                          :: x0      ! a priori value
    REAL(r8b)                          :: scale   ! Scale factor
  END TYPE t_par_v2

  ! New Parameter Structure
  ! -----------------------
  TYPE t_par
    INTEGER(i4b),DIMENSION(maxlcq)     :: locq    ! locq according to GPSEST
    CHARACTER(LEN=20)                  :: name    ! (usually) station name
    TYPE(t_time)                       :: time    ! validity interval (MJD)
    REAL(r8b)                          :: x0      ! a priori value
    REAL(r8b)                          :: scale   ! Scale factor
    CHARACTER(LEN=1), DIMENSION(2)     :: techn   ! Contributing technique
                                                  ! indicator
    TYPE(t_timint)                     :: obstim  ! observed interval (MJD)
    CHARACTER(LEN=parTypeLength)       :: type    ! parameter type
    REAL(r8b)                          :: omega   ! for periodic
  END TYPE t_par

CONTAINS

  ! ---------------------------------------------------------------------------
  ! Read a parameter from a file
  ! ---------------------------------------------------------------------------
  FUNCTION readpar(lfn,bin,version)
    TYPE(t_par)                        :: readpar
    INTEGER(i4b)                       :: lfn     ! file number
    INTEGER(i4b)                       :: bin     ! (1: binary, 2: ascii)
    INTEGER(i4b)                       :: version ! neq version

    ! Local variables
    TYPE(t_par_v2)                     :: par_v2
    INTEGER(i4b),DIMENSION(2)          :: iHelp
    INTEGER(i4b)                       :: jj

    ! Read parameter for NEQ version 1 or 2
    ! -------------------------------------
    IF (version <= 2) THEN
      ! from a binary neq file
      IF (bin == 1) THEN
        READ(lfn) par_v2%locq ,  &
             par_v2%name ,  &
             par_v2%time ,  &
             par_v2%x0   ,  &
             par_v2%scale
      ! from a formatted neq file
      ELSE
        READ(lfn,*)  (par_v2%locq(jj), jj=1,maxLcq)
        READ(lfn,'( A16,/, 2(E22.15,1X,E22.15,/) )')    &
                    par_v2%name,                        &
                    par_v2%time%mean, par_v2%time%half, &
                    par_v2%x0,        par_v2%scale
      ENDIF

      ! Conversion from version 1 and 2
      ! -------------------------------
      readpar%locq  = par_v2%locq
      readpar%name  = ''
      readpar%name  = par_v2%name
      readpar%time  = par_v2%time
      readpar%x0    = par_v2%x0
      readpar%scale = par_v2%scale
      CALL init_techn(readpar)
      CALL add_techn(readpar,gnss=1,gps=1,glo=1)
      readpar%obstim%t(1) = par_v2%time%mean - par_v2%time%half
      readpar%obstim%t(2) = par_v2%time%mean + par_v2%time%half
      readpar%type  = ""
      readpar%omega = 0D0

    ! Read parameter from version 3 and 4
    ! -----------------------------------
    ELSE IF (version <= 4) THEN
      IF (bin == 1) THEN
        READ(lfn) readpar%locq, &
             readpar%name,      &
             readpar%time,      &
             readpar%obstim,    &
             readpar%x0,        &
             readpar%scale,     &
             readpar%techn
      ELSE
        READ(lfn,*)  (readpar%locq(jj), jj=1,maxLcq)
        READ(lfn,'( A20,/, 3(E22.15,1X,E22.15,/),2(B8.8,/) )') &
                    readpar%name,                         &
                    readpar%time%mean, readpar%time%half, &
                    readpar%obstim%t,                     &
                    readpar%x0,        readpar%scale,     &
                    iHelp
        DO jj = 1,2
          readpar%techn(jj) = achar(iHelp(jj))
        ENDDO
      ENDIF
      readpar%type  = ""
      readpar%omega = 0D0

    ! Read parameter from current NEQ version
    ! --------------------------------------
    ELSE
      IF (bin == 1) THEN
        READ(lfn) readpar%locq, &
             readpar%name,      &
             readpar%time,      &
             readpar%obstim,    &
             readpar%x0,        &
             readpar%scale,     &
             readpar%techn,     &
             readpar%type
        IF (readpar%type(1:1) == 'P' .OR. version >= 7) THEN
          READ(lfn) readpar%omega
        ELSE
          readpar%omega = 0D0
        ENDIF
      ELSE
        READ(lfn,*)  (readpar%locq(jj), jj=1,maxLcq)
        READ(lfn,'( A20,/, 3(E22.15,1X,E22.15,/),2(B8.8,/), A2)') &
                    readpar%name,                         &
                    readpar%time%mean, readpar%time%half, &
                    readpar%obstim%t,                     &
                    readpar%x0,        readpar%scale,     &
                    iHelp,             readpar%type
        IF (readpar%type(1:1) == 'P' .OR. version >= 7) THEN
          READ(lfn,'(E22.15)') readpar%omega
        ELSE
          readpar%omega = 0D0
        ENDIF
        DO jj = 1,2
          readpar%techn(jj) = achar(iHelp(jj))
        ENDDO
      ENDIF
    ENDIF

! Define the parameter types for old neqs
! ---------------------------------------
    IF (version <= 6 .AND. readpar%type == '') THEN
      IF (readPar%time%half == 0d0) THEN
        IF (readpar%locq(1) == 23 .OR. readpar%locq(1) == 24) THEN
          readpar%type = parType_epochSpecific
        ELSE
          readpar%type = parType_linear
        ENDIF
      ELSE
        readpar%type = parType_constant
      ENDIF
    ENDIF

  END FUNCTION

  ! ---------------------------------------------------------------------------
  ! Write a parameter to a file
  ! ---------------------------------------------------------------------------
  SUBROUTINE writepar(lfn,bin,par)
    INTEGER(i4b)                            :: lfn     ! file number
    INTEGER(i4b)                            :: bin     ! (1: binary, 2: ascii)
    TYPE(t_par)                             :: par     ! parameter

    ! Local variables
    CHARACTER(LEN=lineLength), DIMENSION(2) :: help
    INTEGER(i4b)                            :: jj

    IF (bin == 1) THEN
      WRITE(lfn) par%locq ,par%name ,par%time , par%obstim,  &
                 par%x0   ,par%scale,par%techn, par%type
      WRITE(lfn) par%omega
    ELSE
      help(1) = ''
      IF ( is_techn(par,gnss=1 ) ) WRITE(help(1)( 1: 5),'(A)') 'GNSS '
      IF ( is_techn(par,vlbi=1 ) ) WRITE(help(1)(11:15),'(A)') 'VLBI '
      IF ( is_techn(par,slr=1  ) ) WRITE(help(1)(21:25),'(A)') 'SLR  '
      IF ( is_techn(par,llr=1  ) ) WRITE(help(1)(31:35),'(A)') 'LLR  '
      IF ( is_techn(par,doris=1) ) WRITE(help(1)(41:45),'(A)') 'DORIS'

      help(2) = ''
      IF ( is_techn(par,gps=1    ) ) WRITE(help(2)( 1: 7),'(A)') 'GPS    '
      IF ( is_techn(par,glonass=1) ) WRITE(help(2)(11:17),'(A)') 'GLONASS'
      IF ( is_techn(par,galileo=1) ) WRITE(help(2)(21:27),'(A)') 'GALILEO'
      IF ( is_techn(par,sbas=1   ) ) WRITE(help(2)(31:37),'(A)') 'SBAS   '

      WRITE(lfn,'(7(I9,1X))')  (par%locq(jj), jj=1,maxLcq)
      WRITE(lfn,'( A20,/, 3(E22.15,1X,E22.15,/),2(B8.8,3X,A,/),A2 )') &
                  par%name,                             &
                  par%time%mean, par%time%half,         &
                  par%obstim%t,                         &
                  par%x0,        par%scale,             &
                  iachar(par%techn(1)),  TRIM(help(1)), &
                  iachar(par%techn(2)),  TRIM(help(2)), &
                  par%type
      WRITE(lfn,'(E22.15)') par%omega
    ENDIF

  END SUBROUTINE

  ! ---------------------------------------------------------------------------
  ! Handle technique flags
  ! ---------------------------------------------------------------------------

  ! Reset flags
  ! -----------
  SUBROUTINE init_techn(par)
    use s_clrflg
    TYPE(t_par)                        :: par
    INTEGER(i4b)                       :: ii,jj
    DO jj = 1,2
      DO ii = 0,7
        CALL clrflg(par%techn(jj),ii)
      ENDDO
    ENDDO
  END SUBROUTINE

  ! Add a new technique
  ! -------------------
  SUBROUTINE add_techn(par,gnss,vlbi,slr,llr,doris, &
                           gps,glonass,glo,galileo,gal,sbas)
    use s_setflg
    TYPE(t_par)           :: par
    INTEGER(i4b),OPTIONAL :: gnss
    INTEGER(i4b),OPTIONAL :: vlbi
    INTEGER(i4b),OPTIONAL :: slr
    INTEGER(i4b),OPTIONAL :: llr
    INTEGER(i4b),OPTIONAL :: doris
    INTEGER(i4b),OPTIONAL :: gps
    INTEGER(i4b),OPTIONAL :: glonass
    INTEGER(i4b),OPTIONAL :: glo
    INTEGER(i4b),OPTIONAL :: galileo
    INTEGER(i4b),OPTIONAL :: gal
    INTEGER(i4b),OPTIONAL :: sbas

    IF ( PRESENT(gnss)  ) CALL setflg(par%techn(1),0)
    IF ( PRESENT(vlbi)  ) CALL setflg(par%techn(1),1)
    IF ( PRESENT(slr)   ) CALL setflg(par%techn(1),2)
    IF ( PRESENT(llr)   ) CALL setflg(par%techn(1),3)
    IF ( PRESENT(doris) ) CALL setflg(par%techn(1),4)

    IF ( PRESENT(gps)    ) CALL setflg(par%techn(2),0)
    IF ( PRESENT(glonass)) CALL setflg(par%techn(2),1)
    IF ( PRESENT(glo)    ) CALL setflg(par%techn(2),1)
    IF ( PRESENT(galileo)) CALL setflg(par%techn(2),2)
    IF ( PRESENT(gal)    ) CALL setflg(par%techn(2),2)
    IF ( PRESENT(sbas)   ) CALL setflg(par%techn(2),3)
  END SUBROUTINE

  ! Test for a new technique
  ! ------------------------
  FUNCTION is_techn(par,gnss,vlbi,slr,llr,doris, &
                        gps,glonass,glo,galileo,gal,sbas)
    use f_tstflg
    TYPE(t_par)                        :: par
    INTEGER(i4b),OPTIONAL              :: gnss
    INTEGER(i4b),OPTIONAL              :: vlbi
    INTEGER(i4b),OPTIONAL              :: slr
    INTEGER(i4b),OPTIONAL              :: llr
    INTEGER(i4b),OPTIONAL              :: doris
    INTEGER(i4b),OPTIONAL              :: gps
    INTEGER(i4b),OPTIONAL              :: glonass
    INTEGER(i4b),OPTIONAL              :: glo
    INTEGER(i4b),OPTIONAL              :: galileo
    INTEGER(i4b),OPTIONAL              :: gal
    INTEGER(i4b),OPTIONAL              :: sbas
    LOGICAL                            :: is_techn
    LOGICAL                            :: isit
    isit = .FALSE.
    IF ( PRESENT(gnss)  ) isit = isit .OR. tstflg(par%techn(1),0)
    IF ( PRESENT(vlbi)  ) isit = isit .OR. tstflg(par%techn(1),1)
    IF ( PRESENT(slr)   ) isit = isit .OR. tstflg(par%techn(1),2)
    IF ( PRESENT(llr)   ) isit = isit .OR. tstflg(par%techn(1),3)
    IF ( PRESENT(doris) ) isit = isit .OR. tstflg(par%techn(1),4)

    IF ( PRESENT(gps)    ) isit = isit .OR. tstflg(par%techn(2),0)
    IF ( PRESENT(glonass)) isit = isit .OR. tstflg(par%techn(2),1)
    IF ( PRESENT(glo)    ) isit = isit .OR. tstflg(par%techn(2),1)
    IF ( PRESENT(galileo)) isit = isit .OR. tstflg(par%techn(2),2)
    IF ( PRESENT(gal)    ) isit = isit .OR. tstflg(par%techn(2),2)
    IF ( PRESENT(sbas)   ) isit = isit .OR. tstflg(par%techn(2),3)
    is_techn = isit
  END FUNCTION

  ! Merge techniques from two parameters
  ! ------------------------------------
  SUBROUTINE merge_techn(par1,par2)
    use s_clrflg
    use s_setflg
    use f_tstflg
    TYPE(t_par)                        :: par1,par2
    INTEGER(i4b)                       :: ii,jj
    CHARACTER(LEN=1)                   :: new
    DO jj=1,2
      DO ii = 0,7
        CALL clrflg(new,ii)
        IF ( tstflg(par1%techn(jj),ii) .OR. tstflg(par2%techn(jj),ii) ) THEN
          CALL setflg(new,ii)
        ENDIF
      ENDDO
      par1%techn(jj) = new
    ENDDO
  END SUBROUTINE

  ! ---------------------------------------------------------------------------
  ! Handle parameter type flags
  ! ---------------------------------------------------------------------------
  FUNCTION isParTyp(par,parTyp)
    TYPE(t_par)                        :: par
    CHARACTER(LEN=parTypeLength)       :: parTyp
    LOGICAL                            :: isParTyp
    INTEGER(i4b)                       :: ii
    ii = LEN_TRIM(parTyp)
    isParTyp = ( par%type(1:ii) == parTyp(1:ii) )
  END FUNCTION isParTyp

END MODULE d_par
