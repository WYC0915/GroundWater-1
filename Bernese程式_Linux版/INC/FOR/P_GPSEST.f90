
! -------------------------------------------------------------------------
! Bernese Software Version 5.2
! -------------------------------------------------------------------------

MODULE p_gpsest

! -------------------------------------------------------------------------
! Purpose:    Maximum dimensions and structures for program GPSEST
!
! Author:     L. Mervart
!
! Created:    12-Feb-2003
!
! Changes:    21-Nov-2004 LM: completely rewritten
!             07-Feb-2005 HB: LARGE: MAXAMP 2800-> 3500, MAXPAR 4000-> 5000
!             02-Nov-2005 AG: LARGE: MAXPAR 5000-> 6000, MAXOFF 30-> MAXSAT,
!                             MAXSPV 30-> MAXSAT
!             10-Jan-2006 RD: Statistics on phase-connected epochs
!             19-Jan-2006 HB: Modifications for phase-connected epochs
!             23-Jan-2006 HB: Set t_ambTimeRec also to PUBLIC
!             13-Feb-2006 HB: ambTime%ambInt()%clu allocated with numamb
!             28-Oct-2008 DT: Remove maxVar (will be taken from M_MAXDIM)
!             04-May-2009 RD: Scaling of loading models added
!             21-Sep-2009 RD: MAXSHD shifted to M_MAXDIM.f90
!             16-Nov-2010 HB: MAXAMB 300=>1000, MAXSTC 97=>241
!             16-Nov-2010 RD: add "red_parTyp"
!             06-Dez-2010 MM: GNSS-specific parameter type
!             12-Jul-2011 PS: MAXPAR 6000 => 7500
!             17-Aug-2011 HB: Correct bug in SR size_ambtime
!             09-Sep-2011 LP: MAXSTA FROM 200 TO 250
!             12-May-2012 RD: Remove DIMENSIONS except of LARGE
!             12-May-2012 RD: Use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
!
!  20-APR-00 RD: MAXAMP 1200->1500 AND MAXPAR 2200->2500
!  15-JAN-01 SS: MAXSTA FROM 150 TO 180
!  05-SEP-01 SS: MAXSNG FROM 155 TO 300
!  05-OCT-01 DI: MAXLOC FROM 15000 TO 20000
!  28-FEB-02 SS: MAXSNG FROM 300 TO 550
!  28-AUG-02 SS: MAXTRM FROM 1201 TO 1801
!  12-NOV-02 SS: MAXPAR FROM 3500 TO 4000
!  12-NOV-02 RS: MAXSPV ADDED
!  07-MAR-03 MR: MAXTYP FROM 24 TO 26
!  08-MAR-03 MR: MAXAMB MOVED TO M_MAXDIM
!  27-JUN-03 SS: MAXSTA FROM 180 TO 200
!  03-NOV-03 HB: SMALL: MAXPAR 300 -> 600, MAXAMP 200 -> 400,
!                       MAXTRM 80 -> 100
!  03-NOV-03 SS: MAXAMP FROM 2600 TO 2800
!  27-JAN-04 HU: MAXSHD ADDED
!  07-APR-04 HB: SMALL: MAXTRM 100 -> 600, MAXAMP 400 -> 500,
!                       MAXPAR 600 -> 700
!

  USE m_bern,   ONLY: i4b, r8b, staNameLength, staNam2Length
  USE m_time,   ONLY: t_timint
  USE d_par,    ONLY: mxlq => maxlcq, &
                      mxtp => maxParTyp
  USE d_grid,   ONLY: typLen
  USE m_maxdim, ONLY: mxst => maxsat

  IMPLICIT NONE

  PRIVATE

  ! Public data types
  ! -----------------
  PUBLIC :: t_ambTime, t_ambTimeRec, t_optLoad, t_isbTime, t_partyp, t_optGsp

  ! Public routines
  ! ---------------
  PUBLIC :: init_ambtime, size_ambtime, red_parTyp

  ! Maximal Numbers of ...
  ! ----------------------
  INTEGER(i4b), PUBLIC :: maxsat ! satellites
  INTEGER(i4b), PUBLIC :: maxamb ! ambiguities for one satellite in a file
  INTEGER(i4b), PUBLIC :: maxfrq ! different freq. to be processed per file
  INTEGER(i4b), PUBLIC :: maxfil ! files to be processed
  INTEGER(i4b), PUBLIC :: maxsta ! stations involved
  INTEGER(i4b), PUBLIC :: maxarc ! satellite arcs
  INTEGER(i4b), PUBLIC :: maxpar ! parameters simultaneously processed
  INTEGER(i4b), PUBLIC :: maxamp ! ambiguity parameters simultan. processed and
                                 ! parameters to be pre-eliminated in one step
  INTEGER(i4b), PUBLIC :: maxloc ! parameters to be processed
  INTEGER(i4b), PUBLIC :: maxcmp ! campaigns to be processed
  INTEGER(i4b), PUBLIC :: maxtrm ! troposphere models
  INTEGER(i4b), PUBLIC :: maxtrp ! troposphere parameters per model
  INTEGER(i4b), PUBLIC :: maxsng ! non-zero elements in one line of first design
                                 ! matrix
  INTEGER(i4b), PUBLIC :: maxfls ! files in a session
  INTEGER(i4b), PUBLIC :: maxsas ! satellites at one epoch
  INTEGER(i4b), PUBLIC :: maxeqn ! = maxfls * maxsas
  INTEGER(i4b), PUBLIC :: maxlcq ! items per parameter in array "locq"
  INTEGER(i4b), PUBLIC :: maxfrs ! different frequencies allowed
  INTEGER(i4b), PUBLIC :: maxwgt ! satellite specific weight intervals
  INTEGER(i4b), PUBLIC :: maxpol ! earth rotation parameter sets
  INTEGER(i4b), PUBLIC :: maxstc ! stochastic epochs per arc
  INTEGER(i4b), PUBLIC :: maxstp ! stochastic special requests
  INTEGER(i4b), PUBLIC :: maxoff ! satellite antenna offset groups
  INTEGER(i4b), PUBLIC :: maxofr ! satellite antenna offset estim. requests
  INTEGER(i4b), PUBLIC :: maxtyp ! parameter types
  INTEGER(i4b), PUBLIC :: maxhil ! hill-type resonance parameters
  INTEGER(i4b), PUBLIC :: maxpot ! potential parameters
  INTEGER(i4b), PUBLIC :: maxalb ! satellite groups for albedo estimation
  INTEGER(i4b), PUBLIC :: maxcal ! antenna phase center estimation requests
  INTEGER(i4b), PUBLIC :: maxgim ! global/local ionosphere models
  INTEGER(i4b), PUBLIC :: maxgit ! terms per global/local ionosphere model
  INTEGER(i4b), PUBLIC :: maxmea ! measurements type
  INTEGER(i4b), PUBLIC :: maxspv ! satellite antenna phase center groups

  PARAMETER (MAXAMB=1000,MAXFRQ=   2,                         &
             MAXFIL= 250,MAXSTA= 250,MAXARC=  20,MAXPAR=7500, &
             MAXAMP=3500,MAXLOC=20000,MAXCMP=  6,MAXTRM=1801, &
             MAXTRP=  10,MAXSNG= 550,                         &
             MAXLCQ=mxlq,MAXFRS=   5,MAXWGT=  20,MAXPOL=  70, &
             MAXSTC= 241,MAXSTP=  97,MAXOFF=mxst,MAXOFR= 300, &
             MAXTYP=mxtp,MAXHIL= 250,MAXPOT=  20,MAXALB=  10, &
             MAXCAL=  20,MAXMEA=   3,MAXSPV=mxst)

  TYPE t_isbTime
    CHARACTER(LEN=staNam2Length)        :: recnam
    INTEGER(i4b), DIMENSION(2)          :: recnum
    REAL(r8b)                           :: isbint
  END TYPE t_isbTime

  TYPE t_optLoad
    CHARACTER(LEN=typLen)               :: keyw
    INTEGER(i4b)                        :: nSta
    CHARACTER(LEN=staNameLength),        &
                  DIMENSION(:), POINTER :: staLst
    INTEGER(i4b), DIMENSION(:), POINTER :: staClu
    INTEGER(i4b)                        :: nPar
    REAL(r8b),    DIMENSION(3)          :: sigma
  END TYPE t_optLoad

  TYPE t_ambTimeRec
    TYPE(t_timint) :: int  ! Connected interval
    INTEGER(i4b)   :: ref  ! Reference set: 0=no 1=code 2=constraint
    INTEGER(i4b),DIMENSION(:), POINTER :: clu  ! Ambiguity cluster numbers
                                               ! within the interval
  END TYPE t_ambTimeRec

  TYPE t_ambTime
    INTEGER(i4b) :: nInter
    TYPE(t_ambTimeRec), DIMENSION(:), POINTER :: ambInt
  END TYPE t_ambTime

  TYPE t_parTyp
    CHARACTER(LEN=2) :: type
    REAL(r8b)        :: omega
  END TYPE t_parTyp

  TYPE t_optGsp
    INTEGER(i4b)            :: traSys  ! = 0: none
                                       ! = 1: GLONASS
                                       ! = 2: Galileo
                                       ! = 3: GLO/Gal
    REAL(r8b),DIMENSION(3)  :: traSig  ! Sigmas for GNSS-spec translations
                                       ! 1: Horizontal component (N)
                                       ! 2: Horizontal component (E)
                                       ! 3: Vertival component   (U)
    INTEGER(i4b)            :: trpSys  ! = 0: none
                                       ! = 1: GLONASS
                                       ! = 2: Galileo
                                       ! = 3: GLO/Gal
    REAL(r8b)               :: trpSig  ! Sigma for troposphere biases
  END TYPE t_optGsp

CONTAINS

! Init the t_ambTime structure
! ----------------------------
  SUBROUTINE init_ambTime(ambTime)
    IMPLICIT NONE
    TYPE(t_ambTime) :: ambTime

    ambTime%nInter = 0
    NULLIFY(ambTime%ambInt)
  END SUBROUTINE init_ambTime

! Check the size of the t_ambTime structure, increase the size if necessary
! -------------------------------------------------------------------------
  SUBROUTINE size_ambTime(ambTime,nSize,aSize,bSize)
    USE m_bern,   ONLY: i4b
    USE s_alcerr
    IMPLICIT NONE

    TYPE(t_ambTime) :: ambTime
    INTEGER(i4b)    :: nSize    ! Next size
    INTEGER(i4b)    :: aSize    ! Size to add if size of ambTime is too small
    INTEGER(i4b)    :: bSize    ! Size to add if size of clu is too small

    TYPE(t_ambTimeRec), DIMENSION(:), POINTER :: hlp
    INTEGER(i4b)                              :: iSize,jSize,kSize,lSize
    INTEGER(i4b)                              :: iac

    NULLIFY(hlp)

    ! There is already an array allocated
    IF (ASSOCIATED(ambTime%ambInt)) THEN
      iSize = SIZE(ambTime%ambInt)

      ! Check the size
      IF (iSize < nSize) THEN

      ! Reallocate, increase
        ALLOCATE(hlp(iSize),stat=iac)
        CALL alcerr(iac,'hlp',(/iSize/),'P_GPSEST:size_ambTime')

        jSize=SIZE(ambTime%ambInt(1)%clu)
        DO kSize = 1,iSize
          NULLIFY(hlp(kSize)%clu)
          ALLOCATE(hlp(kSize)%clu(jSize),stat=iac)
          CALL alcerr(iac,'hlp%clu',(/jSize/),'P_GPSEST:size_ambTime')
          ! Copy into hlp
          hlp(kSize)%ref=ambTime%ambInt(kSize)%ref
          hlp(kSize)%int%t(1:2)=ambTime%ambInt(kSize)%int%t(1:2)
          DO lSize = 1,jSize
            hlp(kSize)%clu(lSize)=ambTime%ambInt(kSize)%clu(lSize)
          ENDDO
          DEALLOCATE(ambTime%ambInt(kSize)%clu)
        ENDDO

        ! Deallocate and reallocate a larger structure
        DEALLOCATE(ambTime%ambInt)
        ALLOCATE(ambTime%ambInt(iSize+aSize),stat=iac)
        CALL alcerr(iac,'ambTime%ambInt',(/iSize+aSize/),'P_GPSEST:size_ambTime')
        DO kSize = 1,iSize+aSize
          NULLIFY(ambTime%ambInt(kSize)%clu)
          ALLOCATE(ambTime%ambInt(kSize)%clu(jSize),stat=iac)
          CALL alcerr(iac,'ambTime%ambInt(iSize)%clu',(/jSize/),'P_GPSEST:size_ambTime')
        ENDDO

        ! Copy from hlp
        DO kSize = 1,iSize
          DO lSize = 1,jSize
            ambTime%ambInt(kSize)%clu(lSize)=hlp(kSize)%clu(lSize)
          ENDDO
          ambTime%ambInt(kSize)%ref=hlp(kSize)%ref
          ambTime%ambInt(kSize)%int%t(1:2)=hlp(kSize)%int%t(1:2)
        ENDDO
        DO kSize = iSize+1,iSize+aSize
          ambTime%ambInt(kSize)%clu(1:jSize)=-1000
        ENDDO

        DO kSize=1,iSize
          DEALLOCATE(hlp(kSize)%clu)
        ENDDO
        DEALLOCATE(hlp)

      ENDIF
      ! First call, allocate
    ELSE

      ALLOCATE(ambTime%ambInt(aSize),stat=iac)
      CALL alcerr(iac,'ambTime%ambInt',(/aSize/),'P_GPSEST:size_ambTime')

      DO iSize = 1,aSize
        NULLIFY(ambTime%ambInt(iSize)%clu)
        ALLOCATE(ambTime%ambInt(iSize)%clu(bSize),stat=iac)
        CALL alcerr(iac,'ambTime%ambInt(iSize)%clu',(/bSize/),'P_GPSEST:size_ambTime')
      ENDDO

      DO iSize = 1,aSize
        ambTime%ambInt(iSize)%clu(1:bSize)=-1000
      ENDDO

    ENDIF

  END SUBROUTINE size_ambTime

! Reduce the partyp structure according to "idel"
! -----------------------------------------------
  SUBROUTINE red_parTyp(npar,idel,partyp)
    USE m_bern,   ONLY: i4b
    IMPLICIT NONE

    INTEGER(i4b)                    :: npar
    INTEGER(i4b),   DIMENSION(npar) :: idel
    TYPE(t_parTyp), DIMENSION(npar) :: partyp

    INTEGER(i4b)                    :: ii,ip

        ii = 0
        DO ip = 1,npar
          IF ( idel(ip) == 0 ) THEN
            ii = ii + 1
            if (ii /= ip) partyp(ii) = partyp(ip)
          ENDIF
        ENDDO
        DO ip = ii+1,npar
          partyp(ip)%type  = ''
          partyp(ip)%omega = 0d0
        ENDDO
  END SUBROUTINE red_parTyp

END MODULE p_gpsest
