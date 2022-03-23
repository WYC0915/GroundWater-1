MODULE s_MYGETARG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE mygetarg(argument, forceGetarg)

! -------------------------------------------------------------------------
! Purpose:    Returns the command line or reads the standard input
!
! Author:     L. Mervart
!
! Created:    15-Aug-2002
! Last mod.:  27-Apr-2010
!
! Changed:    10-Mar-2003 LM: Corrected to use correct getarg
!             14-Mar-2003 HU: Using CMP_SOLARIS
!             27-Jun-2003 HU/LM: f90_unix_env for LF95 removed
!             07-Feb-2005 TF: IFC8 added, IFC7 for old IFC handling
!             19-Dec-2007 LM: CMP_GNU added
!             27-Apr-2010 SL: GNU->G95, GNU added with same definition
!             27-Nov-2012 EO: PG_F90 added
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

#ifdef getarg_supported
#undef getarg_supported
#endif

#ifdef CMP_COMPAQ
#define getarg_supported
  USE dflib,            ONLY: getarg
  USE dfport,           ONLY: iargc
#endif

#ifdef CMP_LF95
#define getarg_supported
  USE service_routines, ONLY: getarg, iargc
#endif

#ifdef CMP_LF90
#define getarg_supported
#endif

#ifdef CMP_G95
#define getarg_supported
#elif CMP_GNU
#define getarg_supported
#endif

#ifdef OS_WIN32
#ifdef CMP_PG_F90
#define getarg_supported
USE dfport,           ONLY: getarg, iargc
#endif
#endif

#ifdef CMP_NAG
#define getarg_supported
  USE f90_unix_env,     ONLY: getarg, iargc
#endif

#ifdef CMP_IFC7
#define getarg_supported
  USE IFLPORT,     ONLY: getarg, iargc
#endif

#ifdef CMP_IFC8
#define getarg_supported
  USE IFPOSIX,     ONLY: pxfgetarg, ipxfargc
#endif

  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*), INTENT(OUT)           :: argument
  LOGICAL         , INTENT(IN), OPTIONAL  :: forceGetarg

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength) :: hlpStr
  INTEGER(i4b)              :: ii,arglength,irc

#ifdef CMP_SOLARIS
#define getarg_supported
  INTEGER :: iargc
#endif

#ifdef OS_UNIX
  IF (PRESENT(forceGetarg)) THEN
    IF (forceGetarg) THEN
#endif

#ifdef getarg_supported

#ifdef CMP_LF90
      CALL getcl(argument)
#else
      argument = ''
#ifdef CMP_IFC8
      DO ii = 1, ipxfargc()
        CALL pxfgetarg(ii, hlpStr,arglength,irc)
        argument = trim(argument) // ' ' // trim(hlpStr)
      END DO
#else
      DO ii = 1, iargc()
        CALL getarg(ii, hlpStr)
        argument = trim(argument) // ' ' // trim(hlpStr)
      END DO
#endif
      argument = ADJUSTL(argument)
#endif

#else
      WRITE(*,*) '*** mygetarg: getarg not supported'
      CALL exitrc(2)
#endif

#ifdef OS_UNIX
      RETURN
    END IF
  END IF
  READ(*,'(A)') argument
#endif

END SUBROUTINE mygetarg


END MODULE
