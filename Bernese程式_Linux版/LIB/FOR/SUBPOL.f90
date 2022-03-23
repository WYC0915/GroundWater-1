MODULE s_SUBPOL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE subpol(time,subnam,erpsub,erpsur)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine computes subdaily erp corrections and erp
!             rates. The model is taken from the keyword 'SUBMOD'.
!             It replaces the old sr SUBPOL.f
!             If time == 0, only the name of the model is returned.
!
! Author:     Peter Steigenberger
!
! Created:    04-Dec-2002
! Last mod.:  19-May-2011
!
! Changes:    23-Apr-2003 RD: Use SR gtflna to get file name
!             06-May-2003 RD: Call gtflna with iexist=1
!             22-Dec-2003 PS: 5 minute buffering
!             09-Feb-2004 PS: Rate bug fixed
!             29-Jan-2007 AG: Extrapolation tolerance for litpol added
!             21-Oct-2008 HB: USE s_submod added
!             19-May-2011 HB: Array subcoe must be adapted
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_submod
  USE s_rdsubm
  USE s_litpol
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
  REAL(r8b)                        :: time    ! Epoch in MJD
  CHARACTER(LEN=16)                :: subnam  ! Subdaily Model Name
  REAL(r8b),DIMENSION(3)           :: erpsub  ! Subdaily ERP Correction
                                              ! i=1: X-Pole (ArcSec)
                                              ! i=2: Y-Pole (ArcSec)
                                              ! i=3: UT1    (Sec)
  REAL(r8b),DIMENSION(3)           :: erpsur  ! Subdaily ERP Rates
                                              ! i=1: X-Pole (ArcSec/Day)
                                              ! i=2: Y-Pole (ArcSec/Day)
                                              ! i=3: UT1    (Sec/Day)

! Local Variables
! ---------------
  CHARACTER(LEN=80)                     :: titles
  CHARACTER(LEN=16),SAVE                :: subnam2
  CHARACTER(LEN=fileNameLength)         :: filsub
  INTEGER,PARAMETER                     :: maxper=1000
  INTEGER(i4b),SAVE                     :: nsub
  INTEGER(i4b)                          :: irc
  LOGICAL,SAVE                          :: first = .TRUE.
  REAL(r8b),DIMENSION(6,6),SAVE         :: subfar
  REAL(r8b),DIMENSION(MAXPER)           :: subper
  REAL(r8b),DIMENSION(6,MAXPER),SAVE    :: subcoe
  INTEGER(i4b),DIMENSION(6,MAXPER),SAVE :: submlt

  REAL(r8b)                             :: deltt = 5D0   ! Buffer step size
                                                         ! in minutes
  REAL(r8b),DIMENSION(2),SAVE           :: tbuf
  REAL(r8b),DIMENSION(3,2),SAVE         :: subbuf
  REAL(r8b),DIMENSION(3,2),SAVE         :: surbuf


! Read Coefficients from File
  IF(first) THEN
    first = .FALSE.

    CALL gtflna(1,'SUBMOD',filsub,irc)

    CALL rdsubm(maxper,filsub,titles,subnam,subfar,nsub,  &
                subper,submlt(:,1:maxper),subcoe)
    subnam2=subnam
    tbuf(1)=1.d20
    tbuf(2)=0.d0

  END IF

  IF(time.eq.0.D0) THEN
    subnam=subnam2
  ELSE
    IF(time > tbuf(1) .AND. time < tbuf(2)) THEN
      CALL litpol(2,1,tbuf,subbuf(1,:),time,0d0,erpsub(1))
      CALL litpol(2,1,tbuf,subbuf(2,:),time,0d0,erpsub(2))
      CALL litpol(2,1,tbuf,subbuf(3,:),time,0d0,erpsub(3))
      CALL litpol(2,1,tbuf,surbuf(1,:),time,0d0,erpsur(1))
      CALL litpol(2,1,tbuf,surbuf(2,:),time,0d0,erpsur(2))
      CALL litpol(2,1,tbuf,surbuf(3,:),time,0d0,erpsur(3))
    ELSE
      tbuf(1)=time
      tbuf(2)=time+(deltt/1440.d0)
      CALL submod(tbuf(1),subfar,nsub,submlt,subcoe,subbuf(:,1),surbuf(:,1))
      erpsub=subbuf(:,1)
      erpsur=surbuf(:,1)
      CALL submod(tbuf(2),subfar,nsub,submlt,subcoe,subbuf(:,2),surbuf(:,2))
    ENDIF
  END IF

END SUBROUTINE subpol



END MODULE
