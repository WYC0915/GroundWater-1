! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_satmrk

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program SATMRK
!
! Author:     H. Bock
!
! Created:    17-SEP-2001
! Last mod.:  __-___-____
!
! Changes:    06-Mar-2003 MM: new option re-initialize ambiguities
!             19-Apr-2010 RD: Re-init. ambiguities only for one GNSS
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

! Options
! -------
  TYPE t_satmrk_opt
    CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam
    CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filSyc
    CHARACTER(LEN=fileNameLength) :: edtFil
    INTEGER(i4b) :: syc
    INTEGER(i4b) :: resAmb
    INTEGER(i4b) :: ambsys
    INTEGER(i4b) :: nFil
    INTEGER(i4b) :: yFil
    INTEGER(i4b) :: mrkTyp
    INTEGER(i4b) :: mrkFrq
    INTEGER(i4b),DIMENSION(2) :: mrkEpo
    INTEGER(i4b),DIMENSION(:),POINTER :: mrkSat
    REAL(r8b),DIMENSION(2) :: mrkTim
    INTEGER(i4b) :: minAmb
    INTEGER(i4b) :: iSampl
  END TYPE t_satmrk_opt

END MODULE p_satmrk
