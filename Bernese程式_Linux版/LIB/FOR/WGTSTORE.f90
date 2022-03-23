MODULE s_WGTSTORE
CONTAINS

! ---------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ---------------------------------------------------------------------

SUBROUTINE wgtstore(hlmOut)

! ---------------------------------------------------------------------
! Purpose:    Write WGT file with variance rescaling factors and
!             Helmert parameters estimated by ADDNEQ2.
!
! Author:     C. Urschl
!
! Created:    31-May-2005
! Last mod.:  02-Dec-2010
!
! Changes:    02-Dec-2010 DT: Add estimated Helmert parameters;
!                             Call of SR only once (not per NEQ);
!                             Format for scale F8.4->F9.5
!
! SR used:    gtflna, opnfil, opnerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ---------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_addneq, ONLY: comstat, opt, t_hlmFil
  USE d_const,  ONLY: filTitle, ars
  USE s_opnerr
  USE s_opnfil
  USE s_gtflna


  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_hlmFil)                     :: hlmOut

! Local Variables
! ---------------
  INTEGER(i4b)                       :: ifil
  INTEGER(i4b)                       :: ios
  INTEGER(i4b)                       :: ii
  INTEGER(i4b)                       :: lfn003

  lfn003 = lfn002 + 1

  IF ( opt%wgtout == '' ) RETURN

! Open wgt file
! -------------
  CALL opnfil(lfn003,opt%wgtout,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfn003,ios,opt%wgtout,'COVCOMO')

! Print header
! ------------
  WRITE(lfn003,'(2(A,/),2(2A,/),83X,A,/,3A)')                             &
        filTitle,                                                         &
        'COVARIANCE COMPONENT ESTIMATION AND HELMERT PARAMETERS',         &
        '------------------------------------------------------',         &
        '--------------------------',                                     &
        'NUM  VALUE                 FILENAME                        ',    &
        '  GRP  HELMERT PARAM HELMERT VALUES (APR./EST.) IN M, MAS, PPM', &
        'TX       TY       TZ       RX       RY       RZ      SC',        &
        '***  ********************  ********************************',    &
        '  ***  * * * * * * * ******** ******** ******** ******** ',      &
        '******** ******** ********'

! Print wgt records
! -----------------
  DO ifil = 1, hlmOut%nNeq

    WRITE(lfn003,'(I3,2X,ES20.14,2X,A32,2X,A3,1X,7(1X,I1),3(F9.4),3(F9.4),F9.5)') &
          ifil, hlmOut%fact(ifil), hlmOut%filNam(ifil),          &
          hlmOut%grpnam(ifil), (hlmOut%ihelm(ii,ifil), ii=1,7),  &
          (hlmOut%rhelm(ii,ifil), ii=1,3),                       &
          (hlmOut%rhelm(ii,ifil)*ars*1.d3, ii=4,6),              &
          (hlmOut%rhelm(7,ifil)-1.d0)*1.d6

  END DO

! Close wgt file
! --------------
  CLOSE(lfn003)

  RETURN

END SUBROUTINE wgtstore

END MODULE
