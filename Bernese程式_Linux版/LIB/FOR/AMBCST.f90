MODULE s_ambcst
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------
SUBROUTINE ambcst(npar,nobs,nfTot,timFil,locq,anor,weight_amb)

! -------------------------------------------------------------------------
! Purpose: This subroutine
!          - constrains the ambiguity of each group using the weight "wgt_amb".
!
! Author:     G. Beutler
!
! Created:    05-Jan-2006
! Last mod.:  29-Jun-2007
!
! Changes:    23-Jan-2006 HB: Replace ALLOCATED with ASSOCIATED
!             09-Feb-2006 HB: Remove idel from parameter list
!             29-Jun-2007 HB: Exit loop if cluster numbers negative
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_gpsest, ONLY: maxLcq,t_ambtime,maxamb
  USE f_ikf
  IMPLICIT NONE

! input/output parameters
! -----------------------
  INTEGER(i4b),            INTENT(in)     :: npar     ! number of parameters in locq
  INTEGER(i4b),            INTENT(inout)  :: nobs     ! number of observations
  INTEGER(i4b),            INTENT(inout)  :: nfTot    ! number of files
  TYPE(t_ambTime),DIMENSION(:),INTENT(inout) :: timfil ! information about ambiguities
  INTEGER(i4b), DIMENSION(MaxLcq,*),INTENT(in) :: locq     ! external locq
  REAL(r8b), DIMENSION(*), INTENT(inout)  :: anor     ! resulting neq-system
  REAL(r8b),               INTENT(in)     :: weight_amb  ! weights for ambiguities to be fixed

! local variables
! ---------------
  REAL(r8b)      :: max_diag
  INTEGER(i4b)   :: ipar, ind_max, iFil, iCon, iCls, ik

! constrain ambiguity with most obs within one group
  DO iFil = 1,nftot
    DO iCon = 1,timFil(iFil)%nInter
      IF (ASSOCIATED(timFil(iFil)%ambInt)) THEN
        max_diag=-1.d0
        ind_max=0
        IF (timFil(iFil)%ambInt(iCon)%ref == 0) THEN
          DO ipar=1,npar
            IF (locq(1,ipar) == 4 .AND. locq(2,iPar) == iFil) THEN
              DO iCls = 1,SIZE(timFil(iFil)%ambInt(iCon)%clu)
                IF (timFil(iFil)%ambInt(iCon)%clu(iCls)==-1000)EXIT
                IF (timFil(iFil)%ambInt(iCon)%clu(iCls) == locq(3,iPar)) THEN
                  ik=ikf(ipar,ipar)
                  IF(anor(ik) > max_diag)THEN
                    ind_max=ipar
                    max_diag=anor(ik)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
          ik=ikf(ind_max,ind_max)
          anor(ik)=anor(ik)+weight_amb
          nobs=nobs+1
          timFil(iFil)%ambInt(iCon)%ref =2
        ENDIF
      ENDIF
    ENDDO
  ENDDO
END SUBROUTINE  ambcst
END MODULE s_ambcst
