MODULE s_SETDIA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE setdia(npar,clk,wgtclk,dclk,wgtcld,diag,offdia,rhs)

!-------------------------------------------------------------------------
! Purpose    :  COMPUTE NEQ-SYSTEM FOR npar CLOCK VALUES
!               WHEN THE CLOCK VALUES (ARRAY clk) AND THE
!               SUBSEQUENT CLOCK DIFFERENCES (ARRAY dclk)
!               ARE AVAILABLE. CLOCK MEASUREMENTS HAVE
!               WEIGHT WGTclk, CLOCK DIFFERENCES HAVE WEIGHTS
!               wgtcld(I)
!
! Author     :  H.Bock and G.Beutler
!
! Created    :  10-Sep-1999
! Last mod.  :  __-___-____
!
! Changes    :  __-___-____ __:
!
! Copyright  :  ASTRONOMICAL INSTITUTE
!               University of Bern
!               Switzerland
!
!-----------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  IMPLICIT NONE

! List of parameters
! ------------------
!        IN  :  npar   : NUMBER OF CLOCK VALUES             I*4
!               clk    : CLOCK MEASUREMENTS                 R*8
!               WGTclk : WEIGHT OF CLOCK MEASUREMENTS       R*8
!               dclk   : CLOCK DIFFERENCES (npar-1)         R*8
!               wgtcld : WEIGHTS OF CLOCK DIFFERENCES       R*8
!        OUT :  diag   : diagONAL TERM                      R*8
!               offdia : OFF-diagONAL TERM                  R*8
!               rhs    : RIGHT HAND SIDE                    R*8

  INTEGER(i4b)              :: npar
  REAL(r8b),DIMENSION(:)    :: clk,wgtclk,dclk,wgtcld
  REAL(r8b),DIMENSION(:)    :: diag,offdia,rhs

! Local variables
! ---------------
  INTEGER(i4b)  :: ipar

  rhs(:)   =0D0
  diag(:)  =0D0
  offdia(:)=0D0

! PUT CLOCK MEASUREMENTS INTO NEQ-SYSTEM
! -------------------------------------
  DO ipar=1,npar
    rhs(ipar)=1.D0*wgtclk(ipar)*clk(ipar)
    diag(ipar)=1.D0*wgtclk(ipar)*1.D0
  ENDDO
!
! ADD CLOCK DIFFERENCE CONTRIBUTIONS INTO NEQ SYSTEM
! --------------------------------------------------
  DO ipar=1,npar-1
    rhs(ipar)   =rhs(ipar)   -1.D0*wgtcld(ipar)*dclk(ipar)
    rhs(ipar+1) =rhs(ipar+1) +1.D0*wgtcld(ipar)*dclk(ipar)
    diag(ipar)  =diag(ipar)  +1.D0*wgtcld(ipar)*1.D0
    diag(ipar+1)=diag(ipar+1)+1.D0*wgtcld(ipar)*1.D0
    offdia(ipar)=            -1.D0*wgtcld(ipar)*1.D0
  ENDDO
RETURN
END SUBROUTINE setdia

END MODULE
