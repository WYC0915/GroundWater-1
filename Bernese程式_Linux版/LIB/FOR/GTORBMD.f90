MODULE s_GTORBMD
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtorbmd(filstd,orbdsc)

! -------------------------------------------------------------------------
! Purpose:    Get orbid description records from standard orbit header.
!
! Author:     U. Hugentobler
!
! Created:    14-Jan-2005
! Last mod.:  02-Sep-2008
!
! Changes:    02-Sep-2008 DT: New format for interval lengths [h]->[s]
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
  USE p_orbgen, ONLY: maxomd,t_orbmodel
  USE s_dimtst
  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  CHARACTER(LEN=*)                    :: filstd      ! Std file name

! OUT:
  TYPE(t_orbmodel)                    :: orbdsc      ! Description lines

! Local Variables
! ---------------
  INTEGER(i4b)                        :: ios,irc,ii
  INTEGER(i4b)                        :: narc,ifmt
  INTEGER(i4b), DIMENSION(2)          :: hlp_i
  REAL(r8b), DIMENSION(2)             :: hlp_r


! Open standard orbit file
! ------------------------
  CALL opnfil(lfnorb,filstd,'OLD','UNFORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnorb,ios,filstd,'GTORBMD')

! Read number of arcs
  READ(lfnorb) narc

! Read format version and orbit description
  IF (narc < 0) THEN
    READ(lfnorb) ifmt,narc
    READ(lfnorb) orbdsc%nlin
    CALL dimtst(1,2,2,'GTORBMD','MAXOMD','ORBIT MODEL LINES',' ', &
                orbdsc%nlin,maxomd,irc)
    DO ii=1,orbdsc%nlin
      READ(lfnorb) orbdsc%orbmod(ii)

! Change to new format: Intervals in [s] instead of [h]; keyword INTEGR->INTEG2
      IF (orbdsc%orbmod(ii)(1:6) .EQ. 'INTEGR') THEN
        READ(orbdsc%orbmod(ii)(9:17),*) hlp_r(1)
        READ(orbdsc%orbmod(ii)(23:31),*) hlp_r(2)

        hlp_i = DNINT(hlp_r * 3600)

        orbdsc%orbmod(ii)(1:6) = 'INTEG2'
        WRITE(orbdsc%orbmod(ii)(9:17),"(I9)") hlp_i(1)
        WRITE(orbdsc%orbmod(ii)(23:31),"(I9)") hlp_i(2)
      END IF

    ENDDO
  ELSE
    orbdsc%nlin=0
  ENDIF

  CLOSE(LFNORB)

  RETURN
END SUBROUTINE gtorbmd

END MODULE
