MODULE s_NEQASCII
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqascii(neq,filout)

! -------------------------------------------------------------------------
! Purpose:    Write formatted NEQ file
!
! Author:     L. Mervart
!
! Created:    25-Aug-1998
!
! Changes:    08-Sep-2000 HB: Use fileNameLength from m_bern
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Oct-2002 MR: Correct format (1x)
!             22-May-2003 RD: Use maxsat from m_maxdim (instead of p_addneq)
!             08-Sep-2003 HU: New NEQ format: antnam, recnam chr16 -> chr20
!             14-Apr-2005 HU: Check actual number of sat per antoff group
!             26-Jul-2005 RD/AG: Write locq with i9 (because of GIM-locq)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             13-Jun-2008 RD: Use writePar from D_PAR.f90
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             30-Apr-2009 SL: neq version set to 5
!             04-May-2009 RD: Scaling of loading models added, neq%version=6
!             15-Nov-2010 RD: New NEQ version=7: D_PAR omega is written anytime
!             14-Jul-2011 LP: Sat-spec. obstypes in NEQ; NEQ-version set to 8
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, fileNameLength
  USE m_global, ONLY: maxsys, g_svnsys
  USE d_par,    ONLY: maxLcq,writePar
  USE d_neq,    ONLY: t_neq,maxFrq

  USE f_ikf
  USE s_opnfil
  USE s_opnerr
  USE s_exitrc
  IMPLICIT NONE

! Dummy Arguments
! ---------------
  TYPE (t_neq)      :: neq
  CHARACTER(LEN=fileNameLength) :: filout

! Local Variables
! ---------------
  INTEGER(i4b)       :: ios
  INTEGER(i4b)       :: ii
  INTEGER(i4b)       :: jj
  INTEGER(i4b)       :: iSys
  INTEGER(i4b)       :: nWrite
  CHARACTER(LEN=3)   :: advFlg

! Open the output file
! --------------------
  CALL opnfil(lfnloc,filout,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filout,'neqascii')

! Write the version of NEQ file
! -----------------------------
  neq%version = 8
  WRITE(lfnloc,'(I3)') neq%version

! Write the header
! ----------------
  WRITE(lfnloc, &
  '( 4(A,/), E22.15,1X,I22,/, E22.15,1X,E22.15,/, 4(I22,1X,I22,/),/,3(A,/))') &
                  neq%misc%title(1),  neq%misc%title(2),  &
                  neq%misc%orbFil(1), neq%misc%orbFil(2), &
                  neq%misc%nobs,      neq%misc%npar,      &
                  neq%misc%lTPl,      neq%misc%nparms,    &
                  neq%misc%nftot,     neq%misc%nsmpnq,    &
                  neq%misc%ielvnq,    neq%misc%itropo,    &
                  neq%misc%iextra,    neq%misc%itrmap,    &
                  neq%misc%itrgrd,    neq%misc%nanoff,    &
                  neq%misc%datum, neq%misc%nutmod, neq%misc%submod

  DO ii = 1, neq%misc%nanoff
    IF (neq%misc%nsaoff(ii) > 99) THEN
      WRITE(lfnerr,*) ' *** neqascii: nsaoff too large',ii,neq%misc%nsaoff(ii)
      CALL exitrc(2)
    END IF
    WRITE(lfnloc,'( I22,/, 99(10X,I22,/) )')   &
                    neq%misc%nsaoff(ii),       &
                    (neq%misc%satoff(jj,ii), jj=1,neq%misc%nsaoff(ii))
  END DO

! Information needed for SINEX format
! -----------------------------------
  WRITE(lfnloc,*) neq%misc%nstat_sinex

  IF (maxFrq > 2) THEN
    WRITE(lfnerr,*) ' *** neqascii: maxFrq too large'
    CALL exitrc(2)
  END IF

  DO ii = 1, neq%misc%nstat_sinex
    WRITE(lfnloc, '( E22.15,1X,E22.15,/ A, /, 3(E22.15,1X),/, ' //  &
                  '  I22,2I11,/,A,/,A,/ )' )                        &
                    neq%misc%sinex(ii)%timint%t(1),                 &
                    neq%misc%sinex(ii)%timint%t(2),                 &
                    neq%misc%sinex(ii)%stname,                      &
                    neq%misc%sinex(ii)%antEcc(1),                   &
                    neq%misc%sinex(ii)%antEcc(2),                   &
                    neq%misc%sinex(ii)%antEcc(3),                   &
                    neq%misc%sinex(ii)%antNum, 0,maxsys-1,          &
                    neq%misc%sinex(ii)%antSta,                      &
                    neq%misc%sinex(ii)%antRec

    DO iSys = 0,maxSys-1
      WRITE(lfnloc,'(A1,1X,A)')          &
                    g_svnsys(iSys),neq%misc%sinex(ii)%antpcv(iSys)%atxStr
      IF (LEN_TRIM(neq%misc%sinex(ii)%antpcv(iSys)%atxStr) == 0) CYCLE

      WRITE(lfnloc,'(3(E22.15,1X,E22.15,/), I22,1X,2I11,/ )')                     &
            (neq%misc%sinex(ii)%antpcv(iSys)%antPhs(1,jj), jj=1,maxFrq), &
            (neq%misc%sinex(ii)%antpcv(iSys)%antPhs(2,jj), jj=1,maxFrq), &
            (neq%misc%sinex(ii)%antpcv(iSys)%antPhs(3,jj), jj=1,maxFrq), &
            neq%misc%sinex(ii)%antpcv(iSys)%nFrq,                        &
            neq%misc%sinex(ii)%antpcv(iSys)%adopted,                     &
            neq%misc%sinex(ii)%antpcv(iSys)%individ
    ENDDO
  ENDDO

! Keywords of for the scaling factors of the Vienna grid files
! ------------------------------------------------------------
  WRITE(lfnloc,'(2I11)') SIZE(neq%misc%grdNeq),LEN(neq%misc%grdNeq(1))
  DO ii = 1,SIZE(neq%misc%grdNeq)
    WRITE(lfnloc,'(A)') neq%misc%grdNeq(ii)
  ENDDO


! Sat-spec obstypes
! -----------------
  WRITE(lfnloc,'(I5)') neq%misc%nobst
  DO ii = 1, neq%misc%nobst
!    WRITE(lfnloc,'(I3,4(1X,A3),2(1X,E22.15))') &
!    neq%misc%obst(ii)%sat,(neq%misc%obst(ii)%obstyp(jj),jj=1,4), &
!    (neq%misc%obst(ii)%timint%t(jj),jj=1,2)
    WRITE(lfnloc,'(4(1X,A3))') &
    (neq%misc%obst(ii)%obstyp(jj),jj=1,4)
  ENDDO


! Parameter Description
! ---------------------
  IF (maxLcq /= 7) THEN
    WRITE(lfnerr,*) ' *** neqascii: maxLcq too large'
    CALL exitrc(2)
  END IF
  DO ii = 1, neq%misc%npar
    CALL writePar(lfnloc,2,neq%par(ii))
  END DO

! ANOR Matrix
! -----------
  nWrite = 0
  DO ii = 1, neq%misc%npar
    DO jj = ii, neq%misc%npar

      nWrite = nWrite + 1

      IF (nWrite == neq%misc%npar * (neq%misc%npar + 1) / 2 ) THEN
        advFlg = 'YES'
      ELSE
        advFlg = 'NO'
      END IF

      IF ( MOD(nWrite,3) == 1 ) THEN
        WRITE(lfnloc, '(2(1X,I5), 1X,E21.15)', ADVANCE=advFlg) &
              ii, jj, neq%aNor(ikf(ii,jj))
      ELSE IF ( MOD(nWrite,3) == 2 ) THEN
        WRITE(lfnloc, '(1X,E21.15)', ADVANCE=advFlg) neq%aNor(ikf(ii,jj))
      ELSE
        WRITE(lfnloc, '(1X,E21.15)') neq%aNor(ikf(ii,jj))
      END IF

    END DO
  END DO

! BNOR Matrix
! -----------
  DO ii = 1, neq%misc%npar
    WRITE(lfnloc,'(1X,I5,1X,E21.15)') ii, neq%bNor(ii)
  END DO

  CLOSE(lfnloc)

END SUBROUTINE neqascii

END MODULE
