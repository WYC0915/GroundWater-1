MODULE s_CHSTACRX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE chstacrx(opt, stacrux, obsHead, irCode)

! -------------------------------------------------------------------------
! Purpose:    Changes from STACRUX file for CHGHED
!
! Author:     R. Dach
!
! Created:    12-Dec-2000
! Last mod.:  08-Jul-2003
!
! Changes:    18-Jun-2001  HB: Use d_stacrx instead of m_stacrux
!             23-Oct-2001  RD: Handle "undef" option in sta info
!             22-Dec-2001  HU: Interface to sr gtstna removed
!             22-Jul-2002  HB: Use modified t_obsHead
!             08-Jul-2003  RD: Station Info flags are handled in CHINPT
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
  USE p_chghed,  ONLY: t_chghed
  USE d_stacrx,  ONLY: t_stacrux, undef_c, undef_i, undef_e
  USE d_gpsobs,  ONLY: t_obsHead
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_chghed), DIMENSION(:) :: opt       ! Input parameter
  TYPE(t_stacrux)              :: stacrux   ! records from STACRUX file

! input/output:
  TYPE(t_obsHead)              :: obsHead   ! Header of the obs. file

! output
  INTEGER(i4b)                 :: irCode    ! return Code


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  INTEGER(i4b), PARAMETER :: iOpt_stanam =  3  ! index of stanam in opt list
  INTEGER(i4b), PARAMETER :: iOpt_recnam =  4  ! index of recnam in opt list
  INTEGER(i4b), PARAMETER :: iOpt_antnam =  5  ! index of antnam in opt list
  INTEGER(i4b), PARAMETER :: iOpt_recnum =  9  ! index of recnum in opt list
  INTEGER(i4b), PARAMETER :: iOpt_antnum = 10  ! index of antnum in opt list
  INTEGER(i4b), PARAMETER :: iOpt_posecc = 13  ! index of posecc in opt list

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(2) :: obstim   ! Time window of the observations (MJD)

  INTEGER(i4b)            :: iSta     ! Counter for station
  INTEGER(i4b)            :: irenam   ! Counter for station rename records
  INTEGER(i4b)            :: iinfo    ! Counter for station info records
  INTEGER(i4b)            :: iEcc     ! Counter for position ecc.

! Set the return code
! -------------------
  irCode=0
!
! Compute the time window of the header file
! ------------------------------------------
  obstim(1)=obsHead%timRef
  obstim(2)=obstim(1)+(obsHead%nepoch-1)*obsHead%ideltt/86400D0
!
! Rename the stations (type 001)
! ------------------------------
  DO iSta=1,obsHead%nDiff+1
    DO irenam=1,stacrux%nrenam
      IF (obsHead%sta(iSta)%stanam == stacrux%renamsta(irenam)%oldnam .AND. &
          ((obstim(1) >= stacrux%renamsta(irenam)%timint%t(1) .AND.     &
            obstim(1) <= stacrux%renamsta(irenam)%timint%t(2))  .OR.    &
           (obstim(2) >= stacrux%renamsta(irenam)%timint%t(1) .AND.     &
            obstim(2) <= stacrux%renamsta(irenam)%timint%t(2))  .OR.    &
           (obstim(1) <= stacrux%renamsta(irenam)%timint%t(1) .AND.     &
            obstim(2) >= stacrux%renamsta(irenam)%timint%t(2))))  THEN
         WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')             &
               ' ',opt(iOpt_stanam)%text,' (',iSta,') : ',    &
               obsHead%sta(iSta)%stanam,' --> ',              &
               stacrux%renamsta(irenam)%stanam
         obsHead%sta(iSta)%stanam=stacrux%renamsta(irenam)%stanam
      ENDIF
    ENDDO
  ENDDO

! Change the station information (typ 002)
! ----------------------------------------
  DO iSta=1,obsHead%nDiff+1
    DO iinfo=1,stacrux%ninfo
      IF (obsHead%sta(iSta)%stanam == stacrux%stainfo(iinfo)%stanam .AND. &
          ((obstim(1) >= stacrux%stainfo(iinfo)%timint%t(1) .AND.     &
            obstim(1) <= stacrux%stainfo(iinfo)%timint%t(2))  .OR.    &
           (obstim(2) >= stacrux%stainfo(iinfo)%timint%t(1) .AND.     &
            obstim(2) <= stacrux%stainfo(iinfo)%timint%t(2))  .OR.    &
           (obstim(1) <= stacrux%stainfo(iinfo)%timint%t(1) .AND.     &
            obstim(2) >= stacrux%stainfo(iinfo)%timint%t(2))))  THEN
!
! receiver type
        IF (undef_c              /= stacrux%stainfo(iinfo)%recnam .AND. &
            obsHead%sta(iSta)%rectyp /= stacrux%stainfo(iinfo)%recnam) THEN
          WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')             &
                ' ',opt(iOpt_recnam)%text,' (',iSta,') : ',    &
                obsHead%sta(iSta)%rectyp,' --> ',              &
                stacrux%stainfo(iinfo)%recnam
          obsHead%sta(iSta)%rectyp=stacrux%stainfo(iinfo)%recnam
        ENDIF
!
! antenna type
        IF (undef_c              /= stacrux%stainfo(iinfo)%antnam .AND. &
            obsHead%sta(iSta)%anttyp /= stacrux%stainfo(iinfo)%antnam) THEN
          WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')             &
                ' ',opt(iOpt_antnam)%text,' (',iSta,') : ',    &
                obsHead%sta(iSta)%anttyp,' --> ',              &
                stacrux%stainfo(iinfo)%antnam
          obsHead%sta(iSta)%anttyp=stacrux%stainfo(iinfo)%antnam
        ENDIF
!
! receiver number
        IF (undef_i              /= stacrux%stainfo(iinfo)%recnum .AND. &
            obsHead%sta(iSta)%irunit /= stacrux%stainfo(iinfo)%recnum) THEN
          WRITE(lfnprt,'(A,A20,A,I1,A,I12,8X,A,I12)')          &
                ' ',opt(iOpt_recnum)%text,' (',iSta,') : ',    &
                obsHead%sta(iSta)%irunit,' --> ',              &
                stacrux%stainfo(iinfo)%recnum
          obsHead%sta(iSta)%irunit=stacrux%stainfo(iinfo)%recnum
        ENDIF
!
! antenna number
        IF (undef_i              /= stacrux%stainfo(iinfo)%antnum .AND. &
            obsHead%sta(iSta)%ianten /= stacrux%stainfo(iinfo)%antnum) THEN
          WRITE(lfnprt,'(A,A20,A,I1,A,I12,8X,A,I12)')          &
                ' ',opt(iOpt_antnum)%text,' (',iSta,') : ',    &
                obsHead%sta(iSta)%ianten,' --> ',              &
                stacrux%stainfo(iinfo)%antnum
          obsHead%sta(iSta)%ianten=stacrux%stainfo(iinfo)%antnum
        ENDIF
!
! position ecc.
        DO iEcc=1,3
          IF (undef_e /= stacrux%stainfo(iinfo)%antecc(iEcc) .AND. &
              obsHead%sta(iSta)%posecc(iEcc) /= &
                               stacrux%stainfo(iinfo)%antecc(iEcc)) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,F17.4,3X,A,F17.4)')         &
                  ' ',opt(iOpt_posecc-1+iEcc)%text,' (',iSta,') : ',&
                  obsHead%sta(iSta)%posecc(iEcc),' --> ',           &
                  stacrux%stainfo(iinfo)%antecc(iEcc)
            obsHead%sta(iSta)%posecc(iEcc)=stacrux%stainfo(iinfo)%antecc(iEcc)
          ENDIF
        ENDDO
      ENDIF
    ENDDO    ! iinfo
  ENDDO      ! iSta
!
  RETURN
  END SUBROUTINE chstacrx

END MODULE
