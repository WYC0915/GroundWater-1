! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_cmc

! -------------------------------------------------------------------------
! Purpose:    Handle CMC for OTL and ATL
!             Provides the corrections for a certain epoch
!             Checks the consistency of the requests
!
! Author:     R. Dach
!
! Created:    30-Nov-2010
! Last mod.:
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_maxdim, ONLY: maxocn,maxatm

  IMPLICIT NONE

! Declare access rights
  PRIVATE

  PUBLIC  :: getcmc, chkcmc

! Private module variables
! ------------------------
! General variables (1: OTL, 2: ATL)
  INTEGER(i4b),      DIMENSION(2) :: my_icmc      ! corr.tide centre of mass
                                      !  -1: no information about mass centre
                                      !      correction
                                      !   0: no centre correction applied
                                      !   1: centre correction applied
  CHARACTER(LEN=16), DIMENSION(2) :: my_cmcmod = &   ! name CMC model
                     (/ '                ','                ' /)

! OTL:
  REAL(r8b),DIMENSION(3,maxocn)   :: my_ocnin     ! in-phase CMC coefficients
  REAL(r8b),DIMENSION(3,maxocn)   :: my_ocncr     ! out-of-phase CMC coefficients

! ATL:
  REAL(r8b),DIMENSION(3,maxatm)   :: my_atmin     ! in-phase CMC coefficients
  REAL(r8b),DIMENSION(3,maxatm)   :: my_atmcr     ! out-of-phase CMC coefficients



CONTAINS


! -------------------------------------------------------------------------
! Compute the CMC corrections for OTL and ATL
! -------------------------------------------------------------------------

SUBROUTINE getcmc(cmcyn,tmjd,cmc,cmcmod,harstr,icmc)

! -------------------------------------------------------------------------
! Purpose:    Compute the CMC corrections for OTL and ATL
!
! Remarks:    Due to the specification of optional parameters it is also
!             possible to ask for the model charaacterization
!             (e.g., CALL getcmc(cmcyn,cmcmod=modelName)
!
! Author:     M. Ploner
!
! Created:    09-Jan-2007
! Last mod.:  02-Sep-2011
!
! Changes:    09-Jan-2007 RD: Calculate CMC with help of SR harload
!             30-Nov-2010 RD: Implement the routine into the module
!             02-Sep-2011 HB: Initialize cmc=0.D0 (needed if cmcyn=FALSE)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const, ONLY: pi

  USE s_harload
  USE s_exitrc

  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  LOGICAL,  DIMENSION(2)          :: cmcyn     ! CMC shall be applied or not
                                               ! 1: OTL
                                               ! 2: ATL
  REAL(r8b), OPTIONAL             :: tmjd      ! time

! OUT:
  REAL(r8b),DIMENSION(3),OPTIONAL :: cmc       ! CMC offset
  CHARACTER(LEN=16),               &
           DIMENSION(2), OPTIONAL :: cmcmod    ! CMC model name
  CHARACTER(LEN=20),     OPTIONAL :: harstr    ! Indication of HARDISP version
  INTEGER(i4b),                    &
           DIMENSION(2), OPTIONAL :: icmc      ! corr.tide centre of mass
                                      !  -1: no information about mass centre
                                      !      correction
                                      !   0: no centre correction applied
                                      !   1: centre correction applied

! Local Variables
! ---------------
  REAL(r8b),DIMENSION(3,maxocn)   :: amp,phs
  REAL(r8b)                       :: dMjd
  INTEGER(i4b)                    :: iTyp
  INTEGER(i4b)                    :: i,j

! Check whether the something need to be loaded
! ---------------------------------------------
  IF (cmcyn(1) .AND. LEN_TRIM(my_cmcmod(1)) == 0) &
      CALL rdcmc(1,my_icmc(1),my_cmcmod(1),my_ocnin,my_ocncr)

  IF (cmcyn(2) .AND. LEN_TRIM(my_cmcmod(2)) == 0) &
      CALL rdcmc(2,my_icmc(2),my_cmcmod(2),my_atmin,my_atmcr)


! Init variables
! --------------
  DO iTyp = 1,2
    IF (cmcyn(iTyp) .AND. PRESENT(cmcmod)) cmcmod(iTyp) = my_cmcmod(iTyp)
    IF (cmcyn(iTyp) .AND. PRESENT(icmc))   icmc(iTyp)   = my_icmc(iTyp)
    IF (iTyp == 1   .AND. cmcyn(iTyp) .AND. &
                          PRESENT(harstr)) harstr = 'HARLOAD: 342 tides'

    IF (cmcyn(iTyp) .AND. PRESENT(cmc)) THEN
      IF (.NOT. PRESENT(tmjd)) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)') &
          ' *** SR GETCMC: Dear programer, if you are asking for the "CMC"', &
                          'you also need to provide the epoch at "tmjd".'
        CALL exitrc(2)
      ENDIF
    ENDIF
  ENDDO

  IF (PRESENT(cmc) .AND. PRESENT(tmjd)) THEN
    cmc=0.D0

! CMC for ocean tidal loading
! ---------------------------
    IF (cmcyn(1) .AND. my_icmc(1) == 0) THEN
      DO i=1,maxocn
        DO j=1,3
          amp(j,i) = DSQRT(my_ocnin(j,i)**2 + my_ocncr(j,i)**2)
          phs(j,i) = DATAN2(my_ocncr(j,i),my_ocnin(j,i))
        ENDDO
      ENDDO
      CALL harload(tmjd,amp,phs,cmc)
    ENDIF


! CMC for atmospheric tidal loading
! ---------------------------------
    IF (cmcyn(2) .AND. my_icmc(2) == 0) THEN
      dMjd = tmjd-DINT(tmjd)
      DO j=1,3
        DO i=1,maxatm
          cmc(j) = cmc(j) + my_atmin(j,i)*DCOS(dmjd*DBLE(I)*2D0*pi) + &
                            my_atmcr(j,i)*DSIN(dmjd*DBLE(I)*2D0*pi)
        ENDDO
      ENDDO
    ENDIF
  ENDIF


  RETURN
END SUBROUTINE getcmc



! -------------------------------------------------------------------------
! Check the CMC model names for OTL and ATL
! -------------------------------------------------------------------------

SUBROUTINE chkcmc(cmcyn,cmcmod)

! -------------------------------------------------------------------------
! Purpose:    Check the CMC model names for OTL and ATL
!
! Remarks:
!
! Author:     R. Dach
!
! Created:    30-Nov-2010
! Last mod.:  30-Nov-2010
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

USE m_bern

USE s_exitrc

IMPLICIT NONE

! IN:
  LOGICAL,  DIMENSION(2)          :: cmcyn     ! CMC shall be applied or not
                                               ! 1: OTL
                                               ! 2: ATL
  CHARACTER(LEN=16), DIMENSION(2) :: cmcmod    ! external CMC model name

! Local variables
! ---------------
  CHARACTER(LEN=16), DIMENSION(2) :: cmcmod1   ! CMC model name frmo file

  INTEGER(i4b)                    :: nErr

  LOGICAL, DIMENSION(2), SAVE     :: first = (/ .TRUE.,.TRUE. /)

! Init local variables
! --------------------
  nErr = 0
  CALL getcmc(cmcyn,cmcmod=cmcmod1)

! Check OTL model name
! --------------------
  IF (cmcyn(1) .AND. first(1) .AND. LEN_TRIM(cmcmod(1)) > 0) THEN
    IF (cmcmod(1) /= cmcmod1(1)) THEN
      WRITE(lfnerr,'(/ A,2(/,16X,A)/)') &
      ' *** SR CHKCMC: Different OTL CMC models in STD-orbit and BLQ-file.', &
                      'OTL CMC model in STD-orbit:  ' // TRIM(cmcmod(1)),    &
                      'OTL CMC model in BLQ-file:   ' // TRIM(cmcmod1(1))
      nErr = nErr + 1
    ENDIF
    first(1) = .FALSE.
  ENDIF

! Check ATL model name
! --------------------
  IF (cmcyn(2) .AND. first(2) .AND. LEN_TRIM(cmcmod(2)) > 0) THEN
    IF (cmcmod(2) /= cmcmod1(2)) THEN
      WRITE(lfnerr,'(/ A,2(/,16X,A)/)') &
      ' *** SR CHKCMC: Different ATL CMC models in STD-orbit and ATL-file.', &
                      'ATL CMC model in STD-orbit:  ' // TRIM(cmcmod(2)),    &
                      'ATL CMC model in ATL-file:   ' // TRIM(cmcmod1(2))
      nErr = nErr + 1
    ENDIF
    first(2) = .FALSE.
  ENDIF

! Stop in case of an error
! ------------------------
  IF (nErr /= 0) CALL exitrc(2)

END SUBROUTINE chkcmc


! -------------------------------------------------------------------------
! Read the CMC corrections for OTL or ATL
! -------------------------------------------------------------------------

SUBROUTINE rdcmc(iTyp,icmc,cmcmod,cmcin,cmccr)

! -------------------------------------------------------------------------
! Purpose:    Read the CMC corrections for OTL or ATL
!
! Remarks:    iTyp decides whether OTL or ATL corrections are read
!
! Author:     M. Ploner/A.Gaede
!
! Created:    23-May-2007
! Last mod.:  30-Nov-2010
!
! Changes:    15-Aug-2006 AG: Error condition modified
!             09-Jan-2007 AG: Error message if no cmc info found
!             23-May-2007 AG: Error message corrected
!             23-Nov-2010 RD: Implement ATL-CMC
!             30-Nov-2010 RD: Implement the routine into the module
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_maxdim, ONLY: maxocn,maxatm

  USE s_opnfil
  USE s_opnerr
  USE s_gtflna
  USE s_upperc
  USE s_exitrc

  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  INTEGER(i4b)           :: iTyp      ! =1: OTL - OCNLOAD
                                      ! =2: ATL - ATMLOAD

! OUT:
  INTEGER(i4b)           :: icmc      ! corr.tide centre of mass
                                      !  -1: no information about mass centre
                                      !      correction
                                      !   0: no centre correction applied
                                      !   1: centre correction applied
  CHARACTER(LEN=16)               :: cmcmod   ! name CMC model
  REAL(r8b),DIMENSION(:,:)        :: cmcin    ! in-phase CMC coefficients
  REAL(r8b),DIMENSION(:,:)        :: cmccr    ! out-of-phase CMC coefficients



! Local Parameter
! ---------------
  CHARACTER(LEN=7), DIMENSION(2),  &  ! keywords for load files
                    PARAMETER     :: loadKey = (/ 'OCNLOAD','ATMLOAD' /)

! Local Variables
! ---------------
  INTEGER(i4b)                    :: iostat=0,irc,ncmc
  REAL(r8b),DIMENSION(6)          :: cmcvalues
  CHARACTER(LEN=2)                :: com
  CHARACTER(LEN=20)               :: keyword1,keyword2,typ
  CHARACTER(LEN=fileNameLength80) :: filnam   ! name of ocean loading file
  CHARACTER(LEN=256)              :: line

  ncmc=1
  icmc=-1
  cmcin=0D0
  cmccr=0D0
  cmcmod=''

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! Get Filename
! ------------
  CALL gtflna(1,loadKey(iTyp),filnam,irc)

! READ FILE
! =========
! ----------------------------------------------
  CALL opnfil(lfnloc,filnam,'OLD',' ', 'READONLY',' ',iostat)
  CALL opnerr(lfnerr,lfnloc,iostat,filnam,'RDCMC')

! Search for first occurance of "CMC frequ :"
! -----------------------
  DO
    READ(lfnloc,'(A2,A)',IOSTAT=iostat)com,line
    IF (iostat /= 0) EXIT
    IF (line == ' END HEADER') EXIT
    IF (com .NE. '$$') CYCLE
    IF (line .EQ. ' ') CYCLE
    READ(line,*)keyword1

    IF (keyword1 == 'CMC:') THEN
      READ(line(6:LEN_TRIM(line)),*)keyword2
      IF (keyword2 == 'YES') icmc = 1
      IF (keyword2 == 'NO')  icmc = 0
    ENDIF

    IF (iTyp == 1 .AND. keyword1 == 'Ocean')        &
                           READ(line(19:LEN_TRIM(line)),*) cmcmod
    IF (iTyp == 2 .AND. keyword1 == 'Model')        &
                           READ(line(19:LEN_TRIM(line)),*) cmcmod

    IF (keyword1 == 'CMC') THEN
      READ(line(5:LEN_TRIM(line)),*)keyword2
      IF (keyword2 == 'end') EXIT
      IF (keyword2 == 'frequ') THEN
        irc = 0
        READ(line(13:LEN_TRIM(line)),*)typ
        READ(line(55:LEN_TRIM(line)),*)cmcvalues(3),cmcvalues(6), &
                                       cmcvalues(1),cmcvalues(4), &
                                       cmcvalues(2),cmcvalues(5)

        CALL upperc(typ)

        ! Ocean
        IF (iTyp == 1) THEN
          IF (TRIM(typ) == 'M2') THEN
            ncmc=ncmc+1
            cmcin(1:3,1) = cmcvalues(1:3)
            cmccr(1:3,1) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'S2') THEN
            ncmc=ncmc+1
            cmcin(1:3,2) = cmcvalues(1:3)
            cmccr(1:3,2) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'N2') THEN
            ncmc=ncmc+1
            cmcin(1:3,3) = cmcvalues(1:3)
            cmccr(1:3,3) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'K2') THEN
            ncmc=ncmc+1
            cmcin(1:3,4) = cmcvalues(1:3)
            cmccr(1:3,4) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'K1') THEN
            ncmc=ncmc+1
            cmcin(1:3,5) = cmcvalues(1:3)
            cmccr(1:3,5) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'O1') THEN
            ncmc=ncmc+1
            cmcin(1:3,6) = cmcvalues(1:3)
            cmccr(1:3,6) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'P1') THEN
            ncmc=ncmc+1
            cmcin(1:3,7) = cmcvalues(1:3)
            cmccr(1:3,7) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'Q1') THEN
            ncmc=ncmc+1
            cmcin(1:3,8) = cmcvalues(1:3)
            cmccr(1:3,8) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'MF') THEN
            ncmc=ncmc+1
            cmcin(1:3,9) = cmcvalues(1:3)
            cmccr(1:3,9) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'MM') THEN
            ncmc=ncmc+1
            cmcin(1:3,10) = cmcvalues(1:3)
            cmccr(1:3,10) = cmcvalues(4:6)
          ELSE IF (TRIM(typ) == 'SSA') THEN
            ncmc=ncmc+1
            cmcin(1:3,11) = cmcvalues(1:3)
            cmccr(1:3,11) = cmcvalues(4:6)
          ENDIF
        ELSEIF ( iTyp == 2 ) THEN
          IF (TRIM(typ) == 'S1') THEN
            ncmc=ncmc+1
            cmcin(1,1) = cmcvalues(3)
            cmccr(1,1) = cmcvalues(6)
            cmcin(2:3,1) = cmcvalues(1:2)
            cmccr(2:3,1) = cmcvalues(4:5)
          ELSE IF (TRIM(typ) == 'S2') THEN
            ncmc=ncmc+1
            cmcin(1:3,2) = cmcvalues(3)
            cmccr(1:3,2) = cmcvalues(6)
            cmcin(2:3,2) = cmcvalues(1:2)
            cmccr(2:3,2) = cmcvalues(4:5)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO

! Error if no CMC found
  IF (icmc == -1) THEN
    WRITE(lfnerr,"(/,' *** SR RDCMC: NO CMC INFORMATION FOUND IN FILE.', &
                  &/,15X,'Filename: ',A,/)")TRIM(filnam)
    CALL exitrc(2)
  ENDIF
  IF ((iTyp == 1 .AND. ncmc < maxocn) .OR. &
      (iTyp == 2 .AND. ncmc < maxatm)) THEN
    WRITE(lfnerr,"(/,' *** SR RDCMC: CMC VALUES NOT FOUND IN FILE', &
                  &/,15X,'Filename: ',A,/)")TRIM(filnam)
    call exitrc(2)
  ENDIF

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE rdcmc

END MODULE s_cmc
