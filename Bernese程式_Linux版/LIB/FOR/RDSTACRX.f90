MODULE s_RDSTACRX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdstacrx(iFil,neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads the STACRUX file, changes station
!             information according to this file and adds some elimination
!             requests to opt%elimi structure.
!
! Author:     L. Mervart
!
! Created:    02-Sep-1998
!
! Changes:    09-Mar-2000 LM: Renaming before anything else, print messages
!             26-Jun-2001 RD: Use alcerr for allocation
!             26-Jun-2001 HB: Use readCrux for reading of STACRUX file
!             23-Oct-2001 RD: Use undef values from d_stacrx
!             23-Oct-2001 RD: Apply the flags in the station info file
!             24-Oct-2001 RD: Generate an extended program output
!             26-Oct-2001 RD: Rename also station names for DCB
!             08-Nov-2001 RD: Use the mean epoch also for the SINEX entries
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Nov-2002 RD: Allocate lines to size 0 at least
!             27-Nov-2002 DT: Extend parTyp due to maxParTyp 24 -> 26
!             27-Feb-2003 HU: DATUM from D_DATUM
!             16-May-2003 HU: Initialize structures
!             09-Jul-2003 RD: StaInfo comes in from AOPTCRX now
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             06-Mar-2005 HU: Check antenna position with range of 0.001mm
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             23-Mar-2006 RD: Extent old/new values to 20 (radome code)
!             22-Jun-2006 SS: Suppress notification of changes if indicated
!             03-Dec-2007 RD: Correct deallocation bug
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             04-Jan-2010 SL: HOI added to parTyp
!             04-Mar-2010 RD: Output cosmetics
!             26-Oct-2010 SL: flg INTEGER->CHARACTER, use m_bern with ONLY
!             30-Nov-2010 DT: changes due to maxpartyp 27->28
!             15-Dec-2010 MM: GNSS-specific parameters added
!             03-May-2011 RD: Correct list for station renaming
!             25-Nov-2011 RD: No renaming for grid factors
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lineLength, staNameLength, lfnErr, lfnPrt
  USE m_time,   ONLY: OPERATOR(.isIn.)
  USE d_datum,  ONLY: datum
  USE d_par,    ONLY: maxParTyp
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt, staInfo, prtCrx
  USE d_stacrx, ONLY: t_staInfo,undef_c,undef_i,undef_e

  USE f_ikf
  USE s_alcerr
  USE s_timst2
  USE s_ellecc
  USE s_xyzell
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)     :: iFil      ! Number of the NEQ file for output
  TYPE(t_neq)      :: neq       ! NEQ for update the information


! Local Parameters
! ----------------
  CHARACTER(LEN= 3), DIMENSION(maxParTyp) :: parTyp = &
   (/ 'CRD','   ','   ','   ','   ','TRP','   ','DCB','   ','   ', &
      '   ','   ','   ','   ','   ','   ','   ','   ','   ','   ', &
      '   ','GRD','   ','   ','   ','   ','HOI','   ','   ','GSP' /)

  CHARACTER(LEN=11), DIMENSION(3)         :: eccTyp = &
   (/ 'North:     ','East:      ','Up:        ' /)

! Local Variables
! ---------------
  TYPE(t_staInfo),                       &
               DIMENSION(:),ALLOCATABLE :: prtInfo

  CHARACTER(LEN=lineLength),             &
               DIMENSION(:),ALLOCATABLE :: hlpLine
  CHARACTER(LEN=lineLength),             &
               DIMENSION(:),POINTER,SAVE:: lines
  CHARACTER(LEN=19)                     :: epostr

  REAL(r8b),   DIMENSION(neq%misc%npar) :: dC
  REAL(r8b),   DIMENSION(3)             :: dCneu
  REAL(r8b),   DIMENSION(neq%misc%npar) :: hlp
  REAL(r8b),   DIMENSION(3)             :: xsta
  REAL(r8b),   DIMENSION(3)             :: xell
  REAL(r8b)                             :: snxMean

  INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: prtRenam
  INTEGER(i4b)                          :: iCrx
  INTEGER(i4b),SAVE                     :: nLines
  INTEGER(i4b)                          :: iLines
  INTEGER(i4b)                          :: iPar,lPar
  INTEGER(i4b)                          :: iSta
  INTEGER(i4b)                          :: ii
  INTEGER(i4b)                          :: iac
  INTEGER(i4b)                          :: lants
  INTEGER(i4b)                          :: lantr

  LOGICAL,SAVE                          :: first = .TRUE.

  IF ( opt%stacrux == ' ') RETURN

! Init output lines
! -----------------
  IF (first) THEN
    first = .FALSE.

    NULLIFY(lines)
    nLines = 0
    ALLOCATE(lines(nLines),stat=iac)
    CALL alcerr(iac,'lines',(/nLines/),'rdstacrx')
  ENDIF

! Init the protocoll section
! --------------------------
  IF (nLines >= 0) THEN
    ALLOCATE(prtRenam(staInfo%nRenam),stat=iac)
    CALL alcerr(iac,'prtRenam',(/staInfo%nRenam/),'rdstacrx')
    ALLOCATE(prtInfo(staInfo%nInfo),stat=iac)
    CALL alcerr(iac,'prtInfo',(/staInfo%nInfo/),'rdstacrx')

    prtRenam(:) = 0

    prtInfo(:)%timint%t(1) =  1d20
    prtInfo(:)%timint%t(2) =  0d0
    prtInfo(:)%flg    = '000'
    prtInfo(:)%recnam = undef_c
    prtInfo(:)%antnam = undef_c
    prtInfo(:)%antnum = undef_i
    prtInfo(:)%antecc(1) = undef_e
    prtInfo(:)%antecc(2) = undef_e
    prtInfo(:)%antecc(3) = undef_e
  ENDIF


! Perform the operations required by STACRUX file
! ===============================================

! Rename the station
! ------------------
  DO iCrx = 1, staInfo%nrenam
    lPar = 0
    DO iPar = 1, neq%misc%npar
      IF ( neq%par(iPar)%locq(1) /=  1 .AND.  &
           neq%par(iPar)%locq(1) /=  2 .AND.  &
           neq%par(iPar)%locq(1) /=  6 .AND.  &
          (neq%par(iPar)%locq(1) /=  8 .OR. neq%par(iPar)%locq(2) == 1) .AND. &
           neq%par(iPar)%locq(1) /= 19 .AND.  &
!           neq%par(iPar)%locq(1) /= 22 .AND.  &
           neq%par(iPar)%locq(1) /= 23 .AND.  &
           neq%par(iPar)%locq(1) /= 26 .AND.  &
           neq%par(iPar)%locq(1) /= 27 .AND.  &
           neq%par(iPar)%locq(1) /= 30) CYCLE

      IF ( neq%par(iPar)%name(1:staNameLength)==staInfo%renamsta(iCrx)%oldNam &
                                                                        .AND. &
           neq%par(iPar)%name(1:staNameLength)/=staInfo%renamsta(iCrx)%staNam &
                                                                        .AND. &
           (neq%par(iPar)%time%mean .isIn. staInfo%renamsta(iCrx)%timint)) THEN

        neq%par(iPar)%name = staInfo%renamsta(iCrx)%stanam

        IF (nLines >= 0) prtRenam(iCrx) = 1

        IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1) THEN
          IF (lPar > 0) THEN
            IF(neq%par(iPar)%name /= neq%par(lPar)%name) lPar = 0
          ENDIF
          IF (lPar == 0) THEN
            WRITE(lfnerr,'(/,A,/,18X,A)')                               &
            ' ### SR RDSTACRX: Station renamed ' //                     &
                               staInfo%renamsta(iCrx)%oldNam(1:16) //   &
                               ' -> ' // neq%par(iPar)%name,            &
                              'File name: ' // TRIM(opt%neqFileName(iFil))
            lPar = iPar
          ENDIF
          CALL timst2(1,1,neq%par(iPar)%time%mean,epostr)
          WRITE(lfnerr,'(18X,A)')                                       &
               'Parameter: ' // TRIM(parTyp(neq%par(iPar)%locq(1))) //  &
               '   ' // epostr
        ENDIF
      ENDIF
    ENDDO

! Rename the station (SINEX)
! --------------------------
    DO iSta = 1, neq%misc%nstat_sinex
      snxMean = neq%misc%sinex(iSta)%timint%t(1)/2d0 + &
                neq%misc%sinex(iSta)%timint%t(2)/2d0

      IF ( neq%misc%sinex(iSta)%stname == staInfo%renamsta(iCrx)%oldNam .AND. &
           neq%misc%sinex(iSta)%stname /= staInfo%renamsta(iCrx)%staNam .AND. &
          (snxMean .isIn. staInfo%renamsta(iCrx)%timint))THEN

        neq%misc%sinex(iSta)%stname = staInfo%renamsta(iCrx)%stanam

        IF (nLines >= 0) prtRenam(iCrx) = 1

        IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1) THEN
          IF (lPar > 0) THEN
            IF (neq%misc%sinex(iSta)%stname /= &
                                  neq%par(lPar)%name(1:staNameLength)) lPar = 0
          ENDIF
          IF (lPar == 0) THEN
            WRITE(lfnerr,'(/,A,/,18X,A)')                               &
            ' ### SR RDSTACRX: Station renamed ' //                     &
                               staInfo%renamsta(iCrx)%oldNam(1:16) //   &
                               ' -> ' // neq%par(iPar)%name,            &
                              'File name: ' // TRIM(opt%neqFileName(iFil))
            lPar = 0
          ENDIF

          CALL timst2(1,1,snxMean,epostr)
          WRITE(lfnerr,'(18X,A)')                                       &
               'Parameter: SNX   ' // epostr
        ENDIF
        EXIT ! Exit loop for all misc%sinex records
      ENDIF
    ENDDO
  ENDDO

! Apply the changes of antenna eccentricities on bMat
! ---------------------------------------------------
  dC(:) = 0.d0

  DO iCrx = 1, staInfo%ninfo

    lPar = 0
    DO iPar = 1, neq%misc%npar

      IF ( neq%par(iPar)%locq(1)     ==   1                  .AND. &
           neq%par(iPar)%locq(3)     ==   1                  .AND. &
           neq%par(iPar)%name(1:staNameLength) == staInfo%stainfo(iCrx)%stanam &
                                                                         .AND. &
           (neq%par(iPar)%time%mean .isIn. staInfo%stainfo(iCrx)%timint) ) THEN

        dCneu(:) = 0.d0
        DO iSta = 1, neq%misc%nstat_sinex

          snxMean = neq%misc%sinex(iSta)%timint%t(1)/2d0 + &
                    neq%misc%sinex(iSta)%timint%t(2)/2d0

          IF (neq%misc%sinex(iSta)%stname == staInfo%stainfo(iCrx)%stanam .AND.&
              ( snxMean .isIn. staInfo%stainfo(iCrx)%timint) ) THEN

            DO ii = 1,3
              IF (undef_e /= staInfo%stainfo(iCrx)%antecc(ii) .AND. &
                  ABS(staInfo%stainfo(iCrx)%antecc(ii) - &
                      neq%misc%sinex(iSta)%antecc(ii)) > 0.000001D0) THEN
                dCneu(ii) = staInfo%stainfo(iCrx)%antecc(ii)-&
                            neq%misc%sinex(iSta)%antecc(ii)

                IF (nLines >= 0) THEN
                  prtInfo(iCrx)%flg = '001'
                  prtInfo(iCrx)%antecc(ii) = neq%misc%sinex(iSta)%antecc(ii)
                ENDIF

                IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1) THEN
                  IF (lPar > 0) THEN
                    IF (neq%par(iPar)%name /= neq%par(lPar)%name) lPar = 0
                  ENDIF
                  IF (lPar == 0) THEN
                    WRITE(lfnerr,'(/,A,/,18X,A)')                          &
                    ' ### SR RDSTACRX: Ant ecc changed for station ' //    &
                                       staInfo%staInfo(iCrx)%staNam(1:16), &
                    'File name: ' // TRIM(opt%neqFileName(iFil))
                    lPar = iPar
                  ENDIF
                  WRITE(lfnerr,'(18X,A,F14.4,A,F14.4,A)')                &
                        eccTyp(ii),neq%misc%sinex(iSta)%antecc(ii),      &
                        ' m -> ',staInfo%stainfo(iCrx)%antecc(ii),' m'
                  CALL timst2(1,1,neq%par(iPar)%time%mean,epostr)
                  WRITE(lfnerr,'(18X,A)')                                &
                  'Parameter: ' // TRIM(parTyp(neq%par(iPar)%locq(1))) //&
                  '   ' // epostr
                  CALL timst2(1,1,snxMean,epostr)
                  WRITE(lfnerr,'(18X,A)')                                &
                  'Parameter: SNX   ' // epostr
                ENDIF
              ENDIF
            ENDDO

            EXIT  ! End loop all misc%sinex records
          ENDIF ! Correct SINEX record found
        ENDDO ! Loop all misc%sinex records

        xsta(1) = neq%par(iPar  )%x0
        xsta(2) = neq%par(iPar+1)%x0
        xsta(3) = neq%par(iPar+2)%x0

        CALL xyzell( datum%aell, datum%bell, datum%dxell, datum%drell, &
             datum%scell, xsta(:), xell(:) )

        CALL ellecc( xell(:), dCneu(:), dC(iPar:iPar+2) )
      ENDIF ! Parameter found
    ENDDO ! Loop parameters
  ENDDO ! Loop all station renaming records

! Perform the transformation
! --------------------------
  DO iPar = 1, neq%misc%npar
    hlp(iPar) = 0.d0
    DO ii = 1, neq%misc%npar
      hlp(iPar) = hlp(iPar) + neq%aNor(ikf(iPar,ii)) * dC(ii)
    ENDDO
    neq%misc%lTPl  = neq%misc%lTPl  + dC(iPar) * &
         (-2.d0*neq%bNor(iPar) + hlp(iPar))
    neq%bNor(iPar) = neq%bNor(iPar) - hlp(iPar)
  ENDDO

! Change Remaining SINEX information
! ----------------------------------
  DO iSta = 1, neq%misc%nstat_sinex
    snxMean = neq%misc%sinex(iSta)%timint%t(1)/2d0 + &
              neq%misc%sinex(iSta)%timint%t(2)/2d0

! Length of antenna name (compatibility with NEQ version 1)
    lants=20
    lantr=20


! Change station information
! --------------------------
    DO iCrx = 1, staInfo%ninfo
      IF ( neq%misc%sinex(iSta)%stname == staInfo%stainfo(iCrx)%stanam .AND. &
           ( snxMean .isIn. staInfo%stainfo(iCrx)%timint) ) THEN

! Adapt antenna eccentricity
        DO ii = 1,3
          IF (staInfo%stainfo(iCrx)%antecc(ii) /= undef_e .AND. &
             ABS(staInfo%stainfo(iCrx)%antecc(ii) - &
                      neq%misc%sinex(iSta)%antecc(ii)) > 0.000001D0) THEN
            IF (nLines >= 0) THEN
              prtInfo(iCrx)%flg = '001'
              prtInfo(iCrx)%antecc(ii) = neq%misc%sinex(iSta)%antecc(ii)
            ENDIF

            neq%misc%sinex(iSta)%antecc(ii) = staInfo%stainfo(iCrx)%antecc(ii)
          ENDIF
        ENDDO

! Something else to change?
        IF ((staInfo%stainfo(iCrx)%recNam  == undef_c .OR. &
             neq%misc%sinex(iSta)%antrec(1:lantr) == &
                                   staInfo%stainfo(iCrx)%recNam(1:lantr)) .AND.&
            (staInfo%stainfo(iCrx)%antNam  == undef_c .OR. &
             neq%misc%sinex(iSta)%antsta(1:lants) == &
                                   staInfo%stainfo(iCrx)%antNam(1:lants)) .AND.&
            (staInfo%stainfo(iCrx)%antNum  == undef_i .OR. &
             neq%misc%sinex(iSta)%antnum == staInfo%stainfo(iCrx)%antNum)) CYCLE

! Report sinex changes
        IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1) THEN
          WRITE(lfnerr,'(/,A,/,18X,A)')                        &
                ' ### SR RDSTACRX: Station information changed for ' //  &
                                       staInfo%staInfo(iCrx)%staNam(1:16), &
                'File name: ' // TRIM(opt%neqFileName(iFil))
        ENDIF

        IF (staInfo%stainfo(iCrx)%recNam  /= undef_c .AND. &
            neq%misc%sinex(iSta)%antrec(1:lantr) /= &
                                 staInfo%stainfo(iCrx)%recNam(1:lantr)) THEN

          IF (nLines >= 0) THEN
            prtInfo(iCrx)%flg = '001'
            IF (neq%misc%sinex(iSta)%antrec(17:20) == '????' .AND. &
                neq%misc%sinex(iSta)%antrec(1:16) == &
                                     staInfo%stainfo(iCrx)%recNam(1:16)) &
              prtInfo(iCrx)%flg = '000'
            prtInfo(iCrx)%recnam = neq%misc%sinex(iSta)%antrec
          ENDIF

          IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1)                  &
            WRITE(lfnerr,'(18X,A)')                                    &
                 'Rec. name: ' // neq%misc%sinex(iSta)%antrec(1:20) // &
                              ' -> ' // staInfo%stainfo(iCrx)%recNam(1:20)

          neq%misc%sinex(iSta)%antrec = staInfo%stainfo(iCrx)%recNam
        ENDIF

        IF (staInfo%stainfo(iCrx)%antNam  /= undef_c .AND. &
            neq%misc%sinex(iSta)%antsta(1:lants) /= &
                                 staInfo%stainfo(iCrx)%antNam(1:lants)) THEN

          IF (nLines >= 0) THEN
            prtInfo(iCrx)%flg = '001'
            IF ((neq%misc%sinex(iSta)%antsta(17:20) == '????' .OR.   &
                 neq%misc%sinex(iSta)%antsta(17:20) == '    ') .AND. &
                 neq%misc%sinex(iSta)%antsta(1:16) == &
                                      staInfo%stainfo(iCrx)%antNam(1:16)) &
              prtInfo(iCrx)%flg = '000'
            prtInfo(iCrx)%antnam = neq%misc%sinex(iSta)%antsta
          ENDIF

          IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1)                  &
            WRITE(lfnerr,'(18X,A)')                                    &
                 'Ant. name: ' // neq%misc%sinex(iSta)%antsta(1:20) // &
                              ' -> ' // staInfo%stainfo(iCrx)%antNam(1:20)

          neq%misc%sinex(iSta)%antsta = staInfo%stainfo(iCrx)%antNam
        ENDIF

        IF (staInfo%stainfo(iCrx)%antNum  /= undef_i .AND. &
            neq%misc%sinex(iSta)%antnum /= staInfo%stainfo(iCrx)%antNum) THEN

          IF (nLines >= 0) THEN
            prtInfo(iCrx)%flg = '001'
            IF (neq%misc%sinex(iSta)%antnum == undef_i) &
              prtInfo(iCrx)%flg = '000'
            prtInfo(iCrx)%antnum = neq%misc%sinex(iSta)%antnum
          ENDIF

          IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1) THEN
            WRITE(lfnerr,'(18X,2(A,I10,6X))') &
                 'Ant. numb: ', neq%misc%sinex(iSta)%antnum, &
                              ' -> ', staInfo%stainfo(iCrx)%antnum
          ENDIF

          neq%misc%sinex(iSta)%antnum = staInfo%stainfo(iCrx)%antNum
        ENDIF

        IF (nLines >= 0 .AND. opt%prt(prtCrx) == 1) THEN
          CALL timst2(1,1,snxMean,epostr)
          WRITE(lfnerr,'(18X,A)') 'Parameter: SNX   ' // epostr
        ENDIF

      ENDIF
    ENDDO
  ENDDO

!
! Report all "stacrux" modifications
!
! Count the number of new lines
! -----------------------------
  IF (nLines >= 0) THEN

    nLines = 0
    DO iCrx = 1, staInfo%nRenam
      IF (prtRenam(iCrx) == 1) nLines = nLines+1
    ENDDO

    DO iCrx = 1, staInfo%nInfo
      IF (prtInfo(iCrx)%flg == '000') CYCLE

      IF (prtInfo(iCrx)%recnam /= undef_c) nLines = nLines+1
      IF (prtInfo(iCrx)%antnam /= undef_c) nLines = nLines+1
      IF (prtInfo(iCrx)%antnum /= undef_i) nLines = nLines+1
      IF (prtInfo(iCrx)%antecc(1) /= undef_e) nLines = nLines+1
      IF (prtInfo(iCrx)%antecc(2) /= undef_e) nLines = nLines+1
      IF (prtInfo(iCrx)%antecc(3) /= undef_e) nLines = nLines+1

    ENDDO
  ENDIF

! Extent the memory for the buffer
! --------------------------------
  IF (nLines > 0) THEN
    IF (iFil > 1) THEN
      nLines = nLines+1 ! an empty lines between files
      iLines = SIZE(lines)
      ALLOCATE(hlpLine(iLines),stat=iac)
      CALL alcerr(iac,'hlpLine',(/iLines/),'rdstacrx')

      hlpLine = lines

      DEALLOCATE(lines,stat=iac)
      ALLOCATE(lines(iLines+nLines),stat=iac)
      CALL alcerr(iac,'lines',(/iLines+nLines/),'rdstacrx')

      lines = ' '
      lines(1:iLines) = hlpLine(:)

      DEALLOCATE(hlpLine)
      iLines = iLines+1
    ELSE
      iLines = 0
      ALLOCATE(lines(nLines),stat=iac)
      CALL alcerr(iac,'lines',(/nLines/),'rdstacrx')
      lines = ' '
    ENDIF

! Generate the new lines
! ----------------------
    DO iCrx = 1, staInfo%nRenam
      IF (prtRenam(iCrx) == 1) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,4(3X,A))')                              &
              iFil, staInfo%renamSta(iCrx)%stanam(1:16) // ':',          &
              'Station  name  ',                                         &
              staInfo%renamSta(iCrx)%oldnam(1:16) // '    ' // ' -> ' // &
              staInfo%renamSta(iCrx)%stanam(1:16) // '    ',             &
              TRIM(staInfo%renamSta(iCrx)%remark)
      ENDIF
    ENDDO

    DO iCrx = 1, staInfo%nInfo
      IF (prtInfo(iCrx)%flg == '000') CYCLE

      IF (prtInfo(iCrx)%recnam /= undef_c) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,4(3X,A))')                              &
              iFil, staInfo%staInfo(iCrx)%stanam(1:16) // ':',           &
              'Receiver type  ',                                         &
              prtInfo(iCrx)%recnam(1:20) // &
                       ' -> ' //  staInfo%staInfo(iCrx)%recnam(1:20),    &
              staInfo%staInfo(iCrx)%remark
      ENDIF

      IF (prtInfo(iCrx)%antnam /= undef_c) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,4(3X,A))')                              &
              iFil, staInfo%staInfo(iCrx)%stanam(1:16) // ':',           &
              'Antenna  type  ',                                         &
              prtInfo(iCrx)%antnam(1:20) // &
                       ' -> ' //  staInfo%staInfo(iCrx)%antnam(1:20),    &
              staInfo%staInfo(iCrx)%remark
      ENDIF

      IF (prtInfo(iCrx)%antnum /= undef_i) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,2(3X,A),3X,I18,2X,A,I18,2X,3X,A)')      &
              iFil, staInfo%staInfo(iCrx)%stanam(1:16) // ':',           &
              'Antenna  number',                                         &
              prtInfo(iCrx)%antnum,' -> ',staInfo%staInfo(iCrx)%antnum,  &
              staInfo%staInfo(iCrx)%remark
      ENDIF

      IF (prtInfo(iCrx)%antecc(1) /= undef_e) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,2(3X,A),3X,F18.4,A,F18.4,A,3X,A)')      &
              iFil, staInfo%staInfo(iCrx)%stanam(1:16) // ':',           &
              'Ant ecc (north)',                                         &
              prtInfo(iCrx)%antecc(1), ' m -> ',&
                                  staInfo%staInfo(iCrx)%antecc(1),' m',  &
              staInfo%staInfo(iCrx)%remark
      ENDIF

      IF (prtInfo(iCrx)%antecc(2) /= undef_e) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,2(3X,A),3X,F18.4,A,F18.4,A,3X,A)')      &
              iFil, staInfo%staInfo(iCrx)%stanam(1:16) // ':',           &
              'Ant ecc  (east)',                                         &
              prtInfo(iCrx)%antecc(2), ' m -> ',&
                                  staInfo%staInfo(iCrx)%antecc(2),' m',  &
              staInfo%staInfo(iCrx)%remark
      ENDIF

      IF (prtInfo(iCrx)%antecc(3) /= undef_e) THEN
        iLines = iLines+1
        WRITE(lines(iLines),'(I4,2(3X,A),3X,F18.4,A,F18.4,A,3X,A)')      &
              iFil, staInfo%staInfo(iCrx)%stanam(1:16) // ':',           &
              'Ant ecc  (up)  ',                                         &
              prtInfo(iCrx)%antecc(3), ' m -> ',&
                                  staInfo%staInfo(iCrx)%antecc(3),' m',  &
              staInfo%staInfo(iCrx)%remark
      ENDIF

    ENDDO
  ENDIF

  IF (nLines >= 0) THEN
    DEALLOCATE(prtRenam,stat=iac)
    DEALLOCATE(prtInfo,stat=iac)
  ENDIF

! Write the protocol buffer into the file
! ---------------------------------------
  IF (nLines >= 0 .AND. SIZE(lines) > 0 .AND. &
      iFil == SIZE(opt%neqFileName)) THEN
    WRITE(lfnprt,'(/,2(/,A),/,2(/,A))')                                  &
         ' STATION INFORMATION MODIFICATIONS:         ' // &
                                                      TRIM(opt%stacrux), &
         ' ---------------------------------',                           &
         ' Num   Station name        Changed entry     old value ' //    &
         '              new value              Remark',                  &
         ' ------------------------------------------------------' //    &
         '-------------------------------------------------------' //    &
         '----------------------'
    DO ii = 1,SIZE(lines)
      WRITE(lfnprt,'(A)') TRIM(lines(ii))
    ENDDO
    nLines = -1
  ENDIF

  RETURN
END SUBROUTINE rdstacrx

END MODULE
