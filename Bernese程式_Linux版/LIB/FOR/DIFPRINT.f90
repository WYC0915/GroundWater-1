MODULE s_DIFPRINT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE difprint(neq, numfil, dx, statist)

! -------------------------------------------------------------------------
! Purpose:    This subroutine prints the differences between the combined
!             solution and each of the individual solution
!
! Author:     L. Mervart
!
! Created:    10-Sep-1998
!
! Changes:    09-Mar-2000 LM: Write ifil with i3 format instead of i2
!             07-Sep-2000 SS: Write blank line between N/E/U sequences
!             17-Sep-2001 RD: Generate a list of bad solutions
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             11-Jan-2002 CU: Write helpLine only if used, indicate if
!                             solltext too long
!             25-Apr-2002 CU: Write weekly summary file, if requested
!             26-Apr-2002 CU: Use only 14 characters for stanam and full
!                             title line in weekly summary file
!             06-May-2002 CU: Increase rms/residual decimal places: 1 -> 2
!             06-Jun-2002 HU: Format statement corrected
!             29-Oct-2002 MR: Correct format (1x)
!             27-Feb-2003 HU: DATUM from D_DATUM
!             19-Mar-2003 RD: Write long string with format (because IFC)
!             17-Dec-2003 mm: Output changed
!             22-Jan-2004 HB: Avoid 'ADVANCE=NO' after '(nX)' format
!             09-Nov-2004 CU: Correct timfil computation for plt file
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             01-Mar-2005 HB: Adopt for ifc-Compiler, Version 8.1 (2),
!                             forgot a part ...
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             29-Nov-2005 CU: Correct output format (lowercase)
!             27-Feb-2006 CU: Print statistic for individual solutions
!             22-Jun-2006 SS: Changed format statement wrt plot file
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             24-Mar-2009 SS: Provide dx%rms information in plot file
!             26-Mar-2009 SS: Compute variance-covariance scaling factors
!             24-Nov-2009 SS: Provide parameter-specific MJD in plot file
!             29-Nov-2009 RD: Allow 5 digits in list of "small number of sol."
!             22-Dec-2009 LO: Covariance format of PLT file modified
!             31-Aug-2009 LO: One space added to the format of the PLT-file
!             05-Sep-2010 RD: Plot file is closed
!             25-Oct-2010 RD: Write the complete station name
!             29-Dec-2010 SL: hlpStr shortLineLength -> lineLength
!             29-Jul-2011 SS: Use 16 characters for stanam in weekly sum file
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
!-------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, lfnplt, lfnprt, &
                      fileNameLength, staNameLength, lineLength
  USE m_time,   ONLY: t_timint
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_dx,opt,comstat
  USE d_const,  ONLY: filtitle

  USE s_alcerr
  USE s_opnfil
  USE s_opnerr
  USE s_timst2
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                     :: neq
  INTEGER(i4b)                    :: numfil
!!!!!  TYPE(t_dx), DIMENSION(SIZE(opt%neqFileName),neq%misc%npar) :: dx
  TYPE(t_dx), DIMENSION(numfil,*) :: dx
  REAL(r8b),  DIMENSION(numfil,9) :: statist

! Local Variables
! ---------------
  TYPE(t_timint)             :: tInterv

  CHARACTER(LEN=lineLength)  :: hlpLine
  CHARACTER(LEN=lineLength)  :: hlpStr
  CHARACTER(LEN=lineLength),  &
    DIMENSION(:),ALLOCATABLE :: badText, rmsText, solText, hlpText

  INTEGER(i4b),               &
    DIMENSION(:),ALLOCATABLE :: staIdx
  INTEGER(i4b)               :: nBad, nRMS, nSol
  INTEGER(i4b)               :: ip,jp
  INTEGER(i4b)               :: ii,jj,kk
  INTEGER(i4b)               :: ios, irc
  INTEGER(i4b)               :: icrd
  INTEGER(i4b)               :: iStr
  INTEGER(i4b)               :: nfils
  INTEGER(i4b)               :: ifil, jFil
  INTEGER(i4b)               :: nHlp
  INTEGER(i4b)               :: numsta
  INTEGER(i4b)               :: daynum
  INTEGER(i4b)               :: daytot
  INTEGER(i4b), DIMENSION(5) :: vsfTot
  INTEGER(i4b)               :: nloop
  INTEGER(i4b)               :: nstrg
  INTEGER(i4b)               :: ibeg,iend
  INTEGER(i4b)               :: ndiff
  INTEGER(i4b), DIMENSION(9) :: istatist

  REAL(r8b)                  :: maxRes
  REAL(r8b)                  :: timFil
  REAL(r8b)                  :: sqrMean
  REAL(r8b)   , DIMENSION(3) :: diff
  REAL(r8b)   , DIMENSION(3) :: rms
  REAL(r8b)   , DIMENSION(3) :: sumDif
  REAL(r8b)   , DIMENSION(5) :: vsfSum
  REAL(r8b)                  :: numloop
  REAL(r8b)                  :: tim1, tim2

  CHARACTER(LEN=1) , DIMENSION(3), PARAMETER   :: crd = (/ 'N','E','U' /)
  CHARACTER(LEN=10), DIMENSION(5)              :: parCom
  CHARACTER(LEN=fileNameLength)                :: filsum
  CHARACTER(LEN=1) , DIMENSION(:), ALLOCATABLE :: daystrg
  CHARACTER(LEN=7)           :: strg
  CHARACTER(LEN=36)          :: parTyp

  LOGICAL                    :: sumflg


! Print statistics for individual solutions
! -----------------------------------------
  WRITE(lfnprt,'(/,2(/,A),/,2(/,3A))')             &
       ' Statistics of individual solutions:',        &
       ' ----------------------------------',         &
       ' File     RMS (m)         DOF  Chi**2/DOF ',  &
       ' #Observations authentic  / pseudo ',         &
       ' #Parameters explicit  / implicit / singular',&
       ' -----------------------------------------',  &
       '------------------------------------------',  &
       '-------------------------------------'

  DO ifil = 1, numfil
    istatist(:) = DINT(statist(ifil,:))
    WRITE(lfnprt,'(1X,I4,F12.5,I12,F12.2,13X,I12,I10,10X,I12,I12,I11)') &
      istatist(1),statist(ifil,2),istatist(3),statist(ifil,4),istatist(5:9)
  ENDDO

! Generate an index for alphabetic station list
! ---------------------------------------------
  ALLOCATE(staIdx(neq%misc%npar), stat=irc)
  CALL alcerr(irc,'staIdx',(/neq%misc%npar/),'difprint')
  staIdx(:) = (/ (ii, ii=1,neq%misc%npar) /)

  irc       = 1
  daytot    = 0
  sumDif(:) = 0.D0

  DO WHILE (irc /= 0)

    irc    = 0
    numsta = 0

    DO ip = 1, neq%misc%npar-1

      IF ( neq%par(staIdx(ip))%locq(1) /= 1 .OR. &
           neq%par(staIdx(ip))%locq(4)  > 1 .OR. &
           neq%par(staIdx(ip))%locq(3) /= 1 ) CYCLE

      numsta = numsta + 1

      DO jp = ip, neq%misc%npar

        IF ( neq%par(staIdx(jp))%locq(1) /= 1 .OR. &
             neq%par(staIdx(jp))%locq(4)  > 1 .OR. &
             neq%par(staIdx(jp))%locq(3) /= 1 ) CYCLE

        IF (neq%par(staIdx(ip))%name > neq%par(staIdx(jp))%name) THEN

          ii = staIdx(ip)
          staIdx(ip) = staIdx(jp)
          staIdx(jp) = ii

          irc = 1

        ENDIF

      ENDDO

    ENDDO

  ENDDO

! Number of input NEQ files
! -------------------------
  nfils = SIZE(opt%neqFileName)

! Summary file requested?
! -----------------------
  sumflg = .FALSE.
  CALL gtflna(0,'WEKSUM',filsum,irc)

  IF (irc == 0) THEN
    sumflg = .TRUE.
    CALL opnfil(lfnloc,filsum,'UNKNOWN','FORMATTED',' ',' ',irc)
    numloop   = nfils/7.D0
    nloop     = CEILING(numloop)
    nstrg     = nloop * 7
    ALLOCATE(daystrg(nstrg), stat=irc)
    CALL alcerr(irc, 'daystrg',(/nstrg/),'difprint')
  ENDIF

! Text for bad solution detection
! -------------------------------
  ALLOCATE(badText(20), stat=irc)
  CALL alcerr(irc, 'badText',(/20/),'difprint')

  ALLOCATE(rmsText(20), stat=irc)
  CALL alcerr(irc, 'rmsText',(/20/),'difprint')

  ALLOCATE(solText(20), stat=irc)
  CALL alcerr(irc, 'solText',(/20/),'difprint')

  nBad = 0
  nSol = 0
  nRMS = 0

  IF (opt%plotrs /= '') THEN
    CALL opnfil(lfnplt, opt%plotrs, 'UNKNOWN', 'FORMATTED', ' ', ' ', ios)
    CALL opnerr(lfnerr, lfnplt, ios, opt%plotrs, 'DIFPRINT')
  END IF

  WRITE(lfnprt,'(2/,1x,A,/,1x,34("-"),/)') 'Comparison of individual solutions:'

! Print summary file header, if requested
! ---------------------------------------
  IF (sumflg)                                                  &
    write(lfnloc,'(3(/,1X,A),I4,4(/,1X,A))')                   &
      filtitle,                                                &
      '--------------------------------------------------'//   &
      '------------------------------',                        &
      'Total number of stations: ',numsta,                     &
      '----------------------------------------------------',  &
      '                      Weekday     Repeatability (mm)',  &
      'Station        #Days  0123456      N      E      U  ',  &
      '----------------------------------------------------'

  vsfTot(:) = 0

! Loop all parameter
! -----------------
  DO jp =1, neq%misc%npar

! Use station index
! -----------------
    ip = staIdx(jp)

    IF ( neq%par(ip)%locq(1) /= 1 .OR. neq%par(ip)%locq(4) > 1 .OR. &
         neq%par(ip)%locq(3) /= 1 ) CYCLE

    DO  icrd = 1, 3

      sqrMean = 0.0
      nHlp    = 0

      tInterv%t = (/ 99d99, 00d00 /)
      hlpLine = ' '

      maxRes = 0d0

      DO ifil = 1, nfils

         IF ( dx(ifil,ip)%flag ) THEN
           diff(1:3) = dx(ifil,ip:ip+2)%diff

           vsfSum(icrd) = vsfSum(icrd)+dx(ifil,ip+icrd-1)%rms(3)**2
           vsfTot(icrd) = vsfTot(icrd) + 1

           nHlp = nHlp + 1

           sqrMean = sqrMean + diff(icrd)**2

           IF (dx(ifil,ip)%t%mean-dx(ifil,ip)%t%half < tInterv%t(1)) &
             tInterv%t(1) = dx(ifil,ip)%t%mean-dx(ifil,ip)%t%half
           IF (dx(ifil,ip)%t%mean+dx(ifil,ip)%t%half > tInterv%t(2)) &
             tInterv%t(2) = dx(ifil,ip)%t%mean+dx(ifil,ip)%t%half

           IF (opt%minSol /= 0 .AND. opt%minSol > nHlp .AND.         &
               nHlp*5 <= linelength)                                 &
             WRITE(hlpLine((nHlp-1)*5+1:nHlp*5),'(I5)') iFil
           IF (DABS(diff(icrd)) >= DABS(maxRes)) THEN
             jFil   = iFil
             maxRes = diff(iCrd)
           ENDIF

         END IF
      END DO

      IF (nHlp > 1) THEN
        sqrMean = SQRT(sqrMean/(nHlp-1))
      ELSEIF (nHlp == 1) THEN
        sqrMean = SQRT(sqrMean)
      ENDIF

! Generate the outlier text: min. number of solutions
! -------------------------
      IF (nHlp > 0 .AND. iCrd == 1 .AND. &
          opt%minSol /= 0 .AND. nHlp < opt%minSol) THEN

        IF (nSol == SIZE(solText)) THEN
          ALLOCATE(hlpText(nSol), stat=irc)
          CALL alcerr(irc, 'hlpText',(/nSol/),'difprint')
          hlpText = solText
          DEALLOCATE(solText,stat=irc)
          ALLOCATE(solText(nSol+10), stat=irc)
          CALL alcerr(irc, 'solText',(/nSol+10/),'difprint')
          solText(1:nSol) = hlptext(:)
          DEALLOCATE(hlpText,stat=irc)
        ENDIF

        nSol = nSol + 1
        solText(nSol) = ""
        WRITE(solText(nSol)(2:21),'(A)') neq%par(ip)%name

        ! Indicate if line too long
        solText(nSol) = ' '//neq%par(ip)%name//'  '//hlpLine

        IF (LEN_TRIM(hlpLine) + 24 > lineLength) &
          solText(nSol)(lineLength-4:lineLength) = '... '

      ENDIF

! Generate the outlier text: big component rms
! -------------------------
      IF (nHlp > 0 .AND. opt%badSol(2,iCrd) /= 0d0 .AND. &
          sqrMean > opt%badSol(2,iCrd)/1000d0) THEN

        IF (nRMS == SIZE(rmsText)) THEN
          ALLOCATE(hlpText(nRMS), stat=irc)
          CALL alcerr(irc, 'hlpText',(/nRMS/),'difprint')
          hlpText = rmsText
          DEALLOCATE(rmsText,stat=irc)
          ALLOCATE(rmsText(nRMS+10), stat=irc)
          CALL alcerr(irc, 'rmsText',(/nRMS+10/),'difprint')
          rmsText(1:nRMS) = hlptext(:)
          DEALLOCATE(hlpText,stat=irc)
        ENDIF

        nRMS = nRMS + 1
        rmsText(nRMS) = ""
        WRITE(rmsText(nRMS)(2:21),'(A)') neq%par(ip)%name

        WRITE(rmsText(nRMS)(31:31),'(A)') crd(iCrd)
        WRITE(rmsText(nRMS)(35:42),'(F8.2)') sqrMean*1000d0
!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
        tim1 = dx(jfil,ip)%t%mean-dx(jfil,ip)%t%half
        CALL timst2(1,1,tim1, &
                    rmsText(nRMS)(53:71))
        tim2 = dx(jfil,ip)%t%mean+dx(jfil,ip)%t%half
        CALL timst2(1,1,tim2, &
                    rmsText(nRMS)(74:92))
#else
        CALL timst2(1,1,dx(jfil,ip)%t%mean-dx(jfil,ip)%t%half, &
                    rmsText(nRMS)(53:71))

        CALL timst2(1,1,dx(jfil,ip)%t%mean+dx(jfil,ip)%t%half, &
                    rmsText(nRMS)(74:92))
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ENDIF

! Start writing output comparison
! -------------------------------
      hlpStr = ' '
      WRITE(hlpStr(1:27),'(1x,a,1x,a1,1x,f7.2)') &
            neq%par(ip)%name(1:staNameLength), crd(icrd), sqrMean*1000.0
      iStr = 30

      IF (sumflg) THEN
        daystrg(:)= ""
        daynum    = 0
        rms(iCrd) = sqrMean*1000.0
      ENDIF

      DO ifil = 1, nfils
        IF ( MOD(ifil, 7) == 1 .AND. ifil /= 1 ) THEN
          WRITE(lfnprt,'(A)')TRIM(hlpStr)
          hlpStr = ' '
          iStr = 30
        END IF

        IF ( dx(ifil,ip)%flag ) THEN
          diff(1:3) = dx(ifil,ip:ip+2)%diff

          WRITE(hlpStr(iStr:iStr+7),'(1X,f7.2)') diff(icrd)*1000.0
          iStr = iStr + 8

          IF (sumflg) THEN
            sumDif(iCrd) = sumDif(iCrd) + diff(icrd)**2
            daystrg(ifil)="X"
            daynum       = daynum + 1
          ENDIF

! Generate the outlier text: big residual for indiv. solution
! -------------------------
          IF (opt%badSol(1,iCrd) /= 0d0 .AND. &
              DABS(diff(icrd)) > opt%badSol(1,iCrd)/1000d0) THEN

            IF (nBad == SIZE(badText)) THEN
              ALLOCATE(hlpText(nBad), stat=irc)
              CALL alcerr(irc, 'hlpText',(/nBad/),'difprint')
              hlpText = badText
              DEALLOCATE(badText,stat=irc)
              ALLOCATE(badText(nBad+10), stat=irc)
              CALL alcerr(irc, 'badText',(/nBad+10/),'difprint')
              badText(1:nBad) = hlptext(:)
              DEALLOCATE(hlpText,stat=irc)
            ENDIF

            nBad = nBad + 1
            badText(nBad) = ""
            WRITE(badText(nBad)(2:17),'(A)') neq%par(ip)%name(1:staNameLength)
            WRITE(badText(nBad)(24:27),'(I4)') iFil
            WRITE(badText(nBad)(31:31),'(A)') crd(iCrd)
            WRITE(badText(nBad)(35:42),'(F8.2)') diff(icrd)*1000d0
!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
            tim1 = dx(ifil,ip)%t%mean-dx(ifil,ip)%t%half
            CALL timst2(1,1,tim1, &
                 badText(nBad)(53:71))
            tim2 = dx(ifil,ip)%t%mean+dx(ifil,ip)%t%half
            CALL timst2(1,1,tim2, &
                 badText(nBad)(74:92))
#else
            CALL timst2(1,1,dx(ifil,ip)%t%mean-dx(ifil,ip)%t%half, &
                 badText(nBad)(53:71))
            CALL timst2(1,1,dx(ifil,ip)%t%mean+dx(ifil,ip)%t%half, &
                 badText(nBad)(74:92))
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ENDIF

! Write into the plot file
!-------------------------
          IF (opt%plotrs /= '' .AND. icrd == 1) THEN
!!            timFil = ( comstat%taecml(1,1,ifil) + &
!!                       comstat%taecml(2,1,ifil) ) / 2.0
            timFil = dx(ifil,ip)%t%mean
            DO ii = 1, 3
              WRITE(lfnplt,'(a16,i5,i2,f10.5,f14.5,f23.5,2(1X,E12.5))') &
                neq%par(ip)%name(1:staNameLength), ifil, ii, diff(ii), timFil,&
                dx(ifil,ip+ii-1)%rms(1:3)
            END DO
          END IF

        ELSE
          iStr = iStr + 8
          IF (sumflg) daystrg(ifil) = " "
        END IF

        IF (iFil == nFils) THEN
          IF (iCrd == 3) THEN
            WRITE(lfnprt,'(A,/)')TRIM(hlpStr)
          ELSE
            WRITE(lfnprt,'(A)')TRIM(hlpStr)
          ENDIF
        ENDIF
      END DO

    END DO ! Loop over 3 components

! Write weekly summary, if requested
! ----------------------------------
    IF (sumflg) THEN
      strg = "       "

! only 7 days
      IF (nfils <= 7) THEN
        ndiff = 7 - nfils
        DO ii = 1,ndiff
          daystrg(nfils+ii) = " "
        ENDDO
        WRITE(strg,'(7A)')(daystrg(ii),ii=1,7)
        WRITE(lfnloc,'(1X,A,I4,2X,A,2X,3(F7.2))')             &
          neq%par(ip)%name(1:16),daynum,strg,(rms(ii),ii=1,3)
        daytot = daytot + daynum

! more then 7 days
      ELSEIF (nfils > 7) THEN

        DO ii = 1, nloop
          strg = "       "
          ibeg = ii*7 - 6
          iend = ibeg + 6

          IF (nfils < iend) THEN
            ndiff = iend - nfils
            DO jj = 1,ndiff
             daystrg(nfils+jj) = " "
            ENDDO
          ENDIF

          WRITE(strg,'(7A)')(daystrg(kk),kk=ibeg,iend)

          IF (ii == 1) THEN
            WRITE(lfnloc,'(1X,A,I4,2X,A,2X,3(F7.2))')         &
            neq%par(ip)%name(1:16),daynum,strg,(rms(kk),kk=1,3)
            daytot = daytot + daynum
          ELSE
            WRITE(lfnloc,'(23X,A)')strg
          ENDIF

        ENDDO

      ENDIF

    END IF

  END DO ! Loop over all parameters

! Close the plot file
! -------------------
  IF (opt%plotrs /= '') THEN
    close(lfnplt)
  ENDIF

! Write scaling factors
! ---------------------
  IF (vsfTot(3) > 0) THEN
    WRITE(lfnprt,'(/,2(A,/),/,2A,/,1X,131("-"))')         &
      ' Variance-covariance scaling factors:',            &
      ' -----------------------------------',             &
      ' Parameter type                        Component', &
      '    Scaling factor wrt RMS /  variance         DOF'

    vsfTot(4) = vsfTot(1)+vsfTot(2)
    vsfTot(5) = vsfTot(1)+vsfTot(2)+vsfTot(3)

    vsfSum(4) = (vsfSum(1)+vsfSum(2)          )/vsfTot(4)
    vsfSum(5) = (vsfSum(1)+vsfSum(2)+vsfSum(3))/vsfTot(5)
    DO ii = 1,3
      vsfSum(ii) = vsfSum(ii)/vsfTot(ii)
    ENDDO

! Station coordinates:
    parTyp = 'Station coordinates'
    parCom = (/'N         ','E         ','U/vertical','Horizontal','All       '/)
    DO ii = 1,5
      WRITE(lfnprt,'(1X,A35,3X,A10,13X,2F12.5,I12)') &
        parTyp,parCom(ii),SQRT(vsfSum(ii)),vsfSum(ii),vsfTot(ii)
    ENDDO

    WRITE(lfnprt,'(1X,131("-"))')

! Total:
    WRITE(lfnprt,'(1X,"Total",56X,2F12.5,I12,/)') &
      SQRT(vsfSum(5)),vsfSum(5),vsfTot(5)
  ENDIF

! Write weekly summary, if requested
! ----------------------------------
  IF (sumflg) THEN
    DO ii = 1,3
      IF (daytot <= numsta) THEN
        sumDif(ii) = 0.D0
      ELSE
        sumDif(ii) = DSQRT(sumDif(ii)/(daytot-numsta))*1000.0
      ENDIF
    ENDDO
    write(lfnloc,'(A,/,A,11X,I4,11X,3(F7.2)/,A)')              &
      ' ----------------------------------------------------', &
      ' Total',daytot,(sumDif(ii),ii=1,3),                     &
      ' ----------------------------------------------------'
    close(lfnloc)
  ENDIF

! Write a section header line
! ---------------------------
  IF (nSol + nBad + nRMS > 0) WRITE(lfnprt, '(2(/,A))',ADVANCE='NO') &
          ' Notification of detected outliers:',                       &
          ' ---------------------------------'

! Maximum tolerated residual exceeded
! -----------------------------------
  IF (nBad > 0) THEN
    WRITE(lfnprt,'(2(/,2(/,A)))') &
         ' Residuals exceeding specified threshold',                      &
         ' ---------------------------------------',                      &
         ' Station name           Sol  Com  Residual (mm)' //             &
         '               From                 To',                        &
         ' ---------------------------------------------' //              &
         '----------------------------------------------'
    DO ii = 1, nBad
      WRITE(lfnprt,'(A)') TRIM(badText(ii))
    ENDDO
  ENDIF


! Maximum tolerated rms error exceeded
! ------------------------------------
  IF (nRMS > 0) THEN
    WRITE(lfnprt,'(2(/,2(/,A)))')                                         &
         ' Root-mean-square errors exceeding specified threshold',        &
         ' -----------------------------------------------------',        &
         ' Station name                Com  RMS error (mm)' //            &
         '              From                 To',                         &
         ' -----------------------------------------------' //            &
         '--------------------------------------------'
    DO ii = 1, nRMS
      WRITE(lfnprt,'(A)') TRIM(rmsText(ii))
    ENDDO
  ENDIF


! Small number of solutions for each station
! ------------------------------------------
  IF (nSol > 0) THEN
    WRITE(lfnprt,'(2(/,2(/,A)))') &
         ' Small number of solutions for stations',                       &
         ' --------------------------------------',                       &
         ' Station name           Solution numbers',                      &
         ' ---------------------------------------------------------' //  &
         '--------------------------------'
    DO ii = 1, nSol
      WRITE(lfnprt,'(A)') TRIM(solText(ii))
    ENDDO
  ENDIF


  IF (sumflg) DEALLOCATE(daystrg,stat=irc)

  DEALLOCATE(staIdx, stat=irc)

  DEALLOCATE(badText,stat=irc)
  DEALLOCATE(rmsText,stat=irc)
  DEALLOCATE(solText,stat=irc)

END SUBROUTINE difprint


END MODULE
