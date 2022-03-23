MODULE s_PARELIMI
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE parelimi(neq,ipart,ifil)

! -------------------------------------------------------------------------
! Purpose:    The subroutine pre-eliminates the estimated parameters from
!             the NEQ system according to input options and ipart.
!
! Author:     L.Mervart
!
! Created:    22-Nov-1997
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             18-Oct-2001 RD: Generate a parameter list for printing
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Apr-2002 RD: Preeliminate the all parameters of a NEQ
!             24-Jan-2003 CU: Use new SR SYMINVG instead of SYMINV
!             27-May-2003 CU: New SR prparlst,remove parlst, reduce nparms
!             02-Jun-2003 CU: Bug fixed (parlocq 2-dim.)
!             06-Jan-2005 HU: Efficiency increased using square AII
!             11-May-2005 RD: Give dimensions to SR redtrb2
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             30-Oct-2009 SL: ityp added for prparlst call
!             04-Mar-2010 RD: Add statistic on removed parameters
!             09-Aug-2010 RD: New syminvg used
!             09-Sep-2010 GB: REDTRB2 replaced by REDSYM with bigajj_limit
!             27-Oct-2010 SL: use m_bern with ONLY
!             29-Oct-2010 SL: problem text from %remark
!             14-Dec-2010 MM: Add GNSS-specific parameters
!             15-Dec-2010 SL: deletion list format changed
!             14-Oct-2011 LP: Set main diagonal elements < 0 to zero
!             24-Nov-2011 SL/RD: RETURN after elimination of part 3
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnPrt, &
                      fileNameLength, longLineLength
!!!  USE m_cpu,    ONLY: cpu_now
  USE d_par,    ONLY: maxlcq,maxParTyp
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: comstat,opt,staInfo,prtCrx
  USE f_ikf
  USE s_redtrb2
  USE s_prparlst
  USE s_alcerr
  USE s_syminvg
  USE s_stripdir
  USE s_redsym
  USE f_istobeel
  IMPLICIT NONE

! List of Parameters
! ------------------
! input/output
  TYPE(t_neq)                           :: neq

! input
  INTEGER(i4b), INTENT(IN)              :: ipart
  INTEGER(i4b), INTENT(IN)              :: ifil

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER           :: srName = 'PARELIMI'
  REAL(r8b),        PARAMETER           :: bigajj_limit=5.d-15

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)         :: hlpFil
  CHARACTER(LEN=longLineLength),DIMENSION(2) :: line
  CHARACTER(LEN=longLineLength),DIMENSION(:),ALLOCATABLE,SAVE :: lines
  CHARACTER(LEN=longLineLength),DIMENSION(:),ALLOCATABLE      :: hlpLin

  REAL(r8b), DIMENSION(neq%misc%npar)   :: scl
  REAL(r8b), DIMENSION(neq%misc%npar)   :: bnor_save
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: A01
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: A11
  REAL(r8b),DIMENSION(:),ALLOCATABLE    :: A11h

  INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: parflg
  INTEGER(i4b),DIMENSION(staInfo%nProb,maxParTyp) :: delList
  INTEGER(i4b),DIMENSION(neq%misc%npar,maxlcq) :: parLocq
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: idel
  INTEGER(i4b)                          :: ipar0,ipar1
  INTEGER(i4b)                          :: ipar,nsing
  INTEGER(i4b)                          :: ip0,ip1
  INTEGER(i4b)                          :: np0,np1,npA
  INTEGER(i4b)                          :: jp,jpp,ip,ik
  INTEGER(i4b)                          :: iac
  INTEGER(i4b)                          :: ityp
  INTEGER(i4b)                          :: ii,kk,ik_0
  INTEGER(i4b)                          :: iProb
  INTEGER(i4b), SAVE                    :: nLines

  LOGICAL, SAVE                         :: first = .TRUE.
  LOGICAL                               :: alt = .FALSE.

! Init variables
! --------------
  IF (first) THEN
    ALLOCATE(lines(20),stat=iac)
    CALL alcerr(iac,'lines',(/20/),srName)
    nLines  = 0
    first = .FALSE.
  ENDIF

  np0     = 0
  np1     = 0
  ip0     = 0
  ip1     = 0
  idel    = 0
  parLocq = 0
  ityp    = ipart+1
  IF(ipart == 3) THEN
    ityp = 2
    IF (nLines >= 0) delList = 0
  ENDIF

! Find the parameters which should be pre-eliminated
! --------------------------------------------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 0) THEN
      np0 = np0 + 1
      crsp(ipar) = np0
      idel(ipar) = 0
      scl(ipar) = 0.d0
    ELSE
      IF ( isToBeEl(neq%par(ipar),ipart,ifil,delList) ) THEN
        np1 = np1 + 1
        crsp(ipar) = np1
        idel(ipar) = 1
        ik = ipar*(ipar+1)/2
        IF( neq%anor(ik) > 0.d0 ) THEN
          scl(ipar) = 1.d0 / DSQRT(neq%anor(ik))
        ELSE
          scl(ipar) = 0.d0
        ENDIF
        IF (ipart /= 0) &
          CALL prparlst(1,ityp,iFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                        neq%par(iPar)%time)
        parLocq(ipar,1:maxlcq) = neq%par(ipar)%locq
        neq%par(ipar)%locq(1)  = 0
      ELSE
        np0 = np0 + 1
        crsp(ipar) = np0
        idel(ipar) = 0
        scl(ipar)  = 1.d0
      END IF
    END IF
  END DO

! Report elimination of part 3
! ----------------------------
  IF( iPart == 3 .AND. iFil > 0 .AND. nLines >= 0 ) THEN

    ! Evaluate the "delList" array
    WRITE(line(1),'(1X,A3,2X,A16,2X,A16)') &
      'Num','File name       ','Station name    '
    DO iProb = 1, staInfo%nProb
      hlpFil = opt%neqFileName(iFil)
      CALL stripdir(hlpFil)
      line(2) = ''
      WRITE(line(2),'(1X,I3,2X,A16,2X,A16)') &
        iFil,hlpFil,staInfo%staProb(iProb)%staNam

      ! Extract the statistics per parameter type
      ii = 43
      DO iPar = 1,maxParTyp
        IF (delList(iProb,iPar) > 0) THEN
          WRITE(line(1)(ii:ii+8),'(A4,1X,A3)') '   #','Par'
          IF(iPar== 1) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'CRD'
          IF(iPar== 2) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'RCO'
          IF(iPar== 6) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'TRP'
          IF(iPar== 8) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'DCB'
          IF(iPar==21) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'KIN'
          IF(iPar==22) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'GRD'
          IF(iPar==23) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'CLK'
          IF(iPar==26) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'RBS'
          IF(iPar==27) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'HOI'
          IF(iPar==30) WRITE(line(2)(ii:ii+8),'(I4,1X,A3)') delList(iProb,iPar),'GSP'
          ii = ii + 10
        ENDIF
      ENDDO

      IF(ii > 73) THEN
        WRITE(line(1)(ii:),'(A)') 'Reason'
        WRITE(line(2)(ii:),'(A)') TRIM(staInfo%staProb(iProb)%remark)
      ELSE
        WRITE(line(1)(73:),'(A)') 'Reason'
        WRITE(line(2)(73:),'(A)') TRIM(staInfo%staProb(iProb)%remark)
      ENDIF

      ! Put the new line into the output buffer
      IF (ii > 43) THEN
        ! Extent the buffer if necessary
        IF (nLines == SIZE(lines)) THEN
          ALLOCATE(hlpLin(nLines),stat=iac)
          CALL alcerr(iac,'hlpLin',(/nLines/),srName)
          hlpLin = lines
          DEALLOCATE(lines,stat=iac)
          ALLOCATE(lines(nLines+20),stat=iac)
          CALL alcerr(iac,'lines',(/nLines+20/),srName)
          lines(1:nLines) = hlpLin(1:nLines)
          DEALLOCATE(hlpLin)
        ENDIF

        nLines = nLines+1
        lines(nLines) = line(2)
      ENDIF
    ENDDO

    ! Last file: Write the buffer into the program output
    IF (nLines > 0 .AND. iFil == SIZE(opt%neqFileName)) THEN
      WRITE(lfnprt,'(////,A,11X,A,/,A,/)')                         &
      ' DELETING STATIONS WITH PROBLEMS:', TRIM(opt%stacrux),             &
      ' -------------------------------'
      WRITE(lfnprt,'(A)') TRIM(line(1))
      WRITE(lfnprt,'(A)') &
      ' -------------------------------------------------------------' // &
      '----------------------------------------------------------------------'

      DO iProb = 1,nLines
        IF (iProb == 1) THEN
          WRITE(lfnprt,*)
        ELSE IF (lines(iProb)(1:4) /= lines(iProb-1)(1:4)) THEN
          WRITE(lfnprt,*)
        ENDIF
        WRITE(lfnprt,'(A)') TRIM(lines(iProb))
      ENDDO

      ! Writing once is enough
      DEALLOCATE(lines,stat=iac)
      nLines = -1
    ENDIF
  ENDIF

  IF (np1 == 0) RETURN

! After preelimination the NEQ is empty
! -------------------------------------
  IF (np0 == 0 .AND. iPart == 1) THEN
    WRITE(lfnerr,'(/,A,2(/,18X,A),/)')                                     &
    ' ### SR PARELIMI: You try to preeliminate all parameters from the',   &
                      'normal equation file. Are you sure?',               &
                      'File name:  '//TRIM(opt%neqFileName(iFil))
  ENDIF

! --------------------------------------------------------------------
! Old altorithm
! --------------------------------------------------------------------
  IF ( alt ) THEN
!!!    CALL cpu_now('old redtrb2 started')

  ! Allocate the memory
  ! -------------------
    npA = np0
    IF (np0 == 0) npA = 1    ! Have at least a size of 1 for the array

    ALLOCATE( A01(npA,np1), stat=iac )
    CALL alcerr(iac, 'A01', (/npA,np1/), srName)

    ALLOCATE( A11(np1,np1), stat=iac )
    CALL alcerr(iac, 'A11', (/np1,np1/), srName)

    ALLOCATE( parflg(np1), stat=iac )
    CALL alcerr(iac, 'parflg', (/np1/), srName)

    ALLOCATE( A11h(np1*(np1+1)/2), stat=iac )
    CALL alcerr(iac, 'A11h', (/np1*(np1+1)/2/), srName)


  ! Create two auxiliary matrices
  ! -----------------------------
    DO ipar1 = 1, neq%misc%npar
      DO ipar0 = 1, ipar1

        IF ( idel(ipar0) == 1 .AND. idel(ipar1) == 1) THEN
          ip0 = crsp(ipar0)
          ip1 = crsp(ipar1)
          A11h( ikf(ip0,ip1) ) = neq%aNor( ikf(ipar0,ipar1) )
        ELSE IF ( idel(ipar0) == 0 .AND. idel(ipar1) == 1 ) THEN
          ip0 = crsp(ipar0)
          ip1 = crsp(ipar1)
          IF (np0 > 0) A01(ip0,ip1) = neq%aNor( ikf(ipar0,ipar1) )
        ELSE IF ( idel(ipar0) == 1 .AND. idel(ipar1) == 0 ) THEN
          ip0 = crsp(ipar1)
          ip1 = crsp(ipar0)
          IF (np0 > 0) A01(ip0,ip1) = neq%aNor( ikf(ipar0,ipar1) )
        END IF

      END DO
    END DO

    CALL syminvg(np1,A11h,0,nsing,parflg)

  ! count number of preeliminated singular parameters
    IF (ipart /= 0 .AND. nsing > 0) THEN

      DO ipar = 1, neq%misc%npar

        IF (idel(ipar) == 1) THEN
          IF (parflg(crsp(ipar)) > 0) THEN
            CALL prparlst(1,ityp+4,iFil,neq%par(iPar)%name,parLocq(iPar,1), &
                          neq%par(iPar)%time)
  ! reduce nparms
            neq%misc%nparms = neq%misc%nparms - 1d0
          ENDIF
        ENDIF

      ENDDO
    ENDIF

  ! copy A11 matrix to quadratic form
    DO jp = 1, np1
      jpp = jp*(jp-1)/2
      DO ip = 1, jp
        ik = jpp + ip
        A11( ip, jp ) = A11h( ik )
        A11( jp, ip ) = A11h( ik )
      ENDDO
    ENDDO
    DEALLOCATE( A11h, stat=iac )

    CALL redtrb2(neq%misc%npar,np0,np1,A01(:,:),A11(:,:),idel(:), &
                 neq%aNor(:),neq%bNor(:),neq%misc%lTPl)

    DEALLOCATE( parflg, stat=iac )
    DEALLOCATE( A01, stat=iac )
    DEALLOCATE( A11, stat=iac )
!!!    CALL cpu_now('old redtrb2 finished')

! --------------------------------------------------------------------
! New altorithm
! --------------------------------------------------------------------
  ELSE
!!!    CALL cpu_now('new redsym started')


    ALLOCATE( parflg(neq%misc%npar), stat=iac )
    CALL alcerr(iac, 'parflg', (/neq%misc%npar/), srName)

! perform parameter transformation for parameters to be pre-eliminated
! in such a way that diagonal element of neq-matrix becomes 1
! --------------------------------------------------------------------
    DO ii = 1, neq%misc%npar
      neq%bNor(ii)=scl(ii)*neq%bNor(ii)
      bNor_save(ii)=neq%bNor(ii)
      if(idel(ii) == 0)neq%bNor(ii)=0.d0
      ik_0 = ii*(ii-1)/2
      DO kk = 1, ii
        ik = ik_0+kk
        neq%anor(ik) = neq%anor(ik) * scl(ii) * scl(kk)
        if(kk == ii .AND. idel(ii) > 0)neq%anor(ik) = 1.d0
      ENDDO
    ENDDO

! Perform pre-elimination and
! Check for negative diagonal terms
! ---------------------------------
    CALL redsym(neq%misc%npar,idel(:),neq%aNor(:), &
                nsing,parflg,neq%misc%lTPl,neq%bNor(:),bigajj_limit)

    DO ii = 1,neq%misc%npar
      IF(idel(ii) == 0)THEN
        neq%bNor(ii)=neq%bNor(ii)+bNor_save(ii)
        ik=ii*(ii+1)/2
        IF(neq%aNor(ik) < 0.d0 )THEN
!          write(lfnerr,'(/,A,I8,/)') &
!          ' ### SR PARELIMI: Negative diagonal term in reduced NEQs, i=',ii
          WRITE(*,*)' ### SR PARELIMI: Negative diagonal term in reduced NEQs, i=',ii
          WRITE(*,*) neq%par(ii)%locq(1),';', neq%par(ii)%locq(2)
          WRITE(*,*) neq%par(ii)%locq(3),';', neq%par(ii)%locq(4)
          WRITE(*,*) neq%par(ii)%locq(5),';', neq%par(ii)%locq(6)
          WRITE(*,*) neq%par(ii)%locq(7),';', neq%aNor(ik)
          WRITE(*,*) 'iPart = ',ipart,'; iFil = ', ifil
          neq%aNor(ik) = 0.d0

        ENDIF
      ENDIF
    ENDDO

! count number of preeliminated singular parameters
! -------------------------------------------------
    IF (ipart /= 0 .AND. nsing > 0) THEN

      DO ipar = 1, neq%misc%npar
        IF (idel(ipar) > 0) THEN
          IF (parflg(crsp(ipar)) > 0) THEN

            CALL prparlst(1,ipart+5,iFil,neq%par(iPar)%name,parLocq(iPar,1), &
                          neq%par(iPar)%time)

! reduce nparms (number of singular parameters)
            neq%misc%nparms = neq%misc%nparms - 1d0
          ENDIF
        ENDIF
      ENDDO
    ENDIF
    DEALLOCATE( parflg, stat=iac )

!!!    CALL cpu_now('new redsym finished')

  ENDIF

  comstat%elimi = comstat%elimi + np1

END SUBROUTINE parelimi

END MODULE
