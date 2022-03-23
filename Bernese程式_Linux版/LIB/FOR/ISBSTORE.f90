MODULE s_ISBSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE isbstore(neq)

! -------------------------------------------------------------------------
! Purpose:    Save ISB results computed by ADDNEQ2
!
! Author:     R. Dach
!
! Created:    21-Nov-2009
!
! Changes:    17-May-2011 LM: Arrays in t_isbsta: ALLOCATE->POINTER
!             28-Apr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright:  Astronomical Institute
!              University of Berne
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, staNameLength
  USE m_time,   ONLY: OPERATOR(.isIn.)
  USE d_neq,    ONLY: t_neq
  USE d_isbFil, ONLY: t_isbFil, writeIsb
  USE p_addneq, ONLY: opt,comstat
  USE d_const,  ONLY: filtitle,C

  USE f_ikf
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq) :: neq


! Local types
! -----------
  TYPE t_isbSta
    CHARACTER(LEN=staNameLength)                  :: name
    CHARACTER(LEN=20)                             :: recTyp
    REAL(r8b),       DIMENSION(:),   POINTER      :: isbVal
    REAL(r8b),       DIMENSION(:),   POINTER      :: isbRef
    INTEGER(i4b)                                  :: nRef
    INTEGER(i4b)                                  :: nPar
    REAL(r8b)                                     :: rmsVal
    REAL(r8b)                                     :: rmsRef
    REAL(r8b)                                     :: rmsDif
    REAL(r8b)                                     :: maxVal
    REAL(r8b)                                     :: maxRef
    REAL(r8b)                                     :: maxDif
  END TYPE t_isbSta


! Local parameter
! ---------------
  CHARACTER(LEN=8),  PARAMETER                    :: srName = 'isbstore'

! Local Variables
! ---------------
  TYPE(t_isbFil)                                  :: isbOut
  TYPE(t_isbSta),    DIMENSION(:),   ALLOCATABLE  :: isbSta

  CHARACTER(LEN=20)                               :: oldSta

  INTEGER(i4b)                                    :: iac
  INTEGER(i4b)                                    :: iPar
  INTEGER(i4b)                                    :: iSta,jSta, nSta
  INTEGER(i4b)                                    :: iIsb,nIsb
  INTEGER(i4b)                                    :: iIsb1,iIsb2
  INTEGER(I4b)                                    :: numIsb, numSta
  INTEGER(i4b)                                    :: iSnx

  REAL(r8b)                                       :: tmin, tmax
  REAL(r8b)                                       :: tdel
  REAL(r8b)                                       :: lstEpo


! ---------------------------
! Start with a ISB statistics
! ---------------------------

! Count number of stations and parameter per station
! --------------------------------------------------
  oldSta = ''
  numSta = 1
  numIsb = 0
  nIsb   = 0
  tmin   = 0d0
  tmax   = 0d0
  tdel   = 0d0
  lstEpo = 0d0

  DO iPar = 1,neq%misc%nPar

    IF ( neq%par(iPar)%locq(1) == 2 .AND. neq%par(iPar)%locq(6) == 5 ) THEN
      IF (oldSta == '') oldSta = neq%par(iPar)%name
      IF (oldSta == neq%par(iPar)%name) THEN
        nIsb = nIsb + 1
        IF (nIsb > numIsb) numIsb = nIsb

        IF ( lstEpo /= 0d0 .AND. &
            (tdel == 0d0 .OR. tdel > neq%par(iPar)%time%mean - lstEpo)) &
          tdel = neq%par(iPar)%time%mean - lstEpo
        lstEpo = neq%par(iPar)%time%mean
      ELSE
        numSta = numSta + 1
        oldSta = neq%par(iPar)%name
        nIsb = 0
        lstEpo = 0d0
      ENDIF

      IF (tmin == 0d0 .OR. tmin > neq%par(iPar)%time%mean) &
        tmin = neq%par(iPar)%time%mean
      IF (tmax == 0d0 .OR. tmax < neq%par(iPar)%time%mean) &
        tmax = neq%par(iPar)%time%mean
    ENDIF

  ENDDO

! No ISB-parameters in the file
! -----------------------------
  IF (numSta == 1) THEN
    RETURN
  ENDIF

  numIsb = INT((tmax-tmin) / tdel + 0.1d0/86400d0) + 1

! Allocate the statistics arrays
! ------------------------------
  ALLOCATE(isbSta(numSta),stat=iac)
  CALL alcerr(iac,'isbSta',(/numSta/), srName)

  DO iSta=1,numSta
    NULLIFY(isbSta(iSta)%isbRef)
    NULLIFY(isbSta(iSta)%isbVal)

    ALLOCATE(isbSta(iSta)%isbRef(numIsb),stat=iac)
    CALL alcerr(iac,'isbSta(iSta)%isbRef',(/numIsb/),srName)

    ALLOCATE(isbSta(iSta)%isbVal(numIsb),stat=iac)
    CALL alcerr(iac,'isbSta(iSta)%isbVal',(/numIsb/),srName)

    isbSta(iSta)%name   = ''
    isbSta(iSta)%recTyp = ''
    isbSta(iSta)%isbVal = 1d20
    isbSta(iSta)%isbRef = 0d0
    isbSta(iSta)%nRef   = 0
    isbSta(iSta)%nPar   = 0
    isbSta(iSta)%rmsVal = 0d0
    isbSta(iSta)%rmsRef = 0d0
    isbSta(iSta)%rmsDif = 0d0
    isbSta(iSta)%maxVal = 0d0
    isbSta(iSta)%maxRef = 0d0
    isbSta(iSta)%maxDif = 0d0
  ENDDO

! Extract the ISB from the parameters
! -----------------------------------
  iSta = 0
  DO iPar = 1,neq%misc%nPar
    IF ( neq%par(iPar)%locq(1) /= 2 .OR. neq%par(iPar)%locq(6) /= 5 ) CYCLE

    IF (iSta == 0) THEN
      iSta = iSta + 1
      isbSta(iSta)%name = neq%par(iPar)%name
      iIsb1 = 0
    ELSE IF (isbSta(iSta)%name /= neq%par(iPar)%name) THEN
      iSta = iSta + 1
      isbSta(iSta)%name = neq%par(iPar)%name
      iIsb1 = 0
    ENDIF

    iIsb2 = NINT((neq%par(iPar)%time%mean - tmin) / tdel) + 1
    IF (iIsb1 == 0) iIsb1 = iIsb2

    isbSta(iSta)%isbVal(iIsb2) = neq%par(iPar)%x0 + neq%xxx(iPar)
    DO iIsb = iIsb1+1,iIsb2-1
      isbSta(iSta)%isbVal(iIsb) = isbSta(iSta)%isbVal(iIsb1) + &
                  (isbSta(iSta)%isbVal(iIsb2) - isbSta(iSta)%isbVal(iIsb1)) / &
                  (iIsb2 - iIsb1) * (iIsb - iIsb1)
      isbSta(iSta)%nRef = isbSta(iSta)%nRef + 1
    ENDDO
    iIsb1 = iIsb2

    ! Update the statistics
    isbSta(iSta)%nPar = isbSta(iSta)%nPar + 1
    isbSta(iSta)%nRef = isbSta(iSta)%nRef + 1
    isbSta(iSta)%rmsVal = isbSta(iSta)%rmsVal + isbSta(iSta)%isbVal(iIsb1)**2
    IF (isbSta(iSta)%maxVal == 0d0 .OR. &
        DABS(isbSta(iSta)%maxVal) < DABS(isbSta(iSta)%isbVal(iIsb1))) THEN
      isbSta(iSta)%maxVal = isbSta(iSta)%isbVal(iIsb1)
    ENDIF
    IF (isbSta(iSta)%recTyp == '') THEN
      DO iSnx = 1,neq%misc%nstat_sinex
        IF (neq%misc%sinex(iSnx)%stName == isbSta(iSta)%name .AND. &
            ((tmax + tmin)/2d0 .isIn. neq%misc%sinex(iSnx)%timint) ) THEN
          isbSta(iSta)%recTyp = neq%misc%sinex(iSnx)%antRec
        ENDIF
      ENDDO
    ENDIF
  ENDDO


  DO iIsb = 1,numIsb
    nSta = 0
    DO iSta = 1,numSta
      IF (isbSta(iSta)%isbVal(iIsb) /= 1d20) THEN
        nSta = nSta + 1
        DO jSta = 1,numSta
          IF (isbSta(jSta)%isbVal(iIsb) == 1d20) CYCLE
          isbSta(iSta)%rmsDif = isbSta(iSta)%rmsDif + &
                (isbSta(jSta)%isbVal(iIsb)-isbSta(iSta)%isbVal(iIsb))**2
          IF (isbSta(iSta)%maxDif == 0 .OR. &
              DABS(isbSta(iSta)%maxDif) < &
                  DABS(isbSta(jSta)%isbVal(iIsb)-isbSta(iSta)%isbVal(iIsb))) &
            isbSta(iSta)%maxDif = &
                          isbSta(jSta)%isbVal(iIsb)-isbSta(iSta)%isbVal(iIsb)
        ENDDO
      ENDIF
    ENDDO
    DO iSta = 1,numSta
      DO jSta = iSta+1,numSta
        IF ( isbSta(iSta)%isbVal(iIsb) == 1d20 ) CYCLE
        IF ( isbSta(jSta)%isbVal(iIsb) == 1d20 ) CYCLE
        isbSta(iSta)%isbRef(iIsb) = isbSta(iSta)%isbRef(iIsb) + &
              ( isbSta(iSta)%isbVal(iIsb) - isbSta(jSta)%isbVal(iIsb) )/nSta
        isbSta(jSta)%isbRef(iIsb) = isbSta(jSta)%isbRef(iIsb) + &
              ( isbSta(jSta)%isbVal(iIsb) - isbSta(iSta)%isbVal(iIsb) )/nSta
      ENDDO
    ENDDO
  ENDDO

  DO iSta = 1,numSta
    isbSta(iSta)%rmsRef = 0d0
    DO iIsb = 1,numIsb
      isbSta(iSta)%rmsRef = isbSta(iSta)%rmsRef + &
                         isbSta(iSta)%isbRef(iIsb)**2
      IF (isbSta(iSta)%maxRef == 0d0 .OR. &
          DABS(isbSta(iSta)%maxRef) < DABS(isbSta(iSta)%isbRef(iIsb))) THEN
        isbSta(iSta)%maxRef = isbSta(iSta)%isbRef(iIsb)
      ENDIF
    ENDDO
    isbSta(iSta)%rmsRef = DSQRT(isbSta(iSta)%rmsRef/(isbSta(iSta)%nRef-1))
    isbSta(iSta)%rmsVal = DSQRT(isbSta(iSta)%rmsVal/(isbSta(iSta)%nPar-1))
    isbSta(iSta)%rmsDif = DSQRT(isbSta(iSta)%rmsDif/(isbSta(iSta)%nRef-2))
  ENDDO

! Write the statistics part to the program output
! -----------------------------------------------
  WRITE(lfnprt,'(//,A,/,A,//,A,/,A,/,1X,131("-")) ')                    &
  ' STATISTICS ON INTER-SYSTEM BIASES',                                 &
  ' ---------------------------------',                                 &
  '                              Redefined reference   Station as' //   &
  ' reference   Original reference',                                                          &
  ' Station name          #Par    Rms (ns)  Max (ns)    Rms (ns) ' //   &
  ' Max (ns)    Rms (ns)  Max (ns)   Receiver type'

  DO iSta = 1,numSta
    WRITE(lfnprt,'(1X,A,4X,3X,I3,3(2X,2F10.4),3X,A)') &
          isbSta(iSta)%name,isbSta(iSta)%nPar, &
          isbSta(iSta)%rmsRef/C*1d9,isbSta(iSta)%maxRef/C*1D9, &
          isbSta(iSta)%rmsDif/C*1d9,isbSta(iSta)%maxDif/C*1D9, &
          isbSta(iSta)%rmsVal/C*1d9,isbSta(iSta)%maxVal/C*1D9, &
          TRIM(isbSta(iSta)%recTyp)
  ENDDO


! Clean up the memory
! -------------------
  DO iSta=1,numSta
    DEALLOCATE(isbSta(iSta)%isbRef)
    DEALLOCATE(isbSta(iSta)%isbVal)
  ENDDO
  DEALLOCATE(isbSta,stat=iac)


! Store the results
! -----------------
  IF (opt%isbout == '') RETURN

! Allocate the structure to store the results
! -------------------------------------------
  ALLOCATE(isbOut%isbSta(numSta),stat=iac)
  CALL alcerr(iac,'isbFil%isbSta',(/numSta/),srName)

  isbOut%filnam = opt%isbout
  isbOut%title  = filtitle
  isbOut%nSta   = 0

! Copy the results into the structure
! -----------------------------------
  DO iPar = 1,neq%misc%nPar
    IF (neq%par(iPar)%locq(1) /= 2 .OR. neq%par(iPar)%locq(6) /= 5) CYCLE

    ! Find the station
    jSta = 0
    DO iSta = 1,isbOut%nSta
      IF (isbOut%isbSta(iSta)%staNam == neq%par(iPar)%name) THEN
        jSta = iSta
        EXIT
      ENDIF
    ENDDO

    ! A new station
    IF (jSta == 0) THEN
      isbOut%nSta = isbOut%nSta + 1
      jSta = isbOut%nSta
      ALLOCATE(isbOut%isbSta(jSta)%isbRec(numIsb),stat=iac)
      CALL alcerr(iac,'isbOut%isbSta(jSta)%isbRec',(/numIsb/),srName)

      isbOut%isbSta(jSta)%staNam = neq%par(iPar)%name
      isbOut%isbSta(jSta)%nIsb   = 0
    ENDIF

    ! Add the new value to the output record
    ! --------------------------------------
    isbOut%isbSta(jSta)%nIsb = isbOut%isbSta(jSta)%nIsb + 1
    iIsb = isbOut%isbSta(jSta)%nIsb

    isbOut%isbSta(jSta)%isbRec(iIsb)%epoch  = neq%par(iPar)%time%mean
    isbOut%isbSta(jSta)%isbRec(iIsb)%isb    = &
                              (neq%par(iPar)%x0 + neq%xxx(iPar))/C*1D9
    isbOut%isbSta(jSta)%isbRec(iIsb)%rms    = &
                              comstat%rms*DSQRT(neq%aNor(ikf(iPar,iPar)))/C*1D9
    isbOut%isbSta(jSta)%isbRec(iIsb)%iCarr  = neq%par(iPar)%locq(3)
  ENDDO

! Write the data into the file
! ----------------------------
  CALL writeIsb(isbOut)

! Clean up the memory
! -------------------
  DO iSta = 1,isbOut%nSta
    DEALLOCATE(isbOut%isbSta(iSta)%isbRec,stat=iac)
  ENDDO
  DEALLOCATE(isbOut%isbSta,stat=iac)

END SUBROUTINE isbstore


END MODULE
