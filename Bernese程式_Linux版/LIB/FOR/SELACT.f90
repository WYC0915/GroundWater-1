MODULE s_SELACT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE selact(master,second,miss,ct01,ct02,ct03,ct04,ct05,crant, &
                    crrec,cconsid)

! -------------------------------------------------------------------------
! Purpose:    Searches in all sections for different entries between
!             master and secondary station files applying action options
!             in panel 2 of program STAMERGE
!
! Author:     A.Steinbach
!
! Created:    13-Nov-2007
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY
!             02-Feb-2011 SL: stacrx version 1.01
!             31-Aug-2011 SL: do loops for section 002 corrected
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Remove unused variables, modules and parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,r8b,lfnPrt
  USE d_stacrx, ONLY: t_stacrux

  USE s_alcerr
  USE s_timst2
  USE s_redser

  IMPLICIT NONE

! List of parameters
! ------------------
! IN/OUT:
  TYPE(t_stacrux)                 :: miss    ! missing entries in master file

! IN
  TYPE(t_stacrux)                 :: master,second
  INTEGER(i4b)                    :: ct01    ! return values for combo box
  INTEGER(i4b)                    :: ct02
  INTEGER(i4b)                    :: ct03
  INTEGER(i4b)                    :: ct04
  INTEGER(i4b)                    :: ct05
  INTEGER(i4b)                    :: crant
  INTEGER(i4b)                    :: crrec
  INTEGER(i4b)                    :: cconsid

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b)                            :: iac
  INTEGER(i4b)                            :: i,j,kk
  INTEGER(i4b)                            :: diff,na
  REAL(r8b)                               :: tol
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE   :: idx1,idx2,idx3,idx4,idx5

  CHARACTER(LEN=6),PARAMETER              :: pgmNam = "SELACT"
  CHARACTER(LEN=40)                       :: time

! --------------------
! Apply action options
! --------------------
! Find missing stations in master file, section 001
! -------------------------------------------------
  IF(ct01 == 1.OR.ct01 == 2) THEN
    diff = 0
    ALLOCATE(idx1(second%nrenam),stat=iac)
    CALL alcerr(iac,'idx1',(/second%nrenam/),pgmNam)
    idx1 = 0  ! 0: sta ist nur im sec.-file
              ! 1: gleicher record gefunden
              ! 2: sta ist auch im mst.-file
              ! 3. wie 2 aber schon rapportiert
    DO i=1,second%nrenam
      DO j=1,master%nrenam
        IF(second%renamsta(i)%stanam == master%renamsta(j)%stanam) THEN
          idx1(i) = 2
        ENDIF
        IF(second%renamsta(i)%stanam == master%renamsta(j)%stanam .AND. &
            second%renamsta(i)%flg   == master%renamsta(j)%flg    .AND. &
            ((master%renamsta(j)%timint%t(1) /= 0d0  .AND. &
              master%renamsta(j)%timint%t(2) == 1d20 .AND. &
              second%renamsta(i)%timint%t(1) == 0d0  .AND. &
              second%renamsta(i)%timint%t(2) == 1d20) .OR. &
!             (master%renamsta(j)%timint%t(1) <= 54035.00000 .AND. &
!              master%renamsta(j)%timint%t(2) == 1d20 .AND. &
!              second%renamsta(i)%timint%t(1) <= 54035.00000 .AND. &
!              second%renamsta(i)%timint%t(2) == 1d20) .OR. &
             (second%renamsta(i)%timint%t(1) == &
              master%renamsta(j)%timint%t(1) .AND. &
              second%renamsta(i)%timint%t(2) == &
              master%renamsta(j)%timint%t(2))) .AND. &
            (second%renamsta(i)%oldnam(1:4)  == &
             master%renamsta(j)%oldnam(1:4) .AND. &
             second%renamsta(i)%oldnam(6:16) == &
             master%renamsta(j)%oldnam(6:16))) THEN
!            second%renamsta(i)%oldnam == master%renamsta(j)%oldnam) THEN
          idx1(i) = 1
          EXIT
        ENDIF
      ENDDO
    ENDDO

    DO j=1,master%nrenam
      miss%nrenam = miss%nrenam+1
      miss%renamsta(miss%nrenam) = master%renamsta(j)
    ENDDO

!   Find different entries to same station names, section 001
!   ---------------------------------------------------------
    na = 0
    DO i = 1,second%nrenam
      IF(idx1(i) == 0.AND.(ct01 == 1.OR.ct01 == 2)) THEN
        na = 1
        IF(idx1(i) == 0.AND.ct01 == 2) THEN
          miss%nrenam = miss%nrenam+1
          miss%renamsta(miss%nrenam) = second%renamsta(i)
        ENDIF
      ENDIF
      IF(idx1(i) == 2) THEN
        diff = diff+1
        IF(diff <= 1) THEN
          WRITE(LFNPRT,'(A,/,A,/,A)') &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------',   &
          ' Different entries in section: Type 001   ',                  &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------'
        ENDIF
        DO j = 1,master%nrenam
          IF(second%renamsta(i)%stanam /= master%renamsta(j)%stanam) CYCLE
            CALL timst2(2,2,master%renamsta(j)%timint%t(1:2),time)
            WRITE(LFNPRT,'(1X,A,2X,A16,4x,2X,A3,2X,A40,2X,A16)')          &
                 'M',master%renamsta(j)%stanam,master%renamsta(j)%flg,time, &
                 master%renamsta(j)%oldnam
        ENDDO

        DO j = 1,second%nrenam
          IF(second%renamsta(i)%stanam /= second%renamsta(j)%stanam) CYCLE
          idx1(j) = 3
          CALL timst2(2,2,second%renamsta(j)%timint%t(1:2),time)
          WRITE(LFNPRT,'(1X,A,2X,A16,4x,2X,A3,2X,A40,2X,A16)')         &
               'S',second%renamsta(j)%stanam,second%renamsta(j)%flg,time, &
               second%renamsta(j)%oldnam
        ENDDO
        WRITE(LFNPRT,'(/)')
      ENDIF
    ENDDO

    IF(diff == 0) THEN
      WRITE(LFNPRT,'(A,/,A,/,A)')                                             &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------',   &
      ' No differences found in section: Type 001  ',                         &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------'
    ENDIF

    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'

    IF(ct01 == 1.AND.na == 1) THEN
      WRITE(LFNPRT,'(A)') ' Stations missing in section: Type 001   '
    ELSEIF (ct01 == 2 .AND. na == 1) THEN
      WRITE(LFNPRT,'(A)') ' Stations merged in master file at record: Type 001 '
    ELSEIF (na == 0) THEN
      WRITE(LFNPRT,'(A)') ' No missing stations found in section: Type 001 '
    ENDIF

    WRITE(LFNPRT,'(A)')                                                     &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    DO i = 1, second%nrenam
      IF(idx1(i) == 0.AND.ct01 /= 3) THEN
        DO j = 1,second%nrenam
          IF(second%renamsta(i)%stanam /= second%renamsta(j)%stanam) CYCLE
            idx1(j) = 3
            idx1(i) = idx1(i)+1
        ENDDO
        WRITE(LFNPRT,'(A,I4,A,A)') &
        ' Record ',i,              &
        ' missing in master file: ',second%renamsta(i)%stanam
        idx1(i) = 3
      ENDIF
    ENDDO
    WRITE(LFNPRT,'(/)')

    DEALLOCATE(idx1, stat=iac)
  ENDIF

  IF(ct01 == 3) THEN
    WRITE(LFNPRT,'(A,/,A,/,A,/)')                                           &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------',   &
    ' Entries for section 001 deleted   ',                                  &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
  ENDIF

! Find missing stations in master file, section 002
! -------------------------------------------------
  tol = 1.0
  IF(ct02 == 1.OR.ct02 == 2) THEN
    diff = 0
    IF(crant == 1.OR.crrec == 1) THEN
       CALL redser(crrec,crant,master)
       CALL redser(crrec,crant,second)
    ENDIF
    ALLOCATE(idx2(second%ninfo),stat=iac)
    CALL alcerr(iac,'idx2',(/second%ninfo/),pgmNam)
    idx2 = 0

! Filter for those infos which are newer than the oldest entry of second file
! ---------------------------------------------------------------------------
    DO i=2, second%ninfo
      IF(second%stainfo(i)%stanam /= second%stainfo(i-1)%stanam) THEN
        DO j=1, master%ninfo
          IF(master%stainfo(j)%stanam == second%stainfo(i)%stanam .AND. &
             ((master%stainfo(j)%timint%t(1) /= 0d0 .AND. &
               master%stainfo(j)%timint%t(1) <= &
               second%stainfo(i)%timint%t(1) - tol) .OR. &
              (master%stainfo(j)%timint%t(2) /= 1d20 .AND. &
               master%stainfo(j)%timint%t(2) <= &
               second%stainfo(i)%timint%t(2) - tol))) THEN
            master%stainfo(j)%staNam = ' '
            CYCLE
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    j = 0
    DO i=1,master%ninfo
      IF(master%stainfo(i)%staNam == ' ') CYCLE
      j = j+1
      IF(j /= i) master%stainfo(j) = master%stainfo(i)
    ENDDO
    master%ninfo = j

! Replace per station the first entry in master file by 0d0
! ---------------------------------------------------------
    DO i=2,second%ninfo
    exit
      IF(second%stainfo(i)%stanam /= second%stainfo(i-1)%stanam) THEN
        DO j=1, master%ninfo
          IF(master%stainfo(j)%stanam == second%stainfo(i)%stanam .AND. &
             (master%stainfo(j)%timint%t(1) /= 0d0  .AND. &
              second%stainfo(i)%timint%t(1) == 0d0  .AND. &
              master%stainfo(j)%timint%t(2) /= 1d20 .AND. &
              master%stainfo(j)%timint%t(2) >= &
              second%stainfo(i)%timint%t(2) - tol .OR. &
              master%stainfo(j)%timint%t(2) <= &
              second%stainfo(i)%timint%t(2) + tol)) THEN
            master%stainfo(j)%timint%t(1) = 0d0
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO

! Find different entries to same station names, section 002
! ---------------------------------------------------------
    DO i=1,second%ninfo
      DO j=1,master%ninfo
        IF(second%stainfo(i)%stanam == master%stainfo(j)%stanam) THEN
          idx2(i) = 2
        ENDIF
        IF (second%stainfo(i)%stanam == master%stainfo(j)%stanam .AND. &
            second%stainfo(i)%flg    == master%stainfo(j)%flg    .AND. &

           ((second%stainfo(i)%timint%t(1) == 0d0    .AND. &
             second%stainfo(i)%timint%t(2) == 1d20   .AND. &
             master%stainfo(j)%timint%t(1) == 0d0    .AND. &
             master%stainfo(j)%timint%t(2) == 1d20)   .OR. &

            (second%stainfo(i)%timint%t(1) /= 0d0    .AND. &
             master%stainfo(j)%timint%t(1) /= 0d0    .AND. &
             second%stainfo(i)%timint%t(2) /= 1d20   .AND. &
             master%stainfo(j)%timint%t(2) /= 1d20   .AND. &
            (second%stainfo(i)%timint%t(1) <= &
             master%stainfo(j)%timint%t(1) + tol     .AND. &
             second%stainfo(i)%timint%t(1) >= &
             master%stainfo(j)%timint%t(1) - tol)    .AND. &
            (second%stainfo(i)%timint%t(2) <= &
             master%stainfo(j)%timint%t(2) + tol     .AND. &
             second%stainfo(i)%timint%t(2) >= &
             master%stainfo(j)%timint%t(2) - tol))    .OR. &

            (second%stainfo(i)%timint%t(1) == 0d0    .AND. &
             master%stainfo(j)%timint%t(1) == 0d0    .AND. &
             second%stainfo(i)%timint%t(2) /= 1d20   .AND. &
             master%stainfo(j)%timint%t(2) /= 1d20   .AND. &
             second%stainfo(i)%timint%t(2) <= &
             master%stainfo(j)%timint%t(2) + tol     .AND. &
             second%stainfo(i)%timint%t(2) >= &
             master%stainfo(j)%timint%t(2) - tol)     .OR. &

            (second%stainfo(i)%timint%t(1) /= 0d0    .AND. &
             master%stainfo(j)%timint%t(1) /= 0d0    .AND. &
             second%stainfo(i)%timint%t(2) == 1d20   .AND. &
             master%stainfo(j)%timint%t(2) == 1d20   .AND. &
             second%stainfo(i)%timint%t(1) <= &
             master%stainfo(j)%timint%t(1) + tol     .AND. &
             second%stainfo(i)%timint%t(1) >= &
             master%stainfo(j)%timint%t(1) - tol))   .AND. &

             second%stainfo(i)%recnam    == master%stainfo(j)%recnam    .AND. &
             second%stainfo(i)%antnam    == master%stainfo(j)%antnam    .AND. &
             second%stainfo(i)%recnum    == master%stainfo(j)%recnum    .AND. &
             second%stainfo(i)%antnum    == master%stainfo(j)%antnum    .AND. &
             second%stainfo(i)%antecc(1) == master%stainfo(j)%antecc(1) .AND. &
             second%stainfo(i)%antecc(2) == master%stainfo(j)%antecc(2) .AND. &
             second%stainfo(i)%antecc(3) == master%stainfo(j)%antecc(3)) THEN
          CALL timst2(2,2,second%stainfo(i)%timint%t(1:2),time)
          idx2(i) = 1
          EXIT
        ENDIF
        IF(master%stainfo(j)%timint%t(1) == 0d0 .AND.  &
           second%stainfo(i)%timint%t(1) /= 0d0 .AND. &
           second%stainfo(i)%timint%t(2) <= master%stainfo(j)%timint%t(2) + tol  .AND. &
           second%stainfo(i)%timint%t(2) >= master%stainfo(j)%timint%t(2) - tol  .AND. &
           second%stainfo(i)%stanam        == master%stainfo(j)%stanam      .AND. &
           second%stainfo(i)%flg           == master%stainfo(j)%flg         .AND. &
           second%stainfo(i)%recnam        == master%stainfo(j)%recnam      .AND. &
           second%stainfo(i)%antnam        == master%stainfo(j)%antnam      .AND. &
           second%stainfo(i)%recnum        == master%stainfo(j)%recnum      .AND. &
           second%stainfo(i)%antnum        == master%stainfo(j)%antnum      .AND. &
           second%stainfo(i)%antecc(1)     == master%stainfo(j)%antecc(1)   .AND. &
           second%stainfo(i)%antecc(2)     == master%stainfo(j)%antecc(2)   .AND. &
           second%stainfo(i)%antecc(3)     == master%stainfo(j)%antecc(3)) THEN
          idx2(i) = 1
          EXIT
        ENDIF
        IF(second%stainfo(i)%stanam        == master%stainfo(j)%stanam       .AND. &
           second%stainfo(i+1)%stanam        == master%stainfo(j+1)%stanam   .AND. &
           second%stainfo(i)%flg           == master%stainfo(j)%flg          .AND. &
           second%stainfo(i+1)%flg           == master%stainfo(j+1)%flg      .AND. &

           (second%stainfo(i)%timint%t(2) <= master%stainfo(j+1)%timint%t(1) + tol   .AND. &
            second%stainfo(i)%timint%t(2) >= master%stainfo(j+1)%timint%t(1) - tol   .AND. &

            second%stainfo(i+1)%timint%t(1) >= master%stainfo(j+1)%timint%t(1) - tol .AND. &
            second%stainfo(i+1)%timint%t(1) <= master%stainfo(j+1)%timint%t(1) + tol .OR. &

            master%stainfo(j)%timint%t(1) == 0d0    .AND. &
            second%stainfo(i)%timint%t(1) /= 0d0)   .AND. &

           second%stainfo(i)%recnam        == master%stainfo(j)%recnam      .AND. &
           second%stainfo(i+1)%recnam        == master%stainfo(j+1)%recnam      .AND. &
           second%stainfo(i)%antnam        == master%stainfo(j)%antnam      .AND. &
           second%stainfo(i+1)%antnam        == master%stainfo(j+1)%antnam      .AND. &
           second%stainfo(i)%recnum        == master%stainfo(j)%recnum      .AND. &
           second%stainfo(i+1)%recnum        == master%stainfo(j+1)%recnum      .AND. &
           second%stainfo(i)%antnum        == master%stainfo(j)%antnum      .AND. &
           second%stainfo(i+1)%antnum        == master%stainfo(j+1)%antnum      .AND. &
           second%stainfo(i)%antecc(1)     == master%stainfo(j)%antecc(1)   .AND. &
           second%stainfo(i+1)%antecc(1)     == master%stainfo(j+1)%antecc(1)   .AND. &
           second%stainfo(i)%antecc(2)     == master%stainfo(j)%antecc(2)   .AND. &
           second%stainfo(i+1)%antecc(2)     == master%stainfo(j+1)%antecc(2)   .AND. &
           second%stainfo(i)%antecc(3)     == master%stainfo(j)%antecc(3)   .AND. &
           second%stainfo(i+1)%antecc(3)     == master%stainfo(j+1)%antecc(3)) THEN
          idx2(i) = 1
          EXIT
        ENDIF
        IF(second%stainfo(i)%stanam        == master%stainfo(j)%stanam       .AND. &
           second%stainfo(i+1)%stanam        == master%stainfo(j)%stanam     .AND. &
           second%stainfo(i+1)%flg           == master%stainfo(j)%flg        .AND. &

           (second%stainfo(i)%timint%t(2) <= master%stainfo(j)%timint%t(1)   .AND. &
            second%stainfo(i+1)%timint%t(1) == master%stainfo(j)%timint%t(1) .AND. &
            master%stainfo(j)%timint%t(1) /= 0d0)                             .AND. &
!            second%stainfo(i)%timint%t(1) == 0d0)                            .AND.&

           second%stainfo(i+1)%recnam        == master%stainfo(j)%recnam    .AND. &
           second%stainfo(i+1)%antnam        == master%stainfo(j)%antnam    .AND. &
           second%stainfo(i+1)%recnum        == master%stainfo(j)%recnum    .AND. &
           second%stainfo(i+1)%antnum        == master%stainfo(j)%antnum    .AND. &
           second%stainfo(i+1)%antecc(1)     == master%stainfo(j)%antecc(1) .AND. &
           second%stainfo(i+1)%antecc(2)     == master%stainfo(j)%antecc(2) .AND. &
           second%stainfo(i+1)%antecc(3)     == master%stainfo(j)%antecc(3)) THEN
          idx2(i) = 1
          EXIT
        ENDIF
      ENDDO
    ENDDO

    DO j=1, master%ninfo
      miss%ninfo = miss%ninfo+1
      miss%stainfo(miss%ninfo) = master%stainfo(j)
    ENDDO

!   Find different entries to same station names, section 002
!   ---------------------------------------------------------
    na = 0
    DO i = 1, second%ninfo
      IF(idx2(i) == 0.AND.(ct02 == 1.OR.ct02 == 2)) THEN
        na = 1
        IF(idx2(i) == 0 .AND. ct02 == 2) THEN
          miss%ninfo = miss%ninfo+1
          miss%stainfo(miss%ninfo) = second%stainfo(i)
        ENDIF
      ENDIF
      IF(idx2(i) == 2) THEN
        diff = diff + 1
        IF (diff <= 1) THEN
          WRITE(LFNPRT,'(A,/,A,/,A)') &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------',   &
          ' Different entries in section: Type 002   ', &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------'
        ENDIF
        DO j = 1,master%ninfo
          IF(second%stainfo(i)%stanam /= master%stainfo(j)%stanam) CYCLE
          CALL timst2(2,2,master%stainfo(j)%timint%t(1:2),time)
          WRITE (LFNPRT, &
             '(1X,A,2X,A16,4x,2X,A3,2X,A40,2X,2(A20,2X),2(I6,2X), 3(F8.4,2X),A22)') &
             'M', master%stainfo(j)%stanam, master%stainfo(j)%flg, time, &
             master%stainfo(j)%recnam, master%stainfo(j)%antnam,         &
             master%stainfo(j)%recnum, master%stainfo(j)%antnum,         &
             master%stainfo(j)%antecc(1),master%stainfo(j)%antecc(2),    &
             master%stainfo(j)%antecc(3),master%stainfo(j)%descri
        ENDDO

        DO j = 1,second%ninfo
          IF (second%stainfo(i)%stanam /= second%stainfo(j)%stanam) CYCLE
          CALL timst2(2,2,second%stainfo(j)%timint%t(1:2),time)
          WRITE (LFNPRT, &
             '(1X,A,2X,A16,4x,2X,A3,2X,A40,2X,2(A20,2X),2(I6,2X), 3(F8.4,2X),A22)') &
             'S', second%stainfo(j)%stanam, second%stainfo(j)%flg, time, &
             second%stainfo(j)%recnam, second%stainfo(j)%antnam,         &
             second%stainfo(j)%recnum, second%stainfo(j)%antnum,         &
             second%stainfo(j)%antecc(1),second%stainfo(j)%antecc(2),    &
             second%stainfo(j)%antecc(3),second%stainfo(j)%descri
          idx2(j)=3
        ENDDO
        WRITE(LFNPRT,'(/)')
      ENDIF
    ENDDO
    IF(diff == 0) THEN
      WRITE(LFNPRT,'(A,/,A,/,A)') &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------',   &
      ' No differences found in section: Type 002   ', &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------'
    ENDIF

    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    IF(ct02 == 1 .AND. na == 1) THEN
      WRITE(LFNPRT,'(A)')  ' Stations missing in section: Type 002   '
    ELSEIF (ct02 == 2 .AND. na == 1) THEN
      WRITE(LFNPRT,'(A)')  ' Stations merged in master file at record: Type 002 '
    ELSEIF (na == 0) THEN
      WRITE(LFNPRT,'(A)')  ' No missing stations found in section: Type 002 '
    ENDIF
    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    DO i = 1, second%ninfo
      IF (idx2(i) == 0) THEN
        DO j = 1,second%ninfo
          IF (second%stainfo(i)%stanam /= second%stainfo(j)%stanam) CYCLE
            idx2(j)=3
            idx2(i)=idx2(i)+1
        ENDDO
        WRITE(LFNPRT,'(A,I4,A,A)') ' Record ', i, '  missing in master file: ', &
                  second%stainfo(i)%stanam
        idx2(i)=3
      ENDIF
    ENDDO
    WRITE(LFNPRT,'(/)')

    DEALLOCATE(idx2, stat=iac)
  ENDIF

  IF (ct02 == 3) THEN
    WRITE(LFNPRT,'(A,/,A,/,A,/)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------',   &
    ' Entries for section 002 deleted   ',                  &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
  ENDIF


! -------------------------------------------------
! Find missing stations in master file, section 003
! -------------------------------------------------
  IF(ct03 == 1 .OR. ct03 == 2) THEN
    diff = 0
    IF(cconsid == 0) THEN
      DO i=1,second%nprob
        IF(second%staprob(i)%stanam(5:5) /= ' ') THEN
          second%staprob(i)%stanam = ' '
          CYCLE
        ENDIF
      ENDDO
      j = 0
      DO i=1,second%nprob
        IF(second%staprob(i)%stanam == ' ') CYCLE
        j = j + 1
        IF (j /= i) second%staprob(j) = second%staprob(i)
      ENDDO
      second%nprob = j

      DO i=1, master%nprob
        IF (master%staprob(i)%stanam(5:5) /= ' ') THEN
          master%staprob(i)%stanam = ' '
          CYCLE
        ENDIF
      ENDDO
      j = 0
      DO i=1,master%nprob
        IF (master%staprob(i)%stanam == ' ') CYCLE
        j = j + 1
        IF (j /= i) master%staprob(j) = master%staprob(i)
      ENDDO
      master%nprob = j
    ENDIF

    ALLOCATE(idx3(second%nprob),stat=iac)
    CALL alcerr(iac,'idx3',(/second%nprob/),pgmNam)
    idx3 = 0
    DO i=1, second%nprob
      DO j=1, master%nprob
        IF (second%staprob(i)%stanam      == master%staprob(j)%stanam .OR. &
            second%staprob(i)%stanam      /= ' ') THEN
          idx3(i) = 2
        ENDIF
        IF (second%staprob(i)%stanam      == master%staprob(j)%stanam .AND. &
            second%staprob(i)%flg         == master%staprob(j)%flg .AND. &
            second%staprob(i)%timint%t(1) == master%staprob(j)%timint%t(1) .AND. &
            second%staprob(i)%timint%t(2) == master%staprob(j)%timint%t(2)) THEN
          idx3(i) = 1
          EXIT
        ENDIF
      ENDDO
    ENDDO

    DO j=1, master%nprob
      miss%nprob = miss%nprob+1
      miss%staprob(miss%nprob) = master%staprob(j)
    ENDDO

!   Find different entries to same station names, section 003
!   ---------------------------------------------------------
    na = 0
    DO i = 1, second%nprob
      IF (idx3(i) == 0 .AND. (ct03 == 1 .OR. ct03 == 2)) THEN
        na = 1
        IF (idx3(i) == 0 .AND. ct03 == 2) THEN
          miss%nprob = miss%nprob+1
          miss%staprob(miss%nprob) = second%staprob(i)
        ENDIF
      ENDIF
      IF (idx3(i) == 2) THEN
        diff = diff + 1
        IF (diff <= 1 ) THEN
          WRITE(LFNPRT,'(A,/,A,/,A)') &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------',   &
          ' Different entries in section: Type 003   ',                  &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------'
        ENDIF
        DO j = 1,master%nprob
          IF (second%staprob(i)%stanam /= master%staprob(j)%stanam) CYCLE
          CALL timst2(2,2,master%staprob(j)%timint%t(1:2),time)
          WRITE(LFNPRT, '(1X,A,2X,A16,4x,2X,I3.3,2X,A40)')                 &
                'M', master%staprob(j)%stanam, master%staprob(j)%flg, time
        ENDDO

        DO j = 1,second%nprob
          IF (second%staprob(i)%stanam /= second%staprob(j)%stanam) CYCLE
          idx3(j)=3
          CALL timst2(2,2,second%staprob(j)%timint%t(1:2),time)
          WRITE(LFNPRT, '(1X,A,2X,A16,4x,2X,I3.3,2X,A40)')                 &
               'S', second%staprob(j)%stanam, second%staprob(j)%flg, time
        ENDDO
        WRITE(LFNPRT,'(/)')
      ENDIF
    ENDDO
    IF (diff == 0) THEN
      WRITE(LFNPRT,'(A,/,A,/,A)') &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------',   &
      ' No differences found in section: Type 003  ',                  &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------'
    ENDIF

    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    IF(ct03 == 1 .AND. na == 1) THEN
      WRITE(LFNPRT,'(A)') ' Stations missing in section: Type 003   '
    ELSEIF(ct03 == 2 .AND. na == 1) THEN
      WRITE(LFNPRT,'(A)') ' Stations merged in master file at record: Type 003'
    ELSEIF(na == 0) THEN
      WRITE(LFNPRT,'(A)') ' No missing stations found in section: Type 003 '
    ENDIF
    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    DO i = 1, second%nprob
      IF (idx3(i) == 0) THEN
        DO j = 1,second%nprob
          IF (second%staprob(i)%stanam /= second%staprob(j)%stanam) CYCLE
            idx3(j)=3
            idx3(i)=idx3(i)+1
        ENDDO
        WRITE(LFNPRT,'(A,I4,A,A)') ' Record ', i, '  missing in master file: ', &
                     second%staprob(i)%stanam
        idx3(i)=3
      ENDIF
    ENDDO
    WRITE(LFNPRT,'(/)')

    DEALLOCATE(idx3, stat=iac)
  ENDIF

  IF (ct03 == 3) THEN
    WRITE(LFNPRT,'(A,/,A,/,A,/)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------',   &
    ' Entries for section 003 deleted   ',                  &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
  ENDIF

! Find missing stations in master file, section 004
! -------------------------------------------------
  IF (ct04 == 1 .OR. ct04 == 2) THEN
    diff = 0
    ALLOCATE(idx4(second%ncoovel),stat=iac)
    CALL alcerr(iac,'idx4',(/second%ncoovel/),pgmNam)
    idx4 = 0
    DO i=1, second%ncoovel
      DO j=1, master%ncoovel
        IF (second%coovel(i)%stanam(1) == master%coovel(j)%stanam(1) .AND. &
            second%coovel(i)%stanam(2) == master%coovel(j)%stanam(2)) THEN
          idx4(i) = 2
        ENDIF
        IF (second%coovel(i)%stanam(1) == master%coovel(j)%stanam(1) .AND. &
            second%coovel(i)%stanam(2) == master%coovel(j)%stanam(2) .AND. &
            second%coovel(i)%constr(1) == master%coovel(j)%constr(1) .AND. &
            second%coovel(i)%constr(2) == master%coovel(j)%constr(2) .AND. &
            second%coovel(i)%constr(3) == master%coovel(j)%constr(3) .AND. &
            second%coovel(i)%constr(4) == master%coovel(j)%constr(4) .AND. &
            second%coovel(i)%constr(5) == master%coovel(j)%constr(5) .AND. &
            second%coovel(i)%constr(6) == master%coovel(j)%constr(6)) THEN
          idx4(i) = 1
          EXIT
        ENDIF
      ENDDO
    ENDDO

    DO j=1, master%ncoovel
      miss%ncoovel = miss%ncoovel+1
      miss%coovel(miss%ncoovel) = master%coovel(j)
    ENDDO

!   Find different entries to same station names, section 004
!   ---------------------------------------------------------
    na = 0
    DO i = 1, second%ncoovel
      IF (idx4(i) == 0 .AND. (ct04 == 1 .OR. ct04 == 2)) THEN
        na = 1
        IF (idx4(i) == 0 .AND. ct04 == 2) THEN
          miss%ncoovel = miss%ncoovel+1
          miss%coovel(miss%ncoovel) = second%coovel(i)
        ENDIF
      ENDIF
      IF (idx4(i) == 2) THEN
        diff = diff + 1
        IF (diff <= 1) THEN
          WRITE(LFNPRT,'(A,/,A,/,A)') &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------',   &
          ' Different entries in section: Type 004   ',                  &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------'
        ENDIF
        DO j = 1,master%ncoovel
          IF (second%coovel(i)%stanam(1) /= master%coovel(j)%stanam(1) .AND. &
              second%coovel(i)%stanam(2) /= master%coovel(j)%stanam(2)) CYCLE
          WRITE(LFNPRT, '(1X,A,2X, 2(A16,4x,2X), 6(F8.5,2X) )')              &
               'M', master%coovel(j)%stanam(1), master%coovel(j)%stanam(2),  &
               (master%coovel(j)%constr(kk), kk=1,6)
        ENDDO

        DO j = 1,second%ncoovel
          IF (second%coovel(i)%stanam(1) /= second%coovel(j)%stanam(1)  .AND. &
              second%coovel(i)%stanam(2) /= second%coovel(j)%stanam(2))  CYCLE
          idx4(j)=3
          WRITE(LFNPRT, '(1X,A,2X, 2(A16,4x,2X), 6(F8.5,2X) )')               &
               'S', second%coovel(j)%stanam(1), second%coovel(j)%stanam(2), &
               (second%coovel(j)%constr(kk), kk=1,6)
        ENDDO
        WRITE(LFNPRT,'(/)')
      ENDIF
    ENDDO
    IF (diff == 0) THEN
      WRITE(LFNPRT,'(A,/,A,/,A)') &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------',   &
      ' No differences found in section: Type 004  ',                  &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------'
    ENDIF

    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    IF (ct04 == 1 .AND. na == 1) THEN
       WRITE(LFNPRT,'(A)')  ' Stations missing in section: Type 004   '
    ELSEIF (ct04 == 2 .AND. na == 1) THEN
       WRITE(LFNPRT,'(A)')  ' Stations merged in master file at record: Type 004'
    ELSEIF (na == 0) THEN
       WRITE(LFNPRT,'(A)')  ' No missing stations found in section: Type 004 '
    ENDIF
    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    DO i = 1, second%ncoovel
      IF (idx4(i) == 0) THEN
        DO j = 1,second%ncoovel
           IF (second%coovel(i)%stanam(1) /= second%coovel(j)%stanam(1)  .AND.  &
               second%coovel(i)%stanam(2) /= second%coovel(j)%stanam(2))  CYCLE
            idx4(j)=3
            idx4(i)=idx4(i)+1
        ENDDO
        WRITE(LFNPRT,'(A,I4,A,A,A)') ' Record ', i, '  missing in master file: ', &
                     second%coovel(i)%stanam(1), second%coovel(i)%stanam(2)
        idx4(i)=3
      ENDIF
    ENDDO
    WRITE(LFNPRT,'(/)')

    DEALLOCATE(idx4, stat=iac)
  ENDIF

  IF (ct04 == 3) THEN
    WRITE(LFNPRT,'(A,/,A,/,A,/)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------',   &
    ' Entries for section 004 deleted   ',                  &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
  ENDIF

! Find missing stations in master file, section 005
! -------------------------------------------------
  IF (ct05 == 1 .OR. ct05 == 2) THEN
    diff = 0
    ALLOCATE(idx5(second%nstatype),stat=iac)
    CALL alcerr(iac,'idx5',(/second%nstatype/),pgmNam)
    idx5 = 0
    DO i=1, second%nstatype
      DO j=1, master%nstatype
        IF (second%statype(i)%stanam      == master%statype(j)%stanam) THEN
          idx5(i) = 2
        ENDIF
        IF (second%statype(i)%stanam      == master%statype(j)%stanam .AND. &
            second%statype(i)%flg         == master%statype(j)%flg .AND. &
            second%statype(i)%timint%t(1) == master%statype(j)%timint%t(1) .AND. &
            second%statype(i)%timint%t(2) == master%statype(j)%timint%t(2) .AND. &
            second%statype(i)%markertype  == master%statype(j)%markertype) THEN
          idx5(i) = 1
          EXIT
        ENDIF
      ENDDO
    ENDDO

    DO j=1, master%nstatype
      miss%nstatype = miss%nstatype+1
      miss%statype(miss%nstatype) = master%statype(j)
    ENDDO

!   Find different entries to same station names, section 005
!   ---------------------------------------------------------
    na = 0
    DO i = 1, second%nstatype
      IF (idx5(i) == 0 .AND. (ct05 == 1 .OR. ct05 == 2)) THEN
        na = 1
        IF (idx5(i) == 0 .AND. ct05 == 2) THEN
          miss%nstatype = miss%nstatype+1
          miss%statype(miss%nstatype) = second%statype(i)
        ENDIF
      ENDIF
      IF (idx5(i) == 2) THEN
        diff = diff + 1
        IF (diff <= 1) THEN
          WRITE(LFNPRT,'(A,/,A,/,A)') &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------',   &
          ' Different entries in section: Type 005   ',                  &
          ' -----------------------------------------------------------------' // &
          '------------------------------------------------------------------'
        ENDIF
        DO j = 1,master%nstatype
          IF (second%statype(i)%stanam /= master%statype(j)%stanam) CYCLE
          WRITE(LFNPRT, '(1X,A,2X,A16,4x,2X,I3.3,2X,A40,2X,A20)')         &
                'M', master%statype(j)%stanam, master%statype(j)%flg,     &
                time, master%statype(j)%markertype
        ENDDO

        DO j = 1,second%nstatype
          IF (second%statype(i)%stanam /= second%statype(j)%stanam)  CYCLE
          idx5(j)=3
          WRITE(LFNPRT, '(1X,A,2X,A16,4x,2X,I3.3,2X,A40,2X,A20)')             &
                'S', second%statype(j)%stanam, second%statype(j)%flg,         &
                time, second%statype(j)%markertype
        ENDDO
        WRITE(LFNPRT,'(/)')
      ENDIF
    ENDDO
    IF (diff == 0) THEN
      WRITE(LFNPRT,'(A,/,A,/,A)') &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------',   &
      ' No differences found in section: Type 005  ',                  &
      ' -----------------------------------------------------------------' // &
      '------------------------------------------------------------------'
    ENDIF

    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    IF (ct05 == 1 .AND. na == 1) THEN
       WRITE(LFNPRT,'(A)')  ' Stations missing in section: Type 005   '
    ELSEIF (ct05 == 2 .AND. na == 1) THEN
       WRITE(LFNPRT,'(A)')  ' Stations merged in master file at record: Type 005'
    ELSEIF (na == 0) THEN
       WRITE(LFNPRT,'(A)')  ' No missing stations found in section: Type 005 '
    ENDIF
    WRITE(LFNPRT,'(A)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    DO i = 1, second%nstatype
      IF (idx5(i) == 0) THEN
        DO j = 1,second%nstatype
           IF (second%statype(i)%stanam /= second%statype(j)%stanam)  CYCLE
            idx5(j)=3
            idx5(i)=idx5(i)+1
        ENDDO
        WRITE(LFNPRT,'(A,I4,A,A)') ' Record ', i, '  missing in master file: ', &
                     second%statype(i)%stanam
        idx5(i)=3
      ENDIF
    ENDDO
    WRITE(LFNPRT,'(/)')

    DEALLOCATE(idx5, stat=iac)
  ENDIF

  IF (ct05 == 3) THEN
    WRITE(LFNPRT,'(A,/,A,/,A,/)') &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------',   &
    ' Entries for section 005 deleted   ',                  &
    ' -----------------------------------------------------------------' // &
    '------------------------------------------------------------------'
  ENDIF

  RETURN

END SUBROUTINE selact

END MODULE
