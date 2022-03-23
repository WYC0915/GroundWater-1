MODULE s_findln

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

! -------------------------------------------------------------------------
!
! Purpose:    Find a specific line in the file opened with logical
!             unit number "IUNIT" that starts with on of the strings
!             given in "LOOK"
!
! Author:     J.Johnson, S.Schaer
!
! Created:    23-Feb-1996
!
! Changes:    13-Nov-1997 SS: Consider "ISTOP"
!             16-Nov-1997 SS: "IPOS" to specify "LOOK"'s position
!             14-Mar-2000 SS: Return after checking "NLIN" lines
!             21-Jun-2005 MM: COMLFNUM.inc removed, m_bern added
!             23-Jun-2005 MM: IMPLICIT NONE and declarations added
!             26-Sep-2012 RD: Transission to f90; LOOK may also be an array
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

INTERFACE findln
  MODULE PROCEDURE findln_arg1, findln_arg2
END INTERFACE

CONTAINS

! -------------------------------------------------------------------------
! Subroutine with non-array "LOOK" variable
! -------------------------------------------------------------------------

    SUBROUTINE findln_arg1(iUnit, iStop, nLin, srname, flname, look, &
                           iPos,  line,  iErr  )

    USE m_bern, ONLY: i4b, fileNameLength
    IMPLICIT NONE

! List of parameters
! ------------------
! input:
    INTEGER(i4b)                   :: iUnit   ! Logical file number
    INTEGER(i4b)                   :: iStop   ! Stop if string "LOOK" not found
                                              ! =0: no / =1: yes
    INTEGER(i4b)                   :: nLin    ! Check up to a maximum number of
                                              ! "nlin" lines
                                              ! =0: infinite
    CHARACTER(LEN=*)               :: srname  ! Subroutine name
    CHARACTER(LEN=fileNameLength)  :: flname  ! External file name
    CHARACTER(LEN=*)               :: look    ! String to look for in the file

! input/output:
    INTEGER(i4b)                   :: iPos    ! "LOOK"'s position expected/found
                                              ! =0: arbitrary
                                              ! >0: starting at n-th column

! output
    CHARACTER(LEN=*)               :: line    ! Line found matching the "look"
                                              ! string
    INTEGER(i4b)                   :: iErr    ! Error code
                                              ! =0: string successfully found
                                              ! =1: string not found

! Local variables
! ---------------
    INTEGER(i4b)     :: iLook

    CALL findln_arg2(iUnit ,iStop ,nLin  ,srname,flname,(/look/)  , &
                     iPos  ,iLook, line  ,iErr  )

    RETURN
  END SUBROUTINE findln_arg1


! -------------------------------------------------------------------------
! Subroutine with array "LOOK" variable
! -------------------------------------------------------------------------

    SUBROUTINE findln_arg2(iUnit, iStop, nLin, srname, flname, look, &
                           iPos,  iLook, line, iErr)

    USE m_bern, ONLY: i4b, lfnerr, fileNameLength

    USE s_exitrc
    IMPLICIT NONE

! List of parameters
! ------------------
! input:
    INTEGER(i4b)                   :: iUnit   ! Logical file number
    INTEGER(i4b)                   :: iStop   ! Stop if string "LOOK" not found
                                              ! =0: no / =1: yes
    INTEGER(i4b)                   :: nLin    ! Check up to a maximum number of
                                              ! "nlin" lines
                                              ! =0: infinite
    CHARACTER(LEN=*)               :: srname  ! Subroutine name
    CHARACTER(LEN=fileNameLength)  :: flname  ! External file name
    CHARACTER(LEN=*), DIMENSION(:) :: look    ! String to look for in the file

! input/output:
    INTEGER(i4b)                   :: iPos    ! "LOOK"'s position expected/found
                                              ! =0: arbitrary
                                              ! >0: starting at n-th column

! output
    INTEGER(i4b)                   :: iLook   ! Index of the match in the
                                              ! "LOOK"-array
    CHARACTER(LEN=*)               :: line    ! Line found matching the "look"
                                              ! string
    INTEGER(i4b)                   :: iErr    ! Error code
                                              ! =0: string successfully found
                                              ! =1: string not found



! Local variables
! ---------------
    INTEGER(i4b)     :: iLin, nln, ilk
    INTEGER(i4b)     :: nLook
    INTEGER(i4b)     :: ios


! Init variables
! --------------
    iLin  = 0
    iErr  = 0
    iLook = 0

    nLook = SIZE(look)

! Start the loop
    DO
      READ(iunit,'(A)',iostat=ios) line
      IF (ios /= 0) EXIT

      iLin=iLin+1

      IF (iPos > 0) THEN
        DO ilk = 1,nLook
          nln=LEN_TRIM(look(ilk))
          IF ( nln == 0 ) CYCLE
          IF (TRIM(look(ilk)) == line(iPos:nln+iPos-1)) THEN
            iLook = ilk
            RETURN
          ENDIF
        ENDDO
      ELSE
        DO ilk = 1,nLook
          IF ( LEN_TRIM(look(ilk)) == 0 ) CYCLE
          iPos=INDEX(line,TRIM(look(ilk)))
          IF ( iPos > 0 ) THEN
            iLook = ilk
            RETURN
          ENDIF
        ENDDO
      ENDIF

      IF (ilin >= nLin .AND. nLin /= 0) EXIT

    ENDDO

! Stop with error
    IF (iStop == 1) THEN
      IF ( SIZE(look) == 1 ) THEN
        WRITE(lfnerr,'(/,A,I3,A,/,16X,2A,/)') &
              ' *** SR ' // TRIM(srname) // ': Could not find string "' &
                         // TRIM(look(1)) // '" at ',iPos,'-th column', &
                        'File: ',TRIM(flname)
      ELSE
        WRITE(lfnerr,'(/,A,I3,A)') &
              ' *** SR ' // TRIM(srname) // ': None of the following ' &
                        // 'strings could be found at ',iPos,'-th column'
        DO iLook = 1,nLook
          WRITE(lfnerr,'(16X,A)') '"' // TRIM(look(iLook)) // '"'
        ENDDO
        WRITE(lfnerr,'(16X,2A,/)') 'File: ',TRIM(flname)
      ENDIF
      CALL exitrc(2)
    ENDIF

! Set return code
    IERR=1

    RETURN
  END SUBROUTINE findln_arg2

END MODULE
