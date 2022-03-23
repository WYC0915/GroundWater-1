MODULE s_PRICRH
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE pricrh(nFil, clkFil)

! -------------------------------------------------------------------------
! Purpose:    Prints some information from the clock rinex header
!
! Author:     R. Dach
!
! Created:    03-Aug-2001
! Last mod.:  08-Aug-2005
!
! Changes:    28-Feb-2002  RD: Force writing record if no reference in file
!             19-Mar-2003  RD: Write long string with format (because IFC)
!             16-May-2003  MM: Initialize and deallocate structure
!             08-Aug-2005  HB: Use new SR TIMST2 (module)
!
! SR used:    opnfil, opnerr, rdcrxh, init_clkHead
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_clkrnx, ONLY: t_clkHead, init_clkHead
  USE s_opnfil
  USE s_opnerr
  USE s_timst2
  USE s_rdcrxh
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)              :: nFil   ! Number of input file names
  CHARACTER(LEN=*),          &
      DIMENSION(:), POINTER :: clkFil ! Clock file names

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_clkHead)           :: clkHead  ! Clock rinex header record

  CHARACTER(LEN=lineLength) :: Line

  INTEGER(i4b)              :: iFil
  INTEGER(i4b)              :: iRef, jRef
  INTEGER(i4b)              :: ii
  INTEGER(i4b)              :: i1, i2
  INTEGER(i4b)              :: irCode,iac

! Initialize structure
! --------------------
  CALL init_clkHead(clkHead)

! Write the title of the table
! ----------------------------
  WRITE(lfnprt,'(//,A,/,A,/,A,/,A)')                                         &
  ' CLOCK RINEX FILE CONTENT',                                                &
  ' ------------------------',                                                &
  '                                         # of clocks     Reference clock', &
  ' Num  File name                           Sta.  Sat.     Set    Clock name'
  WRITE(lfnprt,'(1X,131("-"))')

! Loop all files
! --------------
  DO iFil = 1, nFil
    clkHead%TFirst = 0d0

! Open file to read
! -----------------
    CALL opnfil(lfnloc,ClkFil(iFil),'OLD','FORMATTED','READONLY',' ',irCode)
    CALL opnerr(lfnerr,lfnloc,irCode,ClkFil(iFil),'pricrh')

! Read the header of a file
! -------------------------
    IF (irCode == 0) CALL rdcrxh(lfnloc,lfnerr,ClkHead,irCode)
    IF (irCode /= 0) CYCLE
    CLOSE(lfnloc)

! Print some information
! ----------------------
    Line = ' '
    WRITE(Line,'(1X,I3,2X,A32,2X,2I6)') &
      iFil, ClkFil(iFil), clkHead%nSta, clkHead%nSat

    IF (clkHead%numRef == 1 .AND. clkHead%ref(1)%nRef == 0) &
      WRITE(lfnprt,'(A)') TRIM(Line)

! Write the reference clock information
! -------------------------------------
    DO iRef = 1, clkHead%numRef
      WRITE(Line(55:61),'(I6)') iRef

! More than one reference clock
! -----------------------------
      IF (clkHead%ref(iRef)%refWin%t(1) /= 0d0 .OR. &
          clkHead%ref(iRef)%refWin%t(2) /= 0d0) THEN
        WRITE(Line(65:68),'(A)') 'from'
        WRITE(Line(90:91),'(A)') 'to'
        CALL timst2(1,1,clkHead%TFirst+clkHead%ref(iRef)%refWin%t(1)/86400d0,Line(70:89))
        CALL timst2(1,1,clkHead%TFirst+clkHead%ref(iRef)%refWin%t(2)/86400d0,Line(93:112))

        WRITE(lfnprt,'(A)') TRIM(Line)
        Line = ' '
      ENDIF

! Write the reference clock names
      DO jRef = 1, clkHead%ref(iRef)%nRef
        i1 = 64 + MOD(jRef-1,3) * 22
        i2 = i1 + 20
        WRITE(Line(i1:i2),*) clkHead%ref(iRef)%clk(jRef)%Name

        IF (MOD(jRef,3) == 0 .OR. jRef == clkHead%ref(iRef)%nRef) THEN
          WRITE(lfnprt,'(A)') TRIM(Line)
          Line = ' '
        ENDIF
      ENDDO
    ENDDO
  ENDDO

! Close the list
! --------------
  WRITE(lfnprt,'(1X,131("-"),/)')

! Deallocate
! ----------
  DEALLOCATE(clkHead%comment, stat=iac)
  DEALLOCATE(clkHead%DatTyp, stat=iac)
  DEALLOCATE(clkHead%clkName, stat=iac)
  DEALLOCATE(clkHead%staCoord, stat=iac)
  DO ii=1,clkHead%numRef
    DEALLOCATE (clkHead%Ref(ii)%clk,stat=iac)
  ENDDO
  DEALLOCATE (clkHead%Ref,stat=iac)

  RETURN
END SUBROUTINE pricrh

END MODULE
