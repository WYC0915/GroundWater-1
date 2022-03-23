MODULE s_WRITCRUX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE writcrux(filename, stacrux, title)

! -------------------------------------------------------------------------
! Purpose:    This subroutine writes the STACRUX file
!
! Author:     H. Bock
!
! Created:    18-May-1999
! Last mod.:  04-Aug-2011
!
! Changes:    09-Aug-1999 LM: NAG problem (tdummy used)
!             19-Aug-1999 JJ: Fix format statment to be on one line near
!                             line 113
!             08-Sep-2000 HB: Use fileNameLength from m_bern
!             09-Feb-2001 DS: TYPE 5
!             13-Feb-2001 DS: Use d_stacrux instead of m_stacrux
!             15-Jun-2001 HB: Rename d_dtacrux in d_stacrx
!             05-Jul-2001 RD: Length of filename is open
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             03-May-2002 RD: Optional title parameter
!             18-Feb-2003 HU: Use pgmver from m_bern
!             10-Mar-2003 HU: Write station description
!             30-Aug-2003 HU: Write type with I3.3,
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             10-Sep-2003 HU: Merged
!             16-Sep-2003 RD: STACRUX->STAINFO
!             17-Sep-2003 HU: Prepare filTitle if defcon not called
!             28-Jun-2005 MM: Unused variables removed
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             11-Aug-2005 HU: Call timst2 with array
!             20-Nov-2008 DT: technique, version in Header; Type 002 for SLR
!             19-Jul-2010 SL: tab characters removed
!             05-Oct-2010 SL: use m_bern with ONLY, flg(INTEGER->CHARACTER),
!                             TYPE 002 for GNSS
!             11-Oct-2010 RD: Remove unused messages
!             04-Aug-2011 DT: Default format for SLR
!                             (until final format definition for TYPE 002)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,r8b,shortLineLength,lfnLoc,lfnErr
  USE d_const,  ONLY: date,time,const_def
  USE d_stacrx, ONLY: t_stacrux

  USE s_opnfil
  USE s_opnerr
  USE s_dattim
  USE s_timst2
  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename
  TYPE(t_stacrux)              :: stacrux
  CHARACTER(LEN=*),OPTIONAL    :: title

! output:


! Local Variables
! ---------------
  CHARACTER(LEN=shortLineLength) :: titLine
  CHARACTER(LEN=15)              :: dat
  CHARACTER(LEN=40)              :: datstr

  INTEGER(i4b)                   :: icrx
  INTEGER(i4b)                   :: kk
  INTEGER(i4b)                   :: ios

  REAL(r8b), DIMENSION(2)        :: tdummy


! Use default format for SLR (until final format definition of TYPE 002)
! ----------------------------------------------------------------------
  IF (stacrux%technique == 'SLR ') THEN
    stacrux%technique = ''
  ENDIF

!
! Write the STACRUX file
! ======================
! Open the output file
! --------------------
  CALL opnfil(lfnloc,filename,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,'writcrux')

! Get the creation date
! ---------------------
  IF (const_def /= 1) CALL dattim(date,time)
  WRITE(dat,"(A9,1X,A5)") date,time

! WRITE THE STACRUX
! -----------------
  IF (PRESENT(title)) THEN
    titLine = title
  ELSE
    titLine = 'STATION INFORMATION FILE'
  ENDIF

  WRITE(lfnloc,'(a60,5x,a15)') titline(1:60),dat
  WRITE(lfnloc,'(A)') '----------------------------------------&
                      &----------------------------------------'
  WRITE(lfnloc,'(A)')

  WRITE(lfnloc,'(A,1x,F4.2)') 'FORMAT VERSION:', stacrux%version
  WRITE(lfnloc,'(A,6x,A)')    'TECHNIQUE:', TRIM(stacrux%technique)
  WRITE(lfnloc,'(A)')

  WRITE(lfnloc,'(A)') 'TYPE 001: RENAMING OF STATIONS'
  WRITE(lfnloc,'(A)') '------------------------------'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)')'STATION NAME          FLG          FROM                   TO&
        &         OLD STATION NAME      REMARK'
  WRITE(lfnloc,'(A)') '****************      ***  YYYY MM DD HH MM SS  YYYY MM DD &
       &HH MM SS  ********************  ************************'

  DO icrx = 1, stacrux%nrenam
    tdummy(1) = stacrux%renamsta(icrx)%timint%t(1)
    tdummy(2) = stacrux%renamsta(icrx)%timint%t(2)
    CALL timst2(2,2, tdummy(1:2), datstr)
    WRITE(lfnloc,'(A16,4x,2X,A3,2X,A40,2X,A16,4x,2X,A)') &
         stacrux%renamsta(icrx)%stanam, stacrux%renamsta(icrx)%flg,datstr,&
         stacrux%renamsta(icrx)%oldnam, TRIM(stacrux%renamsta(icrx)%remark)
  END DO

  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') 'TYPE 002: STATION INFORMATION'
  WRITE(lfnloc,'(A)') '-----------------------------'
  WRITE(lfnloc,'(A)')

! Type 002 for GNSS (or default)
! ------------------------------
  IF ( stacrux%technique == 'GNSS' .OR. stacrux%technique == '' ) THEN
    WRITE(lfnloc,'(A)') &
      'STATION NAME          '             // &
      'FLG          '                      // &
      'FROM                   TO         ' // &
      'RECEIVER TYPE         '             // &
      'RECEIVER SERIAL NBR   '             // &
      'REC #   '                           // &
      'ANTENNA TYPE          '             // &
      'ANTENNA SERIAL NBR    '             // &
      'ANT #    '                          // &
      'NORTH      '                        // &
      'EAST      '                         // &
      'UP      '                           // &
      'DESCRIPTION             '           // &
      'REMARK'
    WRITE(lfnloc,'(A)') &
      '****************      '                     // &
      '***  '                                      // &
      'YYYY MM DD HH MM SS  YYYY MM DD HH MM SS  ' // &
      '********************  '                     // &
      '********************  '                     // &
      '******  '                                   // &
      '********************  '                     // &
      '********************  '                     // &
      '******  '                                   // &
      '***.****  '                                 // &
      '***.****  '                                 // &
      '***.****  '                                 // &
      '**********************  '                   // &
      '************************'

    DO icrx = 1, stacrux%ninfo
      tdummy(1) = stacrux%stainfo(icrx)%timint%t(1)
      tdummy(2) = stacrux%stainfo(icrx)%timint%t(2)
      CALL timst2(2,2, tdummy(1:2), datstr)
      WRITE(lfnloc,'(A16,4x,2X,A3,2X,A40,2X,2(A20,2X,A20,2X,I6,2X), &
                   & 3(F8.4,2X),A22,2X,A)') &
            stacrux%stainfo(icrx)%stanam, &
            stacrux%stainfo(icrx)%flg, &
            datstr, &
            stacrux%stainfo(icrx)%recnam, &
            stacrux%stainfo(icrx)%recser, &
            stacrux%stainfo(icrx)%recnum, &
            stacrux%stainfo(icrx)%antnam, &
            stacrux%stainfo(icrx)%antser, &
            stacrux%stainfo(icrx)%antnum, &
            stacrux%stainfo(icrx)%antecc(1), &
            stacrux%stainfo(icrx)%antecc(2), &
            stacrux%stainfo(icrx)%antecc(3), &
            stacrux%stainfo(icrx)%descri, &
            TRIM(stacrux%stainfo(icrx)%remark)
    END DO

! Type 002 for SLR
! ----------------
  ELSEIF ( stacrux%technique == 'SLR ' ) THEN
    WRITE(lfnloc,'(A)') 'STATION NAME          FLG          FROM                   TO&
         &         CDP-SOD   &
         &   NORTH       EAST         UP      DESCRIPTION             REMARK'
    WRITE(lfnloc,'(A)') '****************      ***  YYYY MM DD HH MM SS  YYYY MM DD &
         &HH MM SS &
         & ********  *****.**** &
         & *****.****  *****.****  **********************  ************************'

    DO icrx = 1, stacrux%ninfo
      tdummy(1) = stacrux%stainfo(icrx)%timint%t(1)
      tdummy(2) = stacrux%stainfo(icrx)%timint%t(2)
      CALL timst2(2,2, tdummy(1:2), datstr)
      WRITE(lfnloc,'(A16,4x,2X,A3,2X,A40,2X,(A8,2X),3(F10.4,2X),A22,2X,A)') &
           stacrux%stainfo(icrx)%stanam, stacrux%stainfo(icrx)%flg, datstr, &
           stacrux%stainfo(icrx)%cdpsod, &
           stacrux%stainfo(icrx)%antecc(1), stacrux%stainfo(icrx)%antecc(2), &
           stacrux%stainfo(icrx)%antecc(3), stacrux%stainfo(icrx)%descri, &
           TRIM(stacrux%stainfo(icrx)%remark)
    END DO

  END IF

  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') 'TYPE 003: HANDLING OF STATION PROBLEMS'
  WRITE(lfnloc,'(A)') '--------------------------------------'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') 'STATION NAME          FLG          FROM                   TO&
       &         REMARK'
  WRITE(lfnloc,'(A)') '****************      ***  YYYY MM DD HH MM SS  YYYY MM DD&
       & HH MM SS  ************************************************************'

  DO icrx = 1, stacrux%nprob
    tdummy(1) = stacrux%staprob(icrx)%timint%t(1)
    tdummy(2) = stacrux%staprob(icrx)%timint%t(2)
    CALL timst2(2,2, tdummy(1:2), datstr)
    WRITE(lfnloc, '(A16,4x,2X,A3,2X,A40,2X,A)') &
          stacrux%staprob(icrx)%stanam, stacrux%staprob(icrx)%flg, &
          datstr, TRIM(stacrux%staprob(icrx)%remark)
  END DO

  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') 'TYPE 004: STATION COORDINATES AND VELOCITIES (ADDNEQ)'
  WRITE(lfnloc,'(A)') '-----------------------------------------------------'
  WRITE(lfnloc,'(44x,A)') 'RELATIVE CONSTR. POSITION     RELATIVE CONSTR. &
       &VELOCITY'
  WRITE(lfnloc,'(A)') 'STATION NAME 1        STATION NAME 2        NORTH     EAST&
       &      UP        NORTH     EAST      UP'
  WRITE(lfnloc,'(A)') '****************      ****************      **.*****  &
       &**.*****  **.*****  **.*****  **.*****  **.*****'

  DO icrx = 1, stacrux%ncoovel
    WRITE(lfnloc, '( 2(A16,4x,2X), 6(F8.5,2X) )')                        &
         stacrux%coovel(icrx)%stanam(1), stacrux%coovel(icrx)%stanam(2), &
         (stacrux%coovel(icrx)%constr(kk), kk=1,6)
  END DO

  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') 'TYPE 005: HANDLING STATION TYPES'
  WRITE(lfnloc,'(A)') '--------------------------------'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') 'STATION NAME          FLG  FROM                 TO &
                     &                  MARKER TYPE           REMARK'
  WRITE(lfnloc,'(A)') '****************      ***  YYYY MM DD HH MM SS  YYYY MM DD HH MM SS &
                     & ********************  ************************'

  DO icrx = 1, stacrux%nstatype
    tdummy(1) = stacrux%statype(icrx)%timint%t(1)
    tdummy(2) = stacrux%statype(icrx)%timint%t(2)
    CALL timst2(2,2, tdummy(1:2), datstr)
    WRITE(lfnloc, '(A16,4x,2X,A3,2X,A40,2X,A20,2X,A)') &
          stacrux%statype(icrx)%stanam, stacrux%statype(icrx)%flg, &
          datstr,stacrux%statype(icrx)%markertype, &
          TRIM(stacrux%statype(icrx)%remark)
  END DO

  WRITE(lfnloc,'(///)')

! RD: The usage of the flags is open -- this list is not necessary anymore
! ------------------------------------------------------------------------
  IF (1==2) THEN
  WRITE(lfnloc,'(A)') 'TYP FLAG  DESCRIPTION'
  WRITE(lfnloc,'(A)') '------------------------------------------------------'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') '001 001: RENAME STATION IN ALL PROGRAMS. NEW NAME IS &
       &USED FOR ALL FLAGS BELOW'
  WRITE(lfnloc,'(A)') '001 002: RENAME STATION RXOBV3. WILDCARDS ALLOWED. &
       &NEW NAME IS USED FOR ALL FLAGS BELOW'
  WRITE(lfnloc,'(A)') '001 003: RENAME STATION IN ADDNEQ. NEW NAME IS &
       &USED FOR ALL FLAGS BELOW'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') '002 001: STATION INFORMATION RECORD'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') '003 001: EXCLUDE STATION IN ALL PROGRAMS'
  WRITE(lfnloc,'(A)') '003 003: EXCLUDE STATION IN ADDNEQ (PREELIMINATION)'
  WRITE(lfnloc,'(A)') '003 011: STATION NOT BE USED FOR FIXING OR &
       &CONSTRAINING'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') '004 001: SET UP NEW SET OF COORDINATES: CONSTRAIN &
       &N,E,U COORDINATE TO PREVIOUS SET'
  ENDIF

  CLOSE (lfnloc)

  RETURN

END SUBROUTINE writcrux

END MODULE
