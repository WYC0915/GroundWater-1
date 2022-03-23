MODULE s_READCRUX
PRIVATE :: readcrux100,readcrux101,readcrux_exit
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readcrux(filename, stacrux, title)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads the STACRUX file and give back the
!             requests in the stacrux-structure
!
! Author:     H. Bock
!
! Created:    14-May-1999
! Last mod.:  02-Feb-2011
!
! Changes:    19-Aug-1999 JJ: Fix format statment to be on one line near
!                              line 174
!             08-Sep-2000 HB: Use fileNameLength from m_bern
!             09-Feb-2001 DS: TYPE 5
!             12-Mar-2001 DS: m_stacrux renamed to d_stacrux
!             13-Jun-2001 HB: Add SR alcerr
!             15-Jun-2001 HB: Rename d_stacrux in d_stacrx
!             26-Jun-2001 RD: alcerr must have an interface
!             01-Nov-2001 RD: Write time window for type 5 into statype
!             03-May-2002 RD: Optional title parameter
!             10-Mar-2003 HU: Read station desription
!             16-May-2003 HU: Deallocate arrays
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             16-Sep-2003 RD: STACRUX->STAINFO
!             24-Jun-2009 DT: New TYPE 004 (relative parameter constraints)
!             19-Jul-2010 SL: tab characters removed
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             05-Oct-2010 SL: use m_bern with ONLY, consider STAINFO 1.01
!             29-Nov-2010 SL: if condition handling empty strings changed
!             02-Feb-2011 SL: get technique string
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnLoc, lfnErr
  USE d_stacrx, ONLY: t_stacrux

  USE s_opnfil
  USE s_opnerr
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_stacrux)              :: stacrux
  CHARACTER(LEN=*),OPTIONAL    :: title

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER   :: srName = 'READCRUX'

! Local Variables
! ---------------
  CHARACTER(LEN=251)           :: line
  INTEGER(i4b)                 :: ios
  REAL(r8b)                    :: version

! Read the entire STACRUX file
! ----------------------------
  CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,srName)

  version = 0d0
  DO
    READ(lfnloc,'(A)',END=999) line
    IF(line(1:15) == 'FORMAT VERSION:') THEN
      IF(line(17:20) == '1.00') THEN
        version = 1.00d0
      ELSEIF(line(17:20) == '1.01') THEN
        version = 1.01d0
      ENDIF
    ELSEIF(line(1:10) == 'TECHNIQUE:') THEN
      stacrux%technique = TRIM(line(17:80))
    ELSEIF(line(1:12) == 'STATION NAME') THEN
      EXIT
    ENDIF
  ENDDO

  CLOSE(lfnloc)

! Call appropriate readcrux subroutine
! ------------------------------------
  IF(version <= 1.00d0) THEN
    IF(PRESENT(title)) THEN
      CALL readcrux100(filename,stacrux,title)
    ELSE
      CALL readcrux100(filename,stacrux)
    ENDIF
  ELSEIF(version == 1.01d0) THEN
    IF(PRESENT(title)) THEN
      CALL readcrux101(filename,stacrux,title)
    ELSE
      CALL readcrux101(filename,stacrux)
    ENDIF
  ENDIF
  stacrux%stainfo%cdpsod = ''

  RETURN
  999 CONTINUE
  WRITE(lfnerr,*) ' *** SR READCRUX: Error reading STAINFO file : ',filename
  CALL exitrc(2)

END SUBROUTINE readcrux

! -------------------------------------------------------------------------

SUBROUTINE readcrux100(filename,stacrux,title)

! -------------------------------------------------------------------------
!
! Changes:    05-Oct-2010 SL: use m_bern with ONLY, flg: INTEGER->CHARACTER

! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,lfnLoc,lfnErr
  USE d_stacrx, ONLY: t_stacrux

  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_st2tim
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_stacrux)              :: stacrux
  CHARACTER(LEN=*),OPTIONAL    :: title

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE t_stacruxmap
    INTEGER(i4b) :: pos1
    INTEGER(i4b) :: pos2
  END TYPE t_stacruxmap

! Local Parameters
! ----------------
  INTEGER(i4b), PARAMETER :: numTyp = 5

! Local Variables
! ---------------
  TYPE(t_stacruxmap), DIMENSION(numTyp) :: map

  CHARACTER(LEN=206)      :: line
  CHARACTER(LEN=40)       :: datstr

  INTEGER(i4b)            :: nlin
  INTEGER(i4b)            :: icrx
  INTEGER(i4b)            :: ii, kk
  INTEGER(i4b)            :: ios
  INTEGER(i4b)            :: iac

 ! Read the entire STACRUX file
 ! ==============================

     CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
     CALL opnerr(lfnerr,lfnloc,ios,filename,'readcrux')

     DO ii = 1, numTyp
       map(ii)%pos1 = 0
       map(ii)%pos2 = 0
     END DO

   ! Map the stacrux file
   ! --------------------
     nlin = 0

     IF (PRESENT(title)) THEN
       READ(lfnloc,'(A)') title
       nlin = nlin + 1
     ENDIF

     Loop_001: DO
       nlin = nlin + 1
       READ (lfnloc,'(A)',END=999) line
       IF ( line(1:8) == 'TYPE 001') THEN
         nlin = nlin + 4
         READ (lfnloc,'(///)',END=999)
         map(1)%pos1 = nlin + 1
         map(1)%pos2 = nlin
         DO
           nlin = nlin + 1
           READ (lfnloc,'(A)',END=999) line
           IF (line == '') EXIT Loop_001
           map(1)%pos2 = nlin
         END DO
       END IF
     END DO Loop_001

     Loop_002: DO
       nlin = nlin + 1
       READ (lfnloc,'(A)',END=999) line
       IF ( line(1:8) == 'TYPE 002') THEN
         nlin = nlin + 4
         READ (lfnloc,'(///)',END=999)
         map(2)%pos1 = nlin + 1
         map(2)%pos2 = nlin
         DO
           nlin = nlin + 1
           READ (lfnloc,'(A)',END=999) line
           IF (line == '') EXIT Loop_002
           map(2)%pos2 = nlin
         END DO
       END IF
     END DO Loop_002

     Loop_003: DO
       nlin = nlin + 1
       READ (lfnloc,'(A)',END=999) line
       IF ( line(1:8) == 'TYPE 003') THEN
         nlin = nlin + 4
         READ (lfnloc,'(///)',END=999)
         map(3)%pos1 = nlin + 1
         map(3)%pos2 = nlin
         DO
           nlin = nlin + 1
           READ (lfnloc,'(A)',END=999) line
           IF (line == '') EXIT Loop_003
           map(3)%pos2 = nlin
         END DO
       END IF
     END DO Loop_003

     Loop_004: DO
       nlin = nlin + 1
       READ (lfnloc,'(A)',END=999) line
       IF ( line(1:8) == 'TYPE 004') THEN
         nlin = nlin + 4
         READ (lfnloc,'(///)',END=999)
         map(4)%pos1 = nlin + 1
         map(4)%pos2 = nlin
         DO
           nlin = nlin + 1
           READ (lfnloc,'(A)',END=999) line
           IF (line == '') EXIT Loop_004
           map(4)%pos2 = nlin
         END DO
       END IF
     END DO Loop_004

! TYPE 005 is not yet obligatory!!!!!!
! ------------------------------------
     Loop_005: DO
       nlin = nlin + 1
       READ (lfnloc,'(A)',END=111) line
!       READ (lfnloc,'(A)',END=999) line
       IF ( line(1:8) == 'TYPE 005') THEN
         nlin = nlin + 4
         READ (lfnloc,'(///)',END=999)
         map(5)%pos1 = nlin + 1
         map(5)%pos2 = nlin
         DO
           nlin = nlin + 1
           READ (lfnloc,'(A)',END=999) line
           IF (line == '') EXIT Loop_005
           map(5)%pos2 = nlin
         END DO
       END IF
     END DO Loop_005

   ! Allocate memory for Requests
   ! ----------------------------
111  stacrux%nrenam  = map(1)%pos2-map(1)%pos1+1
     stacrux%ninfo   = map(2)%pos2-map(2)%pos1+1
     stacrux%nprob   = map(3)%pos2-map(3)%pos1+1
     stacrux%ncoovel = map(4)%pos2-map(4)%pos1+1
     stacrux%nstatype= map(5)%pos2-map(5)%pos1+1
     DEALLOCATE(stacrux%renamsta,stat=iac)
     ALLOCATE(stacrux%renamsta(stacrux%nrenam),stat=iac)
     CALL alcerr(iac, 'stacrux%renamsta', (/stacrux%nrenam/), 'readcrux')
     DEALLOCATE(stacrux%stainfo,stat=iac)
     ALLOCATE(stacrux%stainfo(stacrux%ninfo),stat=iac)
     CALL alcerr(iac, 'stacrux%staprob', (/stacrux%ninfo/), 'readcrux')
     DEALLOCATE(stacrux%staprob,stat=iac)
     ALLOCATE(stacrux%staprob(stacrux%nprob),stat=iac)
     CALL alcerr(iac, 'stacrux%staprob', (/stacrux%nprob/), 'readcrux')
     DEALLOCATE(stacrux%coovel,stat=iac)
     ALLOCATE(stacrux%coovel(stacrux%ncoovel),stat=iac)
     CALL alcerr(iac, 'stacrux%coovel', (/stacrux%ncoovel/), 'readcrux')
     DEALLOCATE(stacrux%statype,stat=iac)
     ALLOCATE(stacrux%statype(stacrux%nstatype),stat=iac)
     CALL alcerr(iac, 'stacrux%statype', (/stacrux%nstatype/), 'readcrux')


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! für neues Format; bis zur vollständigen Umstellung
!!!
!!!     stacrux%nrelpar = map(4)%pos2-map(4)%pos1+1
!!!     DEALLOCATE(stacrux%staRelPar,stat=iac)
!!!     ALLOCATE(stacrux%staRelPar(stacrux%nrelpar),stat=iac)
!!!     CALL alcerr(iac, 'stacrux%staRelPar', (/stacrux%nrelpar/), 'readcrux')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   ! Read the Requests
   ! -----------------
     REWIND(lfnloc)

     DO ii = 1,               map(1)%pos1 - 1
       READ (lfnloc,'(A)',END=999) line
     END DO
     DO ii = map(1)%pos1, map(1)%pos2
       icrx = ii - map(1)%pos1 + 1
       READ (lfnloc,'(A)',END=999) line
       READ (line, '(A16,4x,2X,A3,2X,A40,2X,A16,4x,2X,A24)')                       &
            stacrux%renamsta(icrx)%stanam, stacrux%renamsta(icrx)%flg,datstr,&
            stacrux%renamsta(icrx)%oldnam, stacrux%renamsta(icrx)%remark
       IF (datstr(22:40) == '                  ') THEN
         CALL st2tim(1, 1, datstr(1:19), stacrux%renamsta(icrx)%timint%t(1) )
         stacrux%renamsta(icrx)%timint%t(2)= 1.D20
       ELSE
         CALL st2tim(1, 2, datstr, stacrux%renamsta(icrx)%timint%t )
       END IF
     END DO

     DO ii = map(1)%pos2 + 1, map(2)%pos1 - 1
       READ (lfnloc,'(A)',END=999) line
     END DO
     DO ii = map(2)%pos1, map(2)%pos2
       icrx = ii - map(2)%pos1 + 1
       READ (lfnloc,'(A)',END=999) line
       READ (line,                                                           &
             '( A16,4x,2X,A3,2X,A40,2X,2(A20,2X),2(I6,2X),3(F8.4,2X),A22,2X,A24)')    &
             stacrux%stainfo(icrx)%stanam, stacrux%stainfo(icrx)%flg, datstr,&
             stacrux%stainfo(icrx)%recnam, stacrux%stainfo(icrx)%antnam,     &
             stacrux%stainfo(icrx)%recnum, stacrux%stainfo(icrx)%antnum,     &
             stacrux%stainfo(icrx)%antecc(1),stacrux%stainfo(icrx)%antecc(2),&
             stacrux%stainfo(icrx)%antecc(3),stacrux%stainfo(icrx)%descri,   &
             stacrux%stainfo(icrx)%remark
       stacrux%stainfo(icrx)%recser = ''
       stacrux%stainfo(icrx)%antser = ''
       CALL st2tim(1, 2, datstr, stacrux%stainfo(icrx)%timint%t )
     END DO

     DO ii = map(2)%pos2 + 1, map(3)%pos1 - 1
       READ (lfnloc,'(A)',END=999) line
     END DO
     DO ii = map(3)%pos1, map(3)%pos2
       icrx = ii - map(3)%pos1 + 1
       READ (lfnloc,'(A)',END=999) line
       READ (line, '(A16,4x,2X,A3,2X,A40,2X,A60)')                              &
             stacrux%staprob(icrx)%stanam, stacrux%staprob(icrx)%flg,        &
             datstr, stacrux%staprob(icrx)%remark
       CALL st2tim(1, 2, datstr, stacrux%staprob(icrx)%timint%t )
     END DO

     DO ii = map(3)%pos2 + 1, map(4)%pos1 - 1
       READ (lfnloc,'(A)',END=999) line
     END DO
     DO ii = map(4)%pos1, map(4)%pos2
       icrx = ii - map(4)%pos1 + 1
       READ (lfnloc,'(A)',END=999) line
       READ (line, '( 2(A16,4x,2X), 6(F8.5,2X) )')                           &
             stacrux%coovel(icrx)%stanam(1), stacrux%coovel(icrx)%stanam(2), &
             (stacrux%coovel(icrx)%constr(kk), kk=1,6)
!!!
!!! for new STA format:
!!!       READ (line,  &
!!!          '(2(A16,6X),A3,2X,A40,2X,A8,2X,A3,2X,3(F11.5,2X),A3,2X,3(F8.5,2X),3(F8.5,2X),A24)')  &
!!!          (stacrux%staRelPar(icrx)%stanam(kk), kk=1,2),  &
!!!          stacrux%staRelPar(icrx)%flg, datstr,           &
!!!          stacrux%staRelPar(icrx)%parTyp,                &
!!!          stacrux%staRelPar(icrx)%sys(1),                &
!!!          (stacrux%staRelPar(icrx)%locTie(kk), kk=1,3),  &
!!!          stacrux%staRelPar(icrx)%sys(2),                &
!!!          (stacrux%staRelPar(icrx)%constr(kk), kk=1,3),  &
!!!          (stacrux%staRelPar(icrx)%correl(kk), kk=1,3),  &
!!!          stacrux%staRelPar(icrx)%remark
!!!
!!!       CALL st2tim(1, 2, datstr, stacrux%staRelPar(icrx)%timWin%t )
!!!
!!!       stacrux%staRelPar(icrx)%applied = .FALSE.

     END DO

     DO ii = map(4)%pos2 + 1, map(5)%pos1 - 1
       READ (lfnloc,'(A)',END=999) line
     END DO
     DO ii = map(5)%pos1, map(5)%pos2
       icrx = ii - map(5)%pos1 + 1
       READ (lfnloc,'(A)',END=999) line
       READ (line, '(A16,4x,2X,A3,2X,A40,2X,A20,2X,A24)')                       &
             stacrux%statype(icrx)%stanam, stacrux%statype(icrx)%flg,        &
             datstr,stacrux%statype(icrx)%markertype,                        &
             stacrux%statype(icrx)%remark
       CALL st2tim(1, 2, datstr, stacrux%statype(icrx)%timint%t )
     END DO

     CLOSE (lfnloc)

  RETURN
  999 CONTINUE
  WRITE(lfnerr,*) ' *** SR READCRUX100: Error reading STAINFO file : ', filename
  CALL exitrc(2)

END SUBROUTINE readcrux100

! -------------------------------------------------------------------------

SUBROUTINE readcrux101(filename, stacrux, title)

! -------------------------------------------------------------------------
!
! Changes:    __-___-____ __:
!
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,lfnLoc,lfnErr
  USE d_stacrx, ONLY: t_stacrux,undef_i,undef_e

  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_st2tim
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_stacrux)              :: stacrux
  CHARACTER(LEN=*),OPTIONAL    :: title

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE t_stacruxmap
    INTEGER(i4b) :: pos1
    INTEGER(i4b) :: pos2
  END TYPE t_stacruxmap

! Local Parameters
! ----------------
  CHARACTER(LEN=11), PARAMETER :: srName = 'READCRUX101'
  INTEGER(i4b), PARAMETER      :: numTyp = 5

! Local Variables
! ---------------
  TYPE(t_stacruxmap), DIMENSION(numTyp) :: map

  CHARACTER(LEN=251)      :: line
  CHARACTER(LEN=40)       :: datstr

  INTEGER(i4b)            :: nlin
  INTEGER(i4b)            :: icrx
  INTEGER(i4b)            :: ii, kk
  INTEGER(i4b)            :: ios
  INTEGER(i4b)            :: iac

 ! Read the entire STACRUX file
 ! ==============================

  CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,srName)

  DO ii = 1, numTyp
    map(ii)%pos1 = 0
    map(ii)%pos2 = 0
  END DO

  ! Map the stacrux file
  ! --------------------
  nlin = 0

  IF (PRESENT(title)) THEN
    READ(lfnloc,'(A)') title
    nlin = nlin + 1
  ENDIF

  Loop_001: DO
    nlin = nlin + 1
    READ (lfnloc,'(A)',END=999) line
    IF ( line(1:8) == 'TYPE 001') THEN
      nlin = nlin + 4
      READ (lfnloc,'(///)',END=999)
      map(1)%pos1 = nlin + 1
      map(1)%pos2 = nlin
      DO
        nlin = nlin + 1
        READ (lfnloc,'(A)',END=999) line
        IF (line == '') EXIT Loop_001
        map(1)%pos2 = nlin
      END DO
    END IF
  END DO Loop_001

  Loop_002: DO
    nlin = nlin + 1
    READ (lfnloc,'(A)',END=999) line
    IF ( line(1:8) == 'TYPE 002') THEN
      nlin = nlin + 4
      READ (lfnloc,'(///)',END=999)
      map(2)%pos1 = nlin + 1
      map(2)%pos2 = nlin
      DO
        nlin = nlin + 1
        READ (lfnloc,'(A)',END=999) line
        IF (line == '') EXIT Loop_002
        map(2)%pos2 = nlin
      END DO
    END IF
  END DO Loop_002

  Loop_003: DO
    nlin = nlin + 1
    READ (lfnloc,'(A)',END=999) line
    IF ( line(1:8) == 'TYPE 003') THEN
      nlin = nlin + 4
      READ (lfnloc,'(///)',END=999)
      map(3)%pos1 = nlin + 1
      map(3)%pos2 = nlin
      DO
        nlin = nlin + 1
        READ (lfnloc,'(A)',END=999) line
        IF (line == '') EXIT Loop_003
        map(3)%pos2 = nlin
      END DO
    END IF
  END DO Loop_003

  Loop_004: DO
    nlin = nlin + 1
    READ (lfnloc,'(A)',END=999) line
    IF ( line(1:8) == 'TYPE 004') THEN
      nlin = nlin + 4
      READ (lfnloc,'(///)',END=999)
      map(4)%pos1 = nlin + 1
      map(4)%pos2 = nlin
      DO
        nlin = nlin + 1
        READ (lfnloc,'(A)',END=999) line
        IF (line == '') EXIT Loop_004
        map(4)%pos2 = nlin
      END DO
    END IF
  END DO Loop_004

! TYPE 005 is not yet obligatory!!!!!!
! ------------------------------------
  Loop_005: DO
    nlin = nlin + 1
    READ (lfnloc,'(A)',END=111) line
!    READ (lfnloc,'(A)',END=999) line
    IF ( line(1:8) == 'TYPE 005') THEN
      nlin = nlin + 4
      READ (lfnloc,'(///)',END=999)
      map(5)%pos1 = nlin + 1
      map(5)%pos2 = nlin
      DO
        nlin = nlin + 1
        READ (lfnloc,'(A)',END=999) line
        IF (line == '') EXIT Loop_005
        map(5)%pos2 = nlin
      END DO
    END IF
  END DO Loop_005

  ! Allocate memory for Requests
  ! ----------------------------
111 stacrux%nrenam  = map(1)%pos2-map(1)%pos1+1
  stacrux%ninfo   = map(2)%pos2-map(2)%pos1+1
  stacrux%nprob   = map(3)%pos2-map(3)%pos1+1
  stacrux%ncoovel = map(4)%pos2-map(4)%pos1+1
  stacrux%nstatype= map(5)%pos2-map(5)%pos1+1
  DEALLOCATE(stacrux%renamsta,stat=iac)
  ALLOCATE(stacrux%renamsta(stacrux%nrenam),stat=iac)
  CALL alcerr(iac, 'stacrux%renamsta', (/stacrux%nrenam/),srName)
  DEALLOCATE(stacrux%stainfo,stat=iac)
  ALLOCATE(stacrux%stainfo(stacrux%ninfo),stat=iac)
  CALL alcerr(iac, 'stacrux%staprob', (/stacrux%ninfo/),srName)
  DEALLOCATE(stacrux%staprob,stat=iac)
  ALLOCATE(stacrux%staprob(stacrux%nprob),stat=iac)
  CALL alcerr(iac, 'stacrux%staprob', (/stacrux%nprob/),srName)
  DEALLOCATE(stacrux%coovel,stat=iac)
  ALLOCATE(stacrux%coovel(stacrux%ncoovel),stat=iac)
  CALL alcerr(iac, 'stacrux%coovel', (/stacrux%ncoovel/),srName)
  DEALLOCATE(stacrux%statype,stat=iac)
  ALLOCATE(stacrux%statype(stacrux%nstatype),stat=iac)
  CALL alcerr(iac, 'stacrux%statype', (/stacrux%nstatype/),srName)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! für neues Format; bis zur vollständigen Umstellung
!!!
!!!     stacrux%nrelpar = map(4)%pos2-map(4)%pos1+1
!!!     DEALLOCATE(stacrux%staRelPar,stat=iac)
!!!     ALLOCATE(stacrux%staRelPar(stacrux%nrelpar),stat=iac)
!!!     CALL alcerr(iac, 'stacrux%staRelPar', (/stacrux%nrelpar/), 'readcrux')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Read the Requests
  ! -----------------
  REWIND(lfnloc)

  ! TYPE 001
  DO ii = 1, map(1)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO
  DO ii = map(1)%pos1, map(1)%pos2
    icrx = ii - map(1)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ(line,'(A16,4x,2X,A3,2X,A40,2X,A16,4x,2X,A24)') &
      stacrux%renamsta(icrx)%stanam, &
      stacrux%renamsta(icrx)%flg, &
      datstr, &
      stacrux%renamsta(icrx)%oldnam, &
      stacrux%renamsta(icrx)%remark
    IF (datstr(22:40) == '                  ') THEN
      CALL st2tim(1, 1, datstr(1:19), stacrux%renamsta(icrx)%timint%t(1) )
      stacrux%renamsta(icrx)%timint%t(2)= 1.D20
    ELSE
      CALL st2tim(1, 2, datstr, stacrux%renamsta(icrx)%timint%t )
    END IF
  END DO

  ! TYPE 002
  DO ii = map(1)%pos2 + 1, map(2)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO
  DO ii = map(2)%pos1, map(2)%pos2
    icrx = ii - map(2)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ(line,'(A16,4x,2X,A3,2X,A40,2X,2(A20,2X,A20,2X,6X,2X), &
              & 3(8X,2X),A22,2X,A24)',iostat=ios) &
      stacrux%stainfo(icrx)%stanam, &
      stacrux%stainfo(icrx)%flg, &
      datstr, &
      stacrux%stainfo(icrx)%recnam, &
      stacrux%stainfo(icrx)%recser, &
!      stacrux%stainfo(icrx)%recnum, &
      stacrux%stainfo(icrx)%antnam, &
      stacrux%stainfo(icrx)%antser, &
!      stacrux%stainfo(icrx)%antnum, &
!      stacrux%stainfo(icrx)%antecc(1), &
!      stacrux%stainfo(icrx)%antecc(2), &
!      stacrux%stainfo(icrx)%antecc(3), &
      stacrux%stainfo(icrx)%descri, &
      stacrux%stainfo(icrx)%remark
    IF(ios /= 0) CALL readcrux_exit(2,icrx,line)
    CALL st2tim(1,2,datstr,stacrux%stainfo(icrx)%timint%t)
    IF(line(114:119) == '      ') THEN
      stacrux%stainfo(icrx)%recnum = undef_i
    ELSE
      READ(line(114:119),'(I6)',iostat=ios) stacrux%stainfo(icrx)%recnum
      IF(ios /= 0) CALL readcrux_exit(2,icrx,line)
    ENDIF
    IF(line(166:171) == '      ') THEN
      stacrux%stainfo(icrx)%antnum = undef_i
    ELSE
      READ(line(166:171),'(I6)',iostat=ios) stacrux%stainfo(icrx)%antnum
      IF(ios /= 0) CALL readcrux_exit(2,icrx,line)
    ENDIF
    IF(line(174:181) == '        ') THEN
      stacrux%stainfo(icrx)%antecc(1) = undef_e
    ELSE
      READ(line(174:181),'(F8.4)',iostat=ios) stacrux%stainfo(icrx)%antecc(1)
      IF(ios /= 0) CALL readcrux_exit(2,icrx,line)
    ENDIF
    IF(line(184:191) == '        ') THEN
      stacrux%stainfo(icrx)%antecc(2) = undef_e
    ELSE
      READ(line(184:191),'(F8.4)',iostat=ios) stacrux%stainfo(icrx)%antecc(2)
      IF(ios /= 0) CALL readcrux_exit(2,icrx,line)
    ENDIF
    IF(line(194:201) == '        ') THEN
      stacrux%stainfo(icrx)%antecc(3)= undef_e
    ELSE
      READ(line(194:201),'(F8.4)',iostat=ios) stacrux%stainfo(icrx)%antecc(3)
      IF(ios /= 0) CALL readcrux_exit(2,icrx,line)
    ENDIF
  END DO

  ! TYPE 003
  DO ii = map(2)%pos2 + 1, map(3)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO
  DO ii = map(3)%pos1, map(3)%pos2
    icrx = ii - map(3)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ(line,'(A16,4x,2X,A3,2X,A40,2X,A60)') &
      stacrux%staprob(icrx)%stanam, &
      stacrux%staprob(icrx)%flg, &
      datstr, &
      stacrux%staprob(icrx)%remark
    CALL st2tim(1, 2, datstr, stacrux%staprob(icrx)%timint%t )
  END DO

  ! TYPE 004
  DO ii = map(3)%pos2 + 1, map(4)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO
  DO ii = map(4)%pos1, map(4)%pos2
    icrx = ii - map(4)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ(line,'( 2(A16,4x,2X), 6(F8.5,2X) )') &
      stacrux%coovel(icrx)%stanam(1), &
      stacrux%coovel(icrx)%stanam(2), &
      (stacrux%coovel(icrx)%constr(kk),kk=1,6)
!!!
!!! for new STA format:
!!!       READ (line,  &
!!!          '(2(A16,6X),I3,2X,A40,2X,A8,2X,A3,2X,3(F11.5,2X),A3,2X,3(F8.5,2X),3(F8.5,2X),A24)')  &
!!!          (stacrux%staRelPar(icrx)%stanam(kk), kk=1,2),  &
!!!          stacrux%staRelPar(icrx)%flg, datstr,           &
!!!          stacrux%staRelPar(icrx)%parTyp,                &
!!!          stacrux%staRelPar(icrx)%sys(1),                &
!!!          (stacrux%staRelPar(icrx)%locTie(kk), kk=1,3),  &
!!!          stacrux%staRelPar(icrx)%sys(2),                &
!!!          (stacrux%staRelPar(icrx)%constr(kk), kk=1,3),  &
!!!          (stacrux%staRelPar(icrx)%correl(kk), kk=1,3),  &
!!!          stacrux%staRelPar(icrx)%remark
!!!
!!!       CALL st2tim(1, 2, datstr, stacrux%staRelPar(icrx)%timWin%t )
!!!
!!!       stacrux%staRelPar(icrx)%applied = .FALSE.

  END DO

  ! TYPE 005
  DO ii = map(4)%pos2 + 1, map(5)%pos1 - 1
    READ(lfnloc,'(A)',END=999) line
  END DO
  DO ii = map(5)%pos1, map(5)%pos2
    icrx = ii - map(5)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ(line,'(A16,4x,2X,A3,2X,A40,2X,A20,2X,A24)') &
      stacrux%statype(icrx)%stanam, &
      stacrux%statype(icrx)%flg, &
      datstr, &
      stacrux%statype(icrx)%markertype, &
      stacrux%statype(icrx)%remark
    CALL st2tim(1, 2, datstr, stacrux%statype(icrx)%timint%t )
  END DO

  CLOSE (lfnloc)

  RETURN
  999 CONTINUE
  WRITE(lfnerr,*) ' *** SR READCRUX101: Error reading STAINFO file : ',filename
  CALL exitrc(2)

END SUBROUTINE readcrux101

! -------------------------------------------------------------------------

SUBROUTINE readcrux_exit(ityp,icrx,line)

! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr
  USE s_exitrc

  INTEGER(i4b)            :: ityp
  INTEGER(i4b)            :: icrx
  CHARACTER(LEN=251)      :: line

  WRITE(lfnerr,"(/,' *** SR READCRUX101: Error reading STAINFO file', &
               & /,'                  Type   : ',I3, &
               & /,'                  Number : ',I3, &
               & /,'                  Line   : ',/,A &
               & /)") ityp,icrx,TRIM(line)
  CALL exitrc(2)

END SUBROUTINE readcrux_exit

END MODULE
