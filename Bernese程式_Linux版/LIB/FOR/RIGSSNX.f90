MODULE s_RIGSSNX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rigssnx(filename,stacrux,redInf)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads a SINEX file and store the coordinates,
!             receiver, antenna and eccentricity information
!
! Author:     H.Bock
!
! Created:    24-Jun-1999
!
! Changes:    09-Jan-2003 PS: Modified call to LFNUM.inc and definition of
!                             t_stacrux
!             17-Feb-2003 DL: Considered case of last receiver end time not
!                             being equal to 00:000:00000 in SINEX-file;
!                             modified setting of start time
!             18-Feb-2003 DL: eccentricities added to check for same info
!             19-Feb-2003 DL: check for character part in receiver and antenna
!                             number
!             21-May-2003 DL: removed call to LFNUM.inc
!             30-Aug-2003 HU: Error output adapted, filename has open length
!                             Read station description
!                             Write name of sinex file to remark
!             16-Oct-2003 HU: Read receiver/antenna info in any order
!             29-Oct-2003 HB: Correct format statement
!             04-Dec-2003 CU: Check indices or size of arrays before access
!                             to arrays
!             28-Jun-2004 RD: Use MAXCRD from M_MAXDIM for MAXSTA
!             09-Aug-2005 SS: Information reduction
!             25-May-2007 RD: Special case: receiver but no antenna anymore
!             04-Mar-2008 RD: Allow for comments inbetween data records
!             16-Jul-2008 DT: Check whether all blocks are found;
!                             set receiver / antenna for SLR
!             20-Nov-2008 DT: Technique-specific reading (GNSS, SLR)
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             04-Oct-2010 SL: use m_bern with ONLY, f_extrint added
!             11-Oct-2010 SL: ignore blanks between eccentricities
!             26-Oct-2010 SL: three truncation bugs corrected, techn from obsTyp
!             14-Jan-2011 SL: sort lists
!             29-Sep-2011 SL: use maxsta instead of maxcrd
!             11-Oct-2011 RD: Handle overlapping intervals
!             01-Dec-2011 SL: skip illegal intervals
!             27-Apr-2012 RD: Nullify all pointers
!             17-Mar-2013 SL: Reset serial numbers if redInf(1) or (2)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

!  USE m_stacrux
  USE m_bern,   ONLY: i4b, r8b, lfnLoc, lfnErr
  USE m_maxdim, ONLY: maxsta
  USE d_stacrx, ONLY: t_stacrux, undef_i
  USE f_extrint
  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_opnerr
  USE s_stripdir
  USE s_sindat
  USE s_exitrc
  USE s_upperc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)   :: filename  ! SINEX file
  INTEGER(i4b), &
    DIMENSION(3)     :: redInf    ! Information reduction options
! output:
  TYPE(t_stacrux)    :: stacrux   ! information of SINEX file

! Local Variables
! ---------------
  CHARACTER(LEN=180) :: line
  CHARACTER(LEN=4)   :: siteCode1, siteCode2, siteCode3
  CHARACTER(LEN=2)   :: pointCode1, pointCode2, pointCode3
  CHARACTER(LEN=4)   :: solID1, solID2, solID3
  CHARACTER(LEN=1)   :: technique1, technique2, technique3
  CHARACTER(LEN=12)  :: startTime1, startTime2, startTime3
  CHARACTER(LEN=12)  :: endTime1, endTime2, endTime3

  CHARACTER(LEN=4)   :: siteCode1_0, siteCode2_0, siteCode3_0
  CHARACTER(LEN=2)   :: pointCode1_0, pointCode2_0, pointCode3_0
  CHARACTER(LEN=4)   :: solID1_0, solID2_0, solID3_0
  CHARACTER(LEN=1)   :: technique1_0, technique2_0, technique3_0
  CHARACTER(LEN=12)  :: startTime1_0, startTime2_0, startTime3_0
  CHARACTER(LEN=12)  :: endTime1_0, endTime2_0, endTime3_0

  CHARACTER(LEN=20)  :: recsta, recsta_0
  CHARACTER(LEN=20)  :: antsta, antsta_0
  CHARACTER(LEN=22)  :: descr
  CHARACTER(LEN=24)  :: snxfile                       ! name of sinex file
  CHARACTER(LEN=9)   :: domes
  CHARACTER(LEN=3)   :: refsys, refsys_0
  CHARACTER(LEN=5)   :: recnumc, antnumc              ! rec/ant serial numb
  CHARACTER(LEN=5)   :: recnumc_0, antnumc_0
  CHARACTER(LEN=11)  :: recfirm, recfirm_0            ! receiver firmware
  CHARACTER(LEN=1)   :: obstyp,obstyp0                ! observation type
  CHARACTER(LEN=8)   :: cdpsod

  INTEGER(i4b), PARAMETER :: numsin = 3
  INTEGER(i4b)            :: nlinsin
  INTEGER(i4b)            :: ista
  INTEGER(i4b)            :: recnum
  INTEGER(i4b)            :: antnum
  INTEGER(i4b)            :: ii, jj, kk, ll, mm, nn, oo, pp
  INTEGER(i4b)            :: q
  INTEGER(i4b)            :: ios,iac

  REAL(r8b)               :: s1, s2, s3, e1, e2, e3
  REAL(r8b)               :: ecc1, ecc2, ecc3
  REAL(r8b)               :: ecc1_0, ecc2_0, ecc3_0

!!!  CHARACTER(LEN=20), DIMENSION(maxSta)      :: station
  CHARACTER(LEN=26), DIMENSION(maxSta)      :: station
  CHARACTER(LEN=22), DIMENSION(maxSta)      :: descri

  CHARACTER(LEN=180), DIMENSION(:), POINTER :: receiv
  CHARACTER(LEN=180), DIMENSION(:), POINTER :: antenna
  CHARACTER(LEN=180), DIMENSION(:), POINTER :: eccent

  INTEGER(i4b),       DIMENSION(:), POINTER :: recFlg
  INTEGER(i4b),       DIMENSION(:), POINTER :: antFlg
  INTEGER(i4b),       DIMENSION(:), POINTER :: eccFlg

! Local Types
! -----------
  TYPE t_stacruxmap
    INTEGER(i4b)     :: pos1
    INTEGER(i4b)     :: pos2
  END TYPE t_stacruxmap

  TYPE(t_stacruxmap), DIMENSION(numsin) :: mapsin

  obstyp = ' '

! Nullify pointers
! ----------------
  NULLIFY(eccFlg)
  NULLIFY(antFlg)
  NULLIFY(eccent)
  NULLIFY(recFlg)
  NULLIFY(antenna)
  NULLIFY(receiv)

! Name of snx file
! ----------------
  line=filename
  CALL stripdir(line)
  snxfile=TRIM(line)

! Get the information from the sinex-file
! ---------------------------------------
! TYPE 002
! --------
! Open the input file
! -------------------
  CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,'rigssnx')

  DO ii = 1, numsin
      mapsin(ii)%pos1 = 0
      mapsin(ii)%pos2 = 0
  END DO

! Map the SINEX file
! ------------------
  nlinsin = 0
  Loop_rec: DO
    nlinsin = nlinsin + 1
    READ (lfnloc,'(A)',IOSTAT=ios) line
    IF(ios<0) EXIT Loop_rec
    IF(ios>0) GOTO 999

  ! Check first line for observation technique
  ! ------------------------------------------
    IF(line(1:5) == '%=SNX') THEN
      obstyp      = line(59:59)                            ! Observation code
    ENDIF

    IF(line == '+SITE/RECEIVER') THEN
      mapsin(1)%pos1 = nlinsin
      mapsin(1)%pos2 = nlinsin
      DO
        nlinsin = nlinsin + 1
        mapsin(1)%pos1 = nlinsin
        READ(lfnloc,'(A)',END=999) line
        IF(line(1:1) /= '*') EXIT
      ENDDO
      DO
        nlinsin = nlinsin + 1
        READ(lfnloc,'(A)',END=999) line
        IF(line == '-SITE/RECEIVER') EXIT
        mapsin(1)%pos2 = nlinsin
      END DO
    END IF

    IF(line == '+SITE/ANTENNA') THEN
      mapsin(2)%pos1 = nlinsin
      mapsin(2)%pos2 = nlinsin
      DO
        nlinsin = nlinsin + 1
        mapsin(2)%pos1 = nlinsin
        READ(lfnloc,'(A)',END=999) line
        IF(line(1:1) /= '*') EXIT
      ENDDO
      DO
        nlinsin = nlinsin + 1
        READ(lfnloc,'(A)',END=999) line
        IF(line == '-SITE/ANTENNA') EXIT
        mapsin(2)%pos2 = nlinsin
      END DO
    END IF

    IF(line == '+SITE/ECCENTRICITY') THEN
      mapsin(3)%pos1 = nlinsin
      mapsin(3)%pos2 = nlinsin
      DO
        nlinsin = nlinsin + 1
        mapsin(3)%pos1 = nlinsin
        READ(lfnloc,'(A)',END=999) line
        IF(line(1:1) /= '*') EXIT
      ENDDO
      DO
        nlinsin = nlinsin + 1
        READ (lfnloc,'(A)',END=999) line
        IF(line == '-SITE/ECCENTRICITY') EXIT
        mapsin(3)%pos2 = nlinsin
      END DO
    END IF
  END DO Loop_rec

! Get technique string from obstyp
! --------------------------------
  obstyp0 = obstyp
  IF(obstyp == 'C') THEN
    stacrux%technique = 'Combined techniques'
    obstyp0 = ' '
  ELSEIF(obstyp == 'D') THEN
    stacrux%technique = 'DORIS'
  ELSEIF(obstyp == 'L') THEN
    stacrux%technique = 'SLR'
    redInf(:) = 1
  ELSEIF(obstyp == 'M') THEN
    stacrux%technique = 'LLR'
  ELSEIF(obstyp == 'P') THEN
    stacrux%technique = 'GNSS'
  ELSEIF(obstyp == 'R') THEN
    stacrux%technique = 'VLBI'
  ELSE
    WRITE(lfnerr,*) ' *** SR RIGSSNX: Error in decoding observation code'
    CALL exitrc(2)
  ENDIF

! Check whether all blocks are found
! ----------------------------------
  IF(mapsin(1)%pos1 == 0 .AND. stacrux%technique == 'GNSS' .AND. &
     obstyp /= 'L' ) THEN
    WRITE(lfnerr,"(/,' *** SR RIGSSNX: Block SITE/RECEIVER not found in SINEX file:', /, &
                   & '                 ',A,/)") TRIM(filename)
  END IF

  IF(mapsin(2)%pos1 == 0 .AND. stacrux%technique == 'GNSS' .AND. &
     obstyp /= 'L' ) THEN
    WRITE(lfnerr,"(/,' *** SR RIGSSNX: Block SITE/ANTENNA not found in SINEX file:', /, &
                   & '                 ',A,/)") TRIM(filename)
  END IF

  IF(mapsin(3)%pos1 == 0 ) THEN
    WRITE(lfnerr,"(/,' *** SR RIGSSNX: Block SITE/ECCENTRICITY not found in SINEX file:', /, &
                   & '                 ',A,/)") TRIM(filename)
  END IF

! List of all stations
! --------------------
  REWIND(lfnloc)
  List_Loop: DO
    line = TRIM(nextline(lfnloc,0))
    IF(line      == '' ) EXIT  List_Loop
    IF(line(1:1) /= '+') CYCLE List_Loop

! Read SITE/ID Block
! ------------------
    IF(line == '+SITE/ID') THEN
      ista = 0
      Stat_Loop: DO
        line = TRIM(nextline(lfnloc,0))
        IF(line(1:1) == '-') CYCLE List_Loop
        IF(line(1:1) == '*') CYCLE Stat_Loop
        READ(line,'(1x,a4, 1x,a2, 1x,a9,3x,a22)') &
             siteCode1, pointCode1, domes, descr
        ista = ista + 1
        IF(ista > maxSta) THEN
          WRITE(lfnerr,"(/,' *** SR RIGSSNX: Too many stations ', &
                          & 'in SINEX file ',A,/)") TRIM(filename)
          CALL exitrc(2)
        END IF

        IF(domes == '     M   ') domes = ''

        IF(stacrux%technique == 'GNSS' .OR. &
           (stacrux%technique == 'Combined techniques' .AND. &
            line(20:20) == 'P')) THEN
          WRITE(station(ista),'(A4,1X,2A,1X,A9)') siteCode1,pointCode1,domes
          IF(obstyp0 == ' '        ) obstyp0 = line(20:20)
          IF(obstyp0 /= line(20:20)) obstyp0 = 'C'

        ELSEIF(stacrux%technique == 'SLR ' .OR. &
            (stacrux%technique == 'Combined techniques' .AND. &
             line(20:20) == 'L')) THEN
          READ(line, '(80x, a8)') cdpsod
          WRITE(station(ista),'(A4,1X,2A,1X,A9,1X,A8)') siteCode1,pointCode1,domes,cdpsod
          IF(obstyp0 == ' '        ) obstyp0 = line(20:20)
          IF(obstyp0 /= line(20:20)) obstyp0 = 'C'

        END IF

        descri(ista)=descr

      END DO Stat_Loop
    END IF
  END DO List_Loop

  IF(stacrux%technique == 'Combined techniques' .AND. &
     obstyp0 /= 'C' .AND. obstyp0 /= ' ' ) THEN
    IF(obstyp0 == 'D') THEN
      stacrux%technique = 'DORIS'
    ELSEIF(obstyp0 == 'L') THEN
      stacrux%technique = 'SLR'
      redInf(:) = 1
    ELSEIF(obstyp0 == 'M') THEN
      stacrux%technique = 'LLR'
    ELSEIF(obstyp0 == 'P') THEN
      stacrux%technique = 'GNSS'
    ELSEIF(obstyp0 == 'R') THEN
      stacrux%technique = 'VLBI'
    ELSE
      WRITE(lfnerr,*) ' *** SR RIGSSNX: Error in decoding observation code'
      CALL exitrc(2)
    ENDIF
  ENDIF

! Allocate memory for lists
! -------------------------
  ii = mapsin(1)%pos2-mapsin(1)%pos1+1
  jj = mapsin(2)%pos2-mapsin(2)%pos1+1
  oo = mapsin(3)%pos2-mapsin(3)%pos1+1

  IF(mapsin(1)%pos1 == 0) ii=ista
  IF(mapsin(2)%pos1 == 0) jj=ista
  IF(mapsin(3)%pos1 == 0) oo=ista

  ALLOCATE(receiv(ii), STAT=iac)
  CALL alcerr(iac, 'receiv',(/ii/),'rigssnx')
  receiv = ' '
  ALLOCATE(recFlg(ii), STAT=iac)
  CALL alcerr(iac, 'recFlg',(/ii/),'rigssnx')
  recFlg = 0

  ALLOCATE(antenna(jj), STAT=iac)
  CALL alcerr(iac, 'antenna',(/jj/),'rigssnx')
  antenna = ' '
  ALLOCATE(antFlg(jj), STAT=iac)
  CALL alcerr(iac, 'antFlg',(/jj/),'rigssnx')
  antFlg = 0

  ALLOCATE(eccent(oo), STAT=iac)
  CALL alcerr(iac, 'eccent',(/oo/),'rigssnx')
  eccent = ' '
  ALLOCATE(eccFlg(oo), STAT=iac)
  CALL alcerr(iac, 'eccFlg',(/oo/),'rigssnx')
  eccFlg = 0

  ALLOCATE(stacrux%stainfo(ii+jj+oo),STAT=iac)
  CALL alcerr(iac, 'stacrux%stainfo',(/ii+jj+oo/),'rigssnx')

! Copy SITE/RECEIVER block into array
! -----------------------------------
  REWIND lfnloc

  IF(mapsin(1)%pos1 /= 0 ) THEN
    DO ii = 1, mapsin(1)%pos1-1
      READ(lfnloc,'(A)',END=999) line
    ENDDO
    jj = 0
    DO ii = mapsin(1)%pos1, mapsin(1)%pos2
      jj = jj + 1
      READ(lfnloc,'(A)', END=999) receiv(jj)
      IF(receiv(jj)(1:1) == '*') THEN
        receiv(jj) = ' '
        jj = jj - 1
      ENDIF
    ENDDO
    DO ii = mapsin(1)%pos2+1, mapsin(2)%pos1-1
      READ(lfnloc,'(A)',END=999) line
    ENDDO
  ELSE
    DO ii=1, ista
      receiv(ii)(2:8) = station(ii)(1:7)
      IF(obstyp=='L') receiv(ii)(43:62)='SLR                 '
    ENDDO
    IF(mapsin(2)%pos1 /= 0 ) THEN
      DO ii = 1, mapsin(2)%pos1-1
        READ(lfnloc,'(A)',END=999) line
      ENDDO
    ENDIF
  ENDIF

! Copy SITE/ANTENNA block into array
! ----------------------------------
  oo = 0
  IF(mapsin(2)%pos1 /= 0 ) THEN
    DO ii = mapsin(2)%pos1,mapsin(2)%pos2
      oo = oo + 1
      READ(lfnloc,'(A)', END=999) antenna(oo)
      IF(antenna(oo)(1:1) == '*') THEN
        antenna(oo) = ' '
        oo = oo - 1
      ENDIF
    ENDDO
    DO ii = mapsin(2)%pos2+1,mapsin(3)%pos1-1
      READ(lfnloc,'(A)',END=999) line
    ENDDO
  ELSE
    DO ii=1,ista
      antenna(ii)(2:8) = station(ii)(1:7)
      IF(obstyp=='L') antenna(ii)(43:62)='SLR                 '
    ENDDO
    IF(mapsin(3)%pos1 /= 0 ) THEN
      DO ii = mapsin(2)%pos2+1,mapsin(3)%pos1-1
        READ(lfnloc,'(A)',END=999) line
      ENDDO
    ENDIF
  ENDIF

! Copy SITE/ECCENTRICITY block into array
! ---------------------------------------
  oo = 0
  DO ii = mapsin(3)%pos1,mapsin(3)%pos2
    oo = oo + 1
    READ(lfnloc,'(A)', END=999) eccent(oo)
    IF(eccent(oo)(1:1) == '*') THEN
      eccent(oo) = ' '
      oo = oo - 1
    ENDIF
  ENDDO

  CLOSE (lfnloc)

! Loop over all stations
! ----------------------
  nn = 1
  ll = 1
  pp = 1
  mm = 1
  IF(receiv(1)(1:1)  == '*') ll = 2
  IF(antenna(1)(1:1) == '*') pp = 2
  IF(eccent(1)(1:1)  == '*') THEN
    mm = 2
    IF(eccent(2)(1:1) == '*') THEN
      mm = 3
    ENDIF
  ENDIF

  CALL rigssnx_sort(receiv, recFlg)
  CALL rigssnx_sort(antenna,antFlg)
  CALL rigssnx_sort(eccent, eccFlg)

  Loop_Station: DO ii = 1,ista
    q = 1

    DO WHILE(receiv(ll)(2:8)==station(ii)(1:7) .AND. &
            antenna(pp)(2:8)==station(ii)(1:7) .AND. &
             eccent(mm)(2:8)==station(ii)(1:7))
      recnum = undef_i
      antnum = undef_i

! Read the information
! --------------------
      ! Receiver information
      READ(receiv(ll),'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,        &
        &       1x,a20, 1x,a5, 1x,a11)')                                   &
        siteCode1, pointCode1, solID1, technique1, startTime1, endTime1,   &
        recsta, recnumc, recfirm

      ! Receiver information in case of overlapping intervals
      kk = 0
      DO WHILE (recFlg(ll+kk) > 0 .AND. recFlg(ll+kk) < 3)
        kk = kk + 1
        READ(receiv(ll+kk),'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,   &
          &       1x,a20, 1x,a5, 1x,a11)')                                 &
          siteCode1_0, pointCode1_0, solID1_0, technique1_0,               &
          startTime1_0, endTime1_0, recsta_0, recnumc_0, recfirm_0

        IF ( (recSta == recSta_0) .AND. &
             (recnumc == recnumc_0 .OR. redInf(1) == 1) ) THEN
          endTime1 = endTime1_0
        ELSE
          WRITE(lfnerr,'(/,3A,/,17X,A,/)') &
          ' ### SR RIGSSNX: Records from station "',siteCode1,'" are skipped', &
                           'because of overlapping intervals'
          DO WHILE (recFlg(ll+kk) /= 3)
            kk = kk + 1
          ENDDO
!!        ENDIF

!!        IF (recFlg(ll+kk) == 3) THEN
          ll = ll + kk + 1
          DO WHILE (antenna(pp)(2:8) /= receiv(ll)(2:8) .AND. &
                    pp < SIZE(antenna))
            pp = pp + 1
          ENDDO
          DO WHILE (eccent(mm)(2:8) /= receiv(ll)(2:8) .AND. &
                    mm < SIZE(eccent))
            mm = mm + 1
          ENDDO
          CYCLE Loop_Station
        ENDIF
      ENDDO
      ll = ll + kk

      ! Antenna information
      READ(antenna(pp),'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,       &
        &       1x,a20, 1x,a5)')                                           &
        siteCode2, pointCode2, solID2, technique2, startTime2, endTime2,   &
        antsta, antnumc

      ! Antenna information in case of overlapping intervals
      kk = 0
      DO WHILE (antFlg(pp+kk) > 0 .AND. antFlg(pp+kk) < 3)
        kk = kk + 1
        READ(antenna(pp+kk),'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,  &
          &       1x,a20, 1x,a5)')                                         &
          siteCode2_0, pointCode2_0, solID2_0, technique2_0,               &
          startTime2_0, endTime2_0, antsta_0, antnumc_0

        IF ( ( (antSta == antSta_0 .AND. redInf(3) /= 1) .OR. &
               (antSta(1:16) == antSta_0(1:16) .AND. redInf(3) == 1) ) .AND. &
             (antnumc == antnumc_0 .OR. redInf(2) == 1) ) THEN
          endTime2 = endTime2_0
        ELSE
          WRITE(lfnerr,'(/,3A,/,17X,A,/)') &
          ' ### SR RIGSSNX: Records from station "',siteCode2,'" are skipped', &
                           'because of overlapping intervals'
          DO WHILE (antFlg(pp+kk) /= 3)
            kk = kk + 1
          ENDDO
!!        ENDIF

!!        IF (antFlg(pp+kk) == 3) THEN
          pp = pp + kk + 1
          DO WHILE (receiv(ll)(2:8) /= antenna(pp)(2:8) .AND. &
                    ll < SIZE(receiv))
            ll = ll + 1
          ENDDO
          DO WHILE (eccent(mm)(2:8) /= antenna(pp)(2:8) .AND. &
                    mm < SIZE(eccent))
            mm = mm + 1
          ENDDO
          CYCLE Loop_Station
        ENDIF
      ENDDO
      pp = pp + kk

      ! Eccentricity information
      READ(eccent(mm),'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,        &
        &       1x,a3, 3(f9.4))')                                          &
        siteCode3, pointCode3, solID3, technique3, startTime3, endTime3,   &
        refsys, ecc1, ecc2, ecc3

      ! Eccentricity information in case of overlapping intervals
      kk = 0
      DO WHILE (eccFlg(mm+kk) > 0 .AND. eccFlg(mm+kk) < 3)
        kk = kk + 1
        READ(eccent(mm+kk),'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,   &
          &       1x,a3, 3(f9.4))')                                        &
        siteCode3_0, pointCode3_0, solID3_0, technique3_0,                 &
        startTime3_0, endTime3_0, refsys_0, ecc1_0, ecc2_0, ecc3_0

        IF ( ecc1 == ecc1_0 .AND. ecc2 == ecc2_0 .AND. ecc3 == ecc3_0  ) THEN
          endTime3 = endTime3_0
        ELSE
          WRITE(lfnerr,'(/,3A,/,17X,A,/)') &
          ' ### SR RIGSSNX: Records from station "',siteCode3,'" are skipped', &
                           'because of overlapping intervals'
          DO WHILE (eccFlg(mm+kk) /= 3)
            mm = mm + 1
          ENDDO
!!        ENDIF

!!        IF (eccFlg(mm+kk) == 3) THEN
          mm = mm + kk + 1
          DO WHILE (receiv(ll)(2:8) /= eccent(mm)(2:8) .AND. &
                    ll < SIZE(receiv))
            ll = ll + 1
          ENDDO
          DO WHILE (antenna(pp)(2:8) /= eccent(mm)(2:8) .AND. &
                    mm < SIZE(antenna))
            pp = pp + 1
          ENDDO
          CYCLE Loop_Station
        ENDIF
      ENDDO
      mm = mm + kk

      IF(nn >= SIZE(stacrux%stainfo)) THEN
        WRITE(lfnerr,"(/,' *** SR RIGSSNX: Too many requests in ', &
          & 'SINEX file ',A,/)") TRIM(filename)
        EXIT Loop_Station
      ENDIF

! Check for character part of recnumc and antnumc
! -----------------------------------------------
      IF(recnumc /= '-----' .AND. recnumc /= '     ') recnum = extrInt(recnumc)
      IF(antnumc /= '-----' .AND. antnumc /= '     ') antnum = extrInt(antnumc)

      IF(redInf(1) == 1) THEN
        recnum  = undef_i
        recnumc = ''
      ENDIF
      IF(redInf(2) == 1) THEN
        antnum  = undef_i
        antnumc = ''
      ENDIF
      IF(redInf(3) == 1) THEN
        antsta(17:20) = '    '
      ENDIF

! Writing station info
! --------------------
      IF(recnumc == '-----') recnumc = ''
      IF(antnumc == '-----') antnumc = ''

      WRITE(stacrux%stainfo(nn)%stanam,'(A5,A9)') &
                                              station(ii)(1:5),station(ii)(8:16)
      stacrux%stainfo(nn)%flg    = '001'
      stacrux%stainfo(nn)%recnam = recsta
      stacrux%stainfo(nn)%recser = recnumc
      stacrux%stainfo(nn)%recnum = recnum
      stacrux%stainfo(nn)%antnam = antsta
      stacrux%stainfo(nn)%antser = antnumc
      stacrux%stainfo(nn)%antnum = antnum

      IF(stacrux%technique == 'SLR ') THEN
        WRITE(stacrux%stainfo(nn)%cdpsod, '(A8)') station(ii)(19:26)
      ENDIF

      IF(refsys == 'UNE') THEN
        stacrux%stainfo(nn)%antecc(1) = ecc2
        stacrux%stainfo(nn)%antecc(2) = ecc3
        stacrux%stainfo(nn)%antecc(3) = ecc1
      ENDIF
      stacrux%stainfo(nn)%descri = descri(ii)
      stacrux%stainfo(nn)%remark = snxfile

! Start time
      IF(startTime1(1:2) /= '  ') THEN
        CALL sindat(1,s1,startTime1)
      ELSE
        IF(startTime2(1:2) /= '  ') THEN
          CALL sindat(1,s1,startTime2)  ! Set antenna time
        ELSEIF(startTime3(1:2) /= '  ') THEN
          CALL sindat(1,s1,startTime3)  ! Set eccentricity time
        ENDIF
      ENDIF

      IF(startTime2(1:2) /= '  ') THEN
        CALL sindat(1,s2,startTime2)
      ELSE
        IF(startTime1(1:2) /= '  ') THEN
          CALL sindat(1,s2,startTime1)  ! Set receiver time
        ELSEIF(startTime3(1:2) /= '  ') THEN
          CALL sindat(1,s2,startTime3)  ! Set eccentricity time
        ENDIF
      ENDIF

      IF(startTime3(1:2) /= '  ') THEN
        CALL sindat(1,s3,startTime3)
      ELSE
        IF(startTime1(1:2) /= '  ') THEN
          CALL sindat(1,s3,startTime1)  ! Set receiver time
        ELSEIF(startTime2(1:2) /= '  ') THEN
          CALL sindat(1,s3,startTime2)  ! Set antenna time
        ENDIF
      ENDIF

! End time
      IF(endTime1(1:2) /= '  ') THEN
        CALL sindat(2,e1,endTime1)
      ELSE
        IF(endTime2(1:2) /= '  ') THEN
          CALL sindat(2,e1,endTime2)  ! Set antenna time
        ELSEIF(startTime3(1:2) /= '  ') THEN
          CALL sindat(2,e1,endTime3)  ! Set eccentricity time
        ENDIF
      ENDIF

      IF(endTime2(1:2) /= '  ') THEN
        CALL sindat(2,e2,endTime2)
      ELSE
        IF(endTime1(1:2) /= '  ') THEN
          CALL sindat(2,e2,endTime1)  ! Set receiver time
        ELSEIF(endTime3(1:2) /= '  ') THEN
          CALL sindat(2,e2,endTime3)  ! Set eccentricity time
        ENDIF
      ENDIF

      IF(endTime3(1:2) /= '  ') THEN
        CALL sindat(2,e3,endTime3)
      ELSE
        IF(endTime1(1:2) /= '  ') THEN
          CALL sindat(2,e3,endTime1)  ! Set receiver time
        ELSEIF(endTime2(1:2) /= '  ') THEN
          CALL sindat(2,e3,endTime2)  ! Set antenna time
        ENDIF
      ENDIF

! Check for same information as previous one (only software has changed=>not
! interesting for Bernese)
! And setting of start time
! --------------------------------------------------------------------------
      IF(nn == 1) THEN
        IF(q == 1) THEN
          stacrux%stainfo(nn)%timint%t(1) = s1
        ELSEIF(q == 2) THEN
          stacrux%stainfo(nn)%timint%t(1) = s2
        ELSEIF(q == 3) THEN
          stacrux%stainfo(nn)%timint%t(1) = s3
        END IF
      ELSEIF(nn > 1) THEN
        IF(stacrux%stainfo(nn-1)%stanam==stacrux%stainfo(nn)%stanam .AND.&
           stacrux%stainfo(nn-1)%recnam==stacrux%stainfo(nn)%recnam .AND.&
           stacrux%stainfo(nn-1)%recnum==stacrux%stainfo(nn)%recnum .AND.&
           stacrux%stainfo(nn-1)%antnam==stacrux%stainfo(nn)%antnam .AND.&
           stacrux%stainfo(nn-1)%antnum==stacrux%stainfo(nn)%antnum .AND.&
           stacrux%stainfo(nn-1)%antecc(1)==stacrux%stainfo(nn)%antecc(1) .AND.&
           stacrux%stainfo(nn-1)%antecc(2)==stacrux%stainfo(nn)%antecc(2) .AND.&
           stacrux%stainfo(nn-1)%antecc(3)==stacrux%stainfo(nn)%antecc(3)) THEN
          nn = nn - 1
        ELSE
          IF(q == 1) THEN
            stacrux%stainfo(nn)%timint%t(1) = s1
          ELSEIF(q == 2) THEN
            stacrux%stainfo(nn)%timint%t(1) = s2
          ELSEIF(q == 3) THEN
            stacrux%stainfo(nn)%timint%t(1) = s3
          ENDIF
        ENDIF
      ENDIF

! If end time is 00:000:00000 check if there is another request for this
! station (sometimes wrong in IGS.SNX-file)
! -----------------------------------------
      IF(ll < SIZE(receiv)) THEN
        IF(e1 == 1d20 .AND. receiv(ll+1)(2:8) == station(ii)(1:7) .AND. &
           receiv(ll)(17:41)/=receiv(ll+1)(17:41)) THEN
          CALL sindat(1,e1,receiv(ll+1)(17:28))
        ELSEIF(pp < SIZE(antenna)) THEN
          IF(e2 == 1d20 .AND. antenna(pp+1)(2:8) == station(ii)(1:7) .AND.&
              antenna(pp)(17:41)/=antenna(pp+1)(17:41)) THEN
            CALL sindat(1,e2,antenna(pp+1)(17:28))
          ELSEIF(mm < SIZE(eccent)) THEN
            IF(e3 == 1d20 .AND. eccent(mm+1)(2:8) == station(ii)(1:7) .AND. &
               eccent(mm)(17:41)/=eccent(mm+1)(17:41)) THEN
              CALL sindat(1,e3,eccent(mm+1)(17:28))
            ENDIF
          ENDIF
        ENDIF
      ENDIF

! Setting of end time
! -------------------
      IF((e1 == e2) .AND. (e1 == e3)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e1
        q = 1
        nn = nn + 1
        ll = ll + 1
        pp = pp + 1
        mm = mm + 1
      ELSEIF((e1 < e2) .AND. (e1 < e3) .AND. (e1 > 1d0)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e1
        q = 1
        nn = nn + 1
        ll = ll + 1
        IF(ll > SIZE(receiv)) EXIT
        IF(e2 == 1d20 .AND. &
           e3 == 1d20 .AND. receiv(ll)(2:8)/=station(ii)(1:7)) THEN
          pp = pp + 1
          mm = mm + 1
        ENDIF
      ELSEIF((e1 == e2) .AND. (e1 < e3) .AND. (e1 > 1d0)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e1
        q = 1
        nn = nn + 1
        ll = ll + 1
        pp = pp + 1
      ELSEIF((e1 == e3) .AND. (e1 < e2) .AND. (e1 > 1d0)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e1
        q = 1
        nn = nn + 1
        ll = ll + 1
        mm = mm + 1
      ELSEIF((e2 == e3) .AND. (e2 < e1) .AND. (e2 > 1d0)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e2
        q = 2
        nn = nn + 1
        IF(e1 == 1d20 .AND. antenna(pp+1)(2:8)/=station(ii)(1:7) .AND. &
                            eccent (mm+1)(2:8)/=station(ii)(1:7)) ll = ll + 1
        pp = pp + 1
        mm = mm + 1
      ELSEIF((e2 < e3) .AND. (e2 < e1) .AND. (e2 > 1d0)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e2
        q = 2
        nn = nn + 1
        pp = pp + 1
      ELSEIF((e3 < e1) .AND. (e3 < e2) .AND. (e3 > 1d0)) THEN
        stacrux%stainfo(nn)%timint%t(2) = e3
        q = 3
        nn = nn + 1
        mm = mm + 1
      ENDIF

! Skip illegal intervals
      IF(stacrux%stainfo(nn-1)%timint%t(2) <= &
         stacrux%stainfo(nn-1)%timint%t(1)) THEN
        nn = nn - 1
      ENDIF

      IF(ll > SIZE(receiv)        ) EXIT
      IF(LEN_TRIM(receiv(ll)) == 0) EXIT
      IF(LEN_TRIM(receiv(ll)) == 0) EXIT
      IF(pp > SIZE(antenna)       ) EXIT

    ENDDO

  ENDDO Loop_Station

! Store number of station information
! -----------------------------------
  stacrux%ninfo = nn - 1
  DO ii = 1,stacrux%ninfo
    CALL upperc(stacrux%stainfo(ii)%stanam)
  END DO

! Deallocate arrays
! -----------------
  DEALLOCATE(receiv)
  DEALLOCATE(antenna)
  DEALLOCATE(eccent)
  DEALLOCATE(recFlg)
  DEALLOCATE(antFlg)
  DEALLOCATE(eccFlg)

  RETURN

999 CONTINUE
  WRITE(lfnerr,"(/,' *** SR RIGSSNX: Reading error in SINEX file ',A,/)") &
    TRIM(filename)

  CALL exitrc(2)

END SUBROUTINE rigssnx

! ------------------------------------------------------------------------------
!
! Name:       rigssnx_sort
!
! Purpose:    Sort lists from SINEX file according to name and startTime
!
! Authors:    S.Lutz
!
! Created:    14-Jan-2011
!
! Changes:    17-Aug-2011 SL: Use SINDAT to sort by time
!
! ------------------------------------------------------------------------------

SUBROUTINE rigssnx_sort(list,timFlg)

  USE m_bern,   ONLY: i4b,r8b,lfnerr
  USE s_sindat
  IMPLICIT NONE

! List of Parameters
! ------------------
! Input/Output
  CHARACTER(LEN=180),DIMENSION(:)           :: list      ! SINEX block
  INTEGER(i4b),      DIMENSION(:)           :: timFlg    ! 1: overlapp. interval

! Local Variables
! ---------------
  INTEGER(i4b)                              :: ii,jj     ! counter
  CHARACTER(LEN=180)                        :: help      ! help string
  REAL(r8b)                                 :: mjdii,mjdjj

! Sort according to name and startTime
! ------------------------------------
  DO ii=1,SIZE(list)
    CALL sindat(1,mjdii,list(ii)(17:28))
    DO jj=ii+1,SIZE(list)
      CALL sindat(1,mjdjj,list(jj)(17:28))
      ! sort name
      IF(list(ii)(2:8) .GT. list(jj)(2:8)) THEN
        help     = list(ii)
        list(ii) = list(jj)
        list(jj) = help
      ! sort startTime
      ELSEIF(list(ii)(2:8) .EQ. list(jj)(2:8) .AND. mjdii .GT. mjdjj) THEN
        help     = list(ii)
        list(ii) = list(jj)
        list(jj) = help
      ENDIF
    ENDDO
  ENDDO

! Test time interval
! ------------------
  DO ii=1,SIZE(list)-1
    jj = ii+1
    IF(list(ii)(2:8) .NE. list(jj)(2:8)) CYCLE
    CALL sindat(2,mjdii,list(ii)(30:41))
    CALL sindat(1,mjdjj,list(jj)(17:28))
    IF(mjdjj+1d0/86400d0 < mjdii) THEN
      WRITE(lfnerr,'(/,A,/,2(17X,A,/))') &
        ' ### SR RIGSSNX: Wrong time intervals in SINEX files', &
        TRIM(list(ii)),TRIM(list(jj))
      IF(mjdii /= 1d20 .OR. (mjdii == 1d20 .AND. mjdjj == 0d0) ) THEN
        IF(timFlg(ii) == 0) timFlg(ii) = 1
        IF(timFlg(ii) == 3) timFlg(ii) = 2
        IF(timFlg(jj) == 0) timFlg(jj) = 3
      ENDIF
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE rigssnx_sort

END MODULE
