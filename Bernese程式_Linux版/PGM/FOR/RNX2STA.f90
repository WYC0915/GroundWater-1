
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM rnx2sta

! -------------------------------------------------------------------------
! Purpose:    Generate a station information table from RINEX files
!
! Author:     R. Dach
!
! Created:    25-Feb-2003
!
! Changes:    23-Apr-2003 HU: Nullify local pointers
!             15-May-2003 AJ: Initialize structure
!             30-Jun-2003 HB: Adopt to changes in SR R2RDOH (SPACEBORNE)
!             10-Jul-2003 RD: Extract also marker type information
!             17-Sep-2003 HU: Remark modified to show full filename
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             29-Apr-2004 HB: Initialize rnxInfo%stainfo%descr
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON
!             18-Jul-2008 PW: No negative antenna values allowed in STA info
!                             file
!             07-Aug-2009 RD: Adapt pgm-out to change from 17-Sep-2003
!             23-Sep-2010 RD: Enable CPU counter
!             05-Oct-2010 SL: use m_bern with ONLY, STAINFO vers. 1.01, MRKNUM
!             18-Jan-2011 SL: maxtyp from d_rinex3
!             27-May-2011 SL: readkeys call corrected
!             01-Dec-2011 SL: new title string for pritit
!             24-Oct-2012 SL: MAXCOM 10->150
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, keyValueLength, &
                      lfn001, lfnErr, lfnPrt
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsat
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: filTitle
  USE d_stacrx, ONLY: t_stacrux, t_staInfo, t_staType, &
                      undef_c,undef_i,undef_e, &
                      init_stacrux
  USE d_rinex3,  ONLY: maxtyp
  USE f_extrint
  USE s_gtfile2
  USE s_alcerr
  USE s_opnfil
  USE s_r2rdoh
  USE s_pritit
  USE s_readinpf
  USE s_opnerr
  USE s_prfile
  USE s_readkeys
  USE s_stripdir
  USE s_selrenam
  USE s_writcrux
  USE s_exitrc
  USE s_ckoptb
  USE s_opnsys
  USE s_defcon
  USE s_ckoptl
  USE s_gtflna
  IMPLICIT NONE

! Parameters
! ----------
  CHARACTER(LEN=7), PARAMETER :: pgName = 'RNX2STA'
  INTEGER(i4b),     PARAMETER :: maxcom = 150      ! Max.number of comment lines

! Variables
! ---------
  TYPE(t_staCrux)                                       :: rnxInfo
  TYPE(t_staInfo)                                       :: hlpInfo
  TYPE(t_staType)                                       :: hlpType

  CHARACTER(LEN=fileNameLength), DIMENSION(:,:),POINTER :: rnxFil
  CHARACTER(LEN=fileNameLength)                         :: filNam
  CHARACTER(LEN=fileNameLength)                         :: staFil
  CHARACTER(LEN=keyValueLength), DIMENSION(:),  POINTER :: keyValue
  CHARACTER(LEN=80),           DIMENSION(maxcom)        :: coment  ! r2rdoh
  CHARACTER(LEN=60)                                     :: stanam  ! r2rdoh
  CHARACTER(LEN=40)                                     :: agency  ! r2rdoh
  CHARACTER(LEN=20)                                     :: prgnam  ! r2rdoh
  CHARACTER(LEN=20)                                     :: runby   ! r2rdoh
  CHARACTER(LEN=20)                                     :: rcvers  ! r2rdoh
  CHARACTER(LEN=40)                                     :: anttyp  ! r2rdoh
  CHARACTER(LEN=40)                                     :: rectyp  ! r2rdoh
  CHARACTER(LEN=20)                                     :: oprnam  ! r2rdoh
  CHARACTER(LEN=40)                                     :: stanum  ! r2rdoh
  CHARACTER(LEN= 9)                                     :: crdate  ! r2rdoh
  CHARACTER(LEN= 5)                                     :: crtime  ! r2rdoh
  CHARACTER(LEN= 2),           DIMENSION(maxtyp)        :: obstyp  ! r2rdoh

  INTEGER(i4b)                                          :: iFil
  INTEGER(i4b)                                          :: nFil
  INTEGER(i4b)                                          :: iRinex
  CHARACTER(LEN=3)                                      :: iFlag1, iFlag2
  INTEGER(i4b)                                          :: iRadom
  INTEGER(i4b)                                          :: iMarkn
  INTEGER(i4b)                                          :: iRecsn, iAntsn
  INTEGER(i4b)                                          :: irCode
  INTEGER(i4b)                                          :: irc,ios
  INTEGER(i4b)                                          :: nCom    ! r2rdoh
  INTEGER(i4b)                                          :: nwlsat  ! r2rdoh
  INTEGER(i4b),                DIMENSION(2)             :: iwlfac  ! r2rdoh
  INTEGER(i4b)                                          :: nSatel  ! r2rdoh
  INTEGER(i4b),                DIMENSION(maxsat)        :: numsat  ! r2rdoh
  INTEGER(i4b),                DIMENSION(3,maxsat)      :: iwlsat  ! r2rdoh
  INTEGER(i4b),                DIMENSION(maxsat,maxtyp) :: numobs  ! r2rdoh
  INTEGER(i4b)                                          :: irunit  ! r2rdoh
  INTEGER(i4b)                                          :: numtyp  ! r2rdoh
  INTEGER(i4b)                                          :: ianten  ! r2rdoh
  INTEGER(i4b)                                          :: ideltt  ! r2rdoh
  INTEGER(i4b)                                          :: irxvrs  ! r2rdoh

  REAL(r8b),                   DIMENSION(3)             :: posxyz  ! r2rdoh
  REAL(r8b),                   DIMENSION(3,3)           :: posecc  ! r2rdoh
  REAL(r8b)                                             :: tfirst  ! r2rdoh
  REAL(r8b)                                             :: tlast   ! r2rdoh

  LOGICAL                                               :: sorted

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)


! Nullify pointers
! ----------------
  CALL init_stacrux(rnxInfo)
  NULLIFY(rnxFil)
  NULLIFY(keyValue)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files and constants
! ---------------------------------
  CALL opnsys
  CALL defcon(0)

! Write title and file list
! -------------------------
  CALL pritit(pgName,'Extract station information from RINEX headers')

  irCode = 0
  CALL ckoptb(1,(/ 'RADIO_O','RADIO_S' /), pgName, &
              'Type of RINEX input files',irCode,  &
              result1=iRinex)

  IF (iRinex == 1) THEN
    CALL gtfile2('RXOFILE',1,nFil,rnxFil)
    CALL prfile ('RXOFILE', &
                 'RINEX file for extracting the station information',1)
  ELSE
    CALL gtfile2('SMTFILE',1,nFil,rnxFil)
    CALL prfile ('SMTFILE', &
                 'RINEX file for extracting the station information',1)
  ENDIF

! Read other input options
! ------------------------
  CALL readKeys('FLG1',keyValue,irc)
  CALL ckoptl(1,'FLG1',keyValue,pgName, &
              'Flag for TYPE 001',irc,irCode, &
              maxLength=3,maxVal=1,result1=iFlag1)

  CALL readKeys('FLG2',keyValue,irc)
  CALL ckoptl(1,'FLG2',keyValue,pgName, &
              'Flag for TYPE 002 and TYPE 005',irc,irCode, &
              maxLength=3,maxVal=1,result1=iFlag2)

  CALL ckoptb(1,(/ 'RADOME' /),pgName,'Consider radome code',irCode, &
              result1 = iRadom)
  CALL ckoptb(1,(/ 'MRKNUM' /),pgName,'Consider marker number',irCode, &
              result1 = iMarkn)
  CALL ckoptb(1,(/ 'RECSER' /),pgName,'Convert receiver serial number',irCode, &
              result1 = iRecsn)
  CALL ckoptb(1,(/ 'ANTSER' /),pgName,'Convert antenna serial number',irCode, &
              result1 = iAntsn)

  IF (irCode /= 0) CALL exitrc(2)

! Allocate and init rnxInfo record
! --------------------------------
  rnxInfo%technique='GNSS'
  rnxInfo%nInfo    = nFil
  rnxInfo%nStatype = nFil

  ALLOCATE(rnxInfo%staInfo(nFil),stat=irc)
  CALL alcerr(irc,'rnxInfo%staInfo',(/nFil/),pgName)

  rnxInfo%staInfo(:)%stanam      = ' '
  rnxInfo%staInfo(:)%flg         = iFlag2
  rnxInfo%staInfo(:)%timint%t(1) = 44244D0
  rnxInfo%staInfo(:)%timint%t(2) = 88068D0
  rnxInfo%staInfo(:)%recnam      = undef_c
  rnxInfo%staInfo(:)%antnam      = undef_c
  rnxInfo%staInfo(:)%recser      = undef_c
  rnxInfo%staInfo(:)%antser      = undef_c
  rnxInfo%staInfo(:)%recnum      = undef_i
  rnxInfo%staInfo(:)%antnum      = undef_i
  rnxInfo%staInfo(:)%antecc(1)   = undef_e
  rnxInfo%staInfo(:)%antecc(2)   = undef_e
  rnxInfo%staInfo(:)%antecc(3)   = undef_e
  rnxInfo%staInfo(:)%descri      = ' '
  rnxInfo%staInfo(:)%remark      = ' '

  ALLOCATE(rnxInfo%staType(nFil),stat=irc)
  CALL alcerr(irc,'rnxInfo%staType',(/nFil/),pgName)

  rnxInfo%staType(:)%stanam      = ' '
  rnxInfo%staType(:)%flg         = iFlag2
  rnxInfo%staType(:)%timint%t(1) = 44244D0
  rnxInfo%staType(:)%timint%t(2) = 88068D0
  rnxInfo%staType(:)%markertype  = ' '
  rnxInfo%staType(:)%remark      = ' '

! Loop all RINEX files
! --------------------
  DO iFil = 1,nFil

    ! Read header from RINEX file
    CALL opnfil(lfn001,rnxFil(1,iFil),'OLD','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfn001,ios,rnxFil(1,iFil),pgName)

    CALL r2rdoh(lfn001,lfnerr,maxsat,maxcom,0,                     &
                prgnam,runby,crdate,crtime,ncom,coment,            &
                stanam,stanum,oprnam,agency,irunit,rectyp,rcvers,  &
                ianten,anttyp,posxyz,posecc,iwlfac,iwlsat,nwlsat,  &
                numtyp,obstyp,ideltt,tfirst,tlast,                 &
                nsatel,numsat,numobs,irxvrs,ircode)

    CLOSE(lfn001)

! Put information into the station information block
! --------------------------------------------------
    rnxInfo%staInfo(iFil)%stanam = stanam(1:16)
    IF(iMarkn == 1) &
      rnxInfo%staInfo(iFil)%stanam = stanam(1:4) // ' ' // stanum(1:9)
    rnxInfo%staInfo(iFil)%timint%t(1) = tfirst
    rnxInfo%staInfo(iFil)%recnam = rectyp(1:20)
    rnxInfo%staInfo(iFil)%recser = rectyp(21:40)
    IF(iRecsn == 1) rnxInfo%staInfo(iFil)%recnum = extrInt(rectyp(21:40),size=5)
    rnxInfo%staInfo(iFil)%antnam = anttyp(1:16)
    IF(iRadom == 1) rnxInfo%staInfo(iFil)%antnam = anttyp(1:20)
    rnxInfo%staInfo(iFil)%antser = anttyp(21:40)
    IF(iAntsn == 1) rnxInfo%staInfo(iFil)%antnum = extrInt(anttyp(21:40),size=5)

! If it is a RINEX file of a LEO, no antenna offsets have to be written
! into the station info file
    IF (stanum(21:30) == 'SPACEBORNE') THEN
      rnxInfo%staInfo(ifil)%antecc = 0.D0
    ELSE
      rnxInfo%staInfo(ifil)%antecc = posecc(:,1)
    ENDIF

    filNam = rnxFil(1,iFil)
    CALL stripDir(filNam)
    rnxInfo%staInfo(iFil)%remark = 'From ' // TRIM(filNam)

! Put information into the station type block
! -------------------------------------------
    rnxInfo%staType(iFil)%stanam      = rnxInfo%staInfo(iFil)%stanam
    rnxInfo%staType(iFil)%timint%t(1) = rnxInfo%staInfo(iFil)%timint%t(1)
    rnxInfo%staType(iFil)%markertype  = stanum(21:40)
    rnxInfo%staType(iFil)%remark      = rnxInfo%staInfo(iFil)%remark

  ENDDO

! Sort the entries
! ----------------
  sorted = .FALSE.
  DO WHILE (.NOT. sorted)
    sorted = .TRUE.
    DO iFil = 1, nFil-1
      IF ((rnxInfo%staInfo(iFil)%stanam > rnxInfo%staInfo(iFil+1)%stanam) .OR. &
          (rnxInfo%staInfo(iFil)%stanam == rnxInfo%staInfo(iFil+1)%stanam .AND.&
           rnxInfo%staInfo(iFil)%timint%t(1) > &
                                    rnxInfo%staInfo(iFil+1)%timint%t(1))) THEN
        sorted = .FALSE.

        hlpInfo                 = rnxInfo%staInfo(iFil)
        rnxInfo%staInfo(iFil)   = rnxInfo%staInfo(iFil+1)
        rnxInfo%staInfo(iFil+1) = hlpInfo
      ENDIF

      IF ((rnxInfo%staType(iFil)%stanam > rnxInfo%staType(iFil+1)%stanam) .OR. &
          (rnxInfo%staType(iFil)%stanam == rnxInfo%staType(iFil+1)%stanam .AND.&
           rnxInfo%staType(iFil)%timint%t(1) > &
                                    rnxInfo%staType(iFil+1)%timint%t(1))) THEN
        sorted = .FALSE.

        hlpType                 = rnxInfo%staType(iFil)
        rnxInfo%staType(iFil)   = rnxInfo%staType(iFil+1)
        rnxInfo%staType(iFil+1) = hlpType
      ENDIF

    ENDDO
  ENDDO

! Check time windows for more than one rinex file per station
! -----------------------------------------------------------
  DO iFil = 1,nFil
    IF (iFil == 1) THEN
      rnxInfo%staInfo(iFil)%timint%t(1) = 44244D0
    ELSE IF (rnxInfo%staInfo(iFil-1)%stanam /= &
                                       rnxInfo%staInfo(iFil)%stanam) THEN
      rnxInfo%staInfo(iFil)%timint%t(1) = 44244D0
    ELSE
      rnxInfo%staInfo(iFil-1)%timint%t(2) =  &
                          rnxInfo%staInfo(iFil)%timint%t(1) - 1d0/86400d0
    ENDIF

    IF (iFil == 1) THEN
      rnxInfo%staType(iFil)%timint%t(1) = 44244D0
    ELSE IF (rnxInfo%staType(iFil-1)%stanam /= &
                                       rnxInfo%staType(iFil)%stanam) THEN
      rnxInfo%staType(iFil)%timint%t(1) = 44244D0
    ELSE
      rnxInfo%staType(iFil-1)%timint%t(2) =  &
                          rnxInfo%staType(iFil)%timint%t(1) - 1d0/86400d0
    ENDIF
  ENDDO

! Remove empty markertypes
! ------------------------
  rnxInfo%nStaType = 0

  DO iFil = 1,nFil
    IF (rnxInfo%staType(iFil)%markerType /= ' ') THEN
      rnxInfo%nStaType = rnxInfo%nStaType+1
      rnxInfo%staType(rnxInfo%nStaType) = rnxInfo%staType(iFil)
    ENDIF
  ENDDO

! Write a program output
! ----------------------
  WRITE(lfnprt,'(2(/,A))') &
        ' File      Station name      Receiver na' // &
        'me         Antenna name           Height',   &
        ' ---------------------------------------' // &
        '----------------------------------------'
  DO iFil = 1,nFil
    WRITE(lfnprt,'(1X,4(A,2X),F7.3)') &
    rnxInfo%staInfo(iFil)%remark(6:13), &
    rnxInfo%staInfo(iFil)%stanam, rnxInfo%staInfo(iFil)%recnam, &
    rnxInfo%staInfo(iFil)%antnam, rnxInfo%staInfo(iFil)%antecc(3)
  ENDDO
 WRITE(lfnprt,'(A,/)') &
        ' ---------------------------------------' // &
        '----------------------------------------'

  IF (rnxInfo%nstaType > 0) THEN
    WRITE(lfnprt,'(/,2(/,A))') &
          ' File      Station name      Marker type' // &
          '                                        ',   &
          ' ---------------------------------------' // &
          '----------------------------------------'
    DO iFil = 1,rnxInfo%nStaType
      WRITE(lfnprt,'(1X,3(A,2X))') &
      rnxInfo%staType(iFil)%remark(6:13), &
      rnxInfo%staType(iFil)%stanam, rnxInfo%staType(iFil)%markerType
    ENDDO
    WRITE(lfnprt,'(A,/)') &
          ' ---------------------------------------' // &
          '----------------------------------------'
  ENDIF

! Write the RINEX info into the output file
! -----------------------------------------
  CALL gtflna(0,'RNXINFO',staFil,irc)

  IF (irc == 0) THEN
    CALL selrenam(rnxInfo,iFlag1)
    CALL writcrux(staFil,rnxInfo,filTitle)
  ENDIF

! Exit program
! ------------
  CALL exitrc(0)

END PROGRAM rnx2sta
