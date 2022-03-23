
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM sp3cpf

! -------------------------------------------------------------------------
! Purpose:    Convert SP3 files into CPF slr prediction files.
!
! Author:     W.Gurtner
!
! Created:    05-Jul-2005
!
! Changes:    07-Nov-2005 CU: Adopted to BSW
!             22-Nov-2005 CU: Change leap second handling
!             28-Nov-2005 CU: Add col82 for header line H2
!             04-Apr-2006 CU: Add timsys == '' for SP3a format
!             10-Aug-2006 HB: RDPREI called with t_epoch-structure
!                             USE s_timst2 module
!             27-Feb-2007 AG: Call DEFCON with parameter
!             30-Mar-2007 CU: Correct .realToEpoch. bug, use .epochToReal.
!             08-Aug-2007 CU: Add retroreflector z-offset as H5 record
!             23-Sep-2010 RD: Enable CPU counter
!             07-Oct-2010 RD: Do not close the lfnprt file
!             08-Oct-2010 RD: TIMST2 calls corrected
!             18-Jul-2011 HB: Allow empty value for SAMPLE
!             24-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!             27-Apr-2012 RD: Nullify pointers
!             04-May-2012 RD: Use DMOD from module
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength80, &
                      timStrgLength, filePathLength, fileExtLength, &
                      lfnPrt, lfnErr, lfnOrb, lfn001
  USE m_cpu,    ONLY: cpu_start
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
  USE m_maxdim, ONLY: maxsat
  USE d_inpkey, ONLY: inpkey, init_inpkey
  USE d_satfil, ONLY: typeSLR
  USE l_basfun, ONLY: dmod
  USE f_dgpsut
  USE f_djul
  USE s_readinpf
  USE s_opnsys
  USE s_defcon
  USE s_pritit
  USE s_prflna
  USE s_prfile
  USE s_gtflna
  USE s_readkeys
  USE s_alcerr
  USE s_ckoptu
  USE s_ckopti
  USE s_ckoptl
  USE s_exitrc
  USE s_prn2cos
  USE s_clocks
  USE s_dstime
  USE s_rdpreh
  USE s_rdprei
  USE s_opnfil
  USE s_opnerr
  USE s_timst2
  USE s_gtsensor
  IMPLICIT NONE

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER           :: pgName   = 'PG SP3CPF'

! Local Types
! -----------
  TYPE t_satlst
    INTEGER(i4b)                        :: prn    ! BSW sat. number
    CHARACTER(LEN=7)                    :: cospar ! COSPAR number
    INTEGER(i4b)                        :: sic    ! SIC number (NASA)
    INTEGER(i4b)                        :: norad  ! NORAD number
    CHARACTER(LEN=10)                   :: satnam ! Satellite name
  END TYPE t_satlst
  TYPE(t_satlst),DIMENSION(:),POINTER   :: satlst ! List of satellites

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:), POINTER            :: keyValue
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:,:), ALLOCATABLE      :: hlpStr
  CHARACTER(LEN=fileNameLength80)       :: filnam
  CHARACTER(LEN=fileNameLength80)       :: filsp3
  CHARACTER(LEN=fileNameLength80), DIMENSION(:), POINTER :: filcpf
  CHARACTER(LEN=57), DIMENSION(4)       :: title
  CHARACTER(LEN=5)                      :: coosys
  CHARACTER(LEN=5)                      :: datdes
  CHARACTER(LEN=4)                      :: agency
  CHARACTER(LEN=3)                      :: orbtyp
  CHARACTER(LEN=3)                      :: timsys
  CHARACTER(LEN=2)                      :: filtyp
  CHARACTER(LEN=1), DIMENSION(4,maxsat) :: evtflg
  CHARACTER(LEN=9)                      :: satcos
  CHARACTER(LEN=timStrgLength)          :: timstrnow, timstrfirst, timstrlast
  CHARACTER(LEN=filePathLength)         :: dir_cpf
  CHARACTER(LEN=fileExtLength)          :: ext_cpf

  INTEGER(i4b)                          :: idelta ! Sampling interval in CPF
                                                  ! file (sec)
                                                  ! 0 = take from SP3 file
  INTEGER(i4b)                          :: idtsp3
  INTEGER(i4b), DIMENSION(maxsat)       :: satnmf
  INTEGER(i4b), DIMENSION(maxsat)       :: satwgt
  INTEGER(i4b)                          :: indsat
  INTEGER(i4b), DIMENSION(4,maxsat)     :: accpos, accvel
  INTEGER(i4b), DIMENSION(2)            :: ierec
  INTEGER(i4b)                          :: ireade
  INTEGER(i4b)                          :: ivers
  INTEGER(i4b)                          :: irc, iac, irCode, ircSav, iostat
  INTEGER(i4b)                          :: isat, nsat, nsatfl
  INTEGER(i4b), DIMENSION(8)            :: mm
  INTEGER(i4b)                          :: doynow
  INTEGER(i4b)                          :: iepo, nepo, nrec
  INTEGER(i4b)                          :: ii, kk
  INTEGER(i4b)                          :: ifrmat
  INTEGER(i4b)                          :: leapflag

  REAL(r8b), DIMENSION(3,maxsat)        :: pos, vel
  REAL(r8b), DIMENSION(maxsat)          :: dtsatc, ddtsat
  REAL(r8b)                             :: baspos, basclk
  REAL(r8b), DIMENSION(4,maxsat)        :: sdevp, sdevv
  REAL(r8b), DIMENSION(6,maxsat)        :: corrp, corrv
  REAL(r8b)                             :: tprev, tepoch
  REAL(r8b)                             :: tfirst, tlast, tnow
  REAL(r8b), DIMENSION(2)               :: mesz
  REAL(r8b), DIMENSION(2)               :: utccor
  REAL(r8b), DIMENSION(2)               :: leapsec
  REAL(r8b)                             :: dttab
  REAL(r8b)                             :: day
  REAL(r8b)                             :: fac, fract
  REAL(r8b), DIMENSION(3)               :: antoff

  TYPE(t_epoch)                         :: tEpo

  LOGICAL                               :: nosat
  LOGICAL                               :: jump

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(keyValue)
  NULLIFY(satLst)
  NULLIFY(filcpf)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpkey)
  CALL readinpf('',inpkey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)

! Automatic output generation
! ---------------------------
  CALL pritit(pgName,'Convert precise orbits to CPF files',131)
  CALL prflna(131)

! Get name of SP3 file
! --------------------
  CALL gtflna(0,'PREFIL',filsp3,irc)
  IF (irc /= 0) THEN
    WRITE(lfnerr,'(/,A,/,2A)')                              &
      ' *** PG SP3CPF: Error while reading SP3 file name.', &
      '                SP3 file name: ', TRIM(filsp3)
    CALL exitrc(2)
  ENDIF

! Print input file names into protocol file
! -----------------------------------------
  CALL prfile('PREFIL','',1)

! Get input options
! -----------------
! Get sampling interval
  idelta = 0
  CALL readKeys('SAMPLE', keyValue, irc)
  CALL ckopti(1,'SAMPLE', keyValue, pgName, 'Sampling interval', irc,     &
       irCode, empty=0, ge=0, maxval=1, result1=idelta)

! Read the satellite request uniline
  CALL readKeys('SATSTR', keyValue, irc)
  nsat = SIZE(keyValue)

! Allocate the buffer string
  ALLOCATE(hlpStr(5,nsat),stat=iac)
  CALL alcerr(iac,'hlpStr',(/4,nsat/),pgName)
  hlpStr(:,:) = ' '

! Allocate list of satellite names
  ALLOCATE(satlst(nsat),stat=iac)
  CALL alcerr(iac,'satlst',(/nsat/), pgName)

! Allocate list of CPF files
  ALLOCATE(filcpf(nsat),stat=iac)
  CALL alcerr(iac,'filcpf',(/nsat/), pgName)

  ircSav = irCode
  CALL ckoptu(1,'SATSTR', keyValue,pgName,    &
       'List of satellites',irc,irCode,5,     &
       maxVal=nsat,result2=hlpStr)

  ircSav = irCode-ircSav

! Get PRN number
  CALL ckopti(1,'SATSTR', hlpStr(1,:),pgName, &
       'List of satellites',ircSav,irCode,    &
       ge=0,colTit='PRN',maxVal=nsat,         &
       result2=satlst(:)%prn)

! Get SIC number
  CALL ckopti(1,'SATSTR', hlpStr(2,:),pgName, &
       'List of satellites',ircSav,irCode,    &
       ge=0,colTit='SIC',maxVal=nsat,         &
       result2=satlst(:)%sic)

! Get NORAD number
  CALL ckopti(1,'SATSTR', hlpStr(3,:),pgName, &
       'List of satellites',ircSav,irCode,    &
       ge=0,colTit='NORAD',maxVal=nsat,       &
       result2=satlst(:)%norad)

! Get satellite name
  satlst(:)%satnam = hlpStr(4,:)

! Get CPF file name
  filcpf(:) = hlpStr(5,:)
  CALL readKeys('DIR_CPF',keyValue,irc)
  CALL ckoptl(1,'DIR_CPF',keyValue,pgName,'Path DIR_CPF',      &
       irc,irCode,maxVal=1,result1=dir_cpf)
  CALL readKeys('EXT_CPF',keyValue,irc)
  CALL ckoptl(1,'EXT_CPF',keyValue,pgName,'Extension EXT_CPF', &
       irc,irCode,maxVal=1,result1=ext_cpf)

! Read SP3 header portion for tfirst and timsys
  CALL rdpreh(filsp3,-1,ifrmat,nsatfl,satnmf,satwgt, &
       tfirst,nepo,dttab,title,datdes,coosys,orbtyp, &
       agency, filtyp,timsys,baspos,basclk)

  DO isat = 1, nsat

  ! Get COSPAR ID
    CALL prn2cos(11,satlst(isat)%prn,tfirst,satcos,irc)
    WRITE(satlst(isat)%cospar,'(A2,A3,I2.2)') &
      satcos(3:4),satcos(6:8),MOD(ICHAR(satcos(9:9)),16)

  ! Create CPF file names
    filnam       = filcpf(isat)
    filcpf(isat) = TRIM(dir_cpf)//TRIM(filnam)//'.'//TRIM(ext_cpf)

  ENDDO

! Get current epoch (mjd) = local time on ubecx
  CALL clocks(mm)
  day    = mm(3) + mm(5)/24d0 + mm(6)/60d0/24d0 + mm(7)/3600d0/24d0
  tnow   = DJUL(mm(1),mm(2),day)
! Convert local time to UTC
  CALL dstime(mm(1),mesz(1),mesz(2))
  IF (tnow >= mesz(1) .AND. tnow <= mesz(2)) THEN
  ! UTC  = MESZ - 2h
    tnow = tnow - 2d0/24d0
  ELSE
  ! UTC  = MEZ  - 1h
    tnow = tnow - 1d0/24d0
  ENDIF
  doynow = tnow - DJUL(mm(1),1,1d0) + 1 + 500

! Last epoch in SP3 file
  tlast  = tfirst + (nepo-1) * dttab/86400d0

! Last epoch in CPF file
  idtsp3 = IDNINT(dttab)
  IF(idelta /= 0) THEN
    IF(idelta < idtsp3) THEN
      WRITE(lfnerr,'(A,I4,A)')                                 &
           ' ### PG SP3CPF: Requested sampling interval set to ', &
           idtsp3, ' seconds.'
      idelta = idtsp3
    ELSE
      fract = idelta*1d0/idtsp3 - INT(idelta*1d0/idtsp3)

      IF (fract /= 0d0) THEN
        fac = 1d0
        DO
          fac = fac + 1d0
          IF ((idelta*1d0/idtsp3) <= fac) THEN
            idelta = idtsp3 * fac
            WRITE(lfnerr,'(A,I4,A)')                                    &
                 ' ### PG SP3CPF: Requested sampling interval set to ', &
                 idelta, ' seconds.'
            EXIT
          ENDIF
        ENDDO
      ENDIF

      nrec  = IDINT((tlast-tfirst)/idelta*86400d0)+1
      tlast = tfirst + (nrec-1)*idelta/86400d0

    END IF
  ELSE
    idelta = idtsp3
  END IF

! Correct for leap second
  IF(timsys == 'GPS' .OR. timsys == '') THEN
    utccor(1) = DGPSUT(tfirst)/86400d0
    utccor(2) = DGPSUT(tlast)/86400d0
  ELSE
    utccor(1) = 0d0
    utccor(2) = 0d0
  END IF
  tfirst = tfirst - utccor(1)
  tlast  = tlast  - utccor(1)

  jump     = .FALSE.
  IF (utccor(1) /= utccor(2)) jump = .TRUE.

  CALL timst2(2,1,tnow,  timstrnow)
  CALL timst2(2,1,tfirst,timstrfirst)
  CALL timst2(2,1,tlast, timstrlast)

! Version number
  ivers = 1

! Print input options
! -------------------
  WRITE(lfnprt,'(/,2(A,/))') &
    ' INPUT OPTIONS ',       &
    ' -------------'

  WRITE(lfnprt,'(A,I6,A)')   &
    ' Sampling interval: ', idelta, ' seconds'

  WRITE(lfnprt,'(//,A,/,2(/,A))')                             &
    ' List of satellites: ',                                  &
    ' PRN  COSPAR   SIC   NORAD  Name        CPF file name',  &
    ' -------------------------------------------------------------------------'

! Loop over all satellites
! ------------------------
  DO isat = 1, nsat

  ! Initialization
  ! --------------
    ireade = 0
    tprev  = 0d0

  ! Read SP3 header portion
  ! -----------------------
    CALL rdpreh(filsp3,lfnorb,ifrmat,nsatfl,satnmf,satwgt, &
         tfirst,nepo,dttab,title,datdes,coosys,orbtyp,     &
         agency, filtyp,timsys,baspos,basclk)

    agency = " COD"

  ! Find required satellite number
  ! ------------------------------
    nosat = .TRUE.
    DO ii = 1, nsatfl
      IF(satlst(isat)%prn == satnmf(ii))THEN
        indsat = ii
        nosat  = .FALSE.
        EXIT
      END IF
    ENDDO

    IF (nosat) THEN
      WRITE(lfnerr,'(A,/,2A)')                                  &
        ' ### PG SP3CPF: Satellite not found in orbit file. ',  &
        '                Sat. COSPAR-ID: ',satlst(isat)%cospar
      CLOSE(lfnorb)
      CYCLE
    ENDIF

    WRITE(lfnprt,'(1X,I3,2X,A7,2X,I4,2X,I5,2X,A10,2X,A)') &
      satlst(isat)%prn,    &
      satlst(isat)%cospar, &
      satlst(isat)%sic,    &
      satlst(isat)%norad,  &
      satlst(isat)%satnam, &
      TRIM(filcpf(isat))

  ! Get Laser retroreflector offset
  ! -------------------------------
    CALL gtsensor(prn=satlst(isat)%prn,epo=tfirst,type1=typeSLR,antoff=antoff)

  ! Open new CPF file
  ! -----------------
    CALL opnfil(lfn001,filcpf(isat),'UNKNOWN','FORMATTED',' ',' ',iostat)

  ! Write header of CPF file
  ! ------------------------
    WRITE(lfn001,'(A2,1X,A3,1X,I2,1X,A4,1X,A13,1X,I4,I1,1X,A)')              &
      'H1','CPF',ivers,agency,timstrnow(1:13),doynow,1,TRIM(satlst(isat)%satnam)
    WRITE(lfn001,'(A2,2X,A7,I5,I9,1X,2(A19,1X),I5,2(1X,I1),1X,I2,2(1X,I1))') &
      'H2',satlst(isat)%cospar,satlst(isat)%sic,satlst(isat)%norad,          &
      timstrfirst,timstrlast,idelta,1,1,0,0,0
    WRITE(lfn001,'(A2,1X,F7.4)') &
      'H5',antoff(3)
    WRITE(lfn001,'(A2)') 'H9'

  ! Read all epochs of SP3 file
  ! ---------------------------
    utccor(1)  = DGPSUT(tfirst)/86400d0
    leapflag   = 0
    leapsec(1) = DGPSUT(tfirst-utccor(1))/86400d0

    DO iepo = 1, nepo

    ! Read observation concerning one epoch
      CALL rdprei(lfnorb,ifrmat,ireade,nsatfl,satnmf,tEpo,pos,vel,dtsatc, &
           ddtsat,accpos,accvel,evtflg,ierec,sdevp,sdevv,corrp,corrv,irc)
      tepoch = .epochToReal.tEpo

      IF (irc == 1) EXIT
      IF (jump .AND. leapflag == 0) THEN
        leapsec(2) = DGPSUT(tepoch-utccor(1))/86400d0
        IF (leapsec(2) /= leapsec(1)) THEN
          leapflag = 1
        ENDIF
        leapsec(1) = leapsec(2)
      ENDIF

    ! Print records
      tepoch   = tepoch - utccor(1)
      IF( (tepoch-tprev+1d-6)*86400d0 >= idelta) THEN
        WRITE(lfn001,'(A2,1X,I1,1X,I5,1X,F13.6,2X,I1,1X,3F14.3)') &
          '10',0,IDINT(tepoch),DMOD(tepoch,1d0)*86400d0,leapflag, &
          (pos(kk,indsat),kk=1,3)
        tprev = tepoch
      END IF

    ENDDO

    WRITE(lfn001,'(A2)') '99'

    CLOSE(lfn001)
    CLOSE(lfnorb)

  ENDDO

!  CLOSE(lfnprt)

  CALL exitrc(0)

END PROGRAM sp3cpf
