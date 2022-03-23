! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM crdmerge

! -----------------------------------------------------------------------------
! Purpose:    Merge coordinate/velocity files using station flags.
!             Flag priority is selectable, flags can be replaced.
!             A master file can be supplemented with new stations from
!             merge files.
!
! Author:     M.Meindl
!
! Created:    17-Jun-2003
!
! Changes:    05-Aug-2003 mm: bugfix wrt useFil()
!             10-Sep-2003 rd: Update default flag list
!                             (no more "E","F","G" but "U","A","W","N" added)
!             05-Nov-2003 hb: Format corrected
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             28-Jun-2004 RD: Use maxCrd from M_MAXDIM.f90 for maxsta
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             30-May-2006 MM: Fix file implemented
!             31-May-2006 MM: Synchronization wrt DOMES number
!             27-Feb-2007 AG: Call DEFCON
!             23-Sep-2010 RD: Enable CPU counter
!             17-Feb-2011 RD: Use GETSTA; minor revision
!             24-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!             14-Feb-2012 RD: Resolve "out-of-bound" problem in IF-statments
!             12-Mar-2012 RD: Use SPLSTR as module now
!             12-Mar-2012 RD: Use LISTC1 as module now
!             20-Aug-2012 SL: Shape of listc1 parameter changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -----------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength, lineLength, &
                      staFla2Length, staNameLength, lfnPrt, lfnErr, lfnRes
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsta => maxcrd
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE s_alcerr
  USE s_ckoptu
  USE s_opnfil
  USE s_defcon
  USE s_prflna
  USE s_gtfile
  USE s_pritit
  USE s_splstr
  USE s_readinpf
  USE s_opnerr
  USE s_dattim
  USE s_prfile
  USE s_getco3
  USE s_cordup
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_opnsys
  USE s_gtflna
  USE s_ckoptl
  USE s_ckoptd
  USE s_ckoptt
  USE s_wtstat
  USE f_listc1
  IMPLICIT NONE

! Local variables
! ---------------
! parameters
  INTEGER(i4b),PARAMETER                       :: maxFil = 100

! general
  CHARACTER(LEN=8)                             :: pgmNam = 'CRDMERGE'
  CHARACTER(LEN=keyValueLength),&
                          DIMENSION(:),POINTER :: keyValue
  CHARACTER(LEN=6)                             :: keyWd
  CHARACTER(LEN=fileNameLength)                :: mtrFil
  CHARACTER(LEN=fileNameLength)                :: resFil
  CHARACTER(LEN=fileNameLength)                :: resFix
  CHARACTER(LEN=fileNameLength),&
                          DIMENSION(maxFil)    :: mrgFil
  CHARACTER(LEN=lineLength)                    :: nextLine
  CHARACTER(LEN=lineLength),&
                      DIMENSION(:),POINTER     :: fLine
  REAL(r8b)                                    :: dist
  INTEGER(i4b)                                 :: flgLen
  INTEGER(i4b)                                 :: allSta, useSta
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE        :: fromFil
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE        :: allFil
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE        :: useFil

! options
  CHARACTER(LEN=80)                            :: title
  CHARACTER(LEN=1),DIMENSION(40)               :: cPrio
  CHARACTER(LEN=1),DIMENSION(40)               :: cFix
  CHARACTER(LEN=16)                            :: newDat
  CHARACTER(LEN=80)                            :: prioStr
  CHARACTER(LEN=80)                            :: fixStr
  CHARACTER(LEN=staFla2Length),DIMENSION(:,:),ALLOCATABLE  :: flgRep
  INTEGER(i4b)                                 :: iCrdVel
  REAL(r8b)                                    :: newTim
  LOGICAL                                      :: flgMrg
  LOGICAL                                      :: flgDOMES

! indices and loop variables
  INTEGER(i4b)                                 :: iFil, nFil
  INTEGER(i4b)                                 :: iSta, jSta, nSta
  INTEGER(i4b)                                 :: nPrio, nFix, idxFix
  INTEGER(i4b)                                 :: numSta
  INTEGER(i4b)                                 :: iFlg, iFlgT, jFlg
  INTEGER(i4b)                                 :: iLine, nLine

! date and time, datum
  CHARACTER(LEN=9)                             :: date
  CHARACTER(LEN=5)                             :: time
  REAL(r8b)                                    :: epoch

! station variables
  CHARACTER(LEN=staNameLength),&
                   DIMENSION(:),ALLOCATABLE    :: stName
  CHARACTER(LEN=staNameLength),&
                   DIMENSION(:),POINTER        :: staNamT
  CHARACTER(LEN=staNameLength),&
                   DIMENSION(maxSta)           :: staNam
  CHARACTER(LEN=1),DIMENSION(maxSta)           :: staFix
  CHARACTER(LEN=1),DIMENSION(maxSta)           :: hlpFlg
  CHARACTER(LEN=staFla2Length),DIMENSION(:),POINTER        :: staFlgT
  CHARACTER(LEN=staFla2Length),DIMENSION(:),ALLOCATABLE    :: staFlg5
  CHARACTER(LEN=4),DIMENSION(:),POINTER        :: plateT
  CHARACTER(LEN=4),DIMENSION(:),ALLOCATABLE    :: plate
  REAL(r8b),DIMENSION(:,:),POINTER             :: xStatT
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE         :: xStat

! list and sorting
  INTEGER(i4b)                                 :: iPos, nList
  INTEGER(i4b),DIMENSION(maxSta)               :: crdIdx

! error and return codes
  INTEGER(i4b)                                 :: irc, ircSum
  INTEGER(i4b)                                 :: iac, ioStat

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Some initializations
! --------------------
  nullify(staNamT)
  nullify(xStatT)
  nullify(staFlgT)
  nullify(plateT)
  nullify(keyValue)
  nullify(fline)
  irc    = 0
  ircSum = 0
  numSta = 0
  staNam = " "
  nList  = 0
  epoch  = 1d20

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Open system files and define constants
! --------------------------------------
  CALL opnsys
  CALL defcon(0)

! Read options
! ------------
! coordinates or velocities?
  CALL ckoptb(1,(/'RADIO_1','RADIO_2'/),pgmNam,'File selection',ircSum,    &
              result1=iCrdVel)

! get master file
  IF (iCrdVel==1) keyWd = 'CRDMTR'
  IF (iCrdVel==2) keyWd = 'VELMTR'
  CALL gtflna(0,keyWd,mtrFil,irc)

! get observation files
  IF (iCrdVel==1) keyWd = 'CRDMRG'
  IF (iCrdVel==2) keyWd = 'VELMRG'
  CALL gtfile(keyWd,1,maxFil,nFil,mrgFil)

! no files selected
  IF (nFil==0) THEN
    write(lfnerr,"(/,2(A,/))")                                             &
                       " *** PG CRDMERGE: No coordinate files selected",   &
                       "                  Program stopped."
    call exitrc(2)
  END IF

! fix file requested?
  CALL gtflna(0,'FIXRES',resFix,irc)
  IF (TRIM(resFix) /= '') THEN
    CALL readkeys('FLGFIX',keyValue,irc)
    CALL ckoptl(0,'FLGFIX',keyValue,pgmNam,'Flags for sta info file', &
                irc,ircSum,empty='I',result1=fixStr)
    CALL splstr(fixStr,40," ",nFix,cFix,irc)
  ENDIF

! get title
  CALL readkeys('TITLE',keyValue,irc)
  CALL ckoptl(0,'TITLE',keyValue,pgmNam,'Title',irc,ircSum,                &
              empty=' ',result1=title)

! get new datum
  CALL readkeys('NEWDAT',keyValue,irc)
  CALL ckoptl(0,'NEWDAT',keyValue,pgmNam,'New datum',irc,ircSum,           &
              empty=' ',result1=newDat)

! get new epoch
  CALL readkeys('NEWEPO',keyValue,irc)
  CALL ckoptd(0,'NEWEPO',keyValue,pgmNam,'New epoch',irc,ircSum,           &
              empty=1d20,result1=epoch)

! get new time
  IF (epoch /= 1d20) THEN
    CALL readkeys('NEWTIM',keyValue,irc)
    CALL ckoptt(0,'NEWTIM',keyValue,pgmNam,'New time',irc,ircSum,          &
                empty=0d0,result1=newTim)
    epoch = epoch + newTim/24d0
  ENDIF

! flag priority
  CALL readkeys('FLGPRI',keyValue,irc)
  CALL ckoptl(0,'FLGPRI',keyValue,pgmNam,'Flag priority',irc,ircSum,       &
              empty=' ',result1=prioStr)
  CALL splstr(prioStr,40," ",nPrio,cPrio,irc)

! replace only in merge files
  CALL ckoptb(1,(/'FLGMRG'/),pgmNam,'Replace only in merge files',ircSum,  &
              resultL=flgMrg)

! Synchronize stations with same DOMES number
  CALL ckoptb(1,(/'DOMES'/),pgmNam,'Synchronize stations',ircSum,  &
              resultL=flgDOMES)

! flag replacement
  CALL readkeys('FLGREP',keyValue,irc)
  ALLOCATE(flgRep(2,SIZE(keyValue)),stat=iac)
  CALL alcerr(iac,'flgRep',(/2,SIZE(keyValue)/),pgmNam)
  CALL ckoptu(1,'FLGREP',keyValue,pgmNam,'Flags to replace',irc,ircSum,    &
              2,init=(/' ',' '/),result2=flgRep)

! use default priority
  IF (nPrio==0) THEN
    nPrio      = 9
    cPrio(1:9) = (/'R','C','T','P','M','E','F','G','I'/)
    prioStr = "R C T P M E F G I (default)"
    nPrio       = 10
    cPrio(1:10) = (/'R','C','U','T','P','M','A','W','N','I'/)
    prioStr = "R C U T P M A W N I (default)"
  END IF


! Print header and files
! ----------------------
  CALL pritit(pgmNam,'Merge coordinate/velocity files')
  CALL prflna
  CALL prfile(keyWd,'Files to be merged',1)


! Get datum and epoch from first file or master file
! --------------------------------------------------
  IF (LEN_TRIM(newDat)==0) THEN
    IF (LEN_TRIM(mtrFil)/=0) THEN
      CALL getco3(mtrFil,1,(/'@'/),nSta,staNamT,datum=newDat)
    ELSE
      CALL getco3(mrgFil(1),1,(/'@'/),nSta,staNamT,datum=newDat)
    END IF
  END IF
  IF (epoch == 1d20) THEN
    IF (LEN_TRIM(mtrFil)/=0) THEN
      CALL getco3(mtrFil,1,(/'@'/),nSta,staNamT,timcrd=epoch)
    ELSE
      CALL getco3(mrgFil(1),1,(/'@'/),nSta,staNamT,timcrd=epoch)
    END IF
  END IF

! write date to title line
  CALL dattim(date,time)
  title(66:74) = date
  title(76:80) = time


! Get a list of all involved stations
! -----------------------------------
! from master file
  IF (LEN_TRIM(mtrFil)/=0) THEN
    CALL getco3(mtrFil,1,(/'@'/),nSta,staNamT)
    DO iSta=1,nSta
      iPos = listc1(1,staNameLength,maxSta,staNam,staNamT(iSta),nList)
    END DO
  END IF

! from merge files
  DO iFil=1,nFil
    CALL getco3(mrgFil(iFil),1,(/'@'/),nSta,staNamT)
    DO iSta=1,nSta
      iPos = listc1(1,staNameLength,maxSta,staNam,staNamT(iSta),nList)
    END DO
  END DO
  numSta = nList

! sort station arry
  call cordup(staNam,numSta,1,16,crdIdx)


! Allocate some arrays
! --------------------
  ALLOCATE(stName(numSta),stat=iac)
  CALL alcerr(iac,'stName',(/numSta/),pgmNam)
  ALLOCATE(xStat(3,numSta),stat=iac)
  CALL alcerr(iac,'xStat',(/3,numSta/),pgmNam)
  ALLOCATE(staFlg5(numSta),stat=iac)
  CALL alcerr(iac,'staFlg5',(/numSta/),pgmNam)
  ALLOCATE(plate(numSta),stat=iac)
  CALL alcerr(iac,'plate',(/numSta/),pgmNam)
  ALLOCATE(fromFil(numSta),stat=iac)
  CALL alcerr(iac,'fromFil',(/numSta/),pgmNam)
  ALLOCATE(allFil(nFil),stat=iac)
  CALL alcerr(iac,'allFil',(/nFil/),pgmNam)
  ALLOCATE(useFil(nFil),stat=iac)
  CALL alcerr(iac,'useFil',(/nFil/),pgmNam)

  xStat   = 0.d0
  staFlg5 = '    @'
  plate   = '    '
  allFil  = 0
  useFil  = 0
  fromFil = 0


! Fill station name array
! -----------------------
  DO iSta=1,numSta
    stName(iSta) = staNam(crdIdx(iSta))
  END DO


! Loop over all files
! -------------------
  DO iFil=1,nFil
    CALL getco3(mrgFil(iFil),1,(/'@'/),nSta,staNamT,xStat=xStatT,          &
                staFlg5=staFlgT,plate=plateT)
    allFil(iFil) = nSta
    DO iSta=1,nSta
      iPos = listc1(0,staNameLength,numSta,stName,staNamT(iSta),numSta)
      IF (iPos==0) CYCLE

! compare flags
      iFlgT = listc1(0,1,nPrio,cPrio,staFlgT(iSta)(1:1),nPrio)
      iFlg  = listc1(0,1,nPrio,cPrio,staFlg5(iPos)(1:1),nPrio)
      IF (iFlgT>iFlg .OR. staFlg5(iPos)=='    @') THEN
        xStat(:,iPos) = xStatT(:,iSta)
        staFlg5(iPos) = staFlgT(iSta)
        plate(iPos)   = plateT(iSta)
        fromFil(iPos) = iFil
      END IF
    END DO
  END DO


! Use data from master file
! -------------------------
  IF (LEN_TRIM(mtrFil)/=0) THEN
    CALL getco3(mtrFil,1,(/'@'/),nSta,staNamT,xStat=xStatT,                &
                staFlg5=staFlgT,plate=plateT,footer=fline,title=title)
    DO iSta=1,nSta
      iPos = listc1(0,staNameLength,numSta,stName,staNamT(iSta),numSta)
      IF (iPos==0) CYCLE
      xStat(:,iPos) = xStatT(:,iSta)
      staFlg5(iPos) = staFlgT(iSta)
      IF (LEN_TRIM(plateT(iSta)) > 0) plate(iPos)   = plateT(iSta)
      fromFil(iPos) = 0
    END DO
  END IF


! Count number of stations per file
! ---------------------------------
  DO iSta=1,numSta
    IF (fromFil(iSta)==0) CYCLE
    useFil(fromFil(iSta)) = useFil(fromFil(iSta))+1
  END DO


! Write flag priority
! -------------------
  write(lfnprt,"(A,/,A,/)") " Flag priority",                              &
                            " -------------"
  write(lfnprt,"(1X,A,//)") prioStr


! Write replacements
! ------------------
  IF (SIZE(flgRep(1,:))==1 .AND. LEN_TRIM(flgRep(1,1))==0 .AND.            &
                                 LEN_TRIM(flgRep(2,1))==0       ) THEN
  ELSE
    write(lfnprt,"(A,/,A,/,A)") " --------------------",                   &
                                " Flag    replaced by ",                   &
                                " --------------------"
    DO iFlg=1,SIZE(flgRep(1,:))
      WRITE(lfnprt,"(1X,A5,6X,A5)") flgRep(1,iFlg),flgRep(2,iFlg)
    END DO
    write(lfnprt,"(A,//)")      " --------------------"
  END IF


! Write file overview
! -------------------
  allSta = SUM(allFil)
  useSta = SUM(useFil)
  write(lfnprt,"(A,/,A,/,A)") " -------------------------------------",    &
                              " File    #Stations   #Stations merged ",    &
                              " -------------------------------------"
  IF (LEN_TRIM(mtrFil)/=0) THEN
    write(lfnprt,"(1X,I4,6X,I5,10X,I5)") 0,nSta,nSta
    allSta = allSta+nSta
    useSta = useSta+nSta
  END IF
  DO iFil=1,nFil
    write(lfnprt,"(1X,I4,6X,I5,10X,I5)") iFil,allFil(iFil),useFil(iFil)
  END DO
  write(lfnprt,"(A)")    " -------------------------------------"
  write(lfnprt,"(9X,I7,8X,I7)") allSta,useSta
  write(lfnprt,"(A,//)") " -------------------------------------"


! Synchronize stations with same DOMES number
! -------------------------------------------
  crdIdx = 0
  hlpFlg(1:numSta) = staFlg5(1:numSta)(1:1)
  IF (flgDOMES) THEN
    DO iSta=1,numSta
      IF (fromFil(iSta) == 0) CYCLE
      DO jSta=1,numSta
        IF (iSta == jSta) CYCLE
        IF (stName(iSta)(5:) == stName(jSta)(5:)) THEN
          IF (INDEX(stName(iSta)(6:14),' ') /= 0) CYCLE
          iFlg = listc1(0,1,nPrio,cPrio,hlpFlg(iSta),nPrio)
          jFlg = listc1(0,1,nPrio,cPrio,staFlg5(jSta)(1:1),nPrio)
          dist = SQRT(SUM((xStat(:,iSta)-xStat(:,jSta))**2))
          IF (fromFil(jSta) == 0) THEN
            IF (dist>1.d0) THEN
              WRITE(lfnErr,'(/,A,2(/,A,A16),/,A,F8.3,/)') &
               ' ### PG CRDMERGE: Antenna-sharing stations? Please verify.', &
               '                  Master station: ',stName(jSta),            &
               '                  Second station: ',stName(iSta),            &
               '                  Distance (m)  : ',dist
              CYCLE
            ENDIF
            xStat(:,iSta) = xStat(:,jSta)
            staFlg5(iSta) = staFlg5(jSta)
            crdIdx(iSta) = jSta
          ELSE IF (jFlg >= iFlg .OR. staFlg5(iSta) == '    @') THEN
            IF (dist>1.d0) THEN
              WRITE(lfnErr,'(/,A,2(/,A,A16),/,A,F8.3,/)') &
               ' ### PG CRDMERGE: Antenna-sharing stations? Please verify.', &
               '                  Master station: ',stName(jSta),            &
               '                  Second station: ',stName(iSta),            &
               '                  Distance (m)  : ',dist
              CYCLE
            ENDIF
            IF (crdIdx(iSta) /= 0) THEN
              IF(fromFil(crdIdx(iSta)) == 0) CYCLE
            ENDIF
            xStat(:,iSta) = xStat(:,jSta)
            staFlg5(iSta) = staFlg5(jSta)
            crdIdx(iSta)  = jSta
            hlpFlg(iSta)  = staFlg5(jSta)(1:1)
          END IF
        END IF
      END DO
    END DO

    IF (SUM(crdIdx) > 0) THEN
      WRITE(lfnPrt,'(A,/,A,/,A)') &
              " --------------------------------------------------------", &
              " Num  Station           File    Synchronized with    File", &
              " --------------------------------------------------------"
      DO iSta=1,numSta
        IF (crdIdx(iSta) == 0) CYCLE
        WRITE(lfnPrt,'(I4,2X,A16,2X,I4,4X,A16,2X,I4)') iSta, stName(iSta), &
                 fromFil(iSta),stName(crdIdx(iSta)),fromFil(crdIdx(iSta))
      END DO
      WRITE(lfnPrt,'(A,//)') &
              " --------------------------------------------------------"
    END IF
  END IF


! Write fix file
! --------------
  staFix = ' '
  IF (TRIM(resFix) /= '') THEN
    CALL opnfil(lfnres,resFix,'UNKNOWN',' ',' ',' ',ioStat)
    CALL opnerr(lfnerr,lfnres,ioStat,resFil,pgmNam)
    WRITE(lfnRes,'(A,/,80("-"),//,A,/,A)') title, "Station name", &
                                                  "****************"
    DO iSta=1,numSta
      IF (staFlg5(iSta)(1:1)               == ' ' .OR. &
          INDEX(fixStr,staFlg5(iSta)(1:1)) == 0        ) THEN
        staFix(iSta) = ' '
      ELSE
        staFix(iSta) = '*'
        WRITE(lfnres,'(A)') stName(iSta)
      END IF
    END DO
    CLOSE(lfnRes)
  END IF


! Write station overview
! ----------------------
  WRITE(lfnprt,"(A,/,A,/,A)") " ----------------------------------------", &
                              " Num  Station           File    Flag  FIX", &
                              " ----------------------------------------"
  DO iSta=1,numSta
    WRITE(lfnprt,"(I4,2X,A16,2X,I4,4X,A5,1X,A1)") iSta,stName(iSta),       &
                                fromFil(iSta),staFlg5(iSta),staFix(iSta)
  END DO
  WRITE(lfnprt,"(A,//)")      " ----------------------------------------"


! Replace flags
! -------------
  DO iSta=1,numSta
    IF (flgMrg .AND. fromFil(iSta)==0) CYCLE
    DO iFlg=1,SIZE(flgRep(1,:))
      flgLen = LEN_TRIM(flgRep(1,iFlg))
      IF (flgLen==0) flgLen=1
      IF (staFlg5(iSta)(1:flgLen)==flgRep(1,iFlg) .OR.                     &
           TRIM(flgRep(1,iFlg))=='@'              .OR.                     &
          (TRIM(flgRep(1,iFlg))=='#' .AND. LEN_TRIM(staFlg5(iSta))/=0)) THEN
        staFlg5(iSta) = flgRep(2,iFlg)
        EXIT
      END IF
    END DO
  END DO


! Write result file
! -----------------
  IF (iCrdVel==1) THEN
    CALL gtflna(1,'CRDRES',resFil,irc)
    CALL wtstat(0,resFil,title,newDat,numSta,stName, &
                xStat,staFlg=staFlg5,timCrd=epoch,footer=fLine)
  ELSEIF (iCrdVel==2) THEN
    CALL gtflna(1,'VELRES',resFil,irc)
    CALL wtstat(0,resFil,title,newDat,numSta,stName, &
                xStat,staFlg=staFlg5,plate=plate,footer=fLine)
  END IF


! Deallocate some memory
! ----------------------
  DEALLOCATE(stName,stat=iac)
  DEALLOCATE(xStat,stat=iac)
  DEALLOCATE(staFlg5,stat=iac)
  DEALLOCATE(plate,stat=iac)
  DEALLOCATE(fromFil,stat=iac)
  DEALLOCATE(useFil,stat=iac)
  DEALLOCATE(allFil,stat=iac)
  IF (LEN_TRIM(mtrFil)/=0) DEALLOCATE(fLine,stat=iac)


! Program ends here
! -----------------
  CALL exitrc(0)
END PROGRAM crdmerge
