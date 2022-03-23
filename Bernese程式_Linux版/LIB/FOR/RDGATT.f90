MODULE s_RDGATT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdgatt(stanam,epoch,iOpt,azimuth,zenith,roll,irc)

!-------------------------------------------------------------------------
! Purpose:   Read attitude for the ground station antenna
!
! Author:    D.Svehla
!
! Remark:
!
! Created:   16-Dec-2002
!
! Changes:   04-Jun-2003 HU: Format corrected
!            29-Oct-2003 HB: Correct format statement
!            28-Jun-2005 MM: Unused variables removed
!            13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!            20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr, lfnloc, &
                     staNameLength, lineLength, fileNameLength
  USE d_const, ONLY: pi
  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_gtfile
  USE f_djul
  USE s_opnerr
  USE f_dgpsut
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN:
  CHARACTER(LEN=staNameLength)   :: stanam  ! Station name
  REAL(r8b)                      :: epoch   ! GPS time
  INTEGER(i4b)                   :: iOpt    !=0: attitude for nearest epoch
                                            !=1: attitude for next epoch
                                            !=2: interpolated attitude
! OUT:
  REAL(r8b)                      :: azimuth ! azimuth=yaw  in rad
  REAL(r8b)                      :: zenith  ! zenith=pitch in rad
  REAL(r8b)                      :: roll    ! roll angle   in rad
  INTEGER(i4b)                   :: irc     !=0: OK
                                            !=1: no attitude data for requested epoch
                                            !=2: no attitude file found
                                            !=3: sensor not found in attitude file

! Local Parameters
! ----------------
  INTEGER(i4b),PARAMETER      :: MAXATT = 10

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength) :: line
  CHARACTER(LEN=staNameLength), DIMENSION(MAXATT) :: name
  CHARACTER(LEN=fileNameLength),DIMENSION(MAXATT) :: filnam

  INTEGER(i4b)      :: iac,IGPSUTC,iii
  INTEGER(i4b)      :: iostat,irclin
  INTEGER(i4b)      :: iEpo
  INTEGER(i4b)      :: iTim
  INTEGER(i4b)      :: jj,mo,id,ih,mi,isec1,isec2
  INTEGER(i4b)      :: if,nEpoFil
  INTEGER(i4b),SAVE :: nfiles
  INTEGER(i4b),SAVE :: ircc
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: kEpo
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: lEpo
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: nEpo
  INTEGER(i4b),SAVE                   :: lfnatt

  REAL(r8b)                                     :: epo,GPSUTC
  REAL(r8b)                                     :: day
  REAL(r8b),DIMENSION(3)                        :: att
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE,SAVE     :: epoSeq
  REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE   :: attmat

  LOGICAL,SAVE  :: first=.TRUE.



! FIRST CALL OF SUBROUTINE
! ========================
  IF (first) THEN
    first=.FALSE.
    irc=0
    kEpo(:)=1
    lEpo(:)=1
    nEpo(:)=0

    nfiles=0
    CALL gtfile('GRDATT ',1,MAXATT,nfiles,filnam)
    IF (nfiles > MAXATT) THEN
      WRITE(lfnerr,'(A,/,16X,A,I6,/,16X,A,I6,/)')'*** SR RDGATT: &
         &Too many attitude input files selected',    &
         'Number of files:',nfiles,'Max. allowed   :',MAXATT
      CALL exitrc(2)
    END IF

! IF NO ATTITUDE FILE IS AVAILABLE
! --------------------------------
    IF (nfiles==0) THEN
      ircc=1
      irc=2
      GOTO 999
    ENDIF

! Set attitude lfn number
! -----------------------
    lfnatt=lfnloc+11
!
! LOOP OVER ALL FILES
! ===================
    DO if=1,nfiles
!
! OPEN ATTITUDE FILE
! ------------------
      CALL opnfil(lfnatt,filnam(if),'OLD',' ', ' ',' ',iostat)
      CALL opnerr(lfnerr,lfnatt,iostat,filnam(if),'RDGATT')

      DO iii=1,4
        READ(lfnatt,'(A)',iostat=irclin) line
        IF (irclin /= 0) line=''
      END DO

      READ(lfnatt,'(14X,A16)',iostat=irclin) name(if)

      IF (irclin /= 0) line=''
      READ(lfnatt,'(A)',iostat=irclin) line
      IF (irclin /= 0) line=''

      READ(lfnatt,'(29X,I1)',iostat=irclin) IGPSUTC
      IF (IGPSUTC.NE.1 .AND. IGPSUTC.NE.2) THEN
        WRITE(lfnerr,'(A,/,16X,A,A,/)')'*** SR RDGATT: &
        &Time system is not selected in the ground attitude input file',    &
        'File name: ',filnam(if)
        CALL exitrc(2)
      END IF

      DO iii=1,7
        READ(lfnatt,'(A)',iostat=irclin) line
        IF (irclin /= 0) line=''
      END DO

      LINE_LOOP1: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP1
          nEpo(if) = nEpo(if)+1
      END DO LINE_LOOP1
      REWIND (lfnatt)
      CLOSE (lfnatt)
    END DO

! ALLOCATE MEMORY
! ---------------
    nEpoFil=MAXVAL(nEpo(1:nfiles))
    ALLOCATE(epoSeq(nEpoFil,nfiles),stat=iac)
    CALL alcerr(iac, 'epoSeq', (/nEpoFil,nfiles/), 'rdgatt')
    ALLOCATE(attmat(3,nEpoFil,nfiles),stat=iac)
    CALL alcerr(iac, 'attmat', (/3,nEpoFil,nfiles/), 'rdgatt')

! READ ATTITUDE DATA
! ------------------
    DO if=1,nfiles
!
! OPEN ATTITUDE FILE
! ------------------
      CALL opnfil(lfnatt,filnam(if),'OLD',' ', ' ',' ',iostat)
      CALL opnerr(lfnerr,lfnatt,iostat,filnam(if),'RDGATT')
!
! Standard format
! ---------------
      DO iii=1,14
        READ(lfnatt,'(A)',iostat=irclin) line
      END DO
      iEpo=0
      DATA_LOOP3: DO
        line = nextline(lfnatt,0)
        IF (line=='') EXIT DATA_LOOP3
        iEpo = iEpo+1
        READ (line, '(I2,1X,I2,1X,I4,2X,3(I2,1X),I3,2X,3F12.6)')    &
        &            id,mo,jj,ih,mi,isec1,isec2,attmat(1:3,iEpo,if)
        day=id+ih/24.D0+mi/1440.D0+(isec1+isec2/1000.D0)/86400.D0
        epoSeq(iEpo,if)=DJUL(jj,mo,day)
        IF (IGPSUTC==2) THEN
          GPSUTC=DGPSUT(epoSeq(iEpo,if))
          epoSeq(iEpo,if)=epoSeq(iEpo,if)+GPSUTC/86400.D0
        END IF
      END DO DATA_LOOP3
      CLOSE (lfnatt)
    END DO
!
! END OF THE FIRST CALL
!======================
  ENDIF

! IF NO ATTITUDE FILE AVAILABLE RETURN
! ------------------------------------
  IF (ircc/=1) THEN
    DO if=1,nfiles
      IF (stanam==name(if)) GOTO 500
    END DO
    irc=3
    GOTO 999
  ELSE
    irc=2
    GOTO 999
  END IF

500 CONTINUE

! LOOK FOR REQUESTED EPOCH
! ------------------------
  epo = epoch
!
! Return with different options
! -----------------------------
  SELECT CASE(iOpt)
    CASE(0)  ! Return nearest epoch
      IF (epo >= epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) > 1) lEpo(if)=kEpo(if)-1
        IF (kEpo(if) == 1) lEpo(if)=1
        EPO1_LOOP: DO iEpo = lEpo(if),nEpo(if)-1
          IF (epo >= epoSeq(iEpo,if).AND.epo < epoSeq(iEpo+1,if)) THEN
            IF (epo-epoSeq(iEpo,if) < epoSeq(iEpo+1,if)-epo) THEN
              iTim = iEpo
            ELSE
              iTim = iEpo+1
            ENDIF
            kEpo(if)=iEpo
            EXIT EPO1_LOOP
          ELSEIF (iEpo==nEpo(if)-1) THEN
            iTim=nEpo(if)
            WRITE(lfnerr,'(A,2(/,A,F15.7))')&
            '*** SR rdgatt:  The requested epoch is out of the table. ',&
            '                Requested epoch:    ',epoch,&
            '                Last epoch in table:',epoSeq(nEpo(if),if)
            irc=1
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO1_LOOP
      ELSEIF (epo < epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) < nEpo(if)) lEpo(if)=kEpo(if)+1
        EPO2_LOOP: DO iEpo=lEpo(if),1,-1
          IF (epo >= epoSeq(iEpo,if).AND.epo < epoSeq(iEpo+1,if)) THEN
            IF (epo-epoSeq(iEpo,if)<epoSeq(iEpo+1,if)-epo) THEN
              iTim=iEpo
            ELSE
              iTim=iEpo+1
            ENDIF
            kEpo(if)=iEpo
            EXIT EPO2_LOOP
          ELSEIF (iEpo == 1) THEN
            iTim=iEpo
            WRITE(lfnerr,'(A,2(/,A,F15.7))')&
            '*** SR rdgatt: The requested epoch is out of the table. ',&
            '                Requested epoch:     ',epoch,&
            '                First epoch in table:',epoSeq(iEpo,if)
            irc=1
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO2_LOOP
      ENDIF
    CASE(1)  ! Return the next entry of the table
      IF (epo >= epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) > 1) lEpo(if)=kEpo(if)-1
        IF (kEpo(if) == 1) lEpo(if)=1
        EPO3_LOOP: DO iEpo=lEpo(if),nEpo(if)-1
          IF (epo  >= epoSeq(iEpo,if).AND.         &
          &   epo   < epoSeq(iEpo+1,if)) THEN
            iTim=iEpo+1
            kEpo(if)=iEpo+1
            EXIT EPO3_LOOP
          ELSEIF (iEpo==nEpo(if)-1) THEN
            WRITE(lfnerr,'(A,F15.7)')&
                 '*** SR rdgatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=2
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO3_LOOP
      ELSEIF (epo < epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) < nEpo(if)) lEpo(if)=kEpo(if)+1
        EPO4_LOOP: DO iEpo=lEpo(if),1,-1
          IF (epo >= epoSeq(iEpo,if).AND.    &
            & epo  < epoSeq(iEpo+1,if)) THEN
            iTim=iEpo+1
            kEpo(if)=iEpo+1
            EXIT EPO4_LOOP
          ELSEIF (iEpo == 1) THEN
            WRITE(lfnerr,'(A,F15.7)')&
                 '*** SR rdgatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=2
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO4_LOOP
      ENDIF
    CASE(2)  ! Return the interpolated attitude
      IF (epo >=epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) > 1) lEpo(if)=kEpo(if)-1
        IF (kEpo(if) == 1) lEpo(if)=1
        EPO5_LOOP: DO iEpo=lEpo(if),nEpo(if)-1
          IF (epo >= epoSeq(iEpo,if).AND.epo < epoSeq(iEpo+1,if)) THEN
            kEpo(if)=iEpo
            EXIT EPO5_LOOP
          ELSEIF (iEpo==nEpo(if)-1) THEN
            WRITE(lfnerr,'(A,F15.7)')&
                 '*** SR rdgatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=2
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO5_LOOP
      ELSEIF (epo < epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) < nEpo(if)) lEpo(if)=kEpo(if)+1
        EPO6_LOOP: DO iEpo=lEpo(if),1,-1
          IF (epo >= epoSeq(iEpo,if).AND.epo < epoSeq(iEpo+1,if)) THEN
            kEpo(if)=iEpo
            EXIT EPO6_LOOP
          ELSEIF (iEpo == 1) THEN
            WRITE(lfnerr,'(A,F15.7)')&
                 '*** SR rdgatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=2
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO6_LOOP
      ENDIF
      att(:)=(attmat(:,kEpo(if)+1,if)*(epo-epoSeq(kEpo(if),if))+&
           &(attmat(:,kEpo(if),if)*(epoSeq(kEpo(if)+1,if)-epo)))/&
           &(epoSeq(kEpo(if)+1,if)-epoSeq(kEpo(if),if))
    CASE DEFAULT
  END SELECT

  IF ((iOpt==0.OR.iOpt==1).AND.irc==0) THEN
      att(:)=attmat(:,iTim,if)
  ENDIF

  AZIMUTH=att(1)*pi/180.D0
  ZENITH=att(2)*pi/180.D0
  ROLL=att(3)*pi/180.D0


  999 CONTINUE
  RETURN
END SUBROUTINE rdgatt

END MODULE
