MODULE s_READATT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE readatt(leosvn,iOpt,iSel,epoch,att,epoDif,irc)

!-------------------------------------------------------------------------
! Purpose:    Read LEO attitude file
!
! Author:     H.Bock, D.Svehla
!
! Remark:     Jason hardwired for CNES format
!
! Created:    10-Jun-2004
!
! Changes:    19-Jan-2001 HB: SET RETURN CODE IF ATT-FILE IS NOT AVAILABLE
!             25-Jan-2001 HB: USE M_BERN INSTEAD OF M_CHAMP
!             26-Mar-2001 DS: USE SR GTFLNA INSTEAD OF GTFILE
!             23-May-2001 HB: ADD SR ALCERR
!             05-Jun-2001 HB: SAVE ircc, IF (ircc==1) RETURN
!             15-Jun-2001 HB: use lineLength instead of 255
!             29-Oct-2001 HB: new software standards
!             20-Nov-2001 HB: bug fix, finding right epoch
!             27-Dec-2001 DS: Read attitude directly from CHAMP AUX file
!                             (auxiliary file with acceleratoins,
!                             attitude, menoeuvres, etc.).
!                             Intermediate pre-processed attitude can be read
!                             in addition.
!             07-Jan-2002 HB: new input parameter iOpt, new option for output
!             16-Jul-2002 HB: Possibility to read also CHAMP attitude file
!                             directly (iAuxFlg=1)
!             18-Jul-2002 DS: Correctly handle AUX and
!                             Intermediate attitude file
!             23-Aug-2002 DS: Read JASON CNES quaternion file
!             28-Aug-2002 DS: UTC -> GPS for CNES format
!             11-Sep-2002 DS: Use lfnatt instead of lfnloc
!             11-Sep-2002 DS: Correct epo computation for all cases
!             05-Oct-2002 DS: Changing of input parameter "epoch"
!                             strictly forbidden
!             12-Oct-2002 DS: Allow attitude for LEO constellation
!             08-Mar-2003 DS: lfnatt=lfneph-9 (new convention)
!             28-Apr-2003 DS: Initialize irc
!             28-Apr-2003 DS: Return irc=1 instead of irc=2 when
!                             attitude is missing
!             28-Jan-2003 DS: GRACE Attitude
!             04-Jun-2003 HU: Format corrected
!             08-Dec-2003 AJ: bug fix (reading pre-processed attitude)
!             16-Apr-2004 HU: Interface to SLR2COS added
!             24-Apr-2004 DS: Merge the latest versions
!             12-May-2004 DS: Why REWIND is commented? Is active again
!             13-May-2004 DS: Reorder GRACE quaternions
!             23-Nov-2004 AJ: bug fix for LEO constellation
!             18-Dec-2004 HU: REWIND removed, file is closed anyhow
!             14-Mar-2005 CU: Add isel to call of sr cos2prn
!             10-Jun-2005 HU: Return epoch for iOpt=0 or 1, irc=4 for iOpt=1
!                             SeqEpo is always MJD
!             22-Jul-2005 HB: Compute epoDif and add it to parameter list,
!                             add GOCE, EGG_IAQ-attitude file
!             23-Jul-2005 HU: Correct argument 2 for GPSMJD
!             10-Aug-2005 HU: Use irccos in COS2PRN
!             06-Apr-2006 HB: Read GOCE-IAQ-file with *
!             15-Nov-2006 HB: Include DLR attitude file
!             03-Apr-2007 HB: Interpolation of attitude done with
!                             quaternions and not with rotation matrix
!             08-May-2007 HB: Bernese attitude file did no longer work ...
!                             corrected
!             30-May-2007 HB: Correction for DLR quaternion file
!                             Correction for quaternion interpolation
!             01-Feb-2008 AJ: Check for zero quaternions in DLR file
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             08-Dec-2010 HB: Do not use outliers in attitude
!             11-Jul-2011 HB: Set leonum=915 for GOCE
!             03-Aug-2011 HB: Add Jason-2 attitude
!             14-Feb-2012 HB: Bug corrected for GRACE attitude
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr, lfneph, &
                     staNameLength, fileNameLength, lineLength
  USE d_const, ONLY: PI
  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_cos2prn
  USE s_gtfile
  USE f_djul
  USE s_opnerr
  USE s_slr2cos
  USE s_quamat
  USE s_mat2qua
  USE f_dgpsut
  USE f_gpsmjd
  USE s_exitrc
  USE s_leoprn
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN:
  INTEGER(i4b)             :: leosvn ! LEO number
  INTEGER(i4b)             :: iOpt   !=0: return nearest epoch and attitude data
                                     !=1: return next epoch and attitude data
                                     !=2: interpolate
  INTEGER(i4b)             :: iSel   !=0: return att in T,-N,-R (Spacecraft Body Fixed System)
                                     !=1: return att in R,T,N (nearly equivalent to RSW)

! IN/OUT:
  REAL(r8b)                :: epoch

! OUT:
  REAL(r8b),DIMENSION(3,3) :: att    ! attitude data
  REAL(r8b)                :: epoDif ! Time difference between used attitude values
  INTEGER(i4b)             :: irc    !=0: OK
                                     !=1: no attitude data for requested epoch
                                     !=2: no attitude file found
                                     !=3: LEO not found in attitude file
                                     !=4: End of file reached (iOpt=1)

! Local Parameters
! ----------------
  INTEGER(i4b),PARAMETER      :: MAXATT = 10

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength) :: line
  CHARACTER(LEN=lineLength) :: line1
  CHARACTER(LEN=fileNameLength), DIMENSION(MAXATT) :: filnam
  CHARACTER(LEN=9),DIMENSION(MAXATT)               :: leocos
  CHARACTER(LEN=staNameLength) :: leoname
  CHARACTER(LEN=3) :: dummy

  INTEGER(i4b)      :: iac
  INTEGER(i4b)      :: iostat,irclin,irclin1,irccos
  INTEGER(i4b)      :: iEpo
  INTEGER(i4b)      :: iTim
  INTEGER(i4b)      :: jj,mo,id,ih,mi,isec1,isec2
  INTEGER(i4b)      :: if,nEpoFil,lfnatt
  INTEGER(i4b)      :: iLeft,jLeft,nLeft
  INTEGER(i4b)      :: iRight,jRight,nRight
  INTEGER(i4b)      :: nSize
  INTEGER(i4b),SAVE :: nfiles
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: iAuxFlg
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: leonum
  INTEGER(i4b),SAVE :: ircc
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: kEpo
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: lEpo
  INTEGER(i4b),SAVE,DIMENSION(MAXATT) :: nEpo

  REAL(r8b)                                     :: gap
  REAL(r8b)                                     :: epo,GPSUTC
  REAL(r8b)                                     :: sec,day
  REAL(r8b)                                     :: delta,deltat,kappa
  REAL(r8b)                                     :: delta_nom
  REAL(r8b)                                     :: idum1a,idum1b
  REAL(r8b)                                     :: idum2a,idum2b
  REAL(r8b)                                     :: idum3a,idum3b
  REAL(r8b)                                     :: idum4a,idum4b
  REAL(r8b),DIMENSION(4)                        :: quat,quatJA
  REAL(r8b),DIMENSION(4)                        :: quatA,quatB,quatAB,quatDT
  REAL(r8b),DIMENSION(4)                        :: quatA_nom,quatB_nom,quatAB_nom
  REAL(r8b),DIMENSION(3,3)                      :: hlp
  REAL(r8b),SAVE,DIMENSION(MAXATT)              :: mjdepo
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE,SAVE     :: epoSeq
  REAL(r8b),DIMENSION(:,:,:,:),ALLOCATABLE,SAVE :: attmat
  REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE   :: quatt

  LOGICAL,SAVE  :: first=.TRUE.
  LOGICAL       :: corrFailed
  LOGICAL       :: largeGap

  irc=0

! FIRST CALL OF SUBROUTINE
! ========================
  IF (first) THEN
    first=.FALSE.
    irc=0
    kEpo(:)=1
    lEpo(:)=1
    nEpo(:)=0
    mjdepo(:)=0.D0

    nfiles=0
    CALL gtfile('ATTIT  ',1,MAXATT,nfiles,filnam)
    IF (nfiles > MAXATT) THEN
      WRITE(lfnerr,'(A,/,16X,A,I6,/,16X,A,I6,/)')'*** SR ReadAtt: &
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

! DEFINE LFNATT
! --------------
    lfnatt=lfneph-9

!
! LOOP OVER ALL FILES
! ===================
    DO if=1,nfiles
!
! OPEN ATTITUDE FILE
! ------------------
      CALL opnfil(lfnatt,filnam(if),'OLD',' ', ' ',' ',iostat)
      CALL opnerr(lfnerr,lfnatt,iostat,filnam(if),'READATT')

!     line = nextline(lfnatt,0)
      nepo(if)=0

      READ(lfnatt,'(A)',iostat=irclin) line
      IF (irclin /= 0) line=''

      iAuxFlg(if)=0
!
! CHAMP AUX format
! ----------------
      IF (line(1:1)=='%') THEN
        iAuxFlg(if)=1
        LINE_LOOP1: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP1
          IF (line(1:3)=='att') nEpo(if) = nEpo(if)+1
        END DO LINE_LOOP1
!
! JASON CNES format
! -----------------
      ELSE IF (line(1:16)=='# Parameter list') THEN
        iAuxFlg(if)=2
        LINE_LOOP2: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP2
          nEpo(if) = nEpo(if)+1
        END DO LINE_LOOP2
!
! GRACE format
! ------------
      ELSE IF (line(31:31)==':') THEN
        iAuxFlg(if)=3
        LINE_LOOP3_0: DO
          READ(lfnatt,'(A)',iostat=irclin) line
          IF (irclin /= 0) line=''
          IF (line(1:13)=='END OF HEADER' .OR. line=='') EXIT LINE_LOOP3_0
        END DO LINE_LOOP3_0
        LINE_LOOP3: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP3
          nEpo(if) = nEpo(if)+1
        END DO LINE_LOOP3

! GOCE IAQ-Attitude
! -----------------
      ELSE IF (line(11:11)=='.') THEN
        iAuxFlg(if)=4
        nEpo(if) = nEpo(if)+1
        LINE_LOOP4: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP4
          nEpo(if) = nEpo(if)+1
        END DO LINE_LOOP4
!
! Pre-processed attitude
! ----------------------
      ELSE IF (line(1:15)=='* ATTITUDE FILE') THEN
        CALL SLR2COS(line(26:34),leocos(if))
        line = nextline(lfnatt,0)
        READ(line,'(F7.0)')mjdepo(if)
        LINE_LOOP: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP
          nEpo(if) = nEpo(if)+1
        ENDDO LINE_LOOP
!
! JASON-2 format
! --------------
      ELSE IF (line(1:14)=='# Parameters :') THEN
        iAuxFlg(if)=6
        LINE_LOOP6: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP6
          IF (line(1:1)/='#') THEN
            nEpo(IF) = nEpo(IF)+1
          ENDIF
        END DO LINE_LOOP6
!
! DLR quaternion file
! -------------------
      ELSE IF (line(28:28)=='L') THEN
        iAuxFlg(if)=5
        nEpo(if) = nEpo(if)+1
        LINE_LOOP5: DO
          line = nextline(lfnatt,0)
          IF (line=='')EXIT LINE_LOOP5
          nEpo(if) = nEpo(if)+1
        ENDDO LINE_LOOP5
!
! Wrong Format
! ------------
      ELSE
        WRITE(lfnerr,'(A)') &
              '*** SR readatt: Wrong attitude file format'
        CALL exitrc(2)
      ENDIF
      CLOSE (lfnatt)
    END DO

! ALLOCATE MEMORY
! ---------------
    nEpoFil=MAXVAL(nEpo(1:nfiles))

    ALLOCATE(epoSeq(nEpoFil,nfiles),stat=iac)
    CALL alcerr(iac, 'epoSeq', (/nEpoFil,nfiles/), 'readatt')
    ALLOCATE(attmat(3,3,nEpoFil,nfiles),stat=iac)
    CALL alcerr(iac, 'attmat', (/3,3,nEpoFil,nfiles/), 'readatt')
    ALLOCATE(quatt(4,nEpoFil,nfiles),stat=iac)
    CALL alcerr(iac, 'quatt', (/4,nEpoFil,nfiles/), 'readatt')

! READ ATTITUDE DATA
! ------------------
    DO if=1,nfiles
!
! OPEN ATTITUDE FILE
! ------------------
      CALL opnfil(lfnatt,filnam(if),'OLD',' ', ' ',' ',iostat)
      CALL opnerr(lfnerr,lfnatt,iostat,filnam(if),'READATT')

! CHAMP AUX format
! ----------------
      IF (iAuxFlg(if) == 1) THEN
        iEpo=0
        DO
          READ(lfnatt,'(A)',iostat=irclin) line
          IF (irclin /= 0) line=''
          IF (line(1:10)=='+satellite') THEN
            CALL SLR2COS(line(12:20),leocos(if))
            EXIT
          END IF
          IF (line=='') THEN
            WRITE(lfnerr,'(A)')'*** SR ReadAtt: &
              &No satellite identification number found'
            CALL exitrc(2)
          END IF
        END DO
        DATA_LOOP1: DO
          READ(lfnatt,'(A)',iostat=irclin) line
          IF (irclin /= 0) line=''

          IF (line=='')EXIT DATA_LOOP1
          IF (line(1:3) == 'tim') THEN
            DATA_LOOP2: DO
!             line1 = nextline(lfnatt,0)

              READ(lfnatt,'(A)',iostat=irclin1) line1
              IF (irclin1 /= 0) line1=''

              IF (line1=='')EXIT DATA_LOOP1
              IF (line1(1:3) == 'att') THEN
                READ (line, '(4X,I4,4(1X,I2),1X,F10.7)') jj,mo,id,ih,mi,sec
                READ (line1, '(8X,4(1X,F13.10))') quat(1:4)
                IF (quat(1) == -999.D0) CYCLE
                day=id+ih/24.D0+mi/1440.D0+sec/86400.D0
                iEpo = iEpo+1
                epoSeq(iEpo,if)=DJUL(jj,mo,day)

                IF (iEpo==1) CALL COS2PRN(2,leocos(if),epoSeq(iEpo,if),leonum(if),irccos)
                quatt(1:4,iEpo,IF)=quat(1:4)
                CYCLE DATA_LOOP1
              ELSE IF (line1(1:3) == 'tim') THEN
                line(1:lineLength)=line1(1:lineLength)
                CYCLE DATA_LOOP2
              ELSE
                CYCLE DATA_LOOP2
              END IF
            END DO DATA_LOOP2
          END IF
        ENDDO DATA_LOOP1
!
! JASON CNES format
! -----------------
      ELSE IF (iAuxFlg(if) == 2) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        leonum(if)=908
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        iEpo=0
        DATA_LOOP3: DO
          line = nextline(lfnatt,0)
          IF (line=='') EXIT DATA_LOOP3
          READ (line, '(I4,5(1X,I2),1X,I3,4(1X,F12.6))')            &
          &            jj,mo,id,ih,mi,isec1,isec2,quat(1:4)
          day=id+ih/24.D0+mi/1440.D0+(isec1+isec2/1000.D0)/86400.D0
          iEpo = iEpo+1
          epoSeq(iEpo,if)=DJUL(jj,mo,day)
          GPSUTC=DGPSUT(epoSeq(iEpo,if))
          epoSeq(iEpo,if)=epoSeq(iEpo,if)+GPSUTC/86400.D0
          quatJA(1:3)=quat(2:4)
          quatJA(4)=quat(1)
          quatt(1:4,iEpo,if)=quatJA(1:4)
        END DO DATA_LOOP3
!
! GRACE format
! ------------
      ELSE IF (iAuxFlg(if) == 3) THEN
        iEpo=0
        sec=0.D0
        leonum(if)=0
        DATA_LOOP_GRACE_H:DO
          READ(lfnatt,'(A)',iostat=irclin) line
          IF (line(1:13)=='END OF HEADER')  EXIT DATA_LOOP_GRACE_H
          IF (line(1:14)=='SATELLITE NAME') THEN
            IF (line(33:39)=='GRACE A') THEN
              leonum(if)=909
            ELSE IF (line(33:39)=='GRACE B') THEN
              leonum(if)=910
            ELSE
              WRITE(lfnerr,'(A,A)')'*** SR ReadAtt: &
                &Only GRACE supported,check SATELLITE NAME &
                &in the attitude file',filnam(if)
                CALL exitrc(2)
            END IF
          END IF
          IF (line(1:21)=='TIME EPOCH (GPS TIME)') THEN
            READ (line,'(32X,I4,5(1X,I2))') jj,mo,id,ih,mi,isec1
            day=id+ih/24.D0+mi/1440.D0+isec1/86400.D0
            sec=DJUL(jj,mo,day)
          END IF
        END DO DATA_LOOP_GRACE_H
!
        IF (leonum(if)==0) THEN
          WRITE(lfnerr,'(A)')'*** SR ReadAtt: &
            &Keyword SATELLITE NAME not found in the attitude file',filnam(if)
          CALL exitrc(2)
        ELSE IF (sec==0.D0) THEN
          WRITE(lfnerr,'(A)')'*** SR ReadAtt: &
            &Keyword TIME EPOCH (GPS TIME) not found in the attitude file', &
            &filnam(if)
          CALL exitrc(2)
        END IF
!
        DATA_LOOP_GRACE: DO
          line = nextline(lfnatt,0)
          IF (line=='') EXIT DATA_LOOP_GRACE
          READ (line, *)isec1,line1(1:1),id,quat(1:4)
          quatJA(1:3)=quat(2:4)
          quatJA(4)=quat(1)
          iEpo = iEpo+1
          epoSeq(iEpo,if)=sec+isec1/86400.D0
          quatt(1:4,iEpo,if)=quatJA(1:4)
        END DO DATA_LOOP_GRACE

! GOCE-IAQ format
! ---------------
      ELSE IF (iAuxFlg(if) == 4) THEN
        iEpo=0
        DATA_LOOP_GOCE_IAQ: DO
          line = nextline(lfnatt,0)
          IF (line=='') EXIT DATA_LOOP_GOCE_IAQ
          READ (line,*)sec,quat(1:4)
          iEpo = iEpo+1
          epoSeq(iEpo,if)=GPSMJD(sec,0)
          quatt(1:4,iEpo,if)=quat(1:4)
        END DO DATA_LOOP_GOCE_IAQ
        leonum(if) = 915

! DLR format
! ----------
      ELSE IF (iAuxFlg(if) == 5) THEN
        iEpo=0
        DATA_LOOP_DLR: DO
          line = nextline(lfnatt,0)
          IF (line=='') EXIT DATA_LOOP_DLR
          READ (line, *)jj,mo,id,ih,mi,sec,dummy,quat(4),quat(1:3)
          IF (.NOT.(quat(1) == 0.D0 .AND. quat(2) == 0.D0 .AND. &
                    quat(3) == 0.D0 .AND. quat(4) == 0.D0)) THEN
            day=id+ih/24.D0+mi/1440.D0+sec/86400.D0
            iEpo = iEpo+1
            epoSeq(iEpo,if)=DJUL(jj,mo,day)

            quatt(1:4,iEpo,if)=quat(1:4)
          END IF
        END DO DATA_LOOP_DLR
        IF (dummy == 'L13') THEN
          leoname='TSAR L13'
        ELSEIF (dummy == 'L14') THEN
          leoname='GRAS L14'
        ELSEIF (dummy == 'L20') THEN
          leoname='TDEM L20'
        ENDIF
        CALL LEOPRN(leoname,epoSeq(iEpo,IF),leonum(IF))
!
! JASON-2 format
! --------------
      ELSE IF (iAuxFlg(if) == 6) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        leonum(if)=927
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        iEpo=0
        DATA_LOOP6: DO
          line = nextline(lfnatt,0)
          IF (line=='') EXIT DATA_LOOP6
          IF (line(1:1)/='#') THEN
            READ (line(1:23), '(I4,5(1X,I2),1X,I3,4(1X,F12.6))')            &
            &            jj,mo,id,ih,mi,isec1,isec2
            READ (line(24:179), *) idum1a,quat(1),idum1b,&
                                   idum2a,quat(2),idum2b,&
                                   idum3a,quat(3),idum3b,&
                                   idum4a,quat(4),idum4b

            day=id+ih/24.D0+mi/1440.D0+(isec1+isec2/1000.D0)/86400.D0
            iEpo = iEpo+1
            epoSeq(iEpo,IF)=DJUL(jj,mo,day)
            GPSUTC=DGPSUT(epoSeq(iEpo,if))
            epoSeq(iEpo,IF)=epoSeq(iEpo,IF)+GPSUTC/86400.D0
            quatJA(1:3)=quat(2:4)
            quatJA(4)=quat(1)
            quatt(1:4,iEpo,if)=quatJA(1:4)
          ENDIF
        END DO DATA_LOOP6
!
! Pre-processed attitude
! ----------------------
      ELSE IF (iAuxFlg(if) == 0) THEN
        line = nextline(lfnatt,0)
        DATA_LOOP: DO iEpo=1,nEpo(if)
          line = nextline(lfnatt,0)
          READ(line,'(10F18.14)')epoSeq(iEpo,if),&
!!            attmat(1,:,iEpo,if),attmat(2,:,iEpo,if),attmat(3,:,iEpo,if)
            hlp(1,:),hlp(2,:),hlp(3,:)
          epoSeq(iepo,if)=epoSeq(iepo,if)+mjdepo(if)
          hlp=transpose(hlp)
          CALL mat2qua(hlp,quat)
          quatt(1:4,iEpo,if)=quat(1:4)
          IF (iEpo==1) CALL COS2PRN(2,leocos(if),mjdepo(if),leonum(if),irccos)
        ENDDO DATA_LOOP
      ENDIF
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
      IF (leosvn==leonum(if)) GOTO 500
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
            '*** SR readatt: The requested epoch is out of the table. ',&
            '                Requested epoch:    ',epoch,&
            '                Last epoch in table:',epoSeq(nEpo(if),if)+mjdEpo(if)
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
            '*** SR readatt: The requested epoch is out of the table. ',&
            '                Requested epoch:     ',epoch,&
            '                First epoch in table:',epoSeq(iEpo,if)+mjdEpo(if)
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
          IF (epo +SPACING(mjdEpo(if)) >= epoSeq(iEpo,if).AND.         &
          &   epo+SPACING(mjdEpo(if))<epoSeq(iEpo+1,if)) THEN
            iTim=iEpo+1
            kEpo(if)=iEpo+1
            EXIT EPO3_LOOP
          ELSEIF (iEpo==nEpo(if)-1) THEN
            WRITE(lfnerr,'(A,F15.7)')&
                 '*** SR readatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=4
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO3_LOOP
      ELSEIF (epo < epoSeq(kEpo(if),if)) THEN
        IF (kEpo(if) < nEpo(if)) lEpo(if)=kEpo(if)+1
        EPO4_LOOP: DO iEpo=lEpo(if),1,-1
          IF (epo+SPACING(mjdEpo(if)) >= epoSeq(iEpo,if).AND.    &
            & epo+SPACING(mjdEpo(if))<epoSeq(iEpo+1,if)) THEN
            iTim=iEpo+1
            kEpo(if)=iEpo+1
            EXIT EPO4_LOOP
          ELSEIF (iEpo == 1) THEN
            WRITE(lfnerr,'(A,F15.7)')&
                 '*** SR readatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=4
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
                 '*** SR readatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=1
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
                 '*** SR readatt: No attitude data found for the requested&
                 & epoch:',epoch
            irc=1
            jRight=0
            jLeft=0
            kEpo(if)=1
            lEpo(if)=1
          ENDIF
        ENDDO EPO6_LOOP
      ENDIF

! Interpolation of the quaternions
! --------------------------------
! Copy of two involved quaternions for easier handling
! ----------------------------------------------------

      IF (irc == 0) THEN
! Prepare extended search for good starting values
        nLeft  = 5
        nRight = 5
        nSize  = SIZE(quatt(1,:,IF))
        corrFailed = .TRUE.
        largeGap   = .FALSE.

! Loop to the left
        leftLoop: DO iLeft = 0, nLeft
          jLeft = iLeft
          IF (kepo(IF)-jLeft < 1) EXIT leftLoop
          quatA(1:4)=quatt(1:4,kepo(IF)-jLeft,IF)

! Loop to the right
          rightLoop: DO iRight = 1, nRight
            jRight = iRight
            IF (kepo(IF)+jRight > nSize) EXIT rightLoop
            quatB(1:4)=quatt(1:4,kepo(IF)+jRight,IF)
!!      quatA(1:4)=quatt(1:4,kepo(IF),IF)
!!      quatB(1:4)=quatt(1:4,kepo(IF)+1,IF)

! Ambiguity in quaternions
            IF (quatA(1)*quatB(1)<0.AND.&
                 &quatA(2)*quatB(2)<0.AND.quatA(3)*quatB(3)<0) THEN
              quatB(1:4)=-quatB(1:4)
            ENDIF

! Quaternion describing rotation between two epochs
            quatAB(4) = quatA(4)*quatB(4) + quatA(1)*quatB(1) +&
                        quatA(2)*quatB(2) + quatA(3)*quatB(3)
            quatAB(1) = quatA(4)*quatB(1) - quatA(1)*quatB(4) +&
                        quatA(3)*quatB(2) - quatA(2)*quatB(3)
            quatAB(2) = quatA(4)*quatB(2) - quatA(2)*quatB(4) +&
                        quatA(1)*quatB(3) - quatA(3)*quatB(1)
            quatAB(3) = quatA(4)*quatB(3) - quatA(3)*quatB(4) +&
                        quatA(2)*quatB(1) - quatA(1)*quatB(2)

! Rotation angle
            delta = dmod(2*dacos(DMIN1(quatAB(4),1.D0)),2*PI)

! Save nominal values used for interpolation
            IF (jLeft == 0 .AND. jRight == 1) THEN
              quatA_nom = quatA
              quatB_nom = quatB
              quatAB_nom = quatAB
              delta_nom  = delta
            ENDIF

! Time gap between considered epochs
            gap = ABS(epoSeq(kEpo(IF)+jRight,IF)-epoSeq(kEpo(IF)-jLeft,IF))*86400.d0

! Check for large gaps
            IF (gap > 5.d0) THEN
              largeGap   = .TRUE.
              EXIT leftLoop
            ENDIF

! Check for large rotation angles
            IF (delta < 0.003) THEN
              corrFailed = .FALSE.
              EXIT leftLoop
            ENDIF

          ENDDO rightLoop
        ENDDO leftLoop

! Do not change interpolation if no better boundary values were found
        IF (corrFailed) THEN
          jLeft  = 0
          jRight = 1
          quatA  = quatA_nom
          quatB  = quatB_nom
          quatAB = quatAB_nom
          delta  = delta_nom
          IF (.NOT. largeGap) THEN
            WRITE(lfnerr,'(/,A,/,17X,A,F15.6,/)')                                 &
                 ' ### SR ReadAtt: Could not correct for problematic interpolation ', &
                                  'Epoch: ',epo
          ENDIF
        ENDIF

! Interpolation of rotation angle
        deltat = delta *&
            (epo-epoSeq(kEpo(IF)-jLeft,IF))/(epoSeq(kEpo(IF)+jRight,IF)-epoSeq(kEpo(IF)-jLeft,IF))

! Corresponding quaternion
        quatDT(4) = dcos(deltat/2)
        kappa  = dsqrt((1-quatDT(4)**2)/(1-quatAB(4)**2))
        quatDT(1:3) = kappa*quatAB(1:3)

! Interpolated quaternion
        quat(4) = quatA(4)*quatDT(4) - quatA(1)*quatDT(1) -&
                  quatA(2)*quatDT(2) - quatA(3)*quatDT(3)
        quat(1) = quatA(4)*quatDT(1) + quatA(1)*quatDT(4) -&
                  quatA(3)*quatDT(2) + quatA(2)*quatDT(3)
        quat(2) = quatA(4)*quatDT(2) + quatA(2)*quatDT(4) -&
                  quatA(1)*quatDT(3) + quatA(3)*quatDT(1)
        quat(3) = quatA(4)*quatDT(3) + quatA(3)*quatDT(4) -&
                  quatA(2)*quatDT(1) + quatA(1)*quatDT(2)
        CALL quamat(quat,hlp(:,:))
      ELSE
        hlp=0.D0
      ENDIF

! End: Interpolation of quaternions

!======================================================================
!= Old: Interpolation of rotation matrix ==============================
!======================================================================
!!      hlp(:,:)=(attmat(:,:,kEpo(if)+1,if)*(epo-epoSeq(kEpo(if),if))+&
!!           &(attmat(:,:,kEpo(if),if)*(epoSeq(kEpo(if)+1,if)-epo)))/&
!!           &(epoSeq(kEpo(if)+1,if)-epoSeq(kEpo(if),if))
!======================================================================
      epoDif = epoSeq(kepo(IF)+jRight,IF)-epoSeq(kepo(IF)-jLeft,IF)
      IF (iSel == 0) THEN
! T,-N,-R
        att(:,:)=hlp(:,:)
      ELSEIF (iSel == 1) THEN
! R,T,N
        att(:,1)=-hlp(:,3)
        att(:,2)=+hlp(:,1)
        att(:,3)=-hlp(:,2)
      ENDIF
    CASE DEFAULT
  END SELECT

  IF ((iOpt==0.OR.iOpt==1).AND.irc<2) THEN

    epoch = epoSeq(iTim,if)
    IF (iTim < nEpo(if)) THEN
      epoDif = epoSeq(iTim+1,if)-epoSeq(iTim,if)
    ELSE
      epoDif = epoSeq(iTim,if)-epoSeq(iTim-1,if)
    ENDIF

    IF (iSel == 0) THEN
! T,-N,-R
      att(:,:)=attmat(:,:,iTim,if)
    ELSEIF (iSel == 1) THEN
! R,T,N
      att(:,1)=-attmat(:,3,iTim,if)
      att(:,2)=+attmat(:,1,iTim,if)
      att(:,3)=-attmat(:,2,iTim,if)
    ENDIF
  ENDIF

  999 CONTINUE
  RETURN
END SUBROUTINE readatt


END MODULE
