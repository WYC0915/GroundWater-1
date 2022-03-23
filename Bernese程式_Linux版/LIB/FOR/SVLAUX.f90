MODULE s_SVLAUX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE svlaux(opt,iretrn)

!-------------------------------------------------------------------------
! Purpose:   Save information of LEO-Auxiliary File in seperate
!            Attitude, Acceleration and Manoeuver File
!
! Author:    H.Bock
!
! Created:   10-Aug-2000
! Last mod.: 21-Sep-2010
!
! Changes:   19-Jan-2001  HB: Change format of writing man-file
!            29-Dec-2001  HU: Use d_const
!            20-May-2003  RD: Init time window to (/0d0,1d20/)
!            24-Jun-2003  HB: Correct use of time window
!            08-Nov-2004  HB: Add GOCE QUA file
!            03-Jan-2005  HB: Cut/concatenate GOCE-AUX-files
!                             (STR_QUA or EGG_IAQ)
!            08-Aug-2005  HB: Use new SR TIMST2 (module)
!            29-Sep-2005  HB: Write GOCE-files from strings and not from
!                             real variables (precision ...)
!            12-Feb-2007  HB: Write concatenated file with trim(line)
!            11-Mar-2008  HB: Rotation of star tracker quaternions
!                             implemented (not yet fixed)
!            19-May-2009  UM: Correct angles for star tracker quaternions
!                             implemented, read star tracker ID from file
!            19-Jun-2009  HB: Ignore star tracker quaternions with 0,0,0,1
!            20-Jul-2009  UM: Correct angles from document implemented
!            21-Dec-2009  UM: New angles from in-flight calibration
!                             implemented
!            21-Sep-2010  RD: ST2TIM can be used as a module now
!
! SR used:
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: date,time,pi
  USE p_leoaux, ONLY: t_leoaux_opt, t_chphead, t_chpdata, initia, head
  USE s_opnfil
  USE f_djul
  USE s_opnerr
  USE s_timst2
  USE s_quamat
  USE s_rdlaux
  USE s_st2tim
  USE s_gtflna
  USE s_alcerr
  USE s_gtfile2
  USE f_gpsmjd
  USE f_nextline

  IMPLICIT NONE

! List of Parameters
! ------------------
! IN
  TYPE(t_leoaux_opt) :: opt    ! Option structure
  INTEGER(i4b)       :: iretrn ! Return code

! List of Local Parameters
! ------------------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'SVLAUX'

! Local Variables
! ---------------
  CHARACTER(LEN=filenamelength), DIMENSION(:,:), POINTER :: filcon
  CHARACTER(LEN=fileNameLength),DIMENSION(3),SAVE :: filnam
  CHARACTER(LEN=fileNameLength),SAVE              :: filLeo
  CHARACTER(LEN=lineLength)                       :: line
  CHARACTER(LEN=3),SAVE :: xyzflg
  CHARACTER(LEN=20),SAVE :: datStr
  CHARACTER(LEN=timStrgLength2)                   :: winStr

  INTEGER(i4b),SAVE  :: isel
  INTEGER(i4b)       :: irc
  INTEGER(i4b)       :: iac
  INTEGER(i4b)       :: ii
  INTEGER(i4b)       :: iYear
  INTEGER(i4b)       :: iMonth
  INTEGER(i4b)       :: iDoy
  INTEGER(i4b)       :: iostat
  INTEGER(i4b)       :: nflcol
  INTEGER(i4b)       :: maxfl
  INTEGER(i4b)       :: irLeo
  INTEGER(i4b)       :: nLine
  INTEGER(i4b)       :: iLine
  INTEGER(i4b)       :: nflinp
  INTEGER(i4b)       :: iFil
  INTEGER(i4b)       :: str_id

  REAL(r8b),SAVE                :: fstmjd
  REAL(r8b),SAVE                :: fstepo
  REAL(r8b),SAVE                :: endepo
  REAL(r8b),DIMENSION(3,3),SAVE :: attcal
  REAL(r8b),DIMENSION(3,3)      :: attmat
  REAL(r8b),DIMENSION(3,3)      :: attsys
  REAL(r8b)                     :: dummy
  REAL(r8b)                     :: iDay
  REAL(r8b)                     :: mjdseq
  REAL(r8b)                     :: eposeq
  REAL(r8b)                     :: epMJD
  REAL(r8b)                     :: timGPS
  REAL(r8b)                     :: phi1, phi2, phi3
  REAL(r8b),DIMENSION(4)        :: hq,iq,jq,kq,qu,pq
  REAL(r8b),DIMENSION(1:4,0:3)  :: rq

  TYPE(t_chphead),SAVE :: header
  TYPE(t_chphead)      :: head_out
  TYPE(t_chpdata)      :: cdata

  LOGICAL,SAVE   :: first=.TRUE.
  LOGICAL,SAVE   :: attfst=.TRUE.
  LOGICAL,SAVE   :: accfst=.TRUE.
  LOGICAL,SAVE   :: thrfst=.TRUE.

! INITIALIZE
! ----------
  iretrn = 0

! READ HEADER AT THE FIRST CALL OF THIS SR
! ----------------------------------------
  IF (opt%satnam /= 'GOCE') THEN
    CALL gtflna(1,'LEOAUX',filLeo,irLeo)
    IF (first) THEN
      first=.false.
      datStr = date//' '//time
      isel=1
      CALL rdlaux(isel,head_out,cdata,irc)

! SAVE HEADER INFORMATION
      CALL initia(header)
      header = head_out

! TIME TRANSFORMATION
! FIRST EPOCH (fstmjd= day(MJD), fstepo = partials of the day(fstmjd))
      CALL st2tim(1,1,header%timfst(1:19),fstmjd)
      READ(header%timfst(20:27),'(F8.7)')fstepo
      fstepo = fstepo/86400.D0 + dmod(fstmjd,1.D0)
      fstmjd = dint(fstmjd)
! LAST EPOCH (endepo = partials of the day(fstmjd))
      CALL st2tim(1,1,header%timlst(1:19),dummy)
      READ(header%timlst(20:27),'(F8.7)')endepo
      endepo = endepo/86400.D0 + dmod(dummy,1.D0)
      IF (dummy-fstmjd >=1) endepo = endepo +1

      READ(header%timfst(1:10),*)iYear,iMonth,iDay
      dummy= dJul(iYear,iMonth,iDay)
      iDoy=DNINT(dummy-dJul(iYear,1,1.D0))+1

! ATTITUDE CALIBRATION
      IF (head%asc2sc(1)/=0.D0) THEN
        CALL quamat(head%asc2sc,attcal)
      ELSE
        attcal(:,:)=0.D0
      ENDIF

! ACCELERATION CALIBRATION
      IF (head%faclk(1)==0) THEN
      ENDIF
      IF (head%facak(1)==0) THEN
      ENDIF

! FLAG FOR STORING ACCELERATIONS
      IF (opt%ixyz==1) THEN
        xyzflg='XYZ'
      ELSE IF (opt%ixyz==2) THEN
        xyzflg='RSW'
      ELSE IF (opt%ixyz==0) THEN
        xyzflg='NO '
      ENDIF

! GET FILENAMES OF ATT, ACC AND MAN-FILE
! --------------------------------------
      filnam(:)=''
      nflcol=3
      maxfl=3
      CALL gtflna(1,'ATTIT',filnam(1),irc)
      CALL gtflna(1,'ACCEL',filnam(2),irc)
      CALL gtflna(1,'MANEUV',filnam(3),irc)
    ENDIF

! READ AND PROCESS EPOCH SEQUENCE OF ATTITUDE, ACCELERATION AND THRUSTER DATA
! ---------------------------------------------------------------------------
    IF (opt%window(1)==0.D0.OR.&
         (opt%window(1)>=fstmjd+fstepo .AND. opt%window(1)<=fstmjd+endepo)) THEN
      isel = 2
      sequence_loop: DO
        cdata%flgdat(:) = opt%typlst(:)
        CALL rdlaux(isel,head_out,cdata,irc)
! EPOCH
        IF (cdata%flgdat(1) > 0) THEN
          CALL st2tim(1,1,cdata%timstr(1:19),mjdseq)
          READ(cdata%timstr(20:27),'(F8.7)')eposeq
          eposeq = eposeq/86400.D0 + dint(mjdseq-fstmjd)+dmod(mjdseq,1.D0)
          IF (opt%window(2)/=1.D20 .AND. fstmjd+eposeq>opt%window(2)) EXIT sequence_loop
          IF (fstmjd+eposeq > fstmjd+endepo) EXIT sequence_loop
        ENDIF

! ATTITUDE DATA
        IF (cdata%flgdat(5) > 0) THEN
          CALL quamat(cdata%att,attsys)
          IF (attcal(1,1)/=0.D0) THEN
            attmat=matmul(attsys,attcal)
          ELSE
            attmat(:,:)=attsys(:,:)
          ENDIF
! STORE ATTITUDE IN SEPERAT FILE
          IF (attfst) THEN
            attfst=.FALSE.
! OPEN FILE
            CALL opnfil(lfn002+1,filnam(1),'NEW',' ', ' ',' ',iostat)
            CALL opnerr(lfnerr,lfn002+1,iostat,filnam(1),'SVLAUX')
            WRITE(lfn002+1,'(A,A,A,A,I3,A,A,11X,A,/,A,178("-"),/,A,A,/,A,50X,A,/,&
                 &A,25X,A,2(45X,A))')&
                 '* ATTITUDE FILE OF ',head%satnam(1:6),head%satnum,&
                 ' FOR DOY ',iDoy,'-',header%timfst(1:4),datStr,&
                 '* ',&
                 '* Data derived from ',filLeo,&
                 '* MJD','attitude rotation matrix',&
                 '*    Part of MJD','first row','second row','third row'
            write(lfn002+1,'(A,178("-"))')'* '
            WRITE(lfn002+1,'(F7.0)')fstmjd
          ENDIF
          WRITE(lfn002+1,'(10F18.14)')eposeq,&
               attmat(1,:),attmat(2,:),attmat(3,:)
        ENDIF

! ACCELERATION DATA (linear and angular)
        IF (cdata%flgdat(2) > 0 .AND. cdata%flgdat(3) > 0) THEN

! CORRECTIONS TO ACCELERATION DATA
          IF (cdata%flgdat(4) > 0) THEN
            DO ii=1,cdata%flgdat(4) ! NUMBER OF CORRECTIONS
              IF (cdata%facc(ii,1) < 50) THEN ! CORRECTION FOR acl
                IF (cdata%facc(ii,2) == 0) THEN ! CORRECTION IS NOT APPLIED
                  cdata%acl(:) = cdata%acl(:) + cdata%acc(ii,:)
                ENDIF
              ELSE IF (cdata%facc(ii,1) >= 50) THEN ! CORRECTION FOR aca
                IF (cdata%facc(ii,2) == 0) THEN ! CORRECTION IS NOT APPLIED
                  cdata%aca(:) = cdata%aca(:) + cdata%acc(ii,:)
                ENDIF
              ENDIF
            ENDDO
            IF (cdata%flgdat(5) > 0.AND. xyzflg == 'XYZ') THEN
              cdata%acl(:)=matmul(attsys,cdata%acl)
              cdata%aca(:)=matmul(attsys,cdata%aca)
            ENDIF
          ENDIF

! STORE ACCELERATIONS IN SEPERATE FILE
          IF (accfst) THEN
            accfst=.FALSE.
! OPEN FILE
            CALL opnfil(lfn002+2,filnam(2),'NEW',' ', ' ',' ',iostat)
            CALL opnerr(lfnerr,lfn002+2,iostat,filnam(2),'SVLAUX')
            WRITE(lfn002+2,'(A,A,A,A,I3,A,A,7X,A,/,A,131("-"),/,A,A,2(/,A),&
                 &33X,A,33X,A,/,A,14X,A,5(18X,A))')&
                 '* ACCELERATION FILE OF ',head%satnam(1:6),head%satnum,&
                 ' FOR DOY ',iDoy,'-',header%timfst(1:4),datStr,&
                 '* ',&
                 '* Data derived from ',filLeo,&
                 '* SYSTEM',&
                 '* MJD','linear accelerations','angular accelerations',&
                 '*    Part of MJD','R','S','W','R','S','W'
            WRITE(lfn002+2,'(A,131("-"))')'* '
            WRITE(lfn002+2,'(A)')xyzflg
            WRITE(lfn002+2,'(F7.0)')fstmjd
          ENDIF
          IF(cdata%acl(1)==0.D0.AND.cdata%acl(2)==0.D0.AND.cdata%acl(3)==0.D0.AND.&
               cdata%aca(1)==0.D0.AND.cdata%aca(2)==0.D0.AND.cdata%aca(3)==0.D0) THEN
            CYCLE sequence_loop
          ELSE
            WRITE(lfn002+2,'(7F19.14)')eposeq,cdata%acl,cdata%aca
          ENDIF
        ENDIF
! THRUSTER FIRINGS / MANOEUVRES
        IF (cdata%flgdat(6) > 0) THEN
! STORE THRUSTER FIRINGS / MANOEUVRES IN SEPERAT FILE
          IF (thrfst) THEN
            thrfst=.FALSE.
! OPEN FILE
            CALL opnfil(lfn002+3,filnam(3),'NEW',' ', ' ',' ',iostat)
            CALL opnerr(lfnerr,lfn002+3,iostat,filnam(3),'SVLAUX')
            WRITE(lfn002+3,'(A,A,A,A,I3,A,A,7X,A,/,A,78("-"),/,A,A,/,A,/,&
                 &A,4X,A,2X,A)')&
                 '* MANOEUVER FILE OF ',head%satnam(1:6),head%satnum,&
                 ' FOR DOY ',iDoy,'-',header%timfst(1:4),datStr,&
                 '* ',&
                 '* Data derived from ',filLeo,&
                 '* MJD',&
                 '*    Part of MJD','Duration(s)','Thrusters'
            write(lfn002+3,'(A,78("-"))')'* '
            WRITE(lfn002+3,'(F7.0)')fstmjd
          ENDIF
          WRITE(lfn002+3,'(F18.14,F12.3,1X,14I1)')eposeq,cdata%thrpul,cdata%thrflg
        ENDIF
        IF (irc==2) EXIT sequence_loop
      ENDDO sequence_loop
    ELSE
      CALL timst2(1,2,opt%window(:),winStr)
      WRITE(lfnerr,'(A,/,15X,A,/)')&
           '*** SR svlaux: No data available for requested time window ',&
           winstr
      iretrn=1
    ENDIF
    IF (accfst.AND.opt%typlst(4)>0) THEN
      write(lfnErr,'(A,A,/)')&
           '*** SR svlaux: No acceleration data found in ',filLeo
      iretrn=1
    ENDIF
    IF (attfst.AND.opt%typlst(5)>0) THEN
      write(lfnErr,'(A,A,/)')&
           '*** SR svlaux: No attitude data found in ',filLeo
      iretrn=1
    ENDIF
    IF (thrfst.AND.opt%typlst(6)>0) THEN
      write(lfnErr,'(A,A,/)')&
           '*** SR svlaux: No maneuver data found in ',filLeo
    ENDIF
    close(lfn002+1)
    close(lfn002+2)
    close(lfn002+3)

! Cut/Concatenate GOCE-AUX-files (STR_QUA or EGG_IAQ)
! ---------------------------------------------------
  ELSEIF (opt%satnam == 'GOCE') THEN
    IF (opt%strqua == 1) THEN
      ! along-track, cross-track, radial -> along-track, - cross-track, nadir
      pq(1)=-1.D0
      pq(2)=0.D0
      pq(3)=0.D0
      pq(4)=0.D0

      ! unknown star tracker: no rotation
      iq(1) = 0.D0
      iq(2) = 0.D0
      iq(3) = 0.D0
      iq(4) = 1.D0
      rq(4,0) = pq(4)*iq(4) -pq(1)*iq(1) -pq(2)*iq(2) -pq(3)*iq(3)
      rq(1,0) = pq(4)*iq(1) +pq(1)*iq(4) -pq(3)*iq(2) +pq(2)*iq(3)
      rq(2,0) = pq(4)*iq(2) +pq(2)*iq(4) -pq(1)*iq(3) +pq(3)*iq(1)
      rq(3,0) = pq(4)*iq(3) +pq(3)*iq(4) -pq(2)*iq(1) +pq(1)*iq(2)

      DO ii=1,3
        SELECT CASE (ii)
        CASE (1)
          ! star tracker 1: x-axis => -120 degree
!          phi1 = 120.D0*PI/180.D0
!          phi2 = 0.D0*PI/180.D0
!          phi3 = 0.D0*PI/180.D0
!          phi1 = 119.75*PI/180.D0
!          phi2 = -0.12D0*PI/180.D0
!          phi3 = -0.23D0*PI/180.D0
          phi1 = 119.7550538470522*PI/180.D0
          phi2 = -0.063479224690747D0*PI/180.D0
          phi3 = -0.220901871745061D0*PI/180.D0
        CASE (2)
          ! star tracker 1: x-axis => -160 degree
!          phi1 = 160.D0*PI/180.D0
!          phi2 = 0.D0*PI/180.D0
!          phi3 = 0.D0*PI/180.D0
!          phi1 = 160.50D0*PI/180.D0
!          phi2 = 0.21D0*PI/180.D0
!          phi3 = 0.87D0*PI/180.D0
          phi1 = 160.4596470902013D0*PI/180.D0
          phi2 = 0.227547724890249D0*PI/180.D0
          phi3 = 0.901123149025504D0*PI/180.D0
        CASE (3)
          ! star tracker 1: x-axis => -120 degree
          !                 y-axis =>   40 degree
          !                 z-axis =>   90 degree
!          phi1 = 120.D0*PI/180.D0
!          phi2 = -40.D0*PI/180.D0
!          phi3 = -90.D0*PI/180.D0
!          phi1 = 118.86D0*PI/180.D0
!          phi2 = -39.75D0*PI/180.D0
!          phi3 = -89.13D0*PI/180.D0
          phi1 = 118.8718801556816D0*PI/180.D0
          phi2 = -39.711157882250241D0*PI/180.D0
          phi3 = -89.117654395930728D0*PI/180.D0
        END SELECT

        iq(1)= dsin(phi1/2.D0)
        iq(2)= 0.D0
        iq(3)= 0.D0
        iq(4)= dcos(phi1/2.D0)

        jq(1)= 0.D0
        jq(2)= dsin(phi2/2.D0)
        jq(3)= 0.D0
        jq(4)= dcos(phi2/2.D0)

        kq(1)= 0.D0
        kq(2)= 0.D0
        kq(3)= dsin(phi3/2.D0)
        kq(4)= dcos(phi3/2.D0)

        hq(4) = jq(4)*iq(4) -jq(1)*iq(1) -jq(2)*iq(2) -jq(3)*iq(3)
        hq(1) = jq(4)*iq(1) +jq(1)*iq(4) -jq(3)*iq(2) +jq(2)*iq(3)
        hq(2) = jq(4)*iq(2) +jq(2)*iq(4) -jq(1)*iq(3) +jq(3)*iq(1)
        hq(3) = jq(4)*iq(3) +jq(3)*iq(4) -jq(2)*iq(1) +jq(1)*iq(2)

        rq(4,ii) = kq(4)*hq(4) -kq(1)*hq(1) -kq(2)*hq(2) -kq(3)*hq(3)
        rq(1,ii) = kq(4)*hq(1) +kq(1)*hq(4) -kq(3)*hq(2) +kq(2)*hq(3)
        rq(2,ii) = kq(4)*hq(2) +kq(2)*hq(4) -kq(1)*hq(3) +kq(3)*hq(1)
        rq(3,ii) = kq(4)*hq(3) +kq(3)*hq(4) -kq(2)*hq(1) +kq(1)*hq(2)
      END DO
    ENDIF
    CALL gtflna(1,'OUTAUX',filnam(1),irc)
    CALL opnfil(lfn002+1,filnam(1),'NEW',' ', ' ',' ',iostat)
    CALL opnerr(lfnerr,lfn002+1,iostat,filnam(1),srName)
    NULLIFY(filcon)
    CALL gtfile2('CONAUX',1,nflinp,filcon)
! Open file
! ---------
    DO iFil = 1,nflinp
      CALL opnfil(lfn002,filcon(1,iFil),'OLD',' ', 'READONLY',' ',iostat)
      CALL opnerr(lfnerr,lfn002,iostat,filcon(1,iFil),srName)
      nLine = 0
      DO
        line = nextline(lfn002,0)
        IF (line(1:3) == '') EXIT
        nLine = nLine + 1
      ENDDO
      REWIND (lfn002)
      lineLoop: DO iLine = 1,nLine
        line = nextline(lfn002,0)
        READ(line,*)timGPS
        epMJD = GPSMJD(timGPS,0)
        IF (opt%window(1)==0.D0.OR.&
             (epMJD>=opt%window(1) .AND. epMJD<=opt%window(2))) THEN
          IF (opt%strqua == 1) THEN
            READ(line,*)timGPS,hq(1:4),str_id
            IF (hq(4)==1.D0) CYCLE
            IF (str_id<1 .OR. str_id>3) THEN
              WRITE(lfnErr,'(A,I1,A,A,/)')&
               '*** SR svlaux: Unknown star tracker ',str_id,&
               ' in ',filcon(1,iFil)
              iretrn=1
              str_id=0
            END IF
            qu(4) = hq(4)*rq(4,str_id) -hq(1)*rq(1,str_id) &
             -hq(2)*rq(2,str_id) -hq(3)*rq(3,str_id)
            qu(1) = hq(4)*rq(1,str_id) +hq(1)*rq(4,str_id) &
             -hq(3)*rq(2,str_id) +hq(2)*rq(3,str_id)
            qu(2) = hq(4)*rq(2,str_id) +hq(2)*rq(4,str_id) &
             -hq(1)*rq(3,str_id) +hq(3)*rq(1,str_id)
            qu(3) = hq(4)*rq(3,str_id) +hq(3)*rq(4,str_id) &
             -hq(2)*rq(1,str_id) +hq(1)*rq(2,str_id)
            WRITE(lfn002+1,'(F20.9,4(1X,E15.8E2))')timGPS,qu(1:4)
          ELSE
            WRITE(lfn002+1,'(A)')line(1:85)
          ENDIF
        ELSEIF (epMJD >opt%window(2)) THEN
          EXIT lineLoop
        ENDIF
      ENDDO lineLoop
      CLOSE (lfn002)
    ENDDO
    CLOSE (lfn002+1)
  ENDIF
  RETURN
  END SUBROUTINE svlaux

END MODULE
