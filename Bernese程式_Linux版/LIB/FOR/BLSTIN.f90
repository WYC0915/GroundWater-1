MODULE s_BLSTIN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE blstin(meaTyp, satSys, maxLen, minLen, maxAr, minAr, &
                  rcvrName, exclpatt)

! -------------------------------------------------------------------------
! Purpose:    Read the input options for program BASLST
!
! Author:     M. Meindl
!
! Created:    19-Sep-2001
!
! Changes:    27-Sep-2001 MM: Use option check routines
!             14-Mar-2002 MM: New optios (% of resolved ambiguities,
!                             minimum baseline length)
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 CU: Nullify local pointers
!             02-Sep-2003 MM: minAr added, firstChar removed
!             09-Sep-2003 HU: Receiver names chr16 -> chr20
!             31-Mar-2005 MF: Exclude receiver name pattern
!             19-Oct-2006 MM: New options (GPS-only and GNSS baselines)
!             14-Nov-2011 SL: m_bern w/ ONLY, no PRITIT call (moved to program)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,    ONLY: i4b, r8b, fileNameLength, keyValueLength, lfnPrt
  USE s_ckoptr
  USE s_prflna
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! output:
  INTEGER(i4b)                  :: meaTyp         ! 1 = code
                                                  ! 2 = phase
  INTEGER(i4b)                  :: satSys         ! 1 = any system
                                                  ! 2 = GPS only
                                                  ! 3 = GPS+GLONASS
  REAL(r8b)                     :: maxLen         ! maximum BL length
  REAL(r8b)                     :: minLen         ! minimum BL length
  REAL(r8b)                     :: maxAr          ! max % of resolved amb.
                                                  ! (-1: not used)
  REAL(r8b)                     :: minAr          ! min % of resolved amb.
                                                  ! (-1: not used)
  CHARACTER(len=20)             :: rcvrName       ! name of receiver
  CHARACTER(len=20)             :: exclpatt       ! exclude pattern

! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength)                      :: srName
  CHARACTER(LEN=fileNameLength)                      :: sumAr
  CHARACTER(len=keyValueLength),DIMENSION(:),POINTER :: keyValue
  CHARACTER(LEN=11),DIMENSION(3)                     :: sysStr
  INTEGER(i4b)                                       :: irc, ircSum

! Initialize parameters
! ---------------------
  srName    = 'SR blstin (PG baslst)'
  ircSum    = 0
  sysStr    = (/"Any system ","GPS only   ","GPS+GLONASS"/)
  maxLen    = 0
  minLen    = 0
  rcvrName  = ' '
  exclpatt  = ' '
  maxAr     = -1
  minAr     = -1
  NULLIFY(keyValue)

! Read options
! ------------
! measurement type
  CALL readkeys('MEATYP',keyValue,irc)
  CALL ckoptc(1,'MEATYP',keyValue,(/"CODE ","PHASE"/),srName,              &
              'Measurement type',irc,ircSum,result1=meaTyp)

! satellite system
  CALL readkeys('SATSYS',keyValue,irc)
  CALL ckoptc(1,'SATSYS',keyValue,                                         &
              (/"ANY_SYSTEM ","GPS_ONLY   ","GPS+GLONASS"/),srName,        &
              'Baselines with observations from',irc,ircSum,result1=satSys)

! baseline length
  CALL readkeys('MAXLEN', keyValue, irc)
  CALL ckoptr(1,'MAXLEN',keyValue,srName,'Maximum baseline length',       &
              irc,ircSum,maxVal=1,ge=0d0,result1=maxLen)

  CALL readkeys('MINLEN', keyValue, irc)
  CALL ckoptr(1,'MINLEN',keyValue,srName,'Minimum baseline length',       &
              irc,ircSum,maxVal=1,ge=0d0,result1=minLen)

! resolved ambiguities
  CALL gtflna(0,'ARSUM',sumAr,irc)
  IF (irc==0 .AND. meaTyp==2) THEN
    CALL readkeys('MAXAR',keyValue,irc)
    CALL ckoptr(1,'MAXAR',keyValue,srName,                                &
                'Max number of resolved ambiguities',irc,ircSum,          &
                ge=0.D0,le=100.D0,result1=maxAr)
    CALL readkeys('MINAR',keyValue,irc)
    CALL ckoptr(1,'MINAR',keyValue,srName,                                &
                'Min number of resolved ambiguities',irc,ircSum,          &
                ge=0.D0,le=100.D0,result1=minAr)
  ENDIF

! receiver name
  CALL readkeys('RECVR', keyValue, irc)
  CALL ckoptl(1,'RECVR',keyValue,srName,'Name of receiver',irc,ircSum,    &
              maxLength=20,maxVal=1,empty=' ',result1=rcvrName)

! Exclude pattern for receiver name
  CALL readkeys('EXCLU', keyValue, irc)
  CALL ckoptl(1,'EXCLU',keyValue,srName,'Receiver name pattern',irc,ircSum, &
              maxLength=20,maxVal=1,empty=' ',result1=exclpatt)

! On error exit
! -------------
  IF (ircSum /=0) call exitrc(2)

! Print some output
! -----------------
  CALL prflna
  WRITE(lfnprt,'(A)')           ' Options for baseline selection'
  WRITE(lfnprt,'(A)')           ' ------------------------------'
  WRITE(lfnprt,'(A,A)')     ' Observations from:            ', sysStr(satSys)
  WRITE(lfnprt,'(A,F7.1,A3)')   ' Maximum baseline length:      ', maxLen,' km'
  WRITE(lfnprt,'(A,F7.1,A3)')   ' Minimum baseline length:      ', minLen,' km'

  IF (maxAr /= -1) THEN
    WRITE(lfnprt,'(A,F7.1,A2)') ' Maximum resolved ambiguities:  ',maxAr,' %'
    WRITE(lfnprt,'(A,F7.1,A2)') ' Minimum resolved ambiguities:  ',minAr,' %'
  ENDIF
  WRITE(lfnprt,'(A,A20)')       ' Station receiver name:         ',rcvrName
  WRITE(lfnprt,'(A,A20,//)')    ' Exclude receiver name pattern: ',exclpatt

  IF (meaTyp==1) THEN
    CALL prfile('CSHFIL','',1)
  ELSE
    CALL prfile('PSHFIL','',1)
  ENDIF

  WRITE(lfnprt,'(//,A3,1X,A9,7X,A4,1X,A4,2X,A6,2X,A9,13X,A9,12X,A10)')     &
   ' FL','File name','Sta1','Sta2','Length','Receiver1','Receiver2','AR (%) G R'
  WRITE(lfnprt,'(A,A)') ' ------------------------------------------',    &
                        '-------------------------------------------------'
  DEALLOCATE(keyValue,stat=irc)


! End of subroutine
! -----------------
END SUBROUTINE blstin

END MODULE
