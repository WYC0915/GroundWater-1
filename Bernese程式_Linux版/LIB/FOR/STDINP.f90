MODULE s_STDINP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE stdinp(narc,numsat,tbound,nsat,navnum,jarc,svn,tstart,tend, &
                  deltat,nsub,iplt,ityp,ihelm,niter,edit,title,iretrn)

! -------------------------------------------------------------------------
! Purpose:    Read input options for program STDDIF
!
! SR called:  DJUL  , GTFLNA, JMT   , UPPERC
!
! Author:     U. Hugentobler
!
! Created:    16-May-2001     Adapted from F77 subroutine
! Last mod.:  07-Jan-2009
!
! Changes:    26-Jun-2001 JD: Allow infinitesimal range for epoch testing
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 RD: Nullify local pointers
!             20-May-2003 RD: Use SR gttimwin instead of readsess
!             30-Oct-2003 HU: Allow for empty title, empty satnum is 0
!             25-Nov-2003 RD: Adapt to new input panel
!             15-Aug-2006 HU: Default window from orbit overlap
!             07-Jan-2009 RD: Correct format statement for error message
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_exitrc
  USE s_ckoptr
  USE s_ckoptb
  USE s_ckoptc
  USE s_jmt
  USE s_readkeys
  USE s_prflna
  USE s_priwin
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  USE s_gttimwin
  IMPLICIT NONE

! Declaration of Parameters
! -------------------------

! IN
  INTEGER(i4b)                :: narc     ! number of arcs
  INTEGER(i4b),DIMENSION(*)   :: numsat   ! number of sat. per arc, i=1,narc
  REAL(r8b),DIMENSION(2,*)    :: tbound   ! interval boundaries for arc i
  INTEGER(i4b)                :: nsat     ! number of satellites
  INTEGER(i4b),DIMENSION(*)   :: navnum   ! sat. numbers of all arcs, i=1,nsat

! OUT
  INTEGER(i4b)                :: jarc     ! selected arc number (0: all)
  INTEGER(i4b)                :: svn      ! selected satellite number (0:all)
! IN/OUT
  REAL(r8b)                   :: tstart   ! start of time interval (MDJ)
  REAL(r8b)                   :: tend     ! end of time interval (MJD)
! OUT
  REAL(r8b)                   :: deltat   ! interval between two prints (days)
  INTEGER(i4b)                :: nsub     ! number of sub-intervals
  INTEGER(i4b)                :: iplt     ! =0: no plot file to be created
                                          ! =1: create plot file
  INTEGER(i4b)                :: ityp     ! =1: print residuals
                                          ! =2: print rms
                                          ! =3: print residuals for all sats
  INTEGER(i4b),DIMENSION(8)   :: ihelm    ! Helmert transformation paramteters
                                          ! 1-3: translation
                                          ! 4-6: rotation
                                          !   8: scale
  INTEGER(i4b)                :: niter    ! number of iterations to remove bad
                                          ! satellites from helmert parameter
                                          ! determination:
                                          !  =  1: No screening
                                          !  =100: screening
  REAL(r8b)                   :: edit     ! screening criterium = edit*rms
  CHARACTER(LEN=80)           :: title    ! title for plot file
  INTEGER(i4b)                :: iretrn   ! return code =0: ok
                                          !             =1: end

! Declaration of Local Variables
! ------------------------------
  INTEGER(i4b)                 :: irc,iac,irCode=0
  INTEGER(i4b)                 :: iarc,jarca,jarcb
  INTEGER(i4b)                 :: j1,m1,id1
  INTEGER(i4b)                 :: j2,m2,id2

  REAL(r8b)                    :: d1,h1,d2,h2,deltt
  REAL(r8b),DIMENSION(2)       :: window
  CHARACTER(LEN=22)            :: srName='sr stdinp (pgm stddif)'
  CHARACTER(LEN=fileNameLength):: filnam

! keyword array
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue

! Init variables
! --------------
  NULLIFY(keyValue)

! Title
! -----
  CALL readKeys('TITLE', keyValue, irc)
  CALL ckoptl(0,'TITLE', keyValue, srName, 'Title', irc, irCode, &
              empty=' ',maxLength=80, maxVal=1,result1=title)

! List of File Names
! ------------------
  CALL prflna

! Print List of Available Arcs
! ----------------------------
  WRITE(lfnprt,"(/,' LIST OF SATELLITE ARCS',&
                &/,' ----------------------',&
                &/,' NUMBER OF ARCS  :',I6,/,&
                &/,' ARC         FROM              TO')") narc

  DO iarc=1,narc
    CALL jmt(tbound(1,iarc),j1,m1,d1)
    ID1=D1
    h1=(d1-id1)*24.D0
    CALL jmt(tbound(2,iarc),j2,m2,d2)
    id2=d2
    h2=(d2-id2)*24.D0
    WRITE(lfnprt,"(I4,2(3X,I4,2I3,F5.1))") iarc,j1,m1,id1,h1,j2,m2,id2,h2
  ENDDO

! List of Available Satellites
! ----------------------------
  WRITE(lfnprt,"(/,' SATELLITES:',4(15I4/,12X))")navnum(1:nsat)

! Read Arc Number
! ---------------
  CALL readKeys('ARCNUMB', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'ALL') THEN
    jarc = 0
  ELSE
    CALL ckopti(1,'ARCNUMB', keyValue, srName, 'Arc number', irc, irCode, &
                empty=0, ge=0, maxVal=1, result1=jarc)
  ENDIF
  IF(jarc > narc) THEN
    WRITE(lfnerr,"(/,' *** SR STDINP: Arc not found',&
                  &/,16X,'Arc number:',I3,/)") jarc
    CALL exitrc(2)
  ENDIF

  IF(jarc==0)THEN
    WRITE(lfnprt,"(/,' SELECTED ARC    :   ALL',//)")
    jarca=1
    jarcb=narc
  ELSE
    WRITE(lfnprt,"(/,' SELECTED ARC    :',I6,//)")jarc
    jarca=jarc
    jarcb=jarc
  ENDIF

! Time Interval to be Displayed
! -----------------------------
  CALL gttimwin('WINDOW',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)

  IF (window(1) /= 0D0  .AND. window(1) *(1+EPSILON(window(1))) < tstart .OR. &
      window(2) /= 1D20 .AND. window(2) *(1-EPSILON(window(2))) > tend) THEN

    WRITE(lfnerr,'(/,A,/,16X,A,/)') &
    ' ### SR STDINP: Input window too large. Reduced to part overlapping', &
    'with intersection of the two orbits'
  ENDIF


  IF (window(1) < tstart) window(1)=tstart
  IF (jarc > 0 .AND. window(1)<=tbound(1,jarca)) THEN
    tstart=tbound(1,jarca)
  ELSE
    tstart=window(1)
  ENDIF

  IF (window(2) > tend) window(2)=tend
  IF (jarc > 0 .AND. window(2)>=tbound(2,jarcb)) THEN
    tend  =tbound(2,jarcb)
  ELSE
    tend  =window(2)
  ENDIF


!  WRITE(lfnprt,"(/,' START EPOCH     :',F15.6,&
!                &/,' END EPOCH       :',F15.6)") tstart,tend
  CALL priwin(1,(/tstart,tend/))

! Print Interval
! --------------
  CALL readKeys('DELTAT', keyValue, irc)
  CALL ckoptr(1,'DELTAT', keyValue, srName, 'Table interval', irc, irCode, &
              gt=0D0, maxVal=1, result1=deltt)
  deltat=deltt/1440.D0

! NUMBER OF SUB-INTERVALS
! -----------------------
  CALL readKeys('SUBINT', keyValue, irc)
  CALL ckopti(1,'SUBINT', keyValue, srName, 'Number of subintervals', irc, &
              irCode, ge=1, maxVal=1, result1=nsub)

! Print Residuals or RMS
! ----------------------
  CALL readKeys('TYPE', keyValue, irc)
  CALL ckoptc(1,'TYPE', keyValue, (/'RESIDUALS','RMS      ','RES_TABLE'/), &
              srName, 'Output type', irc, irCode, maxVal=1, result1=ityp)

! Helmert Transformation
! ----------------------
  CALL ckoptb(1, (/'HELMTRSX'/), srName, 'Helmert translation X', &
              irCode, result1=ihelm(1))
  CALL ckoptb(1, (/'HELMTRSY'/), srName, 'Helmert translation Y', &
              irCode, result1=ihelm(2))
  CALL ckoptb(1, (/'HELMTRSZ'/), srName, 'Helmert translation Z', &
              irCode, result1=ihelm(3))
  CALL ckoptb(1, (/'HELMROTX'/), srName, 'Helmert rotation X', &
              irCode, result1=ihelm(4))
  CALL ckoptb(1, (/'HELMROTY'/), srName, 'Helmert rotation Y', &
              irCode, result1=ihelm(5))
  CALL ckoptb(1, (/'HELMROTZ'/), srName, 'Helmert rotation Z', &
              irCode, result1=ihelm(6))
  ihelm(7)=0
  CALL ckoptb(1, (/'HELMSCAL'/), srName, 'Helmert scale', &
              irCode, result1=ihelm(8))


! Screening
! ---------
  CALL readKeys('SCREEN', keyValue, irc)
  CALL ckoptc(1,'SCREEN', keyValue, (/'0','1'/), srName, &
              'Remove bad satellites', irc, irCode, valList=(/1,100/), &
              maxVal=1, result1=niter)

  CALL readKeys('EDIT', keyValue, irc)
  CALL ckoptr(1,'EDIT', keyValue, srName, 'Screening criterium', irc, irCode, &
              gt=0D0, maxVal=1, result1=edit)

! Satellite Number
! ----------------
  CALL readKeys('SATNUM', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'ALL') THEN
    svn = 0
  ELSE
    CALL ckopti(1,'SATNUM', keyValue, srName, 'Satellite number', irc, &
                irCode, empty=0, ge=0, maxVal=1, result1=svn)
  ENDIF

  IF (svn==0) THEN
    WRITE(lfnprt,"(' SATELLITE NUMBER:   ALL'/)")
  ELSE
    WRITE(lfnprt,"(' SATELLITE NUMBER:',I6,/)")svn
  ENDIF

! Create Plot File ?
! ------------------
  CALL gtflna(0, 'PLOTRS', filnam, irc)
  IF (irc==0) THEN
    iplt=1
  ELSE
    iplt=0
  ENDIF

! An Error Occured
! ----------------
  IF(ircode /=0)CALL exitrc(2)

  iretrn=0

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)


  RETURN
END SUBROUTINE stdinp

END MODULE
