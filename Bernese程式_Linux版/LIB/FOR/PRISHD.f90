MODULE s_PRISHD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE prishd(isel,tshd,nsvn,svnlst,ngrp,grplst, &
                    window,maxshd,nshd,satshd,timshd,flgshd)

! ------------------------------------------------------------------------
! Purpose:    Find time intervals with satellites in shadow or
!             tshd within exit of the shadow.
!
! Author:     U. Hugentobler
!
! Created:    09-Jan-2004
!
! Changes:    08-Aug-2005 HB: Use new SR TIMST2 (module)
!             30-May-2007 AG: Use s_suneff
!             04-May-2008 RD: Numsat added to call of sr XYZELE
!             21-Sep-2009 RD: Eclipsing flag for clock results added
!             01-Oct-2010 CR: New call of SUNEFF
!             05-Mar-2012 RD: Use listi4 as module now
!             16-Jul-2013 RD: Use the external allSatNum-list
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt
  USE d_const,  ONLY: gm

  USE s_dimtst
  USE s_iordup
  USE s_satblk
  USE s_ephem
  USE s_getorb
  USE s_timst2
  USE s_suneff
  USE s_shadow1
  USE f_listi4
  USE s_xyzele
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                :: isel   ! 2: Remove eclipsing satellite obs
  REAL(r8b)                   :: tshd   ! Time after shadow (hours)
  INTEGER(i4b)                :: nsvn   ! Number of satellites
  INTEGER(i4b),DIMENSION(*)   :: svnLst ! List of satellite numbers
  INTEGER(i4b)                :: ngrp   ! Number of groups
  INTEGER(i4b),DIMENSION(*)   :: grplst ! Satellite groups
  REAL(r8b),DIMENSION(2)      :: window ! Observation time window
  INTEGER(i4b)                :: maxshd ! maximum number of shadow passages

! output
  INTEGER(i4b)                     :: nshd   ! Number of shadow passages
  INTEGER(i4b),DIMENSION(maxshd)   :: satshd ! Satellite numbers
  REAL(r8b),DIMENSION(2,maxshd)    :: timshd ! Shadow times (mjd)
  INTEGER(i4b),DIMENSION(maxshd)   :: flgshd ! Flag obs. (=2) or not (=0)


! Local variables
! ---------------
  INTEGER(i4b)                   :: ipnt,isat,ipos,ishd,iter,iFlg
  INTEGER(i4b)                   :: npoint
  INTEGER(i4b)                   :: icrarc,iorsys,ishad,irc
  INTEGER(i4b)                   :: iFrq,blkNr
  INTEGER(i4b),DIMENSION(maxshd) :: indx

  REAL(r8b)                      :: delta=1D0/1440D0     ! step size
  REAL(r8b)                      :: tbeg,epoch,tosc,ttt
  REAL(r8b)                      :: dist
  REAL(r8b)                      :: t1,t2,t3,dist1,dist2,dist3
  REAL(r8b),DIMENSION(4)         :: sun
  REAL(r8b),DIMENSION(3)         :: dum3
  REAL(r8b),DIMENSION(7)         :: ele
  REAL(r8b),DIMENSION(6)         :: xvsat
  REAL(r8b),DIMENSION(nSvn)      :: dists

  CHARACTER(LEN=40)              :: tstrng
  CHARACTER(LEN=20)              :: excl

! Function
! --------

! Number of steps
! ---------------
  tbeg=window(1)-tshd/24D0
  npoint = (window(2)-tbeg)/delta

! Initialize array
! ----------------
  nshd=0
  timshd(1:2,1:maxshd)=1D20

! Loop over subintervals
! ----------------------
  DO ipnt = 0,npoint
    epoch = tbeg+ipnt*delta

    DO isat=1,nsvn

! Consider satellite blocks
      iFlg = iSel
      IF (ngrp>0) THEN
        ttt=(window(1)+window(2))/2D0
        CALL satblk(svnlst(isat),ttt,iFrq,blkNr)
        ipos = listi4(0,ngrp,grplst,blkNr,ngrp)
        IF (ipos==0) iFlg = 0
      ENDIF

      IF (epoch<window(1)) THEN
        CALL getorb(svnlst(isat),0,1,0,window(1),icrarc,iorsys, &
                                       xvsat,tosc,ele,irc)
        ttt=(window(1)-tosc)*86400d0
        CALL xyzele(gm,ttt,xvsat,xvsat(4),svnlst(isat),         &
                                          ele(1),ele(2),ele(3), &
                                          ele(4),ele(5),ele(6))
        ttt=(epoch-tosc)*86400d0
        CALL ephem(gm,ele(1),ele(2),ele(3),                     &
                      ele(4),ele(5),ele(6),ttt,xvsat,xvsat(4))
      ELSE
        CALL getorb(svnlst(isat),0,1,0,epoch,icrarc,iorsys,xvsat,tosc,ele,irc)
        ttt=(epoch-tosc)*86400d0
        CALL xyzele(gm,ttt,xvsat,xvsat(4),svnlst(isat),         &
                                          ele(1),ele(2),ele(3), &
                                          ele(4),ele(5),ele(6))
      ENDIF

! Shadow condition
! ----------------
      CALL suneff(iorsys,0.5D0,epoch,sun,dum3)
      CALL shadow1(xvsat,sun,ishad,dist)

! First epoch, in shadow
! ----------------------
      IF (ipnt==0) THEN
        IF (dist<0) THEN
          nshd=nshd+1
          CALL dimtst(1,1,2,'PRISHD','SVNSHD','Number of shadow transits', &
                            ' ',nshd,maxshd,irc)
          satshd(nshd)=svnlst(isat)
          CALL dimtst(1,1,2,'PRISHD','TIMSHD','Number of shadow transits', &
                            ' ',nshd,maxshd,irc)
          timshd(1,nshd)=window(1)
          CALL dimtst(1,1,2,'PRISHD','FLGSHD','Number of shadow transits', &
                            ' ',nshd,maxshd,irc)
          flgshd(nshd)=iFlg
        ENDIF
      ELSE
! Change of shadow condition?
! ---------------------------
        IF (dist*dists(isat) <= 0D0) THEN
! ...prepare regula falsi
          t1= epoch-delta
          ttt=(t1-tosc)*86400d0
          CALL ephem(gm,ele(1),ele(2),ele(3),                      &
                        ele(4),ele(5),ele(6),ttt,xvsat,xvsat(4))
          CALL suneff(iorsys,0.5D0,t1,sun,dum3)
          CALL shadow1(xvsat,sun,ishad,dist1)

          t2= epoch
          ttt=(t2-tosc)*86400d0
          CALL ephem(gm,ele(1),ele(2),ele(3),                      &
                        ele(4),ele(5),ele(6),ttt,xvsat,xvsat(4))
          CALL suneff(iorsys,0.5D0,t2,sun,dum3)
          CALL shadow1(xvsat,sun,ishad,dist2)

! ...regula falsi
          DO iter=1,10
            t3=(t1*dist2-t2*dist1)/(dist2-dist1)
            ttt=(t3-tosc)*86400d0
            CALL ephem(gm,ele(1),ele(2),ele(3),                      &
                          ele(4),ele(5),ele(6),ttt,xvsat,xvsat(4))
            CALL suneff(iorsys,0.5D0,t3,sun,dum3)
            CALL shadow1(xvsat,sun,ishad,dist3)
            IF (ABS(t3-t1)<0.1D0/86400D0 .OR. ABS(t3-t2)<0.1D0/86400D0) EXIT
            IF (t3>(t1+t2)/2D0) THEN
              t1   =t3
              dist1=dist3
            ELSE
              t2   =t3
              dist2=dist3
            ENDIF
          ENDDO
          ttt=t3

! ...entering shadow
          IF (dist < 0D0 .AND. dists(isat) >= 0D0) THEN
            nshd=nshd+1
            CALL dimtst(1,1,2,'PRISHD','SVNSHD','Number of shadow transits', &
                              ' ',nshd,maxshd,irc)
            satshd(nshd)=svnlst(isat)

            CALL dimtst(1,1,2,'PRISHD','TIMSHD','Number of shadow transits', &
                              ' ',nshd,maxshd,irc)
            timshd(1,nshd)=MAX(ttt,window(1))

            CALL dimtst(1,1,2,'PRISHD','FLGSHD','Number of shadow transits', &
                              ' ',nshd,maxshd,irc)
            flgshd(nshd)=iflg
          ELSE
! ...exiting shadow
            DO ishd=nshd,1,-1
              IF (satshd(ishd)==svnlst(isat)) THEN
                timshd(2,ishd)=MIN(ttt+tshd/24D0,window(2))
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDIF

      ENDIF
      dists(isat)=dist
    ENDDO
  ENDDO
  DO ishd=1,nshd
    IF (timshd(2,ishd).EQ.1D20) timshd(2,ishd)=window(2)
  ENDDO

! Write
! -----
  WRITE(lfnprt,"(//,' LIST OF SATELLITES IN SHADOW:',  &
               &  /,' ----------------------------')")
  IF (ngrp==0.AND.isel==0) THEN
    WRITE(lfnprt,'(A,I4,A)') ' LIST OF SATELLITES IN SHADOW AND', &
                  IDNINT(tshd*60D0),' MINUTES AFTER SHADOW'
  ELSE IF (ngrp==0) THEN
    WRITE(lfnprt,'(A,I4,A)') &
               ' REMOVING ALL OBSERVATIONS IN SHADOW AND', &
               IDNINT(tshd*60D0),' MINUTES AFTER SHADOW FOR ALL BLOCKS'
  ELSE
    WRITE(lfnprt,'(A,I4,A,20(I3,:,","))') &
               ' REMOVING ALL OBSERVATIONS IN SHADOW AND', &
               IDNINT(tshd*60D0),' MINUTES AFTER SHADOW FOR BLOCKS:', &
               grplst(1:ngrp)
  ENDIF
  WRITE(lfnprt,"(/,' SAT   FROM                 TO', &
              &  /,1X,131('-'),/)")
  CALL iordup(satshd,nshd,indx)
  DO ishd=1,nshd
    CALL timst2(1,2,timshd(1:2,indx(ishd)),tstrng)
    excl = 'exclude observations'
    IF (flgshd(indx(ishd)) == 0) excl = 'use observations    '
    WRITE(lfnprt,"(1X,I3,2(3X,A))")satshd(indx(ishd)),tstrng,excl
  ENDDO

  CALL suneff(-1,0.5D0,epoch,sun,dum3)

  RETURN
  END SUBROUTINE prishd

END MODULE
