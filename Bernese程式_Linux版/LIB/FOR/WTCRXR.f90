MODULE s_WTCRXR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE wtcrxr(lfnclk,lfnmsg,ClkHead,ClkRec,irCode)

! -------------------------------------------------------------------------
! Purpose:    writes observation records of a rinex clock file (version 2.00)
!             (based on the f77 sr R2WTCR)
!
! Parameters:
!         in: lfnclk  : Logical file number of clock rinex file         i4b
!             lfnmsg  : Logical file number for error messages          i4b
!             ClkHead : Information from rinex file header           t_clkhead
!             ClkRec  : Data records from rinex file                 t_clkrec
!             irCode  : return code                                     i4b
!                         0: ok
!                         9: error writing the file
!
! Author:     R. Dach
!
! Created:    09-Aug-2000
! Last mod.:  19-Jul-2010
!
! Changes:    15-May-2001 RD: Order clocks in the output
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             23-Apr-2003 RD: Nullify local pointers
!             15-Jul-2003 RD: Rounding problem at day boundary
!             25-Nov-2003 HB: Write sigma only on the case
!                             of /= 999999.999999D0
!             21-Sep-2009 RD: Eclipsing flag added
!             19-Jul-2010 SL: tab characters removed
!
! SR used:    jmt, radgms, clksort
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef

  USE s_clksort
  USE s_jmt
  USE s_radgms
  USE f_tstflg
  IMPLICIT NONE
!
! Variables from the parameter list
! ---------------------------------
  INTEGER(i4b)      :: lfnclk         ! Logical file number of clock rinex file
  INTEGER(i4b)      :: lfnmsg         ! Logical file number for error messages
  TYPE(t_clkhead)   :: ClkHead        ! Clock rinex header
  TYPE(t_clkrec)    :: ClkRec         ! Clock rinex data records
  INTEGER(i4b)      :: irCode         ! Return code of this SR
!
! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(:), POINTER :: Index ! Index to order clocks
!
  CHARACTER(len= 1) :: Sign           ! Sign help string
  CHARACTER(LEN= 8) :: flag           ! String with flags
!
  INTEGER(i4b)      :: iEpo           ! Counts the epoch numbers in list
  INTEGER(i4b)      :: iSta           ! Counts the stations in list
  INTEGER(i4b)      :: iSat           ! Counts the satellites in list
  INTEGER(i4b)      :: iyyy,mm,id     ! Date of a record (year, month, day)
  INTEGER(i4b)      :: ih,im          ! Date of a record (hour, min.)
  INTEGER(i4b)      :: ios            ! IO status
!
  REAL(r8b)         :: sec            ! Date of a record (sec.)
  REAL(r8b)         :: Day            ! Day of month of a record
!
! INITIALIZE
! ----------
  irCode=0
  NULLIFY(index)
  CALL clkSort(0,ClkHead,Index)
!
! LOOP OVER ALL EPOCHS
! --------------------
  EpoLoop: DO iEpo=1,ClkRec%nEpo
    CALL jmt(ClkHead%TFirst+Clkrec%Epoch(iEpo)/86400D0,iyyy,mm,day)
    CALL radgms(3,day,SIGN,IH,IM,SEC)
    day = day-DBLE(ih)/24d0-DBLE(im)/3600d0-sec/86400d0
    id=IDINT(day+1d0/86400D0)
!
! WRITE ALL STATION CLOCKS FOR THIS EPOCH
! ---------------------------------------
    DO iSta=1,ClkHead%nSta
!!      IF (ClkRec%Sigma(Index(iSta),iEpo) == 0.D0) write(*,*)Index(iSta),iEpo
      IF (ClkRec%Clock(Index(iSta),iEpo) < unDef .AND.&
           ClkRec%Sigma(Index(iSta),iEpo) < unDef) THEN
        flag=''
        IF (tstflg(clkrec%clkflg(index(iSta),iEpo),0)) flag(1:1) = 'E'
        WRITE(lfnclk,'(A2,1X,A4,1X,I4,4(1X,I2.2),F10.6,I3,2X,2(1X,E19.12),2X,A)',&
              iostat=irCode)                                                 &
              'AR',ClkHead%ClkName(Index(iSta))(1:4),iyyy,mm,id,ih,im,sec,   &
              2,ClkRec%Clock(Index(iSta),iEpo)*1D-6,                         &
              ClkRec%Sigma(Index(iSta),iEpo)*1D-6,flag
      ELSEIF (ClkRec%Clock(Index(iSta),iEpo) < unDef .AND.&
           ClkRec%Sigma(Index(iSta),iEpo) == unDef ) THEN
        flag=''
        IF (tstflg(clkrec%clkflg(index(iSta),iEpo),0)) flag(1:1) = 'E'
        WRITE(lfnclk,'(A2,1X,A4,1X,I4,4(1X,I2.2),F10.6,I3,2X,1X,E19.12,22X,A)',&
              iostat=irCode)                                                 &
              'AR',ClkHead%ClkName(Index(iSta))(1:4),iyyy,mm,id,ih,im,sec,   &
              1,ClkRec%Clock(Index(iSta),iEpo)*1D-6,flag
      ENDIF
      IF (irCode /=0) EXIT EpoLoop
    ENDDO
!
! WRITE ALL SATELLITE CLOCKS FOR THIS EPOCH
! -----------------------------------------
    DO iSat=Clkhead%nSta+1,Clkhead%nSta+ClkHead%nSat
      IF (ClkRec%Clock(Index(iSat),iEpo) < unDef .AND.&
           ClkRec%Sigma(Index(iSat),iEpo) < unDef ) THEN
        flag=''
        IF (tstflg(clkrec%clkflg(index(iSat),iEpo),0)) flag(1:1) = 'E'
        WRITE(lfnclk,'(A2,1X,A3,2X,I4,4(1X,I2.2),F10.6,I3,2X,2(1X,E19.12),2X,A)',&
              iostat=irCode)                                                 &
              'AS',ClkHead%ClkName(Index(isat))(1:3),iyyy,mm,id,ih,im,sec,   &
              2,ClkRec%Clock(Index(iSat),iEpo)*1D-6,                         &
              ClkRec%Sigma(Index(iSat),iEpo)*1D-6,flag
      ELSEIF (ClkRec%Clock(Index(iSat),iEpo) < unDef .AND.&
           ClkRec%Sigma(Index(iSat),iEpo) == unDef ) THEN
        flag=''
        IF (tstflg(clkrec%clkflg(index(iSat),iEpo),0)) flag(1:1) = 'E'
        WRITE(lfnclk,'(A2,1X,A3,2X,I4,4(1X,I2.2),F10.6,I3,2X,1X,E19.12,22X,A)',&
              iostat=irCode)                                                 &
              'AS',ClkHead%ClkName(Index(isat))(1:3),iyyy,mm,id,ih,im,sec,   &
              1,ClkRec%Clock(Index(iSat),iEpo)*1D-6,flag
      ENDIF
      IF (irCode /=0) EXIT EpoLoop
    ENDDO
!
! NEXT EPOCH
! ----------
  ENDDO EpoLoop
!
  DEALLOCATE(Index,stat=ios)
!
  IF (irCode /= 0) THEN
    WRITE(lfnmsg,'(/,A,//)') ' ### WTCRXR: Error writing rinex clock file'
    irCode=9
  ENDIF
!
  RETURN
  END SUBROUTINE

END MODULE
