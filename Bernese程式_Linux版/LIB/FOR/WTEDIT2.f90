MODULE s_WTEDIT2
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE wtedit2(edt, filNam)

!----------------------------------------------------------------------------
! Purpose    :  Write all information in editing information file (new version
!               of old F77-SR WTEDIT)
!
! Author     :  H.Bock
!
! Created    :  22-Feb-2002
!
! Changes    :  02-Oct-2002 RD: Separate resMax for each meatyp
!               25-May-2009 DT: Add filNam as optional parameter
!               26-Mar-2012 RD: Use TIMSTR as module now
!
! Copyright  :  Astronomical Institute
!               University of Bern
!               Switzerland
!-----------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnerr, fileNameLength, timstrgLength
  USE m_global, ONLY: g_meaTyp
  USE d_edit,   ONLY: t_edit
  USE s_timstr
  USE s_opnfil
  USE s_gtflna
  USE s_opnerr
  IMPLICIT NONE

! Parameters
! ----------
  ! input
  TYPE(t_edit)                            :: edt
  CHARACTER(LEN=fileNameLength), OPTIONAL :: filNam

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=timStrgLength) :: tStrng
  INTEGER(i4b)                 :: ircEdt
  INTEGER(i4b)                 :: ioStat
  INTEGER(i4b)                 :: iEdt
  INTEGER(i4b)                 :: iFil

! Get file name of editing info file
! ----------------------------------
  IF (edt%filNam /= 'TAKE FROM FILE' .OR. &
      PRESENT(filNam) ) THEN
    ircEdt=0
  ELSE
    CALL gtflna(0,'EDITRS',edt%filNam,ircEdt)
  ENDIF
  IF (ircEdt/=0) THEN

! No output file name specified
    RETURN
  ELSE

! Open editing file
! -----------------
    CALL opnfil(lfnLoc,edt%filNam,'UNKNOWN','FORMATTED',&
         ' ',' ',ioStat)
    CALL opnerr(lfnErr,lfnLoc,ioStat,edt%filNam,'WTEDIT')

! Write file information
! ----------------------
    WRITE(lfnLoc,'(A,/,80("-"),//,A,/,16("-"),//,A,A,/,80("-"),/)')&
         edt%title,'FILE INFORMATION:',&
         'FILE  SESS SESF  REFERENCE EPOCH   DTOBS TYPE STATION',&
         '1         STATION2'
    DO iFil=1,edt%nEdFil
      CALL timstr(1,(/edt%head(ifil)%timEdt/),tStrng)
      WRITE(lfnLoc,'(I3,3X,A4,2X,A1,4X,A17,I5,3X,A1,3X,A16,1X,A16)')&
           iFil,edt%head(iFil)%cseEdt(1),edt%head(iFil)%cseEdt(2)(1:1),&
           tStrng(1:17),edt%head(iFil)%idtEdt,g_meaTyp(edt%head(iFil)%meaEdt),&
           edt%head(iFil)%staEdt(1:2)
    ENDDO

! Write RESRMS options
! --------------------
    WRITE(lfnLoc,'(/,80("-"),///,A,/,80("-"),/,3(/,A,F9.3,A),2(/,A,I9,A),2(/,A,I9),A)')&
         ' RESRMS INPUT OPTIONS',&
         ' OUTLIER DETECTION LEVEL (PHASE)       :',edt%resmax(1),'  M',&
         ' OUTLIER DETECTION LEVEL (CODE)        :',edt%resmax(2),'  M',&
         ' OUTLIER DETECTION LEVEL (RANGE)       :',edt%resmax(3),'  M',&
         ' SAMPLING RATE OF RESIDUALS            :',edt%nsampl,   '  S',&
         ' MINIMUM TIME INTERVAL FOR SMALL PIECES:',edt%minint,   '  S',&
         ' MINIMUM NUMBER OF OBS. PER AMBIGUITY  :',edt%minamb,         &
         ' SAMPLING RATE FOR COUNTING OBS.       :',edt%isampl,   '  S'

! Write editing requests
! ----------------------
    WRITE(lfnLoc,'(/,80("-"),///,A,/,20("-"),/,A,A,/,&
              &16X,A,/,21X,A,/,A,A,/,80("-"),/)')&
              'EDITING INFORMATION:',&
              '(EDITING TYPES: MARK=1, RESET=-1, ELIM.=2, SLIP=3, ',&
              'NEW AMB.=4, RESET AMB.=-4,',&
              'SET CYC.SLIP FLAG=5, RESET CYC.SLIP FLAG=-5)',&
              'EPOCH NUMBERS',&
              'FILE  SAT  TYPE  FRQ  START  END    SLIP SIZE  ',&
              'REASON  #EPOCHS'

    DO iEdt=1,edt%nEdt
      IF (edt%rec(iEdt)%lstEdt(7)/=3) THEN
        WRITE(lfnLoc,'(I3,I6,2I5,I8,I6,14X,I5,I9)')&
             edt%rec(iEdt)%lstEdt(6),edt%rec(iEdt)%lstEdt(1),&
             edt%rec(iEdt)%lstEdt(7),edt%rec(iEdt)%lstEdt(4),&
             edt%rec(iEdt)%lstEdt(2),edt%rec(iEdt)%lstEdt(3),&
             edt%rec(iEdt)%lstEdt(5),&
             edt%rec(iEdt)%lstEdt(3)-edt%rec(iEdt)%lstEdt(2)+1
      ELSE
        WRITE(lfnLoc,'(I3,I6,2I5,I8,6X,F14.0,I5)') &
             edt%rec(iEdt)%lstEdt(6),edt%rec(iEdt)%lstEdt(1),&
             edt%rec(iEdt)%lstEdt(7),edt%rec(iEdt)%lstEdt(4),&
             edt%rec(iEdt)%lstEdt(2),&
             edt%rec(iEdt)%lstCyc,edt%rec(iEdt)%lstEdt(5)
      ENDIF
    ENDDO

! Final line
    WRITE(lfnLoc,'(/,80("-"),/)')

! Close file
! ----------
    CLOSE(UNIT=lfnLoc)
  ENDIF

  RETURN
END SUBROUTINE wtedit2

END MODULE
