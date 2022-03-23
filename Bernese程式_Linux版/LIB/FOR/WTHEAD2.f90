MODULE s_WTHEAD2
CONTAINS

! -------------------------------------------------------------------------
! Bernese Software Version 5.2
! -------------------------------------------------------------------------

  SUBROUTINE wtHead2(filHed,head)

! -------------------------------------------------------------------------
! Purpose:      Write the entire header information of an observation
!               file (phase or code, zero or single differences)
!
! Remarks:      Updated version of SR WTHEAD.f
!
! Changes in old SR:
!               11-JAN-1993 : USE OF SR "OPNFIL" TO OPEN FILES
!               04-MAY-1993 : NEW FORMAT
!               12-AUG-1994 : MR: FORMAT 4: SESSION AS CHARACTER*4
!               15-AUG-1999 : JJ: RM UNUSED VAR CHRORD.
!
! Author:       M.Rothacher, L.Mervart
!
! Created:      09-Jul-2002
!
! Changes:      08-Sep-2003 HU: antnam, recnam, oprnam chr16 -> chr20
!               26-Jan-2011 LP: Sat.-specific obstypes, format 6
!               26-Apr-2012 LP: Dimension of obstyp changed (4->8)
!
!
! Copyright:    Astronomical Institute
!               University of Bern
!               Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY : maxsat
  USE d_gpsObs, ONLY : t_obsHead

  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*)            :: filHed
  TYPE(t_obsHead)             :: head

! List of functions
! -----------------
! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'wtHead'
  INTEGER(i4b),PARAMETER        :: Fmt5Lgt = 20

! Local Variables
! ---------------
  CHARACTER(LEN=Fmt5Lgt)    :: hlpStr5 ! Help string for writing station
                                       ! name in Format 5
  CHARACTER(LEN=3),DIMENSION(maxSat,4):: obshelp


  REAL(r8b)                 :: rDeltt  ! R*8 sampling, ifrmt>=5

  INTEGER(i4b),DIMENSION(2) :: iSess
  INTEGER(i4b) :: ioStat
  INTEGER(i4b) :: null
  INTEGER(i4b) :: iSta,nSta
  INTEGER(i4b) :: iSat
  INTEGER(i4b) :: iAmb
  INTEGER(i4b) :: iFreq,iFrq,nFrq
  INTEGER(i4b) :: obs


! HEADER FILE = BLANK : NOTHING WRITTEN
! -------------------------------------
  IF(filHed == ' ') RETURN

! OPEN HEADER FILE
! ----------------
  CALL OPNFIL(lfnLoc,filHed,'UNKNOWN','UNFORMATTED',' ',' ',ioStat)
  CALL OPNERR(lfnErr,lfnLoc,ioStat,filHed,'WTHEAD')

! RECORD 1: CAMPAIGN, TITLE AND MEASUREMENT TYPE
! ----------------------------------------------
  WRITE(lfnLoc) head%campgn,head%title,head%meatyp

! RECORD 2: GENERAL INFORMATION (NUMBER OF ...)
! ---------------------------------------------
  null  =0
  isess(1)=0
  isess(2)=0

! SET FORMAT VERSION TO 5
! -----------------------
  head%ifrmat=6

  WRITE(lfnLoc) head%ndiff,head%nfreq,head%nepoch,head%nSatel,null,isess,null,&
       -1,head%irmark,head%nepflg,head%ifrmat,head%timref,&
       head%crdate,head%crtime

! WRITE SESSION AS CHARACTER*4

  WRITE(lfnLoc) head%csess(1:2)

! WRITE SAMPLING AS REAL*8
  rdeltt = head%ideltt
  WRITE(lfnLoc) rdeltt

! LOOP OVER STATIONS
! ------------------
  nSta=head%ndiff+1
  DO iSta=1,nSta

! RECORDS 3,4: STATION AND RECEIVER INFORMATION
! ---------------------------------------------
    hlpStr5 = head%sta(iSta)%staNam

    WRITE(lfnLoc) hlpStr5,head%sta(iSta)%rectyp,&
         head%sta(iSta)%irunit,head%sta(iSta)%ianten,head%sta(ista)%oprnam,&
         head%sta(iSta)%anttyp,head%sta(ista)%iclock
    WRITE(lfnLoc) head%sta(iSta)%posecc(1:3)
  ENDDO

! RECORD 5: SATELLITE NUMBERS
! ---------------------------
  WRITE(lfnLoc) (head%sat(iSat)%numSat,iSat=1,head%nSatel)

! LOOP OVER FREQUENCIES
! ---------------------
  DO iFrq=1,head%nFreq

! RECORDS 6-8: NUMBER OF OBSERVATIONS (USED AND MARKED)
! -----------------------------------------------------
    WRITE(lfnLoc) (head%sat(iSat)%numObs(iFrq),iSat=1,head%nSatel)
    WRITE(lfnLoc) (head%sat(iSat)%numMrk(iFrq),iSat=1,head%nSatel)
  ENDDO

! RECORD 9: TOTAL NUMBER OF AMBIGUITIES
! -------------------------------------
  WRITE(lfnLoc) head%numAmb

! RECORD 10: AMBIGUITIES
! ----------------------
  IF(head%nFreq == 1) THEN
    nFrq=1
  ELSE
    nFrq=3
  ENDIF
  DO iAmb=1,head%numAmb
    WRITE(lfnLoc) head%ambigu(iAmb)%ambSat,head%ambigu(iAmb)%ambIep,&
         (head%ambigu(iAmb)%ambWlf(iFreq),iFreq=1,head%nFreq),&
         (head%ambigu(iAmb)%ambigu(iFrq),head%ambigu(iAmb)%ambCls(iFrq),iFrq=1,nFrq)
  ENDDO

! RECORD 11: Observation types per satellite
! ------------------------------------------
  DO iSat=1,maxsat
    DO obs = 1,4
      obshelp(iSat,obs) = '   '
    ENDDO
  ENDDO

! Phase obs
  IF (head%meaTyp==1) THEN
    DO iSat=1,head%nSatel
      obshelp(iSat,1) = head%sat(iSat)%obstyp(3)
      obshelp(iSat,2) = head%sat(iSat)%obstyp(4)
      obshelp(iSat,3) = head%sat(iSat)%obstyp(7)
      obshelp(iSat,4) = head%sat(iSat)%obstyp(8)
    ENDDO
  ENDIF

! Code obs
  IF (head%meaTyp==2) THEN
    DO iSat=1,head%nSatel
      obshelp(iSat,1) = head%sat(iSat)%obstyp(1)
      obshelp(iSat,2) = head%sat(iSat)%obstyp(2)
      obshelp(iSat,3) = head%sat(iSat)%obstyp(5)
      obshelp(iSat,4) = head%sat(iSat)%obstyp(6)
    ENDDO
  ENDIF
  DO obs = 1,4
    WRITE(lfnLoc) (obshelp(iSat,obs),iSat=1,head%nSatel)
  ENDDO

! CLOSE HEADER FILE
! -----------------
  CLOSE(UNIT=lfnLoc)

  RETURN
  END SUBROUTINE wtHead2

END MODULE
