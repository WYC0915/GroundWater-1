MODULE s_QLWTRX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE qlwtrx(stanam,sta4,sitnam,sitnum,qlobs,indexs,title,  &
                  sessid,yearid,devout)
! -------------------------------------------------------------------------
! Purpose:    Write RINEX observation and meteo file from SLR quick look
!             files for station stanam.
!
! Author:     C. Urschl
!
! Created:    10-Nov-2003
!
! Changes:    16-Jan-2004 CU: Uncomment epoch(2)
!             31-Mar-2004 CU: Additional check, if record ok (iobstp/=0)
!             01-Apr-2004 CU: Write time of signal reception  with nano sec.
!                             accuracy into RINEX file (use last column for
!                             receiver clock offset)
!             09-Dec-2005 CU: Add 2nd dimension to signal,isnmin,isnthr,
!                             isnmax
!             29-Feb-2012 RD: Correct array dimensions of dummy arguments
!             29-Feb-2012 RD: Use R2WTOR as module
!             29-Feb-2012 RD: Use R2WTOH as module
!             29-Feb-2012 RD: Use RXWTMR as module
!             19-Jun-2012 LP: Dimension check: do not use (iobstp > 2)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: maxsat
  USE d_const,  ONLY: date, time
  USE d_qlfil,  ONLY: t_qlobs

  USE f_lengt0
  USE s_r2wtor
  USE s_opnerr
  USE s_r2wtmh
  USE s_rxwtmr
  USE s_opnfil
  USE s_r2wtoh
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=4)             :: stanam ! Station name from QL (7810)
  CHARACTER(LEN=4)             :: sta4   ! Station abbreviation from .ABB (781B)
  CHARACTER(LEN=60)            :: sitnam ! Station name from .STA (7810)
  CHARACTER(LEN=20)            :: sitnum ! DOMES number from .STA (14001S007)
  TYPE(t_qlobs)                :: qlobs  ! Station information and
                                         ! observations for stanam
  INTEGER(i4b), DIMENSION(:)   :: indexs ! Sort index for epochs
  CHARACTER(LEN=80)            :: title  ! Title line
  CHARACTER(LEN=4)             :: sessid ! Session ID for RINEX file name
  CHARACTER(LEN=2)             :: yearid ! Year ID for RINEX file name
  CHARACTER(LEN=filePathLength):: devout ! Path to RINEX files

! Local Variables
! ---------------
  INTEGER(i4b)                    , PARAMETER :: ncom   = 7
  CHARACTER(LEN=10), DIMENSION(10), PARAMETER :: timref = &
    (/'NONE GIVEN','          ','          ','UTC (USNO)','UTC (GPS) ', &
      '          ','          ','UTC (BIH) ','          ','          '/)
  CHARACTER(LEN=8) , PARAMETER        :: srName = 'QLWTRX'
  CHARACTER(LEN=20), PARAMETER        :: prgnam = 'QLRINEXO BSW V5.0'

  INTEGER(i4b)                        :: lfnrxo, lfnrxm
  INTEGER(i4b)                        :: rxvrs, rxvrsm
  INTEGER(i4b)                        :: nobstp, nobstm
  INTEGER(i4b)                        :: nsatel
  INTEGER(i4b)                        :: irec, jrec
  INTEGER(i4b), DIMENSION(1)          :: satepo
  INTEGER(i4b), DIMENSION(2)          :: lli
  INTEGER(i4b), DIMENSION(1)          :: idum01
  INTEGER(i4b), DIMENSION(3,1)        :: idum31
  INTEGER(i4b), DIMENSION(MAXSAT,1)   :: idumS1
  INTEGER(i4b)                        :: idummy
  INTEGER(i4b)                        :: irc
  INTEGER(i4b)                        :: nrec
  INTEGER(i4b)     , SAVE             :: filnum = 0
  INTEGER(i4b)     , DIMENSION(2)     :: isnmin, isnthr, isnmax

  REAL(r8b)                           :: tfirst, tlast
  REAL(r8b)        , DIMENSION(3)     :: rdummy
  REAL(r8b)        , DIMENSION(3)     :: datmet
  REAL(r8b)        , DIMENSION(2)     :: obsepo
  REAL(r8b)        , DIMENSION(2)     :: signal

  CHARACTER(LEN=40)                   :: anttyp, rectyp
  CHARACTER(LEN=20)                   :: recnum
  CHARACTER(LEN=20)                   :: rcvers
  CHARACTER(LEN=20)                   :: runby
  CHARACTER(LEN=2) , DIMENSION(2)     :: obstyp
  CHARACTER(LEN=2) , DIMENSION(3)     :: obstpm
  CHARACTER(LEN=60), DIMENSION(ncom)  :: coment
  CHARACTER(LEN=20)                   :: c20dum
  CHARACTER(LEN=40)                   :: c40dum

  CHARACTER(LEN=fileNameLength80)     :: filout, filhlp
  CHARACTER(LEN=fileNameLength80)     :: filmet


! Create RINEX file header strings
! --------------------------------
  idummy    = 0
  c20dum    = ' '
  c40dum    = ' '
  rdummy(:) = 0D0
  nrec      = SIZE(qlobs%qlrec)

! General information

!!!! Microsecond accuracy for time of signal reception (epoch(1))
!!!  tfirst    = qlobs%qlrec(indexs(1))%epoch(1)
!!!  tlast     = qlobs%qlrec(indexs(nrec))%epoch(1)

! Nanoseconds accuracy for time of signal reception (epoch(1),epoch(2))
  tfirst    = qlobs%qlrec(indexs(1))%epoch(1) + &
              qlobs%qlrec(indexs(1))%epoch(2)/86400d0
  tlast     = qlobs%qlrec(indexs(nrec))%epoch(1) + &
              qlobs%qlrec(indexs(nrec))%epoch(2)/86400d0

  runby     = 'AIUB'
  rectyp    = 'SLR'
  WRITE(recnum,'(I2.2)') qlobs%cdps
  WRITE(rcvers,'(I2.2)') qlobs%syscf
  rectyp(21:40) = recnum
  anttyp    = 'SLR'
  nsatel    = 0
  rxvrs     = 502
  rxvrsm    = 2

! Observation type and number of obs. types
  IF (qlobs%iwlfac(1) /= 0 .AND. qlobs%iwlfac(2) /= 0) THEN
    nobstp    =  2
    obstyp(1) = 'R1'
    obstyp(2) = 'R2'
  ELSEIF (qlobs%iwlfac(1) /= 0) THEN
    nobstp    =  1
    obstyp(1) = 'R1'
  ELSEIF (qlobs%iwlfac(2) /= 0) THEN
    nobstp    =  1
    obstyp(1) = 'R2'
  END IF

! Meteo type and number of types
  nobstm    = 3
  obstpm(1) = 'PR'
  obstpm(2) = 'TD'
  obstpm(3) = 'HR'

! Comment lines
  WRITE(coment(1),'(A60)') &
       title
  WRITE(coment(2),'(A,A4)') &
       'STATION ID:  ',stanam
  WRITE(coment(3),'(2(A,I6.2),A,I6)') &
       'CDP SYSNUM:',qlobs%cdps,'   CDP OCCNUM:',qlobs%cdpo, &
       '   SYS CONFIG:',qlobs%syscf
  WRITE(coment(4),'(2(A,I6))') &
       'W-LENGTH 1:',qlobs%iwlfac(1)/10,'   W-LENGTH 2:',qlobs%iwlfac(2)/10
  WRITE(coment(5),'(2A)') &
       'EPOCH TIME SCALE REFERENCE    : ',timref(qlobs%timsy+1)
  coment(6) = &
       'EPOCHS: TIME OF SIGNAL RECEPTION IN GPS TIME FRAME'
  coment(7) = &
       'RANGES: 0.5 * C * FLIGHT TIME (M)'

! Create RINEX file name
! ----------------------
  WRITE(filout,'(A4,A4,A1,A2,A1)') sta4,sessid,'.',yearid,'O'

  WRITE(filhlp,'(2A)') devout(1:lengt0(devout)),filout(1:lengt0(filout))
  filout = filhlp
  filmet = filout
  filmet(lengt0(filmet):) = 'M'

! Open RINEX file
! ---------------
  lfnrxo = lfn001 + 1
  lfnrxm = lfn001 + 2

  CALL opnfil(lfnrxo,filout,'NEW',' ',' ',' ',irc )
  CALL opnerr(lfnerr,lfnrxo,irc,filout,srName)

  CALL opnfil(lfnrxm,filmet,'NEW',' ',' ',' ',irc )
  CALL opnerr(lfnerr,lfnrxm,irc,filmet,srName)

! Write RINEX header
! ------------------
  CALL r2wtoh(lfnrxo,lfnerr,idummy,prgNam,runby ,date  ,time  ,ncom  ,       &
              coment,sitnam,sitnum,c20dum,c40dum,idummy,rectyp,rcvers,       &
              idummy,anttyp,rdummy,rdummy,qlobs%iwlfac ,idum31,idummy,nobstp,&
              obstyp,idummy,tfirst,tlast ,nsatel,idum01,idumS1,rxvrs,irc)

  CALL r2wtmh(lfnrxm,lfnerr,prgNam,runby ,date  ,time,ncom, &
              coment,sitnam,nobstm,obstpm,rxvrsm,irc)

! Write RINEX records
! -------------------
  DO irec = 1, nrec

    jrec = indexs(irec)
    signal = 0D0

    IF (qlobs%qlrec(jrec)%iobstp == 0) CYCLE
    IF (qlobs%qlrec(jrec)%iobstp  > 2) CYCLE

    IF (qlobs%qlrec(jrec)%numnpt < 2) qlobs%qlrec(jrec)%numnpt = 2

    obsepo(1) = 0D0
    obsepo(2) = 0D0
    IF (nobstp == 1) THEN
      obsepo(1) = qlobs%qlrec(jrec)%range
      lli(1)    = qlobs%qlrec(jrec)%llinpt
      signal(1) = ALOG10( FLOAT(qlobs%qlrec(jrec)%numnpt) ) / ALOG10(2.)
    ELSE
      obsepo(qlobs%qlrec(jrec)%iobstp) = qlobs%qlrec(jrec)%range
      lli(qlobs%qlrec(jrec)%iobstp)    = qlobs%qlrec(jrec)%llinpt
      signal(qlobs%qlrec(jrec)%iobstp) = ALOG10( FLOAT(qlobs%qlrec(jrec)%numnpt) ) / ALOG10(2.)
    END IF

    satepo(1) = qlobs%qlrec(jrec)%satnr

    datmet(1) = qlobs%qlrec(jrec)%press
    datmet(2) = qlobs%qlrec(jrec)%temp
    datmet(3) = qlobs%qlrec(jrec)%humid

    isnmin(:) = 1
    isnthr(:) = 5
    isnmax(:) = 9

!!! Microsecond accuracy for time of signal reception (epoch(1))
!!    CALL r2wtor(lfnrxo,lfnerr,1,rxvrs,nobstp,1,5,9,              &
!!                qlobs%qlrec(jrec)%epoch,0,1,satepo,obsepo,signal,lli,irc)

! Nanoseconds accuracy for time of signal reception (epoch(1),epoch(2))
    CALL r2wtor(lfnrxo,lfnerr,1,rxvrs,nobstp,isnmin,isnthr,isnmax,      &
                qlobs%qlrec(jrec)%epoch,0,-1,satepo,obsepo,signal,lli,irc)

    CALL rxwtmr(lfnrxm,lfnerr,3,qlobs%qlrec(jrec)%epoch(1),datmet,irc)

  ENDDO

! Deallocate station-specific observation array
! ---------------------------------------------
  IF (ASSOCIATED(qlobs%qlrec)) DEALLOCATE(qlobs%qlrec,stat=irc)

! Close RINEX file
! ----------------
  CLOSE(lfnrxo)
  CLOSE(lfnrxm)

! Write RINEX file names to protocol file
! ---------------------------------------
  filnum = filnum + 1
  WRITE(lfnprt,'(1X,I4,2X,A32,2X,A32)') filnum, filout, filmet

  RETURN

END SUBROUTINE qlwtrx

END MODULE
