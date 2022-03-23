MODULE s_rnxstat
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GNSS Software
! ------------------------------------------------------------------------------

SUBROUTINE RNXSTAT(filename,usegeos,gobsdef)

! ------------------------------------------------------------------------------
! Purpose:    Collect information about observations available
!             in a RINEX file and select the observation types to be
!             used in the processing.
!
! Parameters:
!         in: filename: RINEX file name
!        out: gobsdef : observation type selection
!             usegeos : switch for obstype selection
!
!
! Remarks:
!
! Author:     L. Prange
!
! Created:    09-May-2012
!
! Changes:    29-May-2012 LP: Select obstypes considering priorities in OBSSEL
!                             file (introduction of SR READGEOS2)
!             07-Jun-2012 LP: Print selection results only if requested!
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnPrt, lfn001, fileNameLength
  USE m_global, ONLY: maxsys
  USE d_rinex3, ONLY: maxtyp,t_rinstat,OBSTYPESR3,t_gobsdef
  USE m_maxdim, ONLY: MAXSAT
  USE s_exitrc
  USE s_opnfil
  USE s_opnerr
  USE s_gobsdef,ONLY: init_rinstat, init_geos, readgeos2
  USE s_r2rdoh
  USE s_r2rdor
  USE s_ckoptb
  IMPLICIT NONE
!
! Parameters
! ----------
!  CHARACTER(LEN=60)             :: filename
  CHARACTER(LEN=fileNameLength)  :: filename
  INTEGER(i4b)                   :: usegeos ! 1: use sat-specific obstype info
                                            ! 0: don't
  TYPE(t_gobsdef)                :: gobsdef ! Structure containing the
                                            ! sat-specific obstype info

!
! Local Variables
! ---------------
  INTEGER(i4b),PARAMETER        :: MAXCOM=60
  INTEGER(i4b),PARAMETER        :: MAXCHN=40
!  INTEGER(i4b),PARAMETER        :: MAXSLP=300
  INTEGER(i4b)                  :: lfn,iostat,NUMLIN,NCOM,NRUNIT,NRANT
  INTEGER(i4b)                  :: NWLSAT,NOBSTP,INTER,NSATEL,IRXVRS
  INTEGER(i4b)                  :: IFLAG,NEPSAT,isys,isat,ityp
  INTEGER(i4b)                  :: prtostat,irCode,IRC
  INTEGER(i4b),DIMENSION(2)     :: IWLFAC
  INTEGER(i4b),DIMENSION(3,MAXSAT):: IWLSAT
  INTEGER(i4b),DIMENSION(MAXSAT):: NUMSAT
  INTEGER(i4b),DIMENSION(MAXSAT,MAXTYP):: NUMOBS
  INTEGER(i4b),DIMENSION(MAXCHN):: SATEPO
  INTEGER(i4b),DIMENSION(MAXCHN,MAXTYP):: LLI,ISIGN,test

  REAL(r8b),DIMENSION(2)        :: epochArray
  REAL(r8b),DIMENSION(3)        :: POSXYZ
  REAL(r8b),DIMENSION(3,3)      :: POSECC
  REAL(r8b),DIMENSION(MAXCHN,MAXTYP):: OBSEPO
  REAL(r8b)                     :: TFIRST, TLAST

  CHARACTER(LEN=6),PARAMETER    :: srname = 'RNXSTAT'
  CHARACTER(LEN=20)             :: PRGNAM,RUNBY,OPNAME,RECTYP,RCVERS,ANTTYP
  CHARACTER(LEN=9)              :: CRDATE
  CHARACTER(LEN=5)              :: CRTIME
  CHARACTER(LEN=60),DIMENSION(MAXCOM)::COMENT
  CHARACTER(LEN=60)             :: SITNAM
  CHARACTER(LEN=40)             :: SITNUM,AGENCY
  CHARACTER(LEN=2),DIMENSION(MAXTYP):: OBSTYP
!  CHARACTER(LEN=8)              :: helpfmtstr
!  CHARACTER(LEN=7)              :: helpfmtstr
!  CHARACTER(LEN=8)              :: helpfmtstr1
!  CHARACTER(LEN=2)              :: helpfmtstr2
  CHARACTER(LEN=1),DIMENSION(892):: helpstr

  type(t_rinstat)               :: RINSTAT

  LOGICAL,DIMENSION(MAXTYP)     :: usetyp

! Some initializations
! ====================
  irc           = 0
  irCode        = 0
  usegeos       = 0
  gobsdef%norec = 0
  numlin        = 0
  prtostat      = 0
  CALL init_rinstat(RINSTAT)

!
! OPEN RINEX FILE
! ---------------
  LFN=LFN001
  CALL OPNFIL(LFN001,filename,'OLD','FORMATTED',' ',' ',IOSTAT)
  CALL OPNERR(LFNERR,LFN001,IOSTAT,filename,srname)

!
! Check RINEX header for available obstypes and set obstype indices
! -----------------------------------------------------------------
  CALL R2RDOH(LFN,LFNERR,MAXSAT,MAXCOM,NUMLIN,PRGNAM,RUNBY,CRDATE,   &
              CRTIME,NCOM,COMENT,SITNAM,SITNUM,OPNAME,AGENCY,NRUNIT, &
              RECTYP,RCVERS,NRANT ,ANTTYP,POSXYZ,POSECC,IWLFAC,      &
              IWLSAT,NWLSAT,NOBSTP,OBSTYP,INTER, TFIRST,TLAST,NSATEL,&
              NUMSAT,NUMOBS,IRXVRS,IRC,RINSTAT=RINSTAT)


  IF (IRC.NE.0) THEN
    WRITE(lfnerr,'(/,A,A,A,/,16X,A,I1.1,/,16X,A)')               &
                 ' ### SR ',srName,': ERROR READING RINEX HEADER.',&
                 'RETURN CODE OF SR R2RDOH: ',IRC,                 &
                 'STOP PROCESSING FILE : '//TRIM(FILENAME)
    IRC=1
  ELSE
!
! Loop over all epochs and count observations per satellite
! ---------------------------------------------------------
    DO
      OBSEPO(:,:)=0.D0
      ISIGN(:,:)=0
      LLI(:,:)=0
      SATEPO(:)=0
      CALL R2RDOR(LFN,LFNERR,MAXCHN,IRXVRS,NOBSTP,OBSTYP,epochArray,    &
                IFLAG,NEPSAT,SATEPO,OBSEPO,ISIGN,LLI,IRC,RINSTAT=RINSTAT)

      IF(IRC.NE.0) EXIT
    ENDDO
  ENDIF
  CLOSE(LFN)
!
!  Sum up observations
!  ===================
  IF (IRC.NE.1) THEN
    usetyp(:) = .false.
    DO isys=0,(MAXSYS-1)
      IF (rinstat%sys(isys)%syschar.eq.' ') CYCLE
      DO isat=1,49
        IF (rinstat%sys(isys)%sat(isat)%satname.eq.'   ') CYCLE
        DO ityp=1,MAXTYP
          IF (rinstat%sys(isys)%indxs(ityp).eq.0) CYCLE
          IF (.not.usetyp(ityp)) usetyp(ityp)=.true.
          rinstat%sys(isys)%sat(isat)%obssum    =         &
             rinstat%sys(isys)%sat(isat)%obssum +         &
             rinstat%sys(isys)%sat(isat)%numobs(ityp)

          rinstat%sys(isys)%numobs(ityp)    =         &
             rinstat%sys(isys)%numobs(ityp) +         &
             rinstat%sys(isys)%sat(isat)%numobs(ityp)

          rinstat%sys(isys)%obssum    =         &
             rinstat%sys(isys)%obssum +         &
             rinstat%sys(isys)%sat(isat)%numobs(ityp)

          rinstat%numobs(ityp)    =         &
             rinstat%numobs(ityp) +         &
             rinstat%sys(isys)%sat(isat)%numobs(ityp)

          rinstat%obssum    =         &
             rinstat%obssum +         &
             rinstat%sys(isys)%sat(isat)%numobs(ityp)
        ENDDO
      ENDDO
    ENDDO
!
!   Reset systems and satellites without observations and count remaining satellites
!   --------------------------------------------------------------------------------
    rinstat%numsat = 0
    DO isys=0,(MAXSYS-1)
      IF (rinstat%sys(isys)%syschar.eq.' ') CYCLE
      IF (rinstat%sys(isys)%obssum==0) THEN
        rinstat%sys(isys)%syschar     = ' '
        CYCLE
      ENDIF
      DO isat=1,49
        IF (rinstat%sys(isys)%sat(isat)%satname.eq.'   ') CYCLE
        IF (rinstat%sys(isys)%sat(isat)%obssum==0) THEN
          rinstat%sys(isys)%sat(isat)%satname = '   '
        ELSE
          rinstat%numsat = rinstat%numsat + 1
        ENDIF
      ENDDO
    ENDDO

!
!  Print statistics if requested:
!  =============================
!   Is a print requested?
!   --------------------
    CALL ckoptb(1,(/'PRTOSTAT'/),srname,                      &
                'Print RINEX observation statistics?',irCode, &
                result1=prtostat)

!   Stop if the input value is corrupt
!   ----------------------------------
    IF (irCode /= 0) CALL exitrc(2)

    IF (prtostat.EQ.1) THEN

!      WRITE(lfnprt,'(/,A)')'SAT '
!      DO ityp=1,MAXTYP
!        IF (.not.usetyp(ityp)) CYCLE
!        WRITE(lfnprt,'(A3,A1)') OBSTYPESR3(ityp),' '
!      ENDDO
!      WRITE(lfnprt,'(/)')
!      WRITE(lfnprt,'(A,/)')'TEST'

      helpstr(:) = '*'
      WRITE(lfnprt,'(2(/,A))')' RINEX statistics for file: '//TRIM(filename), &
                              ' -------------------------- '
      WRITE(lfnprt,'(/,892A1)')(helpstr(ityp),ityp=1,892)
      WRITE(lfnprt,'(A4,111(5X,A3))')'SAT ',(OBSTYPESR3(ityp),ityp=1,MAXTYP),'SUM'
      WRITE(lfnprt,'(892A1)')(helpstr(ityp),ityp=1,892)

      DO isys=0,(MAXSYS-1)
        IF (rinstat%sys(isys)%syschar.eq.' ') CYCLE
        DO isat=1,49
          IF (rinstat%sys(isys)%sat(isat)%satname.eq.'   ') CYCLE
!          DO ityp=1,MAXTYP
!            IF (.not.usetyp(ityp)) CYCLE
          WRITE(lfnprt,'(A4,111(1X,I7))')rinstat%sys(isys)%sat(isat)%satname, &
                   (rinstat%sys(isys)%sat(isat)%numobs(ityp),ityp=1,MAXTYP), &
                   rinstat%sys(isys)%sat(isat)%obssum
        ENDDO
        WRITE(lfnprt,'(892A1)')(helpstr(ityp),ityp=1,892)
        WRITE(lfnprt,'(A1,A3,111(1X,I7))')rinstat%sys(isys)%syschar,'   ', &
                   (rinstat%sys(isys)%numobs(ityp),ityp=1,MAXTYP), &
                   rinstat%sys(isys)%obssum
        WRITE(lfnprt,'(892A1)')(helpstr(ityp),ityp=1,892)
      ENDDO
      WRITE(lfnprt,'(I4,111(1X,I7))') rinstat%numsat, &
                   (rinstat%numobs(ityp),ityp=1,MAXTYP), &
                   rinstat%obssum
      WRITE(lfnprt,'(892A1,/)')(helpstr(ityp),ityp=1,892)
    ENDIF

!  Select observation types to be used (considering availability and
!  priority)
!  =================================================================
    IF (rinstat%numsat > 0) THEN
      CALL init_geos(rinstat%numsat,gobsdef)
      CALL readgeos2(rinstat,rectyp,gobsdef,usegeos)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE rnxstat

END MODULE
