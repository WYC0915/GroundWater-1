! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM CHOPRE

! -------------------------------------------------------------------------
! Purpose:    Conversion from CHAMP orbit format CHORB to SP3
!
! Author:     D. Svehla
!
! Created:    23-Jan-2001
!
! Changes:    22-Oct-2001 HB: F90, switch to new menu
!             25-Sep-2002 HU: Remove i_astlib
!             11-Nov-2002 HU: SP3c implemented
!             23-Apr-2003 HU: Nullify local pointers
!             21-Aug-2003 HB: Initialize satWgt
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             01-Aug-2005 HU: Epoch as structure
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             10-Jan-2012 SL: title string for pritit changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_cpu,    ONLY: cpu_start
  USE m_epoch,  ONLY: t_epoch
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE s_pritit
  USE s_rdchoh
  USE s_rdchoi
  USE s_defcon
  USE s_choinp
  USE s_leoprn
  USE s_opnsys
  USE s_prflna
  USE s_readinpf
  USE s_wtpreh
  USE s_wtprei
  USE s_exitrc
  IMPLICIT NONE

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------
  INTEGER(i4b),PARAMETER :: nSvn=1

! Local Variables
! ---------------
  REAL(r8b) ::     TFIRST,TLAST,DTTAB
  REAL(r8b),DIMENSION(1) :: DTSATC,DTSATD
  REAL(r8b),DIMENSION(3) :: POS
  REAL(r8b),DIMENSION(3) :: VEL
  REAL(r8b) ::     baspos,basclk
  REAL(r8b),DIMENSION(4,1) :: sdevp,sdevv
  REAL(r8b),DIMENSION(6,1) :: corrp,corrv

  INTEGER(i4b) :: irCode
  INTEGER(i4b) :: accura
  INTEGER(i4b),DIMENSION(1) :: satWgt
  INTEGER(i4b),DIMENSION(1) :: satNum
  INTEGER(i4b) :: iEpo
  INTEGER(i4b) :: nEpo
  INTEGER(i4b) :: iFrmat,iFormt
  INTEGER(i4b) :: iFil
  INTEGER(i4b) :: nFiles
  INTEGER(i4b), DIMENSION(4,1) :: accpos,accvel
!  INTEGER(i4b) :: irc

  CHARACTER(LEN=staNam2Length)                         :: LEONAME
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam

  CHARACTER(LEN=57),DIMENSION(4) :: title
  CHARACTER(LEN=5)               :: COOSYS,DATDES
  CHARACTER(LEN=4)               :: AGENCY
  CHARACTER(LEN=3)               :: ORBTYP,timsys
  CHARACTER(LEN=1),DIMENSION(4,1):: evtflg

  TYPE(t_epoch)                  :: tmjd

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(filNam)
  CALL init_inpkey(inpKey)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Define Constants
! ----------------
  CALL defcon(1)

! Write title and file list
! -------------------------
  CALL pritit('CHOPRE','Convert CHAMP orbits to precise orbits')
  CALL prflna

! Read all informations from i-file
! ---------------------------------
  CALL choinp(filNam,LEONAME,IFRMAT,DATDES,ORBTYP,AGENCY, &
       ACCURA,TITLE(2:4))

  nFiles = size(filNam,2)

! SP3c
! ----
  timsys='GPS'
  baspos=1.25D0
  basclk=1.025D0
  accpos=0
  accvel=0
  evtflg=' '
  sdevp=0D0
  sdevv=0D0
  corrp=0D0
  corrv=0D0
  satWgt = 0

! Loop over all files
! ===================
  DO iFil=1,nFiles

! Read header of LEO orbit
! ------------------------
    IFORMT=0
    CALL rdchoh(filNam(1,iFil),LFNORB,IFORMT,TITLE(1),TFIRST,TLAST, &
         nEpo,DTTAB,COOSYS)

! Get LEO SVN (PRN) number
! ------------------------
    CALL leoprn(LEONAME,TFIRST,SATNUM(1))

! Write header of LEO orbit
! -------------------------
    CALL wtpreh(filNam(2,iFil),LFNOR1,IFRMAT,NSVN,SATNUM,satWgt,TFIRST, &
         nEpo,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,timsys,baspos,basclk)

! Loop over all epochs
! ====================
    DO iEpo=1,nEpo

! Read CHORB records
! --------------------
      CALL rdchoi(LFNORB,IFRMAT,TMJD,POS,VEL,IRCODE)

! Write epoch of precise orbit
! --------------------
      DTSATC=999999.999999D0
      DTSATD=999999.999999D0
      CALL wtprei(LFNOR1,IFRMAT,(/0,0/),1,SATNUM,TMJD,POS,VEL, &
                  DTSATC,DTSATD,accpos,accvel,evtflg,sdevp,sdevv, &
                  corrp,corrv,ircode)
    ENDDO

! Write end of file "EOF"
! -----------------------
    WRITE(LFNOR1,'(A)')'EOF'

! Close input and output file
! ---------------------------
    CLOSE(UNIT=LFNORB)
    CLOSE(UNIT=LFNOR1)
  ENDDO

! End
! ---
  CALL exitrc(0)
END PROGRAM chopre
