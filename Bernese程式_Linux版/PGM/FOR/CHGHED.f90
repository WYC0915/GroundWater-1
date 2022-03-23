
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

      PROGRAM CHGHED

! -------------------------------------------------------------------------
! Purpose:    Read unformatted header and make some changes in the
!             header (phase or code, zero or single differences)
!             then write a new unformatted header file
!
!
! Remark:     updated version of PG chghed.f
!
! Changes in the old SR
!             08-may-92 : print out
!             04-aug-93 : version 3.5, new format
!             09-nov-93 : MR: change of site name: both also ok for
!                             2nd site of single diff. file
!             23-nov-93 : SF: set maxsat to 30
!             10-aug-94 : MR: call exitrc
!             12-aug-94 : MR: format 4: session as character*4
!             20-feb-96 : MR: intnam(1,3),intnam(1,4) exchanged in
!                             call wthead
!             24-sep-97 : DI: use include 'maxsat.inc'
!             08-DEC-00 : RD: use new menu
!
! Author:     M.Rothacher,G.Gu
!
! Created:    11-Dec-2000
! Last mod.:  27-Oct-2010
!
! changes:    11-Dec-2000 RD: F77->F90
!             12-Dec-2000 RD: use (new) STACRUX file to change headers
!             18-Dec-2000 HU: use interface for PRFLNA
!             18-Jun-2001 HB: use interface for READCRUX and d_stacrx
!             26-Jun-2001 RD: deallocation status
!             05-Sep-2001 HU: Interfaces for rdhead2, wthead2 added
!             23-Oct-2001 RD: add pritit
!             16-Dec-2001 HU: use d_const
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 HU: Nullify local pointers
!             15-May-2003 HB: Initialize structure
!             08-Jul-2003 RD: Read "staCrux" in CHINPT (incl flag handling)
!             09-Sep-2003 RD: Indicate station independent items for input
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             30-Mar-2006 AG: ABC deletion added
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             27-Oct-2010 SL: use m_bern with ONLY
!
! SR called:  chinpt, defcon, opnsys, rdhead2, wthead2, gtflna, pritit,
!             chstacrx, chmanual, prflna, exitrc, init_obshead, init_stacrux,
!             readinpf, init_inpkey
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnPrt, lfnLoc, fileNameLength, shortLineLength
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: date,time
  USE p_chghed, ONLY: t_chghed_chr,t_chghed_real,t_chghed_int,t_chghed
  USE d_gpsobs, ONLY: t_obsHead,init_obshead
  USE d_stacrx, ONLY: t_stacrux,init_stacrux
  USE s_prflna
  USE s_wthead2
  USE s_chstacrx
  USE s_chinpt
  USE s_pritit
  USE s_readinpf
  USE s_chmanual
  USE s_rdhead2
  USE s_defcon
  USE s_exitrc
  USE s_opnsys

  IMPLICIT NONE
!
! DECLARATIONS
! ------------
  TYPE(t_obsHead)                :: obsHead! Header of obs. file
  TYPE(t_stacrux)                :: stacrx ! StaCrux rescords
!
  CHARACTER(LEN=fileNameLength)  :: crxfil ! name of the stacrux file
  CHARACTER(LEN=fileNameLength), DIMENSION(:), POINTER :: FILNAM ! list of header file names
  CHARACTER(LEN=53)              :: TITNEW ! new title
  CHARACTER(LEN=shortLineLength) :: Line   ! Protocol line
!
  TYPE(t_chghed_chr),  DIMENSION(:), POINTER  :: chgChr  ! Change of chr-var.
  TYPE(t_chghed_int),  DIMENSION(:), POINTER  :: chgInt  ! Change of int-var.
  TYPE(t_chghed_real), DIMENSION(:), POINTER  :: chReal  ! Change of real-var.
!
  INTEGER(i4b)       :: nChvar    ! Number of chr-param to change
  INTEGER(i4b)       :: nInvar    ! Number of int-param to change
  INTEGER(i4b)       :: nRevar    ! Number of real-param to change
  INTEGER(i4b)       :: iChr      ! counter for characters
  INTEGER(i4b)       :: ifil      ! counter for files
  INTEGER(i4b)       :: nfil      ! number of header files
  INTEGER(i4b)       :: iSta      ! counter of station in header files
  INTEGER(i4b)       :: iac       ! allocation status
  INTEGER(i4b)       :: irc       ! return code from SRs
  INTEGER(i4b), PARAMETER :: iOpt_stanam =  3  ! index of stanam in opt list
!
!          I O                                                           I I I
!          O P                                                           D D D
!          P T  Text for               Keyword in input file             X X X
!          T A  Protocol                                                 C I R
!          I N                                                           H N E
!          O Y  TEXT                   option     old val.   new val.    R T A
!
  TYPE(t_chghed), DIMENSION(15) :: opt = (/ &
  t_chghed(1,0,'TITLE LINE',        (/'OPTTIT  ','        ','TITLE   '/),0,0,0), &
  t_chghed(1,0,'CAMPAIGN NAME',     (/'OPTCMP  ','CMPNAM_O','CMPNAM_N'/),1,0,0), &
  t_chghed(0,0,'STATION NAME',      (/'OPTSTA  ','STANAM_O','STANAM_N'/),2,0,0), &
  t_chghed(0,0,'RECEIVER TYPE',     (/'OPTREC  ','RECNAM_O','RECNAM_N'/),3,0,0), &
  t_chghed(0,0,'ANTENNA TYPE',      (/'OPTANT  ','ANTNAM_O','ANTNAM_N'/),4,0,0), &
  t_chghed(0,0,'OPERATOR NAME',     (/'OPTOBS  ','OBSNAM_O','OBSNAM_N'/),5,0,0), &
  t_chghed(1,0,'SESSION IDENTIFIER',(/'OPTSES  ','SESNAM_O','SESNAM_N'/),6,0,0), &
  t_chghed(1,0,'SUBSESSION IDENTI.',(/'OPTSFL  ','SFLNAM_O','SFLNAM_N'/),7,0,0), &
  t_chghed(0,0,'RECEIVER NUMBER',   (/'OPTRCN  ','RECNUM_O','RECNUM_N'/),0,1,0), &
  t_chghed(0,0,'ANTENNA  NUMBER',   (/'OPTANN  ','ANTNUM_O','ANTNUM_N'/),0,2,0), &
  t_chghed(1,0,'REMARK NUMBER',     (/'OPTRMK  ','REMARK_O','REMARK_N'/),0,3,0), &
  t_chghed(1,0,'OBSERV. INTERVAL',  (/'OPTINT  ','OBSINT_O','OBSINT_N'/),0,4,0), &
  t_chghed(0,0,'POS.ECCENTR. NORTH',(/'OPTEXN  ','EXENTN_O','EXENTN_N'/),0,0,1), &
  t_chghed(0,0,'POS.ECCENTR. EAST', (/'OPTEXE  ','EXENTE_O','EXENTE_N'/),0,0,2), &
  t_chghed(0,0,'POS.ECCENTR. UP',   (/'OPTEXU  ','EXENTU_O','EXENTU_N'/),0,0,3) /)
! Remark: (iOption == 1) indicates that there is only a checkbox because
!                  the item is station independent
!
! NULLIFY POINTERS
! ----------------
  NULLIFY(FILNAM)
  NULLIFY(chgChr)
  NULLIFY(chgInt)
  NULLIFY(chReal)
  CALL init_obshead(obsHead)
  CALL init_stacrux(stacrx)
  CALL init_inpkey(inpKey)

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)
!
! DEFINE SYSTEM FILES
! -------------------
  CALL OPNSYS
!
! DEFINE CONSTANTS
! ----------------
  CALL DEFCON(1)
!
! Print the program title
! -----------------------
  CALL pritit('chghed','Change header of observation files')
!
! READ OPTION INPUT FILE
! ----------------------
  CALL CHINPT(opt, crxfil, staCrx,             &
              nChvar, nInvar, nRevar,          &
              TITNEW, ChgChr, ChgInt, ChReal,  &
              nFil, filnam)
!
! READ stacrux file
! -----------------
  IF (LEN_TRIM(crxfil) > 0) THEN
    CALL prflna
  ENDIF
!
! MAKE THE SAME CHANGES IN ALL THE HEADER FILES
! ---------------------------------------------
  DO IFIL=1,NFIL
!
! WRITE FILE INTO PROTOCOL
! ------------------------
    WRITE(LFNPRT,'(/,A)') ' FILE '//TRIM(FILNAM(IFIL))
    Line=' *****'
    DO iChr=1,LEN_TRIM(FILNAM(IFIL))
      Line=TRIM(Line)//'*'
    ENDDO
    WRITE(LFNPRT,'(A,/)') Line
!
! READ HEADER FILE
! ----------------
    CALL rdhead2(filnam(iFil), obsHead)
!
! FOR ABC DELETION
! ----------------
    DO iSta=1,obsHead%nDiff+1
      IF (obsHead%sta(iSta)%stanam(15:16) /= '  ') THEN
        WRITE(lfnprt,'(A,A20,A,I1,A,A16,A,A14)')          &
           ' ',opt(iOpt_stanam)%text,' (',iSta,') : ',    &
           obsHead%sta(iSta)%stanam,'     --> ',          &
           obsHead%sta(iSta)%stanam(1:14)
        obsHead%sta(iSta)%stanam(15:16)='  '
      ENDIF
    ENDDO
!
! CHANGE HEADER FROM MANUAL INPUTS
! --------------------------------
    IF (LEN_TRIM(crxfil) == 0) THEN
      CALL chmanual(opt, titnew, ChgChr, ChgInt, ChReal, obsHead, irc)
!
! CHANGE HEADER FROM STACRUX FILE
! -------------------------------
    ELSE
      CALL chstacrx(opt, stacrx, obsHead, irc)
    ENDIF
!
! CHANGE CREATION AND MODIFICATION DATE AND TIME
! ----------------------------------------------
    IF (irc == 0) THEN
      obsHead%crdate(2)=DATE
      obsHead%crtime(2)=TIME
!
! WRITE HEADER OF FORMATTED FILE
! ------------------------------
      CALL wthead2(FILNAM(IFIL),obsHead)
    ENDIF
!
! NEXT FILE
! ---------
    CLOSE(UNIT=LFNLOC)
  ENDDO ! Loop all files
!
  IF (LEN_TRIM(crxfil) > 0) THEN
    DEALLOCATE(stacrx%renamsta, stat=iac)
    DEALLOCATE(stacrx%stainfo, stat=iac)
    DEALLOCATE(stacrx%staprob, stat=iac)
    DEALLOCATE(stacrx%coovel, stat=iac)
    DEALLOCATE(stacrx%staType, stat=iac)
  ENDIF
!
  DEALLOCATE(ChgChr, stat=iac)
  DEALLOCATE(ChgInt, stat=iac)
  DEALLOCATE(ChReal, stat=iac)
  DEALLOCATE(filnam, stat=iac)
!
  CALL EXITRC(0)
  END
