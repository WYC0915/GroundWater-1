
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM satmrk

! -------------------------------------------------------------------------
! Purpose:    Mark, reset, or eliminate observations in zero
!             or single, phase or code observation files. Add
!             cycle slips and set or reset ambiguities
!
! Remarks:    updated version of PG satmrk.f
!
! Changes in old PG:
!             23-Dec-91 : Error in format 904
!             15-Apr-92 : Possibility to remove satellite
!             16-Jun-92 : Opnfil used, keep all cycleslip
!                         Flags in case of elimi-option
!                         And update ambiguities
!             11-Aug-92 : More satellites in one run
!             18-Oct-92 : Mark all satellites did not work
!             01-Aug-93 : New format, subroutine uphead
!             23-Nov-93 : SF: Set maxsat to 30
!              1-Jul-94 : MR: Allow edit info file
!             10-Aug-94 : MR: Call exitrc
!             14-Aug-94 : MR: Format 4: session as character*4
!             18-Jan-95 : MR: Decl. of "lstcyc": r*8 instead of i*4
!             17-Sep-95 : JJ: Increase maxfil to 200
!             08-Feb-96 : MR: Add cycle slip and ambiguity option
!                             From edit file
!             06-Jun-96 : MR: Removed unused variables
!             24-Sep-97 : DI: Use include 'i:maxsat'
!             23-Oct-97 : DI: Include glonass wavelengths
!             04-May-98 : SS: Sr defreq modified
!             27-Jan-00 : RD: Add minimum #obs. per ambiguity
!             20-Jul-00 : RD: New call of satmra
!             04-Jan-01 : HB: Increase maxfil from 200 to 1000 and
!                             Maxedt from 5000 to 15000
!             18-Sep-01 : HB: Switch to new menu system
!                             Bug fix: reset-option works again
!             25-Sep-01 : HB: Add synchronizing of observations (from
!                             Sr mrkall)
!             09-Oct-01 : HB: Dynamisation of maxfil and maxedt
!             21-Nov-01 : RD: Give file name into rdedit
!             27-Nov-01 : SS: Tolerate differences concerning first
!                             Observation epoch
!             16-Dec-01 : HU: D_const added
!             07-Jan-02 : RD: Allocate lstedt for manual input
!             09-Feb-02 : RD: Several entries per obs.file in 1 edt-file
!             10-Feb-02 : RD: Report in pgm output what was done
!
! Author:     M.Rothacher, L.Mervart, E.Brockmann
!
! Created:    14-Feb-2002
!
! Changes:    14-Feb-2002 RD: F77->F90
!             22-Jul-2002 HB: Use modified t_obsHead and new SR upHead2
!             30-Jul-2002 HU: Use interface for alcerr
!             10-Sep-2002 RD: Add a summary line to the output
!             25-Sep-2002 HU: Remove i_astlib
!             18-Feb-2003 SS: Nonsensical warning message replaced by a
!                             more reasonable one
!             06-Mar-2003 MM: New option: re-initializing ambiguities
!             23-Apr-2003 HU: Nullify local pointers
!             15-May-2003 AJ: Initialize structure
!             18-May-2003 HU: Initialize structure
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             29-Jun-2005 RD: Make the pgf90 on linux work
!             27-Feb-2007 AG: Call DEFCON with parameter
!             19-Apr-2010 RD: Re-init. ambiguities only for one GNSS
!             23-Sep-2010 RD: Enable CPU counter
!             02-Dec-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, shortlineLength, lfnPrt, lfnErr
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: date,time
  USE d_gpsobs, ONLY: t_obsHead,init_obshead
  USE d_edit,   ONLY: t_edtRec,t_edit,init_edit
  USE p_satmrk, ONLY: t_satmrk_opt
  USE s_dimtst
  USE s_alcerr
  USE s_prflna
  USE s_wthead2
  USE s_rdedit2
  USE s_obsedt
  USE s_pritit
  USE s_seledt
  USE s_readinpf
  USE s_uphead2
  USE s_satmin
  USE s_rdhead2
  USE s_defcon
  USE s_exitrc
  USE s_opnsys
  IMPLICIT NONE

! Maximal dimensions
! ------------------
  CHARACTER(LEN=6),  PARAMETER               :: pgName = 'SATMRK'
  CHARACTER(LEN=1),  DIMENSION(3),PARAMETER  :: meaTyp = (/'P','C','R'/)

! Declarations
! ------------
  TYPE(t_satmrk_opt)                         :: opt
  TYPE(t_obsHead)                            :: obsHead  ! obs. file header
  TYPE(t_edtRec),    DIMENSION(:),  POINTER  :: edtLst,ambLst,hlpLst
  TYPE(t_edit)                               :: edit

  CHARACTER(LEN=shortlineLength)             :: line

  REAL(r8b),         DIMENSION(2)            :: obstim

  INTEGER(i4b),      DIMENSION(8)            :: filSum
  INTEGER(i4b),      DIMENSION(:,:),POINTER  :: filDid    ! Counts the actions
                                              ! filDid(1,:) mark obs.
                                              ! filDid(2,:) reset obs.
                                              ! filDid(3,:) delete obs.
                                              ! filDid(4,:) apply cycle slip
                                              ! filDid(5,:) set cycle slip
                                              ! filDid(6,:) unset cycle slip
                                              ! filDid(7,:) set ambiguity
                                              ! filDid(8,:) unset ambiguity
  INTEGER(i4b),      DIMENSION(:,:),POINTER  :: ifrlst
  INTEGER(i4b)                               :: iFil
  INTEGER(i4b)                               :: iEdit
  INTEGER(i4b)                               :: nEdt,nAmb ! Number of entries
                                                          ! in edtLst, ambLst
  INTEGER(i4b)                               :: numEdt    ! Number of edits/file
  INTEGER(i4b)                               :: iAmb
  INTEGER(i4b)                               :: ambMax
  INTEGER(i4b)                               :: ii
  INTEGER(i4b)                               :: irc

  CHARACTER(LEN=42)                          :: hlpStr
  INTEGER(i4b)                               :: nAmb1, nAmb2, nAmb5

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)


! Nullify pointers
! ----------------
  CALL init_obshead(obsHead)
  CALL init_edit(edit)
  CALL init_inpkey(inpKey)
  NULLIFY(edtLst)
  NULLIFY(ambLst)
  NULLIFY(hlpLst)
  NULLIFY(filDid)
  NULLIFY(ifrlst)
  NULLIFY(opt%filNam)
  NULLIFY(opt%filSyc)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Define constants
! ----------------
  CALL defcon(1)

! Write title and file list
! -------------------------
  CALL pritit(pgName,'Mark/delete observations',132)
  CALL prflna(132)

! Read option input file
! ----------------------
  CALL satmin(opt)

! Only re-initialization of ambiguities requested
! -----------------------------------------------
  IF (opt%syc==4) THEN

! Write header line
    WRITE(lfnprt,"(2(1X,A26,/))") 'Re-initialized ambiguities',            &
                                  '--------------------------'
    WRITE(lfnprt,"(1X,A14,8X,A14,6X,3(6X,A2),3X,A5,/,1X,A79)")             &
     'Station name 1','Station name 2','L1','L2','L5','total',             &
     '------------------------------------------------------' //           &
     '--------------------------'

! Loop over all files
    DO iFil=1,opt%nFil
      CALL rdhead2(opt%filNam(1,iFil),obsHead)
      IF (obsHead%numAmb==0) CYCLE
      nAmb1 = 0
      nAmb2 = 0
      nAmb5 = 0

! Find the biggest ambiguity cluster identifier
      ambMax = 0
      IF (opt%ambsys /= 10) THEN
        DO iAmb=1,obsHead%numAmb
          ambMax = MAX0(ambMax,obsHead%ambigu(iAmb)%ambcls(1), &
                               obsHead%ambigu(iAmb)%ambcls(2), &
                               obsHead%ambigu(iAmb)%ambcls(3))
        ENDDO
      ENDIF

! Reset ambiguities
      DO iAmb=1,obsHead%numAmb
        IF ( opt%ambsys /= 10 .AND. &
             obsHead%ambigu(iAmb)%ambsat/100 /= opt%ambsys) CYCLE

        SELECT CASE(opt%resAmb)
          CASE(1)
            obsHead%ambigu(iAmb)%ambigu(1) = 0.d0
            obsHead%ambigu(iAmb)%ambcls(1) = iAmb + ambMax
            nAmb1 = nAmb1 + 1
          CASE(2)
            obsHead%ambigu(iAmb)%ambigu(2) = 0.d0
            obsHead%ambigu(iAmb)%ambcls(2) = iAmb + ambMax
            nAmb2 = nAmb2 + 1
          CASE(3)
            obsHead%ambigu(iAmb)%ambigu(1:2) = 0.d0
            obsHead%ambigu(iAmb)%ambcls(1:2) = iAmb + ambMax
            nAmb1 = nAmb1 + 1
            nAmb2 = nAmb2 + 1
          CASE(4)
            obsHead%ambigu(iAmb)%ambigu(3) = 0.d0
            obsHead%ambigu(iAmb)%ambcls(3) = iAmb + ambMax
            nAmb5 = nAmb5 + 1
          CASE(5)
            obsHead%ambigu(iAmb)%ambigu(1:3) = 0.d0
            obsHead%ambigu(iAmb)%ambcls(1:3) = iAmb + ambMax
            nAmb1 = nAmb1 + 1
            nAmb2 = nAmb2 + 1
            nAmb5 = nAmb5 + 1
        END SELECT
      END DO

! Write some statistics
      hlpStr = ' '
      hlpStr(1:16) = obsHead%sta(1)%staNam
      IF (obsHead%nDiff==1) hlpStr(23:38) = obsHead%sta(2)%staNam
      WRITE(lfnprt,"(1X,A42,4(2X,I6))") &
            hlpStr, nAmb1, nAmb2, nAmb5, obsHead%numAmb

! Write new observation file header
    CALL wthead2(opt%filNam(1,iFil),obsHead)

    END DO
    CALL exitrc(0)
  END IF




! Prepare the protocol
! --------------------
  line = ' SUMMARY OF ACTION IN THE OBS. FILE(S):'
  IF (opt%syc == 1) THEN
    line(46:77) = '(synchronization)'
  ELSE IF (LEN_TRIM(opt%edtFil) > 0) THEN
    line(46:77) = opt%edtFil
  ELSE
    line(46:77) = '(manual selected changes)'
  ENDIF
  WRITE(lfnprt,'(5(/,A))') TRIM(line),                &
       ' -------------------------------------',      &
       '                                                 Mea-       Observ' // &
       'ations                Cycle slips           Ambiguities',              &
       '  Num  Station name 1        Station name 2      type    mark  unm' // &
       'ark  delete     correct   set   unset       set   unset',              &
       ' -----------------------------------------------------------------' // &
       '------------------------------------------------------------------'


! Allocate statistic array
! ------------------------
  ALLOCATE(filDid(8,1),stat=irc)
  CALL alcerr(irc,'filDid',(/8,1/),pgName)

  filSum = 0

! Loop over all files
! -------------------
  DO iFil = 1, opt%nFil

! Read observation file header
! ----------------------------
    CALL rdhead2(opt%filNam(1,iFil),obsHead)

    CALL dimtst(1,2,1,pgName,'meaTyp','Measurement type ID', &
                'Measurement type is still not implemented.',&
                obsHead%meaTyp,SIZE(meaTyp),irc)

! Get the list of editing requests for this file
! ----------------------------------------------
    numEdt = 0
    iEdit  = 0
    DO WHILE (iEdit < SIZE(fildid,2))
      CALL seledt(iFil,opt,obsHead,nEdt,edtLst,iEdit,fildid)

! All necessary edits for this file performed
! -------------------------------------------
      IF (iEdit > SIZE(fildid,2)) EXIT

! Perform all editing requests
! ----------------------------
      nAmb = 0
      IF (nEdt > 0 .OR. opt%minAmb > 0) THEN
        CALL obsedt(iFil,opt,obsHead,nEdt,edtLst,iEdit,fildid, &
                    obstim,ifrlst,nAmb,ambLst)
      ENDIF

! Mark observation because of opt%minAmb
! --------------------------------------
      IF (nAmb > 0) THEN
        CALL obsedt(iFil,opt,obsHead,nAmb,ambLst,iEdit,fildid, &
                    obstim,ifrlst,iAmb,hlpLst)
      ENDIF

! Report the manipulations in the order of observation files
! (for sync. and manual only)
! ----------------------------------------------------------
      IF (SIZE(filDid,2) == 1) THEN
        IF (obsHead%nDiff == 1) THEN
          WRITE(lfnprt,'(I5,2(2X,A,4X),1X,A,1X,A,3I8,2X,3I8,2X,2I8)')  &
                iFil,obsHead%sta(1:2)%staNam,meaTyp(obsHead%meaTyp),':',   &
                fildid(:,iEdit)
        ELSE
          WRITE(lfnprt,'(I5,2X,A,4X,22X,1X,A,1X,A,3I8,2X,3I8,2X,2I8)') &
                iFil,obsHead%sta(1)%staNam,meaTyp(obsHead%meaTyp),         &
                ':',fildid(:,iEdit)
        ENDIF
        DO ii = 1,SIZE(filSum)
          IF (SIZE(filDid,1) < ii) CYCLE
          filSum(ii) = filSum(ii)+filDid(ii,iEdit)
        ENDDO
      ENDIF

! May be there are several entries for a file in the edit file
! ------------------------------------------------------------
      numEdt = numEdt + nEdt+nAmb

    ENDDO

! No editing request for this file
! --------------------------------
    IF (numEdt == 0) CYCLE

! Update file header information
! ------------------------------
    CALL uphead2(obstim(1),obsHead,ifrlst)

    obsHead%timref = obstim(1)
    obsHead%nEpoch = IDNINT((obstim(2)-obstim(1))/obsHead%iDeltT*86400.D0)+1

    obsHead%crDate(2) = date
    obsHead%crTime(2) = time

    CALL wthead2(opt%filNam(1,iFil),obsHead)

  ENDDO

! Report edits without corresponding file
! ---------------------------------------
  IF (SIZE(filDid,2) > 1) THEN

    edit%filNam = opt%edtFil
    CALL rdedit2(edit)

    DO iEdit=1,SIZE(filDid,2)

      CALL dimtst(1,2,1,pgName,'meaTyp','Measurement type ID', &
                  'Measurement type is still not implemented.',&
                  edit%head(iEdit)%meaEdt,SIZE(meaTyp),irc)

      IF (fildid(1,iEdit) == -1) THEN
        WRITE(lfnprt,'(I5,2(2X,A,4X),1X,A,1X,A,2X,A)')                         &
              iEdit,edit%head(iEdit)%staEdt(:),meaTyp(edit%head(iEdit)%meaEdt),&
              ':      no corresponding observation file selected'
      ELSE
        WRITE(lfnprt,'(I5,2(2X,A,4X),1X,A,1X,A,3I8,2X,3I8,2X,2I8)')            &
              iEdit,edit%head(iEdit)%staEdt(:),meaTyp(edit%head(iEdit)%meaEdt),&
              ':',fildid(:,iEdit)
        DO ii = 1,SIZE(filSum)
          IF (SIZE(filDid,1) < ii) CYCLE
          filSum(ii) = filSum(ii)+filDid(ii,iEdit)
        ENDDO
      ENDIF

    ENDDO

  ENDIF

  WRITE(lfnprt,'(A)')                                                          &
       ' -----------------------------------------------------------------' // &
       '------------------------------------------------------------------'
  WRITE(lfnprt,'(7X,A,40X,3I8,2X,3I8,2X,2I8)') 'Total:',filSum(:)
  WRITE(lfnprt,'(A,/)')                                                        &
       ' -----------------------------------------------------------------' // &
       '------------------------------------------------------------------'

! Print warning message if no editing request at all
! --------------------------------------------------
  IF (sum(filSum(:)) == 0) &
    WRITE(lfnerr,'(/,A,/)') ' ### PG SATMRK: No editing request at all'

  CALL exitrc(0)

END PROGRAM satmrk

