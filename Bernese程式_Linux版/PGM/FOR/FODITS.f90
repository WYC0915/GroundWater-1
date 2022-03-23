
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM fodits

! -------------------------------------------------------------------------
! Name:       Find Outliers and Discontinuities In Time Series (FODITS)
!
! Purpose:    Main program
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 L0: file created
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Second revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             17-Jun-2009 LO: FFT Introduced
!             19-Aug-2009 LO: MENUAUX variables included
!             20-Aug-2009 LO: Check for minimal number of observations added.
!             25-Sep-2009 LO: Changes for F90 consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             23-Sep-2010 RD: Enable CPU counter
!             12-Nov-2010 SL: use p_fodits with ONLY
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             19-Jul-2011 LO: Test datum defintion added
!             14-Nov-2011 SL: PRITIT call changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,     ONLY: i4b, shortLineLength
  USE m_cpu,      ONLY: cpu_start

  USE d_inpkey,   ONLY: inpKey, init_inpkey

  USE p_fodits,   ONLY: t_opt, t_score, t_datfil, &
                        init_score, init_datfil, &
                        mamod, mremo, lscrn, mapri, liter, miden

  USE s_readinpf
  USE s_defcon
  USE s_opnsys
  USE s_pritit
  USE s_prflna

  USE s_fodiropt
  USE s_fodirdat
  USE s_fodiddef

  USE s_fodirevl
  USE s_fodirsta
  USE s_fodirerq
  USE s_fodigeti

  USE s_fodichks
  USE s_fodiidia

  USE s_fodissev
  USE s_fodiadas
  USE s_fodiptst
  USE s_fodiprgl
  USE s_fodimpod

  USE s_fodispar
  USE s_fodivcon

  USE s_fodiputo

  USE s_fodiwevl
  USE s_fodiupdf
  USE s_fodiwsta
  USE s_fodiwdat

  USE s_exitrc

  IMPLICIT NONE

! Local structures
! ----------------
  TYPE(t_opt)         :: opt        ! Panel options struct
  TYPE(t_sCore)       :: sCore      ! Core sturct of FODITS
  TYPE(t_datFil)      :: datFil     ! I/O files struct

! Local types
! -----------
  INTEGER(i4b)                                  :: iSta
  INTEGER(i4b)                                  :: iElem
  INTEGER(i4b)                                  :: iIter
  INTEGER(i4b)                                  :: iScrn
  INTEGER(i4b)                                  :: stateM

! Program
! -------

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

  ! Initializations
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)
  CALL defcon(1)
  CALL opnsys
  CALL pritit('FODITS','Analysis of time series',131)
  CALL prflna(131)

  ! The program FODITS analyzes time series up to 3 components - so far.
  ! In CASE one  needs to analyze time series of more than 3 components
  ! each component could be analyzed as signle one station.

  ! Initialize structs
  CALL init_sCore(sCore)                    ! Struct initialization
  CALL init_datFil(datFil)                  ! Struct initialization

  ! Read inforamtion from input panel and from input files
  CALL fodiropt(opt,sCore)                  ! Read panel FODITS.INP
  CALL fodirdat(opt,sCore,1)                ! Read time series --- part 1

  ! Check datum definition
  IF( opt%modPreDatTest == 1 )THEN
     CALL fodiddef(opt,sCore)
  END IF

  ! Convert XYZ time series to NEU
  CALL fodirdat(opt,sCore,2)                ! Read time series --- part 2

  ! Read inputs and store the information to dataFil and sCore structs
  CALL fodirevl(opt,datFil)                 ! Read EVL -> datFil%evlIn
  CALL fodirsta(opt,datFil)                 ! Read STA -> datFil%staIn
  CALL fodirerq(opt,datFil,sCore)           ! Read ERQ -> datFil%erqIn
  CALL fodigeti(opt,datFil,sCore)           ! datFil -> sCore

  ! In terms of book-keeping three models characterize the algorithm of FODITS:
  ! - the model %apri contains all apriori known elements
  ! - the model %amod contains all elements that are estimated by LSA
  ! - the model %iden contains all new idetified elements

  ! Core of FODITS, the Adaptation, Test, Identification (ATI) procedure:
  ! - the algorithm consists in iteration and screening steps;
  ! - one screeing step inserts the most significant element of %apri in %amod
  !   and removes all non-significant elements from %amod;
  ! - screening steps stop as soon as %amod is equal to one of the
  !   previous %amod (of the previous screening steps of the actual iteration
  !   step) or (short-cut) when no further elements of %apri are inserterd;
  ! - one iteration step contains screening steps and the identification
  !   of new evnts in the time series;
  ! - a iteration step procedure consists in inserting the most probable
  !   element in %amod if it is significant;
  ! - iteration steps stop as soon as amodal %amod is equal one of the
  !   previous ones (of the previous iteration steps) or (short-cut) when
  !   when no further elements of %iden are inserterd - found significant;
  ATI_PROCEDURE: DO iSta = 1,sCore%nSta
     sCore%ctr%nIterLoop = 0
     CALL fodiidia(opt,sCore,iSta,stateM)
     IF( stateM == 1 )CYCLE
     CALL fodiadas(mAmod,opt,sCore,iSta,stateM)
     IF( stateM == 9 )CYCLE
     ITERATION_LOOP: DO iIter = 1,opt%gvarMaxNrIterStep
        sCore%ctr%nIterLoop = sCore%ctr%nIterLoop + 1
        sCore%ctr%nScrnLoop = 0
        SCREENING_LOOP: DO iScrn = 1,opt%gvarMaxNrScrnStep
           sCore%ctr%nScrnLoop = sCore%ctr%nScrnLoop + 1
           sCore%ctr%nRemoLoop = 0
           REMOVE_LOOP: DO
              sCore%ctr%nRemoLoop = sCore%ctr%nRemoLoop + 1
              IF( stateM == 9 ) EXIT ITERATION_LOOP
              REMOVE_LOOP_ITER: DO iElem = 1,sCore%mdl%nAmod
                 CALL fodissev(mRemo,opt,sCore,iSta,iElem,stateM)
                 IF( stateM == 1 )CYCLE
                 CALL fodiadas(mRemo,opt,sCore,iSta,stateM)
                 IF( stateM == 9 ) EXIT ITERATION_LOOP
                 CALL fodiptst(mRemo,opt,sCore)
              END DO REMOVE_LOOP_ITER
              CALL fodichks(mRemo,opt,sCore,iSta,stateM)
              CALL fodiadas(mAmod,opt,sCore,iSta,stateM)
              IF( stateM == 9 ) EXIT ITERATION_LOOP
              IF( stateM == 5 ) EXIT REMOVE_LOOP
           END DO REMOVE_LOOP
           CALL fodiprgl(lScrn,opt,sCore,iSta,stateM)
           IF( stateM == 8 )EXIT SCREENING_LOOP
           INSERT_LOOP_SC: DO iElem = 1,sCore%mdl%nApri
              CALL fodissev(mApri,opt,sCore,iSta,iElem,stateM)
              IF( stateM == 1 )CYCLE
              CALL fodiadas(mApri,opt,sCore,iSta,stateM)
              IF( stateM == 9 ) EXIT ITERATION_LOOP
              CALL fodiptst(mApri,opt,sCore)
           END DO INSERT_LOOP_SC
           CALL fodichks(mApri,opt,sCore,iSta,stateM)
           CALL fodiadas(mAmod,opt,sCore,iSta,stateM)
           IF( stateM == 9 ) EXIT ITERATION_LOOP
           IF( stateM == 5 ) EXIT SCREENING_LOOP
        END DO SCREENING_LOOP
        CALL fodiprgl(lIter,opt,sCore,iSta,stateM)
        IF( stateM == 8 ) EXIT ITERATION_LOOP
        CALL fodimpod(opt,sCore,iSta)
        INSERT_LOOP_ID: DO iElem = 1,sCore%mdl%nIden
           CALL fodissev(mIden,opt,sCore,iSta,iElem,stateM)
           IF( stateM == 1 )CYCLE
           CALL fodiadas(mIden,opt,sCore,iSta,stateM)
           IF( stateM == 9 ) EXIT ITERATION_LOOP
           CALL fodiptst(mIden,opt,sCore)
        END DO INSERT_LOOP_ID
        CALL fodichks(mIden,opt,sCore,iSta,stateM)
        IF( stateM == 5 ) EXIT ITERATION_LOOP
        CALL fodiadas(mAmod,opt,sCore,iSta,stateM)
        IF( stateM == 9 ) EXIT ITERATION_LOOP
     END DO ITERATION_LOOP
     CALL fodiadas(mAmod,opt,sCore,iSta,stateM)
     CALL fodispar(opt,sCore,iSta)
     CALL fodivcon(opt,sCore,iSta)
  END DO ATI_PROCEDURE

  ! Write output files
  CALL fodiputo(opt,sCore,datFil)           ! sCore -> datFil
  CALL fodiwevl(opt,datFil)                 ! Write sCore%evlIn -> EVL
  CALL fodiupdf(opt,sCore,datFil)           ! Update files for ADDNEQ2
  CALL fodiwsta(opt,datFil)                 ! Write STA for ADDNEQ2
  CALL fodiwdat(opt,sCore,datFil)           ! Write sCore -> PLT,CRD,VEL

  ! Exit program
  CALL exitrc(0)                            ! End of program, exit

END PROGRAM fodits
