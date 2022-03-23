
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM mkclus

! -------------------------------------------------------------------------
! Purpose:    Creates global or regional cluster for zero diff. solutions
!             (usefull for BPE, clock solutions) and orders baseline files
!             into clusters
!
! Author:     R.Dach
!
! Created:    13-Jun-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             13-Feb-2003 RD: Optimum number of stations
!             23-Apr-2003 HU: Nullify local pointers
!             18-May-2003 HU: Initialize structure
!             29-Oct-2003 RD: New call of SR MCDBLG.f90
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             15-Dec-2005 RD: New call of SR mcdblg
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             30-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, fileNameLength
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE p_mkclus, ONLY: t_mkclus_opt,t_staFil,t_station,t_cluster
  USE s_prflna
  USE s_mcmaxsta
  USE s_pritit
  USE s_mcinfo
  USE s_readinpf
  USE s_mcdblg
  USE s_mcregio
  USE s_mcglob
  USE s_mcdblr
  USE s_defcon
  USE s_exitrc
  USE s_mcinpt
  USE s_mcrbas
  USE s_opnsys
  USE s_mcprot
  IMPLICIT NONE

! List of functions
! -----------------

! Local types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),              PARAMETER     :: pgName = 'mkclus'

! Local variables
! ---------------
  TYPE(t_mkclus_opt)                           :: opt
  TYPE(t_staFil),  DIMENSION(:), POINTER       :: filInfo
  TYPE(t_station), DIMENSION(:), POINTER       :: station
  TYPE(t_cluster), DIMENSION(:), POINTER       :: cluList
  TYPE(t_cluster), DIMENSION(:), POINTER       :: delList

  CHARACTER(LEN=fileNameLength),                &
                   DIMENSION(:,:), POINTER     :: filLst

  INTEGER(i4b)                                 :: ii
  INTEGER(i4b)                                 :: nFil
  INTEGER(i4b)                                 :: nSta,nSta0
  INTEGER(i4b)                                 :: nClu
  INTEGER(i4b)                                 :: nDel
  INTEGER(i4b)                                 :: ircSta


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(filInfo)
  NULLIFY(station)
  NULLIFY(cluList)
  NULLIFY(delList)
  NULLIFY(filLst)
  CALL init_inpkey(inpKey)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Print the title area
! --------------------
  CALL opnsys
  CALL defcon(1)

  CALL pritit(pgName,'Form clusters of observation files')
  CALL prflna

! Read input options
! ------------------
  CALL mcinpt(opt)

! Get the file and station list
! -----------------------------
  CALL mcinfo(opt,nFil,filLst,filInfo,nSta,station,nClu,cluList)

  nDel = 0
  NULLIFY(delList)
  DO ii=1,SIZE(station)
    NULLIFY(station(ii)%nxtSta)
  ENDDO

! Put stations into global clusters
! ---------------------------------
  IF (opt%cluStrat == 1) THEN

    ! Put all stations into global clusters
    CALL mcglob(opt,nSta,station,nFil,filInfo,nClu,cluList)

    ircSta = 1
    DO WHILE (ircSta /= 0)

      ! Find satellites which are not observed by
      ! enough stations in the clusters
      IF (ircSta == 1) &
        CALL mcdblg(opt,nFil,filLst,filInfo,nClu,cluList,nDel,delList)

      ! Delete stations from list to fill "maxsta" condition
      CALL mcMaxsta(opt,nFil,filLst,filInfo,nSta,station, &
                    nClu,cluList,nDel,delList,ircSta)

    ENDDO

! Put stations into regional clusters
! ---------------------------------
  ELSE IF (opt%cluStrat == 2) THEN

    IF (opt%maxSta > 0) &
      opt%numClu = (nClu-1)/opt%maxSta+1

    nSta0 = opt%numClu+1

    DO WHILE (nSta0 /= opt%numClu)

      nSta0 = opt%numClu

      ! Put all stations into regional clusters
      CALL mcregio(opt,nSta,station,nFil,filInfo,nClu,cluList)

      ! Find satellites which are not observed by
      ! enough stations in the clusters
      CALL mcdblr(opt,nSta,station,nFil,filLst,filInfo,nClu,cluList)

    ENDDO

! Put baselines into regional clusters
! ------------------------------------
  ELSE IF (opt%cluStrat == 3) THEN

    IF (opt%maxSta > 0) &
      opt%numClu = (nClu-1)/opt%maxSta+1

    nSta0 = opt%numClu+1

    DO WHILE (nSta0 /= opt%numClu)

      nSta0 = opt%numClu

      ! Put all baselines into regional clusters
      CALL mcrbas(opt,nSta,station,nFil,filInfo,nClu,cluList)

    ENDDO

  ENDIF

! Write the program output
! ------------------------
  CALL mcprot(opt,nFil,filLst,filInfo,nSta,station,nClu,cluList,nDel,delList)

! Finish the program
! ------------------
  CALL exitrc(0)

END program mkclus
