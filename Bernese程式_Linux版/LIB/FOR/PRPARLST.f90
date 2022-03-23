MODULE s_PRPARLST
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prparlst(iact,ityp,numFil,name,locq,time,nparms)

! -------------------------------------------------------------------------
! Purpose:    Print a summary of all parameters and if requested
!             a (debugging) list with one line for each parameter
!             with: "ityp,numfil,parameter,name,locq,time"
!
! Author:     C. Urschl, S. Schaer
!
! Created:    21-May-2003
! Last mod.:  30-Nov-2010
!
! Changes:    28-May-2003 HU: parStrgShort and parStrgLong extended to 26
!             14-Aug-2003 HU: Satellite pcv added
!             29-Oct-2003 HB: Correct format statement
!             22-Jan-2004 SS/MM: SPV replaced by SAP
!             19-May-2005 CU: Correct format statement
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             08-Feb-2007 RD: nParms i4b->r8b
!             25-Jan-2008 RD: add RAO/RAP parameters
!             04-May-2009 RD: Scaling of loading models added
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             30-Oct-2009 SL: write parameter info only if numFil>0
!             04-Jan-2010 SL: HOI scaling parameters (type 27) added
!             30-Nov-2010 DT: Add Helmert parameters (type 28)
!             30-Nov-2010 MM: GNSS-specific parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_time
  USE d_par,    ONLY: maxParTyp,maxlcq
  USE p_addneq, ONLY: opt,prtParlst

  USE s_timst2
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                :: iact     ! What to do:
                                          !    0: init
                                          !    1: count
                                          !    2: print summary
  INTEGER(i4b)                :: ityp     ! 1: est..estimated (explicitly)
                                          ! 2: ebs..pre-elim. before stack.
                                          ! 3: eas..pre-elim. after stack.
                                          ! 4: del..deleted
                                          ! 5: sbs..singular before stack.
                                          ! 6: sbs..singular before stack.
                                          !         after pre-elimination
                                          ! 7: sas..singular after stack.
                                          !         after pre-elimination
                                          ! 8: sas..singular after stack.
                                          !         after neqsolve
  INTEGER(i4b)                :: numFil   ! File number involved
                                          !    0: all files (after stacking)
  CHARACTER(LEN=20)           :: name     ! Parameter name
  INTEGER(i4b), DIMENSION(maxlcq) :: locq ! Parameter locq
  TYPE(t_time)                :: time     ! Time window for parameter
  REAL(r8b), OPTIONAL         :: nparms   ! Number of parameters (for iact==2)

! Local Types
! -----------
  TYPE t_parlst
    INTEGER(i4b)       :: est
    INTEGER(i4b)       :: ebs
    INTEGER(i4b)       :: eas
    INTEGER(i4b)       :: del
    INTEGER(i4b)       :: sng
  END TYPE t_parlst

  TYPE(t_parlst), DIMENSION(maxParTyp), SAVE  :: parlst

! Local Parameter
! ---------------
  CHARACTER(LEN=3), DIMENSION(6),         PARAMETER :: typStrg =      &
    (/'EST','EBS','EAS','DEL','SBS','SAS'/)

  CHARACTER(LEN=3), DIMENSION(maxParTyp), PARAMETER :: parStrgShort = &
    (/'CRD','RCL','ORB','   ','RAO','TRP','   ','DCB',  &
      '   ','ERP','SOP','SAO','   ','   ','   ','GCC',  &
      '   ','RAP','GIM','   ','   ','GRD','RCK','SCK',  &
      'SAP','RGB','HOI','HLM','   ','GSP'/)

  CHARACTER(LEN=37),DIMENSION(maxParTyp), PARAMETER :: parStrgLong =  &
     (/' Station coordinates / velocities    ', &  !  1
       ' Receiver clocks / Time biases       ', &  !  2
       ' Orbital elements                    ', &  !  3
       '                                     ', &  !  4
       ' Receiver antenna offset parameters  ', &  !  5
       ' Site-specific troposphere parameters', &  !  6
       '                                     ', &  !  7
       ' Differential code bias parameters   ', &  !  8
       '                                     ', &  !  9
       ' Earth rotation parameters           ', &  ! 10
       ' Stochastic orbit parameters         ', &  ! 11
       ' Satellite antenna offset parameters ', &  ! 12
       ' Earth potential parameters          ', &  ! 13
       '                                     ', &  ! 14
       '                                     ', &  ! 15
       ' Geocenter coordinates               ', &  ! 16
       '                                     ', &  ! 17
       ' Receiver antenna pattern            ', &  ! 18
       ' Global ionosphere model parameters  ', &  ! 19
       '                                     ', &  ! 20
       '                                     ', &  ! 21
       ' Scaling fact. for Vienna grid files ', &  ! 22
       ' Epochwise receiver clocks           ', &  ! 23
       ' Epochwise satellite clocks          ', &  ! 24
       ' Satellite antenna phase patterns    ', &  ! 25
       ' Range biases (from SLR)             ', &  ! 26
       ' Higher-order iono scaling parameters', &  ! 27
       ' Helmert transformation parameters   ', &  ! 28
       ' Not used                            ', &  ! 29
       ' GNSS-specific parameters            ' /)  ! 30


! Local Variables
! ---------------
  INTEGER(i4b)            :: iParTyp
  INTEGER(i4b)            :: parAdj
  INTEGER(i4b)            :: sumEst
  INTEGER(i4b)            :: parPre
  INTEGER(i4b)            :: sumDel
  INTEGER(i4b)            :: sumSng
  INTEGER(i4b)            :: testsum
  INTEGER(i4b)            :: iStrg

  REAL(r8b), DIMENSION(2) :: timMjd
  REAL(r8b)               :: meanMjd
  REAL(r8b)               :: sumAdj
  REAL(r8b)               :: sumPre
  REAL(r8b)               :: sumAmb

  CHARACTER(LEN=40)       :: timStr
  CHARACTER(LEN=17)       :: preStrg

  LOGICAL, SAVE           :: prtitle


! ACTION 0: init
! --------------
  IF (iact == 0) THEN

    DO iParTyp = 1, maxParTyp
      parlst(iParTyp)%est = 0
      parlst(iParTyp)%ebs = 0
      parlst(iParTyp)%eas = 0
      parlst(iParTyp)%del = 0
      parlst(iParTyp)%sng = 0
    ENDDO

    prtitle = .TRUE.

  ENDIF

! ACTION 1: count and print parameter list (if required)
! ----------------------------------------
  IF (iact == 1) THEN

! Count: update parameter summary
    iParTyp = locq(1)
    IF (ityp == 1) parlst(iParTyp)%est = parlst(iParTyp)%est + 1
    IF (ityp == 2) parlst(iParTyp)%ebs = parlst(iParTyp)%ebs + 1
    IF (ityp == 3) THEN
                   parlst(iParTyp)%eas = parlst(iParTyp)%eas + 1
                   parlst(iParTyp)%est = parlst(iParTyp)%est - 1
    ENDIF
    IF (ityp == 4) parlst(iParTyp)%del = parlst(iParTyp)%del + 1
    IF (ityp == 5) THEN
                   parlst(iParTyp)%sng = parlst(iParTyp)%sng + 1
    ENDIF
    IF (ityp == 6) THEN
                   parlst(iParTyp)%sng = parlst(iParTyp)%sng + 1
                   parlst(iParTyp)%ebs = parlst(iParTyp)%ebs - 1
    ENDIF
    IF (ityp == 7) THEN
                   parlst(iParTyp)%sng = parlst(iParTyp)%sng + 1
                   parlst(iParTyp)%eas = parlst(iParTyp)%eas - 1
    ENDIF
    IF (ityp == 8) parlst(iParTyp)%sng = parlst(iParTyp)%sng + 1

    iStrg = ityp
    IF (ityp == 6) iStrg = 5
    IF (ityp == 7) iStrg = 6
    IF (ityp == 8) iStrg = 6

! Write parameter information into error message file, if required
! (ityp,numfil,parameter,name,locq,time)
    IF (opt%prt(prtParlst) == 1 .AND. numFil > 0) THEN

! Get time information for parameter (ityp /= 1)
      timStr = ' '
      IF (ityp /= 1) THEN
        meanMjd   = time%mean
        timMjd(1) = meanMjd - time%half
        timMjd(2) = meanMjd + time%half
        IF (time%half == 0d0) timMjd(2) = 1d20
        CALL timst2(1, 2, timMjd, timStr)
      ENDIF

! Print title line
      IF (prtitle) THEN
        WRITE(lfnprt,'(/,2(A,/),/,A,/,1X,131("-"))')                     &
          ' PARAMETER LIST',                                             &
          ' --------------',                                             &
          ' Action  File  Typ   Name                    Locq         '// &
          '                           From                 To'
        prtitle = .FALSE.
      ENDIF

! Print parameter list
      WRITE(lfnprt,'(1X,A,A3,I5,2X,A3,3X,A20,7(I6),2X,A40)') &
        '#PR_',typStrg(iStrg),numfil,parStrgShort(iParTyp),  &
        name,locq(1:7),timStr

    ENDIF

  ENDIF


! ACTION 2: print parameter summary
! ---------------------------------
  IF (iact == 2) THEN

    IF (.NOT. PRESENT(nparms)) THEN
      WRITE(lfnerr,'(/,A,/,18X,A,/)')                                &
            ' *** SR PRPARLST: For iact == 2 the optional argument', &
                              'nparms is required.'
      CALL exitrc(2)
    ENDIF

    sumAdj = 0d0
    sumEst = 0
    sumPre = 0d0
    sumDel = 0
    sumSng = 0

! Print title line
    IF (opt%prt(prtParlst) == 1) WRITE(lfnprt,'(/)')
    WRITE(lfnprt,'(///,2(A,/),/,2(A,/),/,A,/,1X,131("-"))')              &
      ' SUMMARY OF RESULTS',                                             &
      ' ------------------',                                             &
      ' Number of parameters:',                                          &
      ' --------------------',                                           &
      ' Parameter type                               Adjusted  '   //    &
      ' explicitly / implicitly (pre-eliminated)        Deleted  ' //    &
      '   Singular'

! Loop over all parameter types
    DO iParTyp = 1, maxParTyp

      testsum = parlst(iParTyp)%est &
              + parlst(iParTyp)%ebs &
              + parlst(iParTyp)%eas &
              + parlst(iParTyp)%del &
              + parlst(iParTyp)%sng

! Print parameter summary
      IF (testsum > 0) THEN

! Number of adjusted parameters: parAdj
        parAdj = parlst(iParTyp)%est &
               + parlst(iParTyp)%ebs &
               + parlst(iParTyp)%eas

! Number of pre-eliminiated parameters: parPre
        IF (parlst(iParTyp)%ebs > 0 .AND. parlst(iParTyp)%eas == 0) THEN
          parPre  = parlst(iParTyp)%ebs
          preStrg = '(before stacking)'
        ELSEIF (parlst(iParTyp)%eas > 0 .AND. parlst(iParTyp)%ebs == 0) THEN
          parPre  = parlst(iParTyp)%eas
          preStrg = '(after stacking) '
        ELSE
          parPre  = parlst(iParTyp)%ebs + parlst(iParTyp)%eas
          preStrg = '                 '
        ENDIF

! Print table
        WRITE(lfnprt,'(A,3X,3(I13),2X,A17,2(I13))') &
          parStrgLong(iParTyp), parAdj, &
          parlst(iParTyp)%est,          &
          parPre, preStrg,              &
          parlst(iParTyp)%del,          &
          parlst(iParTyp)%sng

! Count total number per parameter type
        sumAdj = sumAdj + parAdj
        sumEst = sumEst + parlst(iParTyp)%est
        sumPre = sumPre + DBLE(parPre)
        sumDel = sumDel + parlst(iParTyp)%del
        sumSng = sumSng + parlst(iParTyp)%sng
      ENDIF

    ENDDO ! end loop over all parameter types

! Number of remaining implicite parameters (pre-elimineted in previous runs)
    sumAmb = nparms - DBLE(sumEst) - sumPre

    sumAdj = sumAdj + sumAmb ! = neq%misc%nparms
    sumPre = sumPre + sumAmb ! = neq%misc%nparms - sumEst

    IF (sumAdj < 1000000000) THEN
      IF (sumAmb > 0) &
        WRITE(lfnprt,'(1X,131("-"),/,A,3X,I13,13X,I13)') &
          ' Previously pre-eliminated parameters',NINT(sumAmb),NINT(sumAmb)
      WRITE(lfnprt,'(1X,131("-"),/,A,27X,3(I13),19X,2(I13),//)') &
      ' Total number',NINT(sumAdj),sumEst,NINT(sumPre),SumDel,sumSng
    ELSE
      IF (sumAmb > 0) &
        WRITE(lfnprt,'(1X,131("-"),/,A,3X,ES13.4,13X,ES13.4)') &
          ' Previously pre-eliminated parameters',sumAmb,sumAmb
      WRITE(lfnprt,'(1X,131("-"),/,A,27X,ES13.4,I13,ES13.4,19X,2(I13),//)') &
      ' Total number',sumAdj,sumEst,sumPre,SumDel,sumSng
    ENDIF


  ENDIF

  RETURN
END SUBROUTINE prparlst

END MODULE
