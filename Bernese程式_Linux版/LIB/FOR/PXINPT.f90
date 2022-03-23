MODULE s_PXINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE pxinpt(maxcoe,titgen,ifiles,iuttyp,anchor, &
                    offset,uselst,irecdf,ipoext,ioutep)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine PXINPT.f that
!             reads the input options of the program POLXTR
!
! Author:     C. Urschl
!
! Created:    08-Nov-2000
!
! Changes:    17-Oct-2001  HU: Records for individual files, check routines
!             21-Dec-2001  HU: Use d_const
!             23-Apr-2003  AJ: Nullify local pointers
!             06-Nov-2003  HU: New options, print options
!             19-Jul-2006  HU: Read additional prediction options
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: date,time

  USE s_dimtst
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_ckoptl
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)       :: maxcoe   ! maximum number of polynomial coeff.
  CHARACTER(LEN=80)  :: titgen   ! general title for plot file
  INTEGER(i4b)       :: ifiles   ! Source of input files
                                 ! =1: From input file list
                                 ! =2: From file containing list of files
                                 !     incl. records
  INTEGER(i4b)       :: iuttyp   ! type of UT1-UTC values to be gene-
                                 ! rated for output file
                                 ! =1: The drifts from mid-day to mid-day
                                 !     are computed from records 1 and 5.
                                 !     UT1-UTC is continuous.
                                 ! =2: The resulting pole file contains
                                 !     the drift rates of the input files
                                 !     and is continous in UT1-UTC.
                                 ! =3: The daily UT1-UTC values are
                                 !     centered at the day-mean-values of
                                 !     the corresponding a priori pole file.
                                 !     Therefore the resulting pole file
                                 !     contains the drift rates of the
                                 !     input files and shows an UT1-UTC
                                 !     longterm-behaviour of the a priori
                                 !     pole. The output file is continuous
                                 !     neither in X nor Y nor UT1-UTC.
  INTEGER(i4b)        :: anchor  ! Anchor point to a priori file
                                 ! =1: first, =2: last
  INTEGER(i4b)        :: offset  ! =1: Compute offsets as average from
                                 !     Records 2 and 4
  INTEGER(i4b)        :: uselst  ! =1: Use records from list
  INTEGER(i4b), DIMENSION(5) :: irecdf  ! Default records to be extracted
                                        ! (1),(5) for UT1-UTC
                                        ! (2),(4) for daily drifts
                                        ! (3)     for pole coordinates
  INTEGER(i4b), DIMENSION(4) :: ipoext  ! ipoext(i)=1,4 : extrapolation options
                                        ! (1): consider a priori for prediction
                                        ! (2): number of days to be extrapolated
                                        ! (3): number of days for fit
                                        ! (4): polynomial degree for extrapolated
  INTEGER(i4b)        :: ioutep  ! number of epochs to be written to the
                                 ! output file per day (1 or 3)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=7)                                      :: keyWord
  CHARACTER(LEN=fileNameLength80)                       :: filnam
  CHARACTER(LEN=22)          :: srName='sr pxinpt (pgm polxtr)'
  INTEGER(i4b)               :: irCode, irc, irec, ipred

  CHARACTER(LEN=6),DIMENSION(3) :: chrtyp= (/'IERS  ','ORBGEN','NONE  '/)
  CHARACTER(LEN=5),DIMENSION(2) :: chranc=(/'FIRST','LAST '/)
  CHARACTER(LEN=3),DIMENSION(2) :: yesno =(/'NO ','YES'/)


  NULLIFY(keyValue)

! Read Options
! ------------
  irCode=0

! Title
  CALL readKeys ('TITLE', keyValue, irc)
  CALL ckoptl(0,'TITLE', keyValue, srName, 'Title', irc, irCode, &
              empty=' ',maxLength=65, maxVal=1, result1=titgen)
  titgen(66:75) = date
  titgen(76:80) = time

! Origin of input files
  CALL ckoptb(1, (/'RADIO_F','RADIO_L'/), srName,    &
              'Origin of input files', irCode, result1 = ifiles)

! Handling of UT1-UTC
  CALL readKeys ('UT1TYP', keyValue, irc)
  CALL ckoptc(1 ,'UT1TYP', keyValue, (/'IERS  ','ORBGEN','NONE  '/), srName, &
              'Handling of UT1-UTC', irc, irCode, maxVal=1, result1=iuttyp)

! Anchor point for UT1-UTC
  CALL readKeys ('ANCHOR', keyValue, irc)
  CALL ckoptc(1 ,'ANCHOR', keyValue, (/'FIRST','LAST '/), srName, &
              'Anchor for UT1-UTC', irc, irCode, maxVal=1, result1=anchor)

! Offsets as average of records 2 and 4
  CALL ckoptb(1, (/'OFFSET'/), srName,    &
              'Offset as average of records 2 and 4', irCode, result1 = offset)

! Read Record Numbers
  CALL readKeys ('USELIST', keyValue, irc)
  CALL ckoptb(1, (/'USELIST'/), srName, &
              'Use file specific records', irCode, result1=uselst)

  IF (ifiles==1.AND.uselst==0) THEN
    DO irec = 1,5
      WRITE(keyWord,'(A,I1)')'RECORD',irec
      CALL readKeys(keyWord, keyValue, irc)
      CALL ckopti(1,keyWord, keyValue, srName, 'Default record', &
                  irc, irCode, ge=1, maxVal=1, result1=irecdf(irec))
    ENDDO
  ENDIF

! Enable prediction
  CALL ckoptb(1, (/'PRED'/), srName,    &
              'Enable prediction', irCode, result1 = ipred)
  IF (ipred == 1) THEN

! Consider a priori as prediction
    CALL ckoptb(1, (/'PREDAPR'/), srName,    &
              'Consider a priori for prediction', irCode, result1 = ipoext(1))
    CALL gtflna(0,'POLE',filnam,irc)
    IF (irc /= 0) ipoext(1)=0

! Number of days for prediction
    CALL readKeys ('NPREDAY', keyValue, irc)
    CALL ckopti(1 ,'NPREDAY', keyValue, srName, &
             'Number of days for prediction', &
              irc, irCode, ge=0, maxVal=1, result1=ipoext(2))

! Number of days for fot
    CALL readKeys ('NFITDAY', keyValue, irc)
    CALL ckopti(1 ,'NFITDAY', keyValue, srName, &
             'Number of days for fit', &
              irc, irCode, ge=0, maxVal=1, result1=ipoext(3))

! Polynomial degree for prediction
    CALL readKeys ('NPREDEG', keyValue, irc)
    CALL ckopti(1 ,'NPREDEG', keyValue, srName,    &
             'Polynomial degree for prediction', &
              irc, irCode, ge=0, maxVal=1, result1=ipoext(4))
  ELSE
    ipoext=0
  ENDIF

! Number of epochs per day
  CALL readKeys ('OUTEPO', keyValue, irc)
  CALL ckoptc(1 ,'OUTEPO', keyValue, (/'1','3'/), srName, &
             'Number of epochs per day',                  &
              irc, irCode, valList=(/1,3/), maxVal=1, result1=ioutep)

! Check maximum Number of polynomial Coefficients
! -----------------------------------------------
  CALL dimtst(1,1,2,'PXINPT','MAXCOE','POLYNOMIAL COEFFICIENTS',' ', &
              ipoext(4), maxcoe, irc)
  IF (irc /= 0) irCode = irCode + 1

! Options not available for IERS
! ------------------------------
  IF (iuttyp == 1) THEN
    ioutep = 1
    anchor = 1
    offset = 0
  ENDIF

! Options not available for NONE
! ------------------------------
  IF (iuttyp == 3) THEN
    anchor = 1
    offset = 0
  ENDIF

  IF (irCode /= 0) THEN
    WRITE(lfnerr,"(/,'  *** SR PXINPT: Number of errors: ',I2)") irCode
    CALL exitrc(2)
  ENDIF

! Write options
! -------------
  WRITE(lfnprt,"(' OPTIONS', &
           &   /,' -------', &
           &  //,1X,79('-'), &
           &   /,' Handling of UT1-UTC             :    ',A, &
           &   /,' Anchor point to a priori        :    ',A, &
           &   /,' Offsets as average of rec 2,4   :    ',A, &
           &   /,' Number of output values/day     :',I5, &
           &   /,' Consider a priori for prediction:',I5, &
           &   /,' Preciction (days)               :',I5, &
           &   /,' Fit (days)                      :',I5, &
           &   /,' Polynomial degree for extrapol. :',I5, &
           &   /,1X,79('-'),/)") &
           chrtyp(iuttyp),chranc(anchor),yesno(offset+1),ioutep, &
           ipoext(1:4)

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE pxinpt

END MODULE
