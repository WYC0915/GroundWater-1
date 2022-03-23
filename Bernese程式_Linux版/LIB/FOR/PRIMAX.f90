MODULE s_PRIMAX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE primax(priopt,imxloc,nmxloc,maxloc,imxfil,nmxfil,maxfil,  &
                  imxsta,nmxsta,maxsta,imxsat,nmxsat,maxsat,         &
                  imxamb,nmxamb,maxamb,imxpar,nmxpar,maxpar,         &
                  imxfls,nmxfls,maxfls,imxsas,nmxsas,maxsas,         &
                  imxamp,nmxamp,maxamp,imxsng,nmxsng,maxsng)

! -------------------------------------------------------------------------
! Purpose:    Print the array dimensions for GPSEST (on request)
!
! Author:     R. Dach
!
! Created:    09-Nov-2004
! Last mod.:  29-Jan-2008
!
! Changes:    28-Jul-2005 HU: priopt 14-17 added, primax=18
!             29-Jan-2008 SL: modified priopt list
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b),     &
      DIMENSION(*) :: priopt ! print options (1: YES)
                             ! i= 1: number of observations
                             ! i= 2: pos. eccentr. and receiver info
                             ! i= 3: clock coefficients (code)
                             ! i= 4: ambiguities
                             ! i= 5: parameter charac. list
                             ! i= 6: constants, antenna offsets,
                             !       ionospheric coefficients
                             ! i= 7: satellite elevations
                             ! i= 8: synchronization errors
                             ! i= 9: number of observ. per file
                             ! i=10: ambiguities every iteration
                             ! i=11: observation statistics (elevation)
                             ! i=12: suppress print of epoch param. solutions
                             ! i=13: observation statistics (nadir angle)
                             ! i=14: print no troposphere parameters
                             ! i=15: print no coordinate parameters
                             ! i=16: print no ambiguity parameters
                             ! i=17: rms of coordinates+coordinate differences
                             ! i=18: slope distances
                             ! i=19: Print array dimensions
                             ! i=20: statistics on phase-connected epochs
  INTEGER(i4b)     :: imxloc ! User input for MAXLOC
  INTEGER(i4b)     :: nmxloc ! Adjusted value for MAXLOC
  INTEGER(i4b)     :: maxloc ! Maximum dimension  for MAXLOC
  INTEGER(i4b)     :: imxfil ! User input for MAXFIL
  INTEGER(i4b)     :: nmxfil ! Adjusted value for MAXFIL
  INTEGER(i4b)     :: maxfil ! Maximum dimension  for MAXFIL
  INTEGER(i4b)     :: imxsta ! User input for MAXSTA
  INTEGER(i4b)     :: nmxsta ! Adjusted value for MAXSTA
  INTEGER(i4b)     :: maxsta ! Maximum dimension  for MAXSTA
  INTEGER(i4b)     :: imxsat ! User input for MAXSAT
  INTEGER(i4b)     :: nmxsat ! Adjusted value for MAXSAT
  INTEGER(i4b)     :: maxsat ! Maximum dimension  for MAXSAT
  INTEGER(i4b)     :: imxamb ! User input for MAXAMB
  INTEGER(i4b)     :: nmxamb ! Adjusted value for MAXAMB
  INTEGER(i4b)     :: maxamb ! Maximum dimension  for MAXAMB
  INTEGER(i4b)     :: imxpar ! User input for MAXPAR
  INTEGER(i4b)     :: nmxpar ! Adjusted value for MAXPAR
  INTEGER(i4b)     :: maxpar ! Maximum dimension  for MAXPAR
  INTEGER(i4b)     :: imxfls ! User input for MAXFLS
  INTEGER(i4b)     :: nmxfls ! Adjusted value for MAXFLS
  INTEGER(i4b)     :: maxfls ! Maximum dimension  for MAXFLS
  INTEGER(i4b)     :: imxsas ! User input for MAXSAS
  INTEGER(i4b)     :: nmxsas ! Adjusted value for MAXSAS
  INTEGER(i4b)     :: maxsas ! Maximum dimension  for MAXSAS
  INTEGER(i4b)     :: imxamp ! User input for MAXAMP
  INTEGER(i4b)     :: nmxamp ! Adjusted value for MAXAMP
  INTEGER(i4b)     :: maxamp ! Maximum dimension  for MAXAMP
  INTEGER(i4b)     :: imxsng ! User input for MAXSNG
  INTEGER(i4b)     :: nmxsng ! Adjusted value for MAXSNG
  INTEGER(i4b)     :: maxsng ! Maximum dimension  for MAXSNG

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER :: srName = 'primax'

  CHARACTER(LEN=60), DIMENSION(10), PARAMETER :: descr =              &
   (/ 'MAXLOC: parameters to be processed                          ', &
      'MAXFIL: files to be processed                               ', &
      'MAXSTA: stations involved                                   ', &
      'MAXSAT: satellites involved                                 ', &
      'MAXAMB: ambiguities in an observation file                  ', &
      'MAXPAR: parameters simultaneously processed                 ', &
      'MAXFLS: files simultaneously processed                      ', &
      'MAXSAS: satellites simultaneously processed                 ', &
      'MAXAMP: ambiguities simultaneously processed                ', &
      'MAXSNG: non-zero elements in one line of first design matrix' /)

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)  :: line


! Nothing to do
! -------------
  IF (priopt(19) == 0) RETURN

! Print the title
! ---------------
  WRITE(lfnprt,'(/,2(A,/),3(A,/))')                                       &
  ' STATISTICS ON PARAMETER DIMENSION:',                                  &
  ' ---------------------------------',                                   &
  '                                                                  ' // &
  '   Dimensions of the parameters',                                      &
  ' Parameter, description                                        adj' // &
  'usted             default        user',                                &
  ' -----------------------------------------------------------------' // &
  '------------------------------------------------------------------'

! Print MAXLOC
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 1),      &
        nmxloc, maxloc, '-', DBLE(nmxloc)/DBLE(maxloc)*100d0,'%'
  IF (imxloc > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxloc,DBLE(nmxloc)/DBLE(imxloc)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXFIL
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 2),      &
        nmxfil, maxfil, '-', DBLE(nmxfil)/DBLE(maxfil)*100d0,'%'
  IF (imxfil > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxfil,DBLE(nmxfil)/DBLE(imxfil)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXSTA
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 3),      &
        nmxsta, maxsta, '-', DBLE(nmxsta)/DBLE(maxsta)*100d0,'%'
  IF (imxsta > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxsta,DBLE(nmxsta)/DBLE(imxsta)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXSAT
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 4),      &
        nmxsat, maxsat, '-', DBLE(nmxsat)/DBLE(maxsat)*100d0,'%'
  IF (imxsat > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxsat,DBLE(nmxsat)/DBLE(imxsat)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXAMB
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 5),      &
        nmxamb, maxamb, '-', DBLE(nmxamb)/DBLE(maxamb)*100d0,'%'
  IF (imxamb > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxamb,DBLE(nmxamb)/DBLE(imxamb)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXPAR
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 6),      &
        nmxpar, maxpar, '-', DBLE(nmxpar)/DBLE(maxpar)*100d0,'%'
  IF (imxpar > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxpar,DBLE(nmxpar)/DBLE(imxpar)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXFLS
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 7),      &
        nmxfls, maxfls, '-', DBLE(nmxfls)/DBLE(maxfls)*100d0,'%'
  IF (imxfls > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxfls,DBLE(nmxfls)/DBLE(imxfls)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXSAS
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 8),      &
        nmxsas, maxsas, '-', DBLE(nmxsas)/DBLE(maxsas)*100d0,'%'
  IF (imxsas > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxsas,DBLE(nmxsas)/DBLE(imxsas)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXAMP
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr( 9),      &
        nmxamp,  maxamp, '-', DBLE(nmxamp)/DBLE(maxamp)*100d0,'%'
  IF (imxamp > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxamp,DBLE(nmxamp)/DBLE(imxamp)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

! Print MAXSNG
! ------------
  line = ''
  WRITE(line,'(A60,I10,10X,I10,A12,F11.1,A)') descr(10),      &
        nmxsng, maxsng, '-', DBLE(nmxsng)/DBLE(maxsng)*100d0,'%'
  IF (imxsng > 0) &
    WRITE(line(91:113),'(I12,F11.1)') imxsng,DBLE(nmxsng)/DBLE(imxsng)*100d0
  WRITE(lfnprt,'(1X,A)') TRIM(line)

  WRITE(lfnprt,'(1X)')

  RETURN
END SUBROUTINE primax

END MODULE
