MODULE f_chrcount
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION chrCount(chr, string, iFirst)

! -------------------------------------------------------------------------
!
! Purpose:    Counts number of characters 'chr' in string
!
!
! Author:     L. Mervart
!
! Created:    26-Nov-2000
!
! Changes:    27-Feb-2003 HU: Replace char by chr
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

  INTEGER(i4b)     :: chrCount

! List of Parameters
! ------------------
  CHARACTER(LEN=1) :: chr
  CHARACTER(LEN=*) :: string
  INTEGER(i4b)     :: iFirst ! index of the first appearance

! Local Variables
! ---------------
  INTEGER(i4b)     :: indOld
  INTEGER(i4b)     :: indNew

  iFirst   = 0
  indOld   = 0
  indNew   = 0
  chrCount = 0

  DO
    indNew = INDEX(string(indOld+1:), chr)
    IF (indNew == 0) EXIT
    IF (iFirst == 0) iFirst = indNew
    chrCount = chrCount + 1
    indOld   = indOld + indNew
  END DO


END FUNCTION chrCount

END MODULE
