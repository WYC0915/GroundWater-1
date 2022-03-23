MODULE f_listc1
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION LISTC1(ITYP,ITMDIM,MAXLST,LIST,ITEM,NLIST)

! -------------------------------------------------------------------------
! PURPOSE    :  SEARCH ITEM IN LIST, UPDATE LIST IF DESIRED
!
! PARAMETERS :
!         IN :  ITYP   : 0: DO NOT UPDATE LIST               I*4
!                        1: UPDATE LIST IF ITEM NOT FOUND
!               ITMDIM : NUMBER OF CHARACTERS PER ITEM       I*4
!               MAXLST : MAXIMUM NUMBER OF ITEMS ALLOWED     I*4
!               LIST   : LIST TO BE SEARCHED               CHR*1(*)
!               ITEM   : ITEM TO BE SEARCHED FOR           CHR*1(ITMDIM)
!     IN/OUT :  NLIST  : NUMBER OF ITEMS IN LIST             I*4
!        OUT :  LISTC1 : POSITION OF ITEM IN LIST            I*4
!                        0: ITEM NOT FOUND          (ITYP=0)
!                           MAX. DIMENSION EXCEEDED (ITYP=1)
!
! AUTHOR     :  T. SCHILDKNECHT
!
! CREATED    :  26-MAY-89
!
! CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
!               27-MAR-12 : RD: USE LISTC1 AS MODULE NOW
!               17-Aug-2012 SL: Converted from f77 to f90
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      1988     UNIVERSITY OF BERN
!               SWITZERLAND
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  IMPLICIT NONE

! DECLARATIONS INSTEAD OF IMPLICIT
! --------------------------------
  INTEGER(i4b)                   :: iList
  INTEGER(i4b)                   :: itmDim
  INTEGER(i4b)                   :: iTyp
  INTEGER(i4b)                   :: listc1
  INTEGER(i4b)                   :: maxLst
  INTEGER(i4b)                   :: nList
  CHARACTER(LEN=*),DIMENSION(:)  :: list
  CHARACTER(LEN=*)               :: item

! LOOK FOR ITEM
  DO ILIST=1,NLIST
    IF(LIST(ILIST)(1:ITMDIM) == ITEM(1:ITMDIM)) THEN
      LISTC1 = ILIST
      RETURN
    ENDIF
  ENDDO

! NOT FOUND: UPDATE LIST
  IF(ITYP == 1) THEN
     IF(NLIST+1.GT.MAXLST) THEN
      LISTC1 = 0
    ELSE
      NLIST=NLIST+1
      LIST(NLIST) = ITEM
      LISTC1=NLIST
    END IF

! NOT FOUND: NO UPDATE
  ELSE
    LISTC1=0
  END IF

  RETURN
END FUNCTION

END MODULE
