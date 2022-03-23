  MODULE f_listi4
  CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

   FUNCTION LISTI4(ityp,maxlst,list,item,nlist)

!-------------------------------------------------------------------------
!
! PURPOSE    :  Search item in list, update list if desired
!
! PARAMETERS :
!         IN :  ityp   : 0: Do not update list               I*4
!                        1: Update list if item not found
!               maxlst : Maximum number of items allowed     I*4
!               list   : List to be searched                 I*4(*)
!               item   : Item to be searched for             I*4
!     IN/OUT :  nlist  : Number of items in list             I*4
!        OUT :  listi4 : Position of item in list            I*4
!                        0: item not found          (ityp=0)
!                           max. dimension exceeded (ityp=1)
!
! SR CALLED  :
!
! REMARKS    :
!
! AUTHOR     :  W. GURTNER, U. HUGENTOBLER
!
! VERSION    :  3.0
!
! CREATED    :  27-AUG-88
!
! CHANGES    :  07-MAY-2000 HU: Translated to F90
!               05-Mar-2012 RD: Use listi4 as module now
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      1988      UNIVERSITY OF BERN
!                    SWITZERLAND
!
!-----------------------------------------------------------------------

  USE M_BERN, ONLY: i4b

  USE s_maxtst
  USE s_exitrc

  IMPLICIT NONE

! Dummy list
! ----------

  INTEGER(i4b)                     :: ityp
  INTEGER(i4b)                     :: maxlst
  INTEGER(i4b),DIMENSION(maxlst)   :: list
  INTEGER(i4b)                     :: item
  INTEGER(i4b)                     :: nlist
  INTEGER(i4b)                     :: listi4

!
! Local variables
! ---------------
  INTEGER(i4b)                     :: ilist
  INTEGER(i4b)                     :: ifound
  INTEGER(i4b)                     :: nlist1
  INTEGER(i4b)                     :: irc1,irc2
!
! Check dimensions
  CALL maxtst(1,'LISTI4','NLIST ',SIZE(list),nList, irc1)
  CALL maxtst(1,'LISTI4','MAXLST',SIZE(list),maxLst,irc2)
!  IF (irc1+irc2 /= 0) CALL exitrc(2)

!
! Look for item
  ifound = 0
  DO ilist = 1,nlist

! found:
    IF (list(ilist) == item) THEN
      ifound = ilist
      EXIT
    END IF
  END DO

! not found: update list
  IF(ifound == 0) THEN
    IF(ityp.EQ.1) THEN
      nlist1=nlist+1
      IF(nlist1 > maxlst) THEN
        ilist=0
      ELSE
        nlist=nlist1
        list(nlist)=item
        ilist=nlist
      END IF

! not found: no update
    ELSE
      ilist=0
    END IF
    listi4=ilist
  ELSE
    listi4=ifound
  END IF


  RETURN
  END FUNCTION LISTI4

  END MODULE
