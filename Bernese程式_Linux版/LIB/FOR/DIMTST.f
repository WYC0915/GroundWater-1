      MODULE s_DIMTST
      CONTAINS

C*
      SUBROUTINE DIMTST(ISTOP ,ITYPE ,IMOD  ,MODNAM,VARNAM,VARTXT,
     1                  ADDTXT,NUMDIM,MAXDIM,IRCODE)
CC
CC NAME       :  DIMTST
CC
CC PURPOSE    :  CHECK MAXIMUM DIMENSIONS
CC
CC PARAMETERS :
CC         IN :  ISTOP  : STOP IF MAXIMUM DIMENSION EXCEEDED I*4
CC                        =0: NO
CC                        =1: YES
CC               ITYPE  : INTERPRETATION OF "NUMDIM"         I*4
CC                        =1: ACTUAL DIMENSION
CC                        =2: CURRENT COUNTER - MIGHT BE
CC                            SMALLER THAN ACTUAL DIMENSION
CC                            NEEDED
CC               IMOD   : MODULE TYPE                        I*4
CC                        =1: PROGRAM (PG)
CC                        =2: SUBROUTINE/FUNCTION (SR)
CC               MODNAM : MODULE NAME                        CH*(*)
CC               VARNAM : VARIABLE NAME (E.G. "MAXMOD")      CH*(*)
CC               VARTXT : TEXT (E.G. "IONOSPHERE MODELS")    CH*(*)
CC               ADDTXT : ADDITIONAL TEXT LINE TO BE ADDED   CH*(*)
CC                        TO THE ERROR MESSAGE
CC                        =' ': NO ADDITIONAL TEXT LINE
CC               NUMDIM : ACTUAL DIMENSION (SEE ALSO         I*4
CC                        "ITYPE")
CC               MAXDIM : MAXIMUM DIMENSION                  I*4
CC        OUT :  IRCODE : RETURN CODE                        I*4
CC                       =0: OK
CC                       =1: DIMENSION EXCEEDED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER, M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  31-DEC-97
CC
CC CHANGES    :  01-JAN-98 : SS: WRITE ERROR MESSAGE IN ANY CASE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICH   , IMOD  , IRCODE, ISTOP , ITYPE , MAXDIM,
     1          NUMDIM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*(*) MODNAM,VARNAM,VARTXT,ADDTXT
C
      CHARACTER*80  LINE
      CHARACTER*2   MODTYP(2)
C
C
      DATA MODTYP/'PG','SR'/
C
C CHECK ACTUAL DIMENSION AGAINST MAXIMUM DIMENSION
C ------------------------------------------------
      IF (NUMDIM.GT.MAXDIM) THEN
        LINE=' *** SR DIMTST: MAXIMUM DIMENSION "'//
     1    VARNAM(1:LENGT1(VARNAM))//'" EXCEEDED IN '//
     2    MODTYP(IMOD)//' '//MODNAM(1:LENGT1(MODNAM))
        IF (ISTOP.EQ.0) LINE(2:4)='###'
        WRITE(LFNERR,901) LINE(1:LENGT1(LINE)),
     1    VARTXT(1:LENGT1(VARTXT))
901     FORMAT(/,A,/,16X,'TOO MANY ',A)
C
        IF (ITYPE.EQ.1) THEN
          WRITE(LFNERR,902) VARTXT(1:LENGT1(VARTXT)),NUMDIM
902       FORMAT(16X,'ACTUAL NUMBER OF ',A,' :',I7)
C
          ICH=LENGT1(VARTXT)+19
        ELSE
          ICH=23
        ENDIF
C
        LINE='MAXIMUM NUMBER ALLOWED'
        LINE(ICH:ICH)=':'
        WRITE(LFNERR,903) LINE(1:LENGT1(LINE)),MAXDIM
903     FORMAT(16X,A,I7)
C
        WRITE(LFNERR,904) VARNAM(1:LENGT1(VARNAM)),
     1    VARTXT(1:LENGT1(VARTXT))
904     FORMAT(16X,'INCREASE MAXIMUM DIMENSION "',A,'" OR',/,
     1    16X,'REDUCE NUMBER OF ',A)
C
        IF (ADDTXT.NE.' ') THEN
          WRITE(LFNERR,905) ADDTXT(1:LENGT1(ADDTXT))
905       FORMAT(16X,A)
        ENDIF
C
        WRITE(LFNERR,*)
C
        IF (ISTOP.EQ.1) CALL EXITRC(2)
C
        IRCODE=1
      ELSE
        IRCODE=0
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
