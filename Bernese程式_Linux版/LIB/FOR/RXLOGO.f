      MODULE s_RXLOGO
      CONTAINS

C*
      SUBROUTINE RXLOGO(TITLE,PRGNAM,VDATE)
CC
CC NAME       :  RXLOGO
CC
CC PURPOSE    : DISPLAY LOGO FOR ALL PC-RINEX PROGRAMS
CC
CC PARAMETERS :
CC         IN :  TITLE  : TITLE TO BE DISPLAYED               CH*80
CC               PRGNAM : PROGRAM NAME                        CH*20
CC               VDATE  : VERSION DATE                        CH*9
CC
CC REMARKS    :
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :   8-JUN-89
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               20-JUL-10 : SL: BERNE -> BERN IN STRING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_lengt0
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , K1    , K2    , LSTR
C
CCC       IMPLICIT INTEGER*4 (I-N)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER TITLE*80,VDATE*9,PRGNAM*20
C
C LOCAL DECLARATIONS
      CHARACTER STRING(24)*80,LINE*80
C
      DO 10 I=1,24
        STRING(I)=' '
10    CONTINUE
C
      STRING( 5)='B E R N E S E   G P S   S O F T W A R E'
      STRING( 7)='---------------------------------------'
      STRING(11)='T r a n s f o r m a t i o n   o f'
      STRING(13)=TITLE
      STRING(15)='i n t o   R I N E X - F o r m a t'
      STRING(23)='Copyright (19'
      STRING(23)(14:)=VDATE(8:)
      STRING(23)(16:)='):  Astronomical Institute, University of Bern'
      WRITE(STRING(19),12) PRGNAM(1:LENGT1(PRGNAM)),
     1                     VDATE (1:LENGT1(VDATE))
12    FORMAT('Program: ',A,'  Date: ',A)
C
C  CLEAR SCREEN
      CALL CLRSCR()
C
      DO 20 I=1,24
        LSTR=LENGT0(STRING(I))
        IF(LSTR.EQ.0) THEN
          WRITE(*,*)
        ELSE
          LINE=' '
          K1=(80-LSTR)/2+1
          K2=K1+LSTR-1
          LINE(K1:)=STRING(I)
          WRITE(*,21) LINE(1:K2)
21        FORMAT(1X,A)
        END IF
20    CONTINUE
C
C  WAIT 2 SECONDS
      CALL WAIT(2.)
C
C  CLEAR SCREEN
      CALL CLRSCR()
C
      RETURN
      END SUBROUTINE

      END MODULE
