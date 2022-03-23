      MODULE s_NXTOBS
      CONTAINS

C*
      SUBROUTINE NXTOBS(NVAL,IDIR,NACT,NEND,ARG,OBSERV,OBSFLG,
     1                  ARGLOC,OBSLOC,INDLOC,IRC)
CC
CC NAME       :  NXTOBS
CC
CC PURPOSE    :  IN AN ARRAY "OBSERV" FIND
CC               NVAL VALUES WHICH
CC                  (A) ARE NOT EQUAL TO 0.D0,
CC                  (B) ARE NOT FLAGGED (ARRAY OBSFLG)
CC                  (C) HAVE INDICES GE NACT FOR IDIR=1
CC                           INDICES LE NACT FOR IDIR=-1
CC               THE VALUES ARE STORED IN ARRAY OBSLOC, THE CORRES-
CC               PONDING ARGUMENTS IN ARGLOC. THE INDICES OF THESE
CC               ELEMENTS ARE STORED IN INDLOC. THE ELEMENTS ARE
CC               ORDERED IN FORWARD DIRECTION FOR IDIR=1
CC               AND     IN BACKWARD DIRECTION FOR IDIR=-1 (THE FIRST
CC               ELEMENT HAVING THE HIGHEST INDEX).
CC
CC PARAMETERS :
CC         IN :  NVAL   : NUMBER OF ELEMENTS SELECTED         I*4
CC               IDIR   : DIRECTION OF SEARCH                 I*4
CC                        = 1: GO FORWARD STARTING AT "NACT"
CC                        =-1: GO BACKWARD STARTING AT "NACT"
CC               NACT   : STARTING INDEX                      I*4
CC               NEND   : LAST ELEMENT TO BE CHECK            I*4
CC               ARG(I),I=1,2,..,NEND: ARGUMENT ARRAY         R*8
CC               OBSERV(I),I=1,2,..,NEND: FUNCTION VALUES     R*8
CC               OBSFLG(I),I=1,2,..,NEND: FLAGS               R*8
CC        OUT :  ARGLOC(I),I=1,2,..,NVAL: SELECTED ARGUMENTS  R*8
CC               OBSLOC(I),I=1,2,..,NVAL: SELECTED VALUES     R*8
CC               INDLOC(I),I=1,2,..,NVAL:                     I*4
CC                        INDLOC(I): INDEX OF OBSLOC(I) IN ARRAY
CC                        OBSERV
CC               IRC    : RETURN CODE                         I*4
CC                        = 0 : O.K.
CC                        =-1 : NO GOOD ELEMENTS FOUND
CC                        ELSE: NUMBER OF GOOD ELEMENTS FOUND
CC                              ( < NVAL !)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/02/29 15:30
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_tstflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDIR, IND , IRC , K   , NACT, NEND, NVAL
C
      REAL*8    ARG0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      REAL*8      ARG(*),OBSERV(*),ARGLOC(*),OBSLOC(*)
      INTEGER*4   INDLOC(*)
      CHARACTER*1 OBSFLG(*)
C
      IND=0
      IRC=0
      DO 100 K=NACT,NEND,IDIR
        IF(OBSERV(K).NE.0.D0.AND.(.NOT.TSTFLG(OBSFLG(K),0)))THEN
          IND=IND+1
          OBSLOC(IND)=OBSERV(K)
          IF(IND.EQ.1)ARG0=ARG(K)
          ARGLOC(IND)=(ARG(K)-ARG0)*1440.D0
          INDLOC(IND)=K
        END IF
        IF(IND.EQ.NVAL)GO TO 999
100   CONTINUE
      IF(IND.EQ.0)THEN
        IRC=-1
      ELSE
        IRC=IND
      END IF
999   RETURN
      END SUBROUTINE

      END MODULE
