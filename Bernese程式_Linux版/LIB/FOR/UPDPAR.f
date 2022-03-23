      MODULE s_UPDPAR
      CONTAINS

C*
      SUBROUTINE UPDPAR(IUPTYP,NPAR,LOCQ,IDEL,PARFLG,NPARMS,PARLST)
CC
CC NAME       :  UPDPAR
CC
CC PURPOSE    :  UPDATE ARRAY CONTAINING NUMBER OF PARAMETERS
CC               WITH THE INFORMATION COMING FROM SR SYMING
CC               ABOUT SINGULAR PARAMETERS AND "NPARMS"
CC
CC PARAMETERS :
CC         IN :  IUPTYP : TYPE OF UPDATE PROCEDURE            I*4
CC                        =0 : WITHOUT ARRAY "IDEL" TO
CC                             IDENTIFY LOCQ-ELEMENTS
CC                        =1 : WITH ARRAY "IDEL"
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(I,K),K=1,...,NPAR,I=1,..,MAXLCQ:        I*4
CC                        DEFINITION OF EACH PARAMETER
CC               IDEL(I),I=1,..,NPAR: ARRAY INDICATING ARRAY  I*4
CC                        ELEMENTS ACTUALLY USED IN INVERSION
CC                        =0 : NOT USED
CC                        =1 : USED IN INVERSION
CC               PARFLG(K),K=1,..NPAR: FLAG FOR SINGULAR PAR. I*4
CC                        =0 : PARAMETER NOT SINGULAR
CC                        =1 : PARAMETER SINGULAR
CC     IN/OUT :  NPARMS : NUMBER OF PARAMETERS TO COMPUTE RMS I*4
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.5  (JAN 93)
CC
CC CREATED    :  27-JUL-94
CC
CC CHANGES    :  05-AUG-94 : MR: UPDATE "NPARMS" FOR SINGULAR PARAM.
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IPAR  , ITYPE , IUPTYP, JPAR  , MXCLCQ, NPAR  , NPARMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*6 MXNLCQ
      INTEGER*4   LOCQ(MXCLCQ,*),PARFLG(*),PARLST(5,*),IDEL(*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C ADD SINGULAR PARAMETERS TO PARLST(5,*)
C --------------------------------------
      JPAR=0
      DO 100 IPAR=1,NPAR
        IF (IUPTYP.EQ.0 .OR. (IUPTYP.EQ.1 .AND. IDEL(IPAR).EQ.1)) THEN
          JPAR=JPAR+1
          IF (PARFLG(JPAR).EQ.1) THEN
            ITYPE=LOCQ(1,IPAR)
            PARLST(5,ITYPE)=PARLST(5,ITYPE)+1
            NPARMS=NPARMS-1
          ENDIF
        ENDIF
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
