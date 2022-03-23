      MODULE s_FPARSE
      CONTAINS

C*
      SUBROUTINE FPARSE(ISEL,FILSPC,NODE,DEVICE,DIR,NAME,EXT,
     1                  VER,IRETC)
CC
CC NAME       :  FPARSE
CC
CC PURPOSE    :  PARSE A FILE SPECIFICATION TO GET THE NODE, DEVICE,
CC               FILE NAME AND THE EXTENSION.
CC
CC PARAMETERS :
CC         IN :  ISEL     : =1: DELIMITERS INCLUDED IN OUTPUT    I*4
CC                          =0 ELSE
CC               FILSPC   : FILE SPECIFICATION                 CHR*(*)
CC        OUT :  NODE     : NODE NAME                          CHR*(*)
CC               DEVICE   : DEVICE NAME                        CHR*(*)
CC               DIR      : DIRECTORY                          CHR*(*)
CC               NAME     : FILE NAME                          CHR*(*)
CC               EXT      : EXTENSION                          CHR*(*)
CC               VER      : VERSION NUMBER                     CHR*(*)
CC               IRC      : RETURN CODE                          I*4
CC                          =0: OK
CC                          =1: FILE SPEC. BLANK
CC                          =2: NO FILE NAME FOUND
CC
CC REMARKS    :  NO WARNING IF THE OUTPUT PARAMETERS CAN NOT FULLY HOLD
CC               THE ITEMS OF THE FILE SPECIFICATION. (E.G.
CC               "FILSPC"='DUMMY.DATA' AND "EXT" DECLARED AS CHR*3 ).
CC               THERE IS NO FULL FILE SPEC VALIDITY TEST PERFORMED !
CC
CC AUTHOR     :  T. SCHILDKNECHT
CC
CC VERSION    :  3.5  (AUG 94)
CC
CC CREATED    :  26-APR-91
CC
CC CHANGES    :  10-AUG-94 : MR: ADD UNIX SPECIFIC DELIMITERS
CC               10-APR-96 : MR: CORRECT LAHEY DELIMITERS
CC               17-FEB-03 : LM: USE PREPROCESSOR COMMANDS
CC               18-FEB-03 : LM: USE BACKSLASH FROM M_BERN
CC               08-MAR-03 : HU: IMPORT BACKSLASH FOR OS_WIN32 ONLY
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-JUL-11 : SS/SL: BUGFIX CONCERNING DIR IF ISEL=0
CC               29-OCT-12 : RD: REMOVE #ifdef OS_VMS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1991     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
#ifdef OS_WIN32
      USE M_BERN, ONLY: BACKSLASH
#endif
      USE f_lengt0
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDI   , IDV   , IEX   , INM   , INO   , IRETC ,
     1          ISEL  , ITEST , K     , LEXTL , LNODL , LSPC  ,
     2          MAXDEL
C
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER  (MAXDEL=5)
      INTEGER*4     LIMPOS(MAXDEL)
      CHARACTER*(*) FILSPC,NODE,DEVICE,DIR,NAME,EXT,VER
      CHARACTER*2   NODLIM
      CHARACTER*1   DEVLIM,DIRLIM,NAMLIM,EXTLIM(2)
C
      EQUIVALENCE   (LIMPOS(1),INO),(LIMPOS(2),IDV),(LIMPOS(3),IDI)
      EQUIVALENCE   (LIMPOS(4),INM),(LIMPOS(5),IEX)
C
C  DOS DEVICE -, DIRECTORY - AND EXTENSION DELIMITER
C
#ifdef OS_WIN32
      NODLIM='XX'
      DEVLIM=':'
      DIRLIM=BACKSLASH
      NAMLIM='.'
      EXTLIM(1)='X'
      EXTLIM(2)='X'
      LNODL=0
      LEXTL=0
#endif

C
C  UNIX DEVICE -, DIRECTORY - AND EXTENSION DELIMITER
C
#ifdef OS_UNIX
      NODLIM='XX'
      DEVLIM=':'
      DIRLIM='/'
      NAMLIM='.'
      EXTLIM(1)='X'
      EXTLIM(2)='X'
      LNODL=0
      LEXTL=0
#endif

C
      NODE  =' '
      DEVICE=' '
      DIR   =' '
      NAME  =' '
      EXT   =' '
      VER   =' '
C
      LSPC=LENGT0(FILSPC)
C  NULL FILE SPEC.
      IF(LSPC.EQ.0) GOTO 910
C
C  JUSTIFY FILE SPEC TO THE LEFT
      DO 10 I=1,LSPC
        IF(FILSPC(I:I).NE.' ') GOTO 20
10    CONTINUE
20    CONTINUE
      FILSPC(1:)=FILSPC(I:LSPC)
      LSPC=LSPC-I+1
C
C  SEARCH ALL DELIMITERS
C  ---------------------
      INO=0
      IDV=0
      IDI=0
      INM=0
      IEX=0
      K=LSPC
      DO 40 I=1,10000
C  NODE
        IF((LNODL.NE.0)     .AND.
     1     (K-LNODL+1.GT.0) .AND.
     1     (FILSPC(K-LNODL+1:K).EQ.NODLIM) ) THEN
          INO=K
          K=K-LNODL+1
          GOTO 40
        ENDIF
C  DEVICE
        IF((IDV.EQ.0) .AND.
     1     (INO.EQ.0)    .AND.
     1     (FILSPC(K:K).EQ.DEVLIM) ) THEN
          IDV=K
          GOTO 39
        ENDIF
C  DIRECTORY
        IF((IDI.EQ.0) .AND.
     1     (FILSPC(K:K).EQ.DIRLIM) ) THEN
          IDI=K
          GOTO 39
        ENDIF
C  EXTENSION
        IF((IEX.EQ.0)   .AND.
     1     (IDI.EQ.0)   .AND.
     1     (LEXTL.EQ.1) .AND.
     3     ((FILSPC(K:K).EQ.EXTLIM(1)) .OR.
     4      (FILSPC(K:K).EQ.EXTLIM(2))     ) ) THEN
          IEX=K
          GOTO 39
        ENDIF
C  NAME
        IF((INM.EQ.0)    .AND.
     1     (IDI.EQ.0)    .AND.
     1     (FILSPC(K:K).EQ.NAMLIM) ) THEN
          INM=K
          GOTO 39
        ENDIF
C  NEXT
39      K=K-1
C  END
        IF(K.EQ.0) GOTO 45
40    CONTINUE
45    CONTINUE
C
C  CHECK IF DELIMITER POSITIONS PLAUSIBLE
C  --------------------------------------
C  (NAME AND VERSION CAN HAVE THE SAME DELIMITER IN VMS)
      IF(INM.EQ.0.AND.IEX.NE.0) THEN
        INM=IEX
        IEX=0
      ENDIF
      ITEST=-1
      DO 50 I=1,MAXDEL
        IF(LIMPOS(I).GT.ITEST) THEN
          ITEST=LIMPOS(I)
        ELSE
          IF(LIMPOS(I).NE.0) GOTO 920
        ENDIF
50    CONTINUE
C
C  SEARCH ALL ITEMS
C  ----------------
C
C  NODE
      IF(INO.GT.LNODL) THEN
        IF(ISEL.EQ.1) NODE=FILSPC(1:INO)
        IF(ISEL.EQ.0) NODE=FILSPC(1:INO-LNODL)
      ENDIF
C
C  DEVICE
      IF(IDV.NE.0) THEN
        IF(ISEL.EQ.1) DEVICE=FILSPC(INO+1:IDV)
        IF(ISEL.EQ.0.AND.INO+1.LE.IDV-1) DEVICE=FILSPC(INO+1:IDV-1)
      ELSE
        IDV=INO
      ENDIF
C
C  DIRECTORY
      IF(IDI.NE.0) THEN
        IF(ISEL.EQ.1) DIR=FILSPC(IDV+1:IDI)
        IF(ISEL.EQ.0.AND.IDV+1.LE.IDI-1) DIR=FILSPC(IDV+1:IDI-1)
      ELSE
        IDI=IDV
      ENDIF
C
C  FILE NAME
      IF(INM.NE.0) THEN
        IF(ISEL.EQ.1) NAME=FILSPC(IDI+1:INM)
        IF(ISEL.EQ.0.AND.IDI+1.LE.INM-1) NAME=FILSPC(IDI+1:INM-1)
      ELSE
        NAME=FILSPC(IDI+1:)
        GOTO 800
      ENDIF
C
C  EXTENSION
      IF(IEX.NE.0) THEN
        IF(ISEL.EQ.1) EXT=FILSPC(INM+1:IEX)
        IF(ISEL.EQ.0.AND.INM+1.LE.IEX-1) EXT=FILSPC(INM+1:IEX-1)
      ELSE
        EXT=FILSPC(INM+1:)
        GOTO 800
      ENDIF
C
C  VERSION
      IF(IEX+1.LE.LSPC) VER=FILSPC(IEX+1:)
C
C  NO FILE NAME
800   IF(ISEL.EQ.0.AND.NAME.EQ.' ') GOTO 920
      IF(ISEL.EQ.1.AND.NAME.EQ.NAMLIM) GOTO 920
C
      IRETC=0
      GOTO 999
C
C  RETURN CODES
C
910   IRETC=1
      GOTO 999
920   IRETC=2
      GOTO 999
C
999   RETURN
      END SUBROUTINE

      END MODULE
