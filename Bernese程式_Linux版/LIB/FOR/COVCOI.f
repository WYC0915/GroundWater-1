      MODULE s_COVCOI
      CONTAINS

C*
      SUBROUTINE COVCOI(FILCOV,NFIL,FILNAM,COVCOM,INDGRP,GRPNAM,
     1                  NHELM,IHELES,RHELES)
CC
CC NAME       :  COVCOI
CC
CC PURPOSE    :  READ COVARIANCE COMPONENTS
CC
CC PARAMETERS :
CC         IN :  FILCOV : FILENAME TO BE SAVED                CH*32
CC               NFIL : NUMBER OF NEQ FILES                   I*4
CC               FILNAM(I),I=1,NFIL FILENMAES                 CH*32
CC        OUT :  COVCOM(I): COVARIANCE COMPONENTS             I*4
CC               INDGRP(I),I=1,NFIL INDEX WHICH FILE BELONGS  I*4
CC                       TO WHICH GROUP
CC               GRPNAM(I),I=1,NFIL GROUP NAME                CH*3
CC               NHELM : =0/1 : HELMERT PARAMETER TO BE EST.  I*4
CC                              (NO/YES)
CC               IHELES(I),I=1,7: I=1,3: TRANSLATION,         I*4
CC                                I=4,6: ROTATION
CC                                I=7: SCALE (0:NO, 1:YES)
CC               RHELES(I),I=1,7: I=1,3: TRANSLATION,         R*4
CC                                I=4,6: ROTATION
CC                                I=7: SCALE (0:NO, 1:YES)
CC                                A PRIORI VALUES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E.BROCKMANN
CC
CC VERSION    :  3.6  (JAN 93)
CC
CC CREATED    :  11-SEP-95
CC
CC CHANGES    :  24-OCT-95 : EB: ALLOW HELMERT PARAMETERS
CC               09-MAR-04 : CU: BUG FIXED (PPM*1D-6), WARNING MESSAGE MODIF.
CC               19-MAY-05 : CU: ADD STRIPEXT, CHECK FILNAM WITHOUT DIR/EXT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               02-DEC-10 : DT: FORMAT FOR SCALE CHANGED F8.4 -> F9.5
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: ars
      USE s_opnfil
      USE s_stripext
      USE s_gtflna
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIL  , IHLP  , ILIN  , IOSTAT, IRC   , ITEST ,
     1          J     , JFIL  , K     , NFIL  , NGRP  , NHELM
C
      REAL*8    RHLP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*32 FILCOV,FILNAM(*),FILSCR,FILNEQ
      CHARACTER*3  GRPNAM(*),CH3
      REAL*8       COVCOM(*),RHELES(7,*),RH(7)
      INTEGER*4    INDGRP(*),IHELES(7,*),IH(7)
      LOGICAL      LFOUND
C
C INITIALISATION
C --------------
      NHELM=0
      DO I=1,NFIL
        COVCOM(I)=-1.D0
        INDGRP(I)=I
        GRPNAM(I)='   '
        DO J=1,7
          IHELES(J,I)=0
          RHELES(J,I)=0.D0
        ENDDO
        RHELES(7,I)=1.D0
      ENDDO
C
C DO NOT SAVE IF NO COORDINATE RESULT FILE NAME IN FILE NAME TABLE
C ----------------------------------------------------------------
      IF (FILCOV.EQ.' ') THEN
        CALL GTFLNA(0,'COVCOMI',FILCOV,IRC)
        IF(IRC.NE.0) RETURN
      ENDIF
      IF (FILCOV.EQ.' ') RETURN
C
      CALL OPNFIL(LFNLOC,FILCOV,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILCOV,'COVCOI')
C
C READ TITLE LINES OF COORDINATE INPUT FILE
C -----------------------------------------
      READ(LFNLOC,11)
11    FORMAT(/////)
C
C LOOP OVER ALL STATIONS OF INPUT FILE
C ------------------------------------
      DO 100 ILIN=1,1000000
        READ(LFNLOC,12,END=110) IHLP,RHLP,FILSCR,CH3,(IH(K),K=1,7),
     1                          (RH(K),K=1,7)
12      FORMAT(I3,2X,E20.14,2X,A32,2X,A3,1X,7(1X,I1),6(F9.4),F9.5)
        IF(IHLP.EQ.0.AND.RHLP.EQ.0.D0) GOTO 110
C
C CHECK, WHICH FILE IT IS
C FIRST PRIORITY: FILENAME
C SECOND PRIORITY: FILEGROUP
C THIRD  PRIORITY: FILENUMBER
C --------------------------
        LFOUND=.FALSE.
        DO IFIL=1,NFIL
          FILNEQ = FILNAM(IFIL)
          CALL STRIPEXT(FILNEQ)
          CALL STRIPEXT(FILSCR)
          IF (FILNEQ == FILSCR .AND. FILSCR /= '') THEN
             COVCOM(IFIL)=RHLP
             GRPNAM(IFIL)=CH3
             DO I=1,3
               IHELES(I,IFIL)=IH(I)
               RHELES(I,IFIL)=RH(I)
               IF (IH(I).NE.0) NHELM=1
             ENDDO
             DO I=4,6
               IHELES(I,IFIL)=IH(I)
               RHELES(I,IFIL)=RH(I)/ars/1000.D0
               IF (IH(I).NE.0) NHELM=1
             ENDDO
             IHELES(7,IFIL)=IH(I)
             RHELES(7,IFIL)=(RH(I)*1.D-6)+1.D0
             IF (IH(7).NE.0) NHELM=1
             LFOUND=.TRUE.
             GOTO 100
          ENDIF
        ENDDO
        IF (.NOT.LFOUND.AND.FILSCR.EQ.' '.AND.CH3.NE.'   ') THEN
          DO IFIL=1,NFIL
            FILNEQ = FILNAM(IFIL)
            CALL STRIPEXT(FILNEQ)
            ITEST=INDEX(FILNEQ,CH3)
            IF (ITEST.NE.0.AND.COVCOM(IFIL).EQ.-1.D0) THEN
               COVCOM(IFIL)=RHLP
               GRPNAM(IFIL)=CH3
               DO I=1,3
                 IHELES(I,IFIL)=IH(I)
                 RHELES(I,IFIL)=RH(I)
                 IF (IH(I).NE.0) NHELM=1
               ENDDO
               DO I=4,6
                 IHELES(I,IFIL)=IH(I)
                 RHELES(I,IFIL)=RH(I)/ars/1000.D0
                 IF (IH(I).NE.0) NHELM=1
               ENDDO
               IHELES(7,IFIL)=IH(I)
               RHELES(7,IFIL)=(RH(I)*1.D-6)+1.D0
               IF (IH(7).NE.0) NHELM=1
               LFOUND=.TRUE.
            ENDIF
          ENDDO
        ELSE IF (.NOT.LFOUND.AND.FILSCR.EQ.' ') THEN
          COVCOM(IHLP)=RHLP
          GRPNAM(IHLP)=CH3
          DO I=1,3
            IHELES(I,IFIL)=IH(I)
            RHELES(I,IFIL)=RH(I)
            IF (IH(I).NE.0) NHELM=1
          ENDDO
          DO I=4,6
            IHELES(I,IFIL)=IH(I)
            RHELES(I,IFIL)=RH(I)/ars/1000.D0
            IF (IH(I).NE.0) NHELM=1
          ENDDO
          IHELES(7,IFIL)=IH(I)
          RHELES(7,IFIL)=(RH(I)*1.D-6)+1.D0
          IF (IH(7).NE.0) NHELM=1
        ENDIF
100   CONTINUE
C
C CHECK, IF ALL FILES ARE FOUND
C
110   DO IFIL=1,NFIL
        IF (COVCOM(IFIL).EQ.-1.D0) THEN
          WRITE(LFNERR,901)FILNAM(IFIL)
901       FORMAT(/,' ### SR COVCOI: Filename not found: ',
     2              A32,/,16X,'Variance factor of one is used.',/)
          COVCOM(IFIL) = 1.D0
        ENDIF
      ENDDO
C
C COMPOSE INDGRP
C
      NGRP=0
      DO 200 IFIL=1,NFIL
        DO JFIL=1,IFIL-1
          IF (GRPNAM(IFIL).EQ.GRPNAM(JFIL).OR.GRPNAM(IFIL).EQ.' ')
     1    THEN
            IF (GRPNAM(IFIL).EQ.' ') THEN
              INDGRP(IFIL)=0
            ELSE
              INDGRP(IFIL)=INDGRP(JFIL)
            ENDIF
            GOTO 200
          ENDIF
        ENDDO
        NGRP=NGRP+1
        INDGRP(IFIL)=NGRP
200   CONTINUE
C
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
