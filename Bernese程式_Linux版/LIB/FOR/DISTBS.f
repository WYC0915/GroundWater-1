      MODULE s_DISTBS
      CONTAINS

C*
      SUBROUTINE DISTBS(CLUINP,NFIL,FLAGBS,FLMLBS,STANAX,FILOUT)
CC
CC NAME       :  DISTBS
CC
CC PURPOSE    :  DISTRIBUTE THE BASELINES ACCORDING TO CLUSTER
CC
CC PARAMETERS :
CC         IN :  CLUINP ... NAME OF FILE WITH CLUSTER DEFINITION  CH*32
CC               NFIL ... NUMBER OF SINGLE DIFF. FILES             I*4
CC               FLAGBS(IFIL)=.TRUE. ... THE BASELINE IS IN        LOG
CC                                     THE OPTIMAL SELECTION
CC               FLMLBS(IFIL)=.TRUE. ... REDUNDANT BASELINE        LOG
CC               STANAX(I,IFIL) ... STATION NAME                  CH*16
CC                   I=1 ... THE FIRST STATION
CC                   I=2 ... THE SECOND STATION
CC               FILOUT(I,IFIL),OUTPUT FILE NAMES (SINGLE-DIFF.)  CH*32
CC                   I=1 ... HEADER FILE
CC                   I=2 ... OBSERVATION FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC VERSION    :  3.4
CC
CC CREATED    :  19-APR-95
CC
CC CHANGES    :  05-MAY-95 : LM: INTERN NAMES CLUINP, CLUOUT
CC               08-MAY-95 : TS: CORRECTED OUTPUT NAME CREATION "CLUOUT"
CC               09-AUG-95 : TS: INCREASED MAXCLU 100 --> 250
CC               28 MAY-96 : RS: ERROR/EXIT IF STATION NOT IN CLUST. DEF.
CC               12-NOV-96 : MR: USE SR FPARSE TO GET FILE NAME
CC               30-JAN-97 : JJ: ADD IMOST TO CALL
CC               04-FEB-97 : JJ: REMOVE IMOST FROM CALL (OOPS)
CC               30-SEP-99 : SS: MAXCLU FROM 250 TO 350
CC               26-OCT-01 : MM: MODIFIED FILENAME CREATION (NO %% NEEDED)
CC               23-NOV-01 : MM: CHANGED CLUSTER-OUTPUT
CC               01-FEB-02 : MM: INCREASED MAXCLU FROM 350 TO 400
CC               17-FEB-03 : LM: INCL_P REMOVED
CC               01-MAY-03 : SS: MAXCLU FROM 400 TO 600
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               25-JUN-13 : DA: CALL OPNERR FOR CLUINP AFTER OPNFIL
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC    1995       UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C DECLARATIONS
C ------------
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      USE s_fparse
      USE s_exitrc
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLUST, IFIL  , IFULL , IOK   , IOSTAT, IRC   , ISTA  ,
     1          MAXCLU, NCLUST, NFIL  , NSTA
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXCLU=600)
C
      CHARACTER*132 LINE
      CHARACTER*32  CLUINP,FILOUT(2,*),CLUOUT
      CHARACTER*32  NODE,DEVICE,DIR,NAME,EXT,VER
      CHARACTER*32  CLUNODE,CLUDEVICE,CLUDIR,CLUNAME,CLUEXT,CLUVER
      CHARACTER*2   CLUNUM

      CHARACTER*16  STANAX(2,*),STANAM(MAXCLU)
      INTEGER*4     CLUST(MAXCLU)
      LOGICAL*4     FLAGBS(*),FLMLBS(*),NOCLUS
C
C
C READ ALL THE STATION NAMES AND THE CORRESPONING CLUSTURS
C --------------------------------------------------------
      NCLUST=0
C
      CALL OPNFIL(LFNLOC,CLUINP,'OLD','FORMATTED','READONLY',' ',
     1            IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,CLUINP,'DISTBS')
C
      READ(LFNLOC,'(////)')
C
      DO 100 NSTA=1,1000000
        READ(LFNLOC,'(A)',END=101) LINE
        IF (LINE .EQ. ' ') GOTO 101
        IF (NSTA .GT. MAXCLU) THEN
          WRITE(LFNERR,1000) MAXCLU
1000      FORMAT(//,' *** SR DISTBS : MAXCLU TOO SMALL: ',I5,//)
          CALL EXITRC(2)
        ENDIF
        READ (LINE,'(A16,2X,I3)') STANAM(NSTA),CLUST(NSTA)
        IF (CLUST(NSTA) .GT. NCLUST) NCLUST=CLUST(NSTA)
100   CONTINUE
101   CONTINUE
      CLOSE (LFNLOC)
      NSTA=NSTA-1
      NCLUST=NCLUST+1
C
C READ THE GENERIC OUTPUT FILE NAME
C ---------------------------------
      CALL GTFLNA(1,'CLUOUT ',CLUOUT,IRC)
      CALL FPARSE(0,CLUOUT,CLUNODE,CLUDEVICE,CLUDIR,CLUNAME,CLUEXT,
     1              CLUVER,IRC)
      CALL FPARSE(1,CLUOUT,CLUNODE,CLUDEVICE,CLUDIR,NAME,CLUEXT,
     1              CLUVER,IRC)

CC      IN = INDEX(CLUOUT,'%%')
CC      IF (IN.EQ.0) THEN
CC        WRITE(LFNERR,1001) CLUOUT
CC 1001    FORMAT(//,' *** SR DISTBS: THE SUBSTRING "%%" IS ',
CC      1         /,16X,'MANDATORY IN THE FILE NAME ',A)
CC        CALL EXITRC(2)
CC      ENDIF


C
C WRITE SOME OUTPUT
C -----------------
      WRITE(LFNPRT,"(//,63('-'))")
      WRITE(LFNPRT,'(14X,A35)') 'ASSIGNMENT OF BASELINES TO CLUSTERS'
      WRITE(LFNPRT,601) 'Cluster','Filename','Station 1','Station 2'
601   FORMAT(A8,5X,A8,5X,A8,13X,A8,/,63('-'))


C
C WRITE THE FILES WITH SNG. DIFF. FILE NAMES
C ------------------------------------------
      DO 200 ICLUST=1,NCLUST
CC       write(lfnprt,*)'iclust,nclust',iclust,nclust
        IFULL=0
        DO 210 IFIL=1,NFIL
CC          write(lfnprt,*)'ifil,fl1,fl2',ifil,flagbs(ifil),flmlbs(ifil)
          IF (FLAGBS(IFIL)) THEN
C
            IF (.NOT. FLMLBS(IFIL)) THEN
              IOK = 0
              NOCLUS=.TRUE.
              DO 220 ISTA=1,NSTA
                IF (STANAM(ISTA).EQ.STANAX(1,IFIL)) THEN
                  NOCLUS=.FALSE.
                  IF (ICLUST.EQ.CLUST(ISTA)) IOK=1
                ENDIF
CCC             IF ((STANAM(ISTA) .EQ. STANAX(1,IFIL)) .AND.
CCC  1               ICLUST .EQ. CLUST(ISTA))
CCC  2            IOK = 1
220           CONTINUE
              IF (NOCLUS) THEN
                 WRITE(LFNERR,2201) STANAX(1,IFIL),CLUINP
2201             FORMAT(//,' *** SR DISTBS: ',
     1                     'NO CLUSTER FOUND FOR STATION: ',A16,/,
     2                 16X,'CLUSTER DEFINITION FILE     : ',A32,/)
                 CALL EXITRC(2)
              ENDIF
            ELSE IF (ICLUST.EQ.NCLUST) THEN
              IOK = 2
            ELSE
              IOK = 0
            END IF
C
            IF (IOK .NE. 0) THEN
              IF (IFULL .EQ. 0) THEN
                IFULL=1
                IF (IOK .EQ. 1) THEN
                  WRITE (CLUNUM,'(I2.2)') ICLUST
                ELSE
                  WRITE (CLUNUM,1002)
1002              FORMAT ('00')
                END IF
C
                CLUOUT = TRIM(CLUNODE)//TRIM(CLUDEVICE)//TRIM(CLUDIR)//
     1                   TRIM(CLUNAME)//CLUNUM//'.'//TRIM(CLUEXT)
C
CC                write(lfnprt,*)'open ',cluout
                CALL OPNFIL(LFNLOC,CLUOUT,'UNKNOWN','FORMATTED',' ',
     1                      ' ',IOSTAT)
                CALL OPNERR(LFNERR,LFNLOC,IOSTAT,CLUOUT,'DISTBS')
              END IF
C
              CALL FPARSE(0,FILOUT(1,IFIL),NODE,DEVICE,DIR,NAME,EXT,
     1                  VER,IRC)
CC              write(lfnprt,*)'write',iclust,irc,name
              WRITE(LFNLOC,'(A)') NAME(1:LENGT1(NAME))
              WRITE(LFNPRT,'(A8,5X,A8,5X,A16,5X,A16)')
     1         TRIM(CLUNAME)//CLUNUM,NAME(1:LENGT1(NAME)),
     2         STANAX(1,IFIL),STANAX(2,IFIL)
            END IF
C
          END IF
210     CONTINUE
        IF (IFULL.EQ.1) CLOSE(LFNLOC)
200   CONTINUE
C
      WRITE(LFNPRT,"(63('-'),//)")
C
      RETURN
      END SUBROUTINE

      END MODULE
