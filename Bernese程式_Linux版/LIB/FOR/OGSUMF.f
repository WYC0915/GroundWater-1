      MODULE s_OGSUMF
      CONTAINS

C*
      SUBROUTINE OGSUMF(MAXARC,MAXFIL,NARC,TFL,NFIL,
     1                  NSATEL,NUMSAT,NUMSUM,RMSSUM,IECLIP,IDASUM)
CC
CC NAME       :  OGSUMF
CC
CC PURPOSE    :  WRITE SUMMARY OUTPUT FILE WITH ORBIT STATISTICS
CC
CC PARAMETERS :
CC        IN  :  MAXARC : MAXIMUM NUMBER OF SATELLITE ARCS    I*4
CC               MAXFIL : MAXIMUM NUMBER OF TABULAR/PRECISE   I*4
CC                        FILES
CC               NARC   :
CC               TFL(K,I),K=1,2, I=1,2,..,NARC: TIRST/LAST RE-  R*8
CC                       QUESTED TIME FOR ARC I
CC               NFIL   :
CC               NSATEL : MAX. NUMBER OF TROPOSPHERE MODELS   I*4
CC               NUMSAT : MAX. NUMBER OF TROP. PARAMETERS     I*4
CC                        PER MODEL
CC               NUMSUM : MAX. SETS OF EARTH ORI. PARAMETERS  I*4
CC               RMSSUM : MAX. NUMBER OF SATELLITE SPECIFIC   I*4
CC                        SIGMAS
CC               IECLIP : MAXIMUM NUMBER OF SATELLITES        I*4
CC               IDASUM : MAXIMUM NUMBER OF SPECIAL STOCHAST. I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  10-MAY-99
CC
CC CHANGES    :  09-OCT-01 : SS: RESET RMS VALUES OF 0 CM TO 1 CM
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               22-APR-03 : SS: WRITE 3-FIGURE DOY
CC               18-AUG-03 : RD: ADD INCLUDE FOR "COMLFNUM.inc"
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-OCT-07 : RD: ADD IGS-ACC-SUMMARY
CC               24-APR-08 : SS: FORMAT STATEMENTS FROM 48 TO 100 SATS
CC               28-JAN-11 : SL: USE M_BERN WITH ONLY, PRINT MOON FLAG FOR ACC
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: longLineLength, lfnLoc, lfnErr, lfnPlt
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: FILTITLE
      USE s_iordup
      USE s_opnfil
      USE s_gtflna
      USE s_opnerr
      USE s_mjdgps
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IARC  , IFIL  , IOSTAT, IRCSUM, ISAT  , MAXARC,
     1          MAXFIL, NARC  , NDAY  , NFIL  , NSAT  , IRCACC,
     2          IDOW1 , IDOW2 , IWEEK1, IWEEK2, II    ,
     3          II1   , II2
      INTEGER*4 IECL(3)
C
      REAL*8    SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER(LEN=longLineLength) :: STRICH
      CHARACTER*32 FILSUM,FILACC
      CHARACTER*1  TXTECL(4)
      CHARACTER*3  TXTEC2(4)
C
      REAL*8       RMSSUM(MAXARC,MAXFIL,MAXSAT),RMSTOT(MAXSAT)
      REAL*8       XRMS(2),TFL(2,*)
C
      INTEGER*4    NSATEL(MAXARC),NUMSAT(MAXSAT,MAXARC),IRMS(2)
      INTEGER*4    NUMSUM(MAXARC,MAXFIL,MAXSAT),IECLIP(MAXARC,MAXSAT)
      INTEGER*4    IDASUM(MAXARC,MAXFIL),NUMSAA(MAXSAT)
      INTEGER*4    IDXSAT(MAXSAT),INTRMS(2,MAXSAT),NUMTOT(MAXSAT)
C
C  COMMON FOR LOGICAL FILE NUMBERS
C  -------------------------------
C
      DATA TXTECL/' ','E',' ','E'/
      DATA TXTEC2/' ..',' E.',' .M',' EM'/
C
C GET SUMMARY OUTPUT FILE NAME
C ----------------------------
      CALL GTFLNA(0,'SUMFIL ',FILSUM,IRCSUM)
      CALL GTFLNA(0,'SUMACC ',FILACC,IRCACC)
      IF (IRCSUM.NE.0.AND.IRCACC.NE.0) RETURN
C
C OPEN SUMMARY FILES
C ------------------
      IF (IRCSUM.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILSUM,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSUM,'OGSUMF')
      ENDIF
C
      IF (IRCACC.EQ.0) THEN
        CALL OPNFIL(LFNPLT,FILACC,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILACC,'OGSUMF')
      ENDIF
C
C LOOP OVER ALL ARCS
C ------------------
      DO IARC=1,NARC
C
C GENERATE THE SEPARATOR
C ----------------------
        STRICH=''
        WRITE(STRICH,'(1X,79("-"))')
        DO ISAT=1,NSATEL(IARC)
          II1=ISAT*4+1
          II2=II1+4
          WRITE(STRICH(II1:II2),'(A)') '----'
        ENDDO
C
C WRITE TITLE LINE(S)
C -------------------
        IF (IARC.EQ.1.AND.IRCACC.EQ.0) THEN
          CALL MJDGPS(TFL(1,1),SEC,IWEEK1)
          IDOW1 = NINT(SEC)/86400
          CALL MJDGPS(TFL(2,NARC),SEC,IWEEK2)
          IDOW2 = NINT(SEC)/86400
          WRITE(LFNPLT,100) FILTITLE,IDOW1,IWEEK1,IDOW2,IWEEK2
100       FORMAT(1X,A,/,
     1           ' TIME FROM DAY : ',I1,' GPS WEEK: ',I4,/
     2           '      TO   DAY : ',I1,' GPS WEEK: ',I4)
        ENDIF
C
        IF (IARC.GT.1.AND.IRCSUM.EQ.0) WRITE(LFNLOC,'(/)')
        IF (IARC.GT.1.AND.IRCACC.EQ.0) WRITE(LFNPLT,'(/)')
C
        NDAY=0
        DO IFIL=1,NFIL
          IF (IDASUM(IARC,IFIL).NE.0) NDAY=NDAY+1
        ENDDO
C
        IF (NARC.GT.1) THEN
          IF (IRCSUM.EQ.0) THEN
            WRITE(LFNLOC,101) NDAY,IARC,TRIM(STRICH)
101         FORMAT(1X,79('-'),/,
     1             ' ORBIT REPEATABILITY FROM A',I2,'-DAY FIT THROUGH',
     2             ' DAILY ORBITS (CM)          ARC:',I3,/,A)
          ENDIF
          IF (IRCACC.EQ.0) THEN
            WRITE(LFNPLT,102) NDAY,IARC
102         FORMAT(1X,79('-'),/,
     1             ' ORBIT REPEATABILITY FROM A',I2,'-DAY FIT THROUGH',
     2             ' DAILY ORBITS (MM)          ARC:',I3)
          ENDIF
        ELSE
          IF (IRCSUM.EQ.0) THEN
            WRITE(LFNLOC,103) NDAY,TRIM(STRICH)
103         FORMAT(1X,79('-'),/,
     1             ' ORBIT REPEATABILITY FROM A',I2,'-DAY FIT THROUGH',
     2             ' DAILY ORBIT SOLUTIONS (CM)',/,A)
          ENDIF
          IF (IRCACC.EQ.0) THEN
            WRITE(LFNPLT,104) NDAY
104         FORMAT(1X,79('-'),/,
     1             ' ORBIT REPEATABILITY FROM A',I2,'-DAY FIT THROUGH',
     2             ' DAILY ORBIT SOLUTIONS (MM)')
          ENDIF
        ENDIF
C
C WRITE SATELLITE LINE
C --------------------
        NSAT=NSATEL(IARC)
        DO ISAT=1,NSAT
          NUMSAA(ISAT)=NUMSAT(ISAT,IARC)
        ENDDO
        CALL IORDUP(NUMSAA,NSAT,IDXSAT)
C
        IF (IRCSUM.EQ.0) THEN
          WRITE(LFNLOC,105) (NUMSAA(IDXSAT(ISAT)),
     1                       TXTECL(IECLIP(IARC,IDXSAT(ISAT))+1),
     2                       ISAT=1,NSAT)
105       FORMAT(5X,100(I3,A1))
        ENDIF
C
        IF (IRCACC.EQ.0) THEN
C
          IF (NARC.GT.1) THEN
            CALL MJDGPS(TFL(1,IARC),SEC,IWEEK1)
            IDOW1 = NINT(SEC)/86400
            CALL MJDGPS(TFL(2,IARC),SEC,IWEEK2)
            IDOW2 = NINT(SEC)/86400
            WRITE(LFNPLT,106) IDOW1,IWEEK1,IDOW2,IWEEK2
106         FORMAT(' TIME FROM DAY : ',I1,' GPS WEEK: ',I4,
     1             ' TO DAY : ',I1,' GPS WEEK: ',I4)
          ENDIF
C
          IECL(:)=0
          DO ISAT=1,NSAT
            IF(IECLIP(IARC,ISAT).EQ.1.OR.IECLIP(IARC,ISAT).EQ.3)
     1        IECL(1)=IECL(1)+1
            IF(IECLIP(IARC,ISAT).EQ.2.OR.IECLIP(IARC,ISAT).EQ.3)
     1        IECL(2)=IECL(2)+1
            IF(IECLIP(IARC,ISAT).EQ.3)
     1        IECL(3)=IECL(3)+1
          ENDDO
          WRITE(LFNPLT,107) IECL(1),IECL(2),IECL(3),TRIM(STRICH),
     1         (TXTEC2(IECLIP(IARC,IDXSAT(ISAT))+1),ISAT=1,NSAT)
107        FORMAT(' # ECLIPSING SATELLITES: ',
     1            I3,' E / ',I3,' M (',I3,' EM)',/,A,
     1            /' ECL',100(1X,A3))
          WRITE(LFNPLT,'(A,100(I4))')
     1          ' DOY',(NUMSAA(IDXSAT(ISAT)),ISAT=1,NSAT)
        ENDIF
C
C WRITE RMS VALUES
C ----------------
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(A)') TRIM(STRICH)
        IF (IRCACC.EQ.0) WRITE(LFNPLT,'(A)') TRIM(STRICH)
C
        DO ISAT=1,NSAT
          NUMTOT(ISAT)=0
          RMSTOT(ISAT)=0.D0
        ENDDO
C
        DO IFIL=1,NFIL
          IF (IDASUM(IARC,IFIL).NE.0) THEN
            INTRMS=0
            DO ISAT=1,NSAT
              IF (NUMSUM(IARC,IFIL,ISAT).NE.0) THEN
                XRMS(1)=DSQRT(RMSSUM(IARC,IFIL,ISAT)/
     1                       NUMSUM(IARC,IFIL,ISAT))*100.D0
                IRMS(1)=MAX0(IDNINT(XRMS(1)),1)
                XRMS(2)=DSQRT(RMSSUM(IARC,IFIL,ISAT)/
     1                       NUMSUM(IARC,IFIL,ISAT))*1000.D0
                IRMS(2)=MAX0(IDNINT(XRMS(2)),1)
                DO II = 1,2
                  IF (XRMS(II).EQ.0.D0) IRMS(II)=0
                  INTRMS(II,ISAT)=IRMS(II)
                  IF (INTRMS(II,ISAT).GT.999) INTRMS(II,ISAT)=999
                ENDDO
              ENDIF
C
              NUMTOT(ISAT)=NUMTOT(ISAT)+NUMSUM(IARC,IFIL,ISAT)
              RMSTOT(ISAT)=RMSTOT(ISAT)+RMSSUM(IARC,IFIL,ISAT)
            ENDDO
            IF (IRCSUM.EQ.0) THEN
              WRITE(LFNLOC,109,IOSTAT=IOSTAT) IDASUM(IARC,IFIL),
     1          (INTRMS(1,IDXSAT(ISAT)),ISAT=1,NSAT)
            ENDIF
            IF (IRCACC.EQ.0) THEN
              WRITE(LFNPLT,109,IOSTAT=IOSTAT) IDASUM(IARC,IFIL),
     1          (INTRMS(2,IDXSAT(ISAT)),ISAT=1,NSAT)
            ENDIF
109         FORMAT(I4.3,100I4)
          ENDIF
        ENDDO
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(A)') TRIM(STRICH)
        IF (IRCACC.EQ.0) WRITE(LFNPLT,'(A)') TRIM(STRICH)
C
C TOTAL RMS VALUES
C ----------------
        DO ISAT=1,NSAT
          INTRMS(:,ISAT)=0
          IF (NUMTOT(ISAT).NE.0) THEN
            XRMS(1)=DSQRT(RMSTOT(ISAT)/NUMTOT(ISAT))*100.D0
            IRMS(1)=MAX0(IDNINT(XRMS(1)),1)
            XRMS(2)=DSQRT(RMSTOT(ISAT)/NUMTOT(ISAT))*1000.D0
            IRMS(2)=MAX0(IDNINT(XRMS(2)),1)
            DO II=1,2
              IF (XRMS(II).EQ.0.D0) IRMS(II)=0
              INTRMS(II,ISAT)=IRMS(II)
              IF (INTRMS(II,ISAT).GT.999) INTRMS(II,ISAT)=999
            ENDDO
          ENDIF
        ENDDO
        IF (IRCSUM.EQ.0) THEN
          WRITE(LFNLOC,110,IOSTAT=IOSTAT)
     1      (INTRMS(1,IDXSAT(ISAT)),ISAT=1,NSAT)
        ENDIF
        IF (IRCACC.EQ.0) THEN
          WRITE(LFNPLT,110,IOSTAT=IOSTAT)
     1      (INTRMS(2,IDXSAT(ISAT)),ISAT=1,NSAT)
        ENDIF
110     FORMAT(1X,'ALL',100I4)
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(A)') TRIM(STRICH)
        IF (IRCACC.EQ.0) WRITE(LFNPLT,'(A)') TRIM(STRICH)
C
      ENDDO
C
C CLOSE FILE
C ----------
      IF (IRCSUM.EQ.0) CLOSE(UNIT=LFNLOC)
      IF (IRCACC.EQ.0) CLOSE(UNIT=LFNPLT)
C
      RETURN
      END SUBROUTINE

      END MODULE
