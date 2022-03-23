      MODULE s_DEFREQ
      CONTAINS

C*
      SUBROUTINE DEFREQ(EPOFRQ,NSAT,SATNUM,NOFRQ,SATNOF,
     1                  USEGEOS,GOBSDEF,MEATYPC,OBSF12)
CC
CC NAME       :  DEFREQ
CC
CC PURPOSE    :  DEFINE FREQUENCIES FOR BERNESE GPS SOFTWARE.
CC               READ THE INFORMATION OUT OF SATELLITE FILE.
CC               TEST IF THE SATELLITES ARE INCLUDED IN THE
CC               ACTUAL TIME WINDOW (SET IN THE SATELLITE FILE).
CC
CC PARAMETERS :
CC         IN :  EPOFRQ : TIME WINDOW (MJD)                  R*8(2)
CC               NSAT   : NUMBER OF SATELLITES TO BE TESTED  I*4
CC               SATNUM : SATELLITE NUMBERS                  I*4(*)
CC               USEGEOS: USE GIOVE EXTERN. OBS. SELECTION   (OPT)I*4
CC               GOBSDEF: GIOVE EXTERN. OBS. SEL. INFO       (OPT)
CC               MEATYPC: USE CODE ('C') or PHASE ('P') OBS
CC                        FILES FOR EXTERNAL SIGNAL SELECTION,
CC                        OR CHECK FOR AVAILABILITY ('U')    (OPT)C*1
CC               OBSF12 : TAKE OBSTYPE INFO FROM FIRST (=1)
CC                        OR SECOND (=2) SET OF GOBSDEF       (OPT)I*4
CC        OUT :  NOFRQ  : NUMBER OF SATELLITES IN LIST WITH  I*4
CC                        NO FREQUENCY (OPTIONAL)
CC               SATNOF : LIST OF SATELLITES WITH NO FREQ    I*4(*)
CC                        (OPTIONAL)
CC
CC REMARKS    :  IF THE TWO OPTIONAL ARGUMENTS NOFRQ AND SATNOF ARE
CC               GIVEN THE PROGRAM DOES NOT STOP IF A FREQUENCY IS
CC               NOT FOUND BUT RETURNS THE LIST OF SATELLTIES WITH
CC               MISSING FREQUENCIES
CC
CC AUTHOR     :  H.HABRICH, D.INEICHEN, S.SCHAER
CC
CC CREATED    :  22-OCT-97
CC
CC CHANGES    :  04-MAY-98 : SS: VERIFY TIME WINDOW
CC               19-MAY-98 : SS: CHECK SATELLITE NUMBERS
CC               16-MAR-01 : DS: Use structure satfil instead of file SATELL
CC               17-MAR-01 : DS: Find last GLONASS (GLLAST) instead of use 124
CC               27-MAR-02 : DS: SET LEO FREQ
CC               04-OCT-02 : RD: DEALLOCATE SATELLITE ARRAYS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               15-MAY-03 : HU: INITIALIZE STRUCTURES
CC               13-SEP-03 : HU: OPTIONAL ARGUMENTS
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               15-DEC-04 : MM: NEW FREQUENCY ASSIGNMENT LOGIC
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               20-AUG-05 : AG: CHANGED POSITION OF IFRQ IN SATELLIT. FILE
CC               02-MAY-06 : MM: BUGFIX WRT NON-MICROWAVE SENSORS
CC               02-MAY-06 : AG: GALILEO FREQUENCIES IMPLEMENTED (L7 = L2)
CC               14-MAY-08 : DT: CYCLE FOR SLR SATELLITES (ASSUME SATNUM>=951)
CC               09-MAY-09 : RD: PROVIDE ALSO FREQUENCY-NUMBER IN COMFREQ.INC
CC               26-JAN-11 : LP: Sat-specific obs. types;suppress output if NOFRQ present
CC               17-NOV-11 : HB: CYCLE ALSO FOR LEO SATELLITES (SATNUM>=901)
CC               25-APR-12 : LP: Generalization of sat-specific obs types;
CC                               FREQ(3), FREQS, FREQC, FREQJ added
CC
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAA
      USE d_satfil, ONLY : t_satfil, init_satfil, typeMWTR
      USE d_const, ONLY: C, FREQ, FREQE, FREQS, FREQC, FREQJ
C!!! FREQG and DFREQG somehow handled in COMFREQ.inc, not in same way as e.g. FREQ, FREQE ..
      USE d_rinex3, ONLY: t_gobsdef
      USE s_dimtst
      USE s_exitrc
      USE s_gtflna
      USE s_rdsatfil
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I   , II  , III , IRC , ISAT, ISVN, NSAT,usealternative
      INTEGER*4 igeos, indgeos, satnumg, ifreq
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)

!
! Dummy list
! ----------
      TYPE(t_satfil), SAVE         :: satfil
!
! Variables
! ---------------
      CHARACTER*40  EPOSTR
      CHARACTER*32  FILNAM
      CHARACTER*4   SENTYP(MAXSAA)
      CHARACTER*1,OPTIONAL  ::   MEATYPC

      REAL*8        SVNNUM(MAXSAA),FROMTO(2,MAXSAA)
      REAL*8        EPOFRQ(2)


      INTEGER*4     SATNUM(*), FRQNUM(MAXSAA), NRECRD
      INTEGER(i4b),OPTIONAL                   :: NOFRQ
      INTEGER(i4b),DIMENSION(:),OPTIONAL      :: SATNOF
      type(t_gobsdef),OPTIONAL                :: GOBSDEF
      INTEGER*4,OPTIONAL                      :: USEGEOS
      INTEGER(i4b)                            :: CODPHAS
      INTEGER*4,OPTIONAL                      :: OBSF12
      INTEGER(i4b)                            :: FIND


      LOGICAL,      SAVE                      :: first= .TRUE.

      INCLUDE 'COMFREQ.inc'

      IF (PRESENT(NOFRQ))  NOFRQ =0
      IF (PRESENT(SATNOF)) SATNOF=0

!
! If called for the first time, read the entire satellite file SATELL
! ===================================================================
      IF (first) THEN
        first = .FALSE.
!
! Get the satellite info file name
! ---------------------------------------------------
        CALL gtflna(1,'SATELL ',FILNAM,IRC)
!
! Read satellite info file (SATELL)
! ---------------------------------
        CALL init_satfil(satfil)
        CALL rdsatfil(FILNAM,satfil)
!
! Check maximum number of satellites
! ----------------------------------
        NRECRD=satfil%nsensor
        CALL DIMTST(1,1,2,'DEFREQ','MAXSAA',
     1              'SATELLITES IN SATELLITE FILE','INCLUDE FILE USED',
     2               NRECRD,MAXSAA,IRC)
!
! Read the structure satfil
! -------------------------
        SVNNUM(1:NRECRD)  =satfil%sensor(1:NRECRD)%svn
        FROMTO(1,1:NRECRD)=satfil%sensor(1:NRECRD)%timint%t(1)
        FROMTO(2,1:NRECRD)=satfil%sensor(1:NRECRD)%timint%t(2)
        FRQNUM(1:NRECRD)  =satfil%sensor(1:NRECRD)%ifrq
        SENTYP(1:NRECRD)  =satfil%sensor(1:NRECRD)%type
!
        DEALLOCATE(satfil%satellite,stat=irc)
        DEALLOCATE(satfil%sensor,stat=irc)
C
C INITIALIZE COMMON RECORDS 'FRQ' AND 'WLGT'
C ------------------------------------------
        DO II=1,MAXSVN
          FRQ(1,II)=0
          FRQ(2,II)=0
          DO III=1,5
            WLGT(III,II)=0
          END DO
        END DO
      END IF
C
C CHECK SATELLITE NUMBERS
C -----------------------
      DO ISAT=1,NSAT
        ISVN=SATNUM(ISAT)
        IF (ISVN.LE.0) THEN
          WRITE(LFNERR,901) ISVN
901       FORMAT(/,' *** SR DEFREQ: INVALID SATELLITE NUMBER',
     1      /,16X,'SATELLITE NUMBER: ',I4,/)
          CALL EXITRC(2)
        ENDIF
C
        CALL DIMTST(1,2,2,'DEFREQ','MAXSVN','SATELLITE NUMBERS',
     1    'SEE INCLUDE FILE "COMFREQ"',ISVN,MAXSVN,IRC)
      ENDDO
C
C LOOP OVER ALL INPUT SATELLITES:
C -------------------------------
      DO 60 I=1,NSAT

C Cycle for LAGEOS etc.
        IF (SATNUM(I).GE.901) CYCLE

        DO II=1,NRECRD
C
C No microwave transmitter (record in SAT file)
          IF (TRIM(SENTYP(II)).NE.TRIM(typeMWTR)) CYCLE

          IF (SATNUM(I).EQ.SVNNUM(II)   .AND.
     1        EPOFRQ(1).GE.FROMTO(1,II) .AND.
     2        EPOFRQ(1).LE.FROMTO(2,II)) THEN
C
C STOP, IF TIME WINDOW NOT COVERED
C --------------------------------
            IF (EPOFRQ(2).GT.FROMTO(2,II)) THEN
              CALL TIMST2(1,2,EPOFRQ,EPOSTR)
              WRITE(LFNERR,902) SATNUM(I),FILNAM,EPOSTR
902           FORMAT(/,' *** SR DEFREQ: TIME WINDOW NOT COVERED ',
     1          'IN SATELLITE FILE',/,
     2          16X,'SATELLITE NUMBER : ',I3,/,
     3          16X,'SATELLITE FILE   : ',A,/,
     4          16X,'TIME WINDOW      : ',A,/)
              CALL EXITRC(2)
            ENDIF
C
C           WRITE FREQUENCY IN COMMON ARRAY 'FRQ':
C           --------------------------------------
            III=SATNUM(I)
            FREQNM(III)=FRQNUM(II)
            usealternative=0
            IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF))) THEN
             IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
              DO igeos=1,gobsdef%norec
               satnumg=gobsdef%sat(igeos)%sysnum*100
     1                 +gobsdef%sat(igeos)%satnum
               IF (satnumg==III) THEN
                  usealternative=1
                  indgeos=igeos
                  EXIT
               ENDIF
              ENDDO
             ENDIF
            ENDIF
c            write(*,*) 'DEFREQ: SAT:',III,', mode:',usealternative
c
            IF (usealternative==0) THEN
c           Hardwired signal selection by DEFCON
              IF (III.LT.100) THEN
C             GPS FREQUENCIES
                FRQ(1,III)=FREQ(1)
                FRQ(2,III)=FREQ(2)
              ELSEIF (III.GE.100 .AND. III.LT.200) THEN
C             GLONASS FREQUENCIES
                FRQ(1,III)=FREQG(1)+DBLE(FRQNUM(II))*DFREQG(1)
                FRQ(2,III)=FREQG(2)+DBLE(FRQNUM(II))*DFREQG(2)
              ELSEIF (III.GE.200 .AND. III.LT.300) THEN
C             GALILEO FREQUENCIES
                FRQ(1,III)=FREQE(1)
                FRQ(2,III)=FREQE(4)
              ELSEIF (III.GE.300 .AND. III.LT.400) THEN
C             SBAS FREQUENCIES
                FRQ(1,III)=FREQS(1)
                FRQ(2,III)=FREQS(2)
              ELSEIF (III.GE.400 .AND. III.LT.500) THEN
C             COMPASS FREQUENCIES
                FRQ(1,III)=FREQC(1)
                FRQ(2,III)=FREQC(3)
              ELSEIF (III.GE.500 .AND. III.LT.600) THEN
C             QZSS FREQUENCIES
                FRQ(1,III)=FREQJ(1)
                FRQ(2,III)=FREQJ(2)
              ELSE
c               ADD_GNSS_HERE
               write(LFNERR,*)'### SR DEFREQ: Unknown satellite system:'
     1                                       ,' SAT ',III
                GOTO 70
c                CALL EXITRC(2)
              ENDIF
c
            ELSE
c           Signal selection from selection list
c             Take the information from the first (default; for ZD and
c             SD obsfiles) or from the second (for SD obsfiles) data set
              FIND=0
              IF (PRESENT(OBSF12)) THEN
                IF (OBSF12==1) THEN
                  FIND=0
                ELSE
                  FIND=4
                ENDIF
              ENDIF
c             Select obsfreq from code or phase signal (default: phase)
              CODPHAS=2
              IF (PRESENT(MEATYPC)) THEN
c               From code selection
                IF (MEATYPC.EQ.'C') THEN
                  CODPHAS=0
c               Unknown (phase has priority)
                ELSEIF (MEATYPC.EQ.'U') THEN
                  IF ((gobsdef%sat(indgeos)%obstyp(3+FIND)(2:2)
     1            .EQ.' ').OR.(gobsdef%sat(indgeos)%obstyp(4+FIND)(2:2)
     2            .EQ.' ')) THEN
                    CODPHAS=0
                    IF ((gobsdef%sat(indgeos)%obstyp(1+FIND)(2:2).EQ.
     2              ' ').OR.(gobsdef%sat(indgeos)%obstyp(2+FIND)(2:2)
     2              .EQ.' ')) THEN
c                      WRITE(LFNERR,907) SATNUM(I)
907                   FORMAT(/,'### SR DEFREQ: NOT BOTH FREQUENCIES',
     1                  ' DEFINED FOR',/,16X,'SATELLITE NUMBER : ',I3)
c                      CALL EXITRC(2)
                      GOTO 70
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
c
              DO ifreq=1,2
                IF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)(2:2)
     1              .EQ.'1') THEN
                  IF (III.LT.100) THEN
                    FRQ(ifreq,III)=FREQ(1)
                  ELSEIF (III.GE.100 .AND. III.LT.200) THEN
                    FRQ(ifreq,III)=FREQG(1)+DBLE(FRQNUM(II))*DFREQG(1)
                  ELSEIF (III.GE.200 .AND. III.LT.300) THEN
                    FRQ(ifreq,III)=FREQE(1)
                  ELSEIF (III.GE.300 .AND. III.LT.400) THEN
                    FRQ(ifreq,III)=FREQS(1)
                  ELSEIF (III.GE.400 .AND. III.LT.500) THEN
                    FRQ(ifreq,III)=FREQC(1)
                  ELSEIF (III.GE.500 .AND. III.LT.600) THEN
                    FRQ(ifreq,III)=FREQJ(1)
                  ELSE
c                 ADD_GNSS_HERE
c                    WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
c                    write(LFNERR,*)'### SR DEFREQ: Frequency L',
c     1              gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)(2:2),
c     2              ',Satellite ',III
c                    CALL EXITRC(2)
                    GOTO 70
                  ENDIF
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'2') THEN
                  IF (III.LT.100) THEN
                    FRQ(ifreq,III)=FREQ(2)
                  ELSEIF (III.GE.100 .AND. III.LT.200) THEN
                    FRQ(ifreq,III)=FREQG(2)+DBLE(FRQNUM(II))*DFREQG(2)
c                  ELSEIF (III.GE.300 .AND. III.LT.400) THEN
c                    FRQ(ifreq,III)=FREQS(2)
                  ELSEIF (III.GE.400 .AND. III.LT.500) THEN
                    FRQ(ifreq,III)=FREQC(2)
                  ELSEIF (III.GE.500 .AND. III.LT.600) THEN
                    FRQ(ifreq,III)=FREQJ(2)
                  ELSE
c                 ADD_GNSS_HERE
c                    WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                    GOTO 70
                  ENDIF
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'3') THEN
c                  WRITE(LFNERR,906) SATNUM(I),
c     1                gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                  GOTO 70
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'4') THEN
c                  WRITE(LFNERR,906) SATNUM(I),
c     1                gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                  GOTO 70
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'5') THEN
                  IF (III.LT.100) THEN
                    FRQ(ifreq,III)=FREQ(3)
                  ELSEIF (III.GE.200 .AND. III.LT.300) THEN
                    FRQ(ifreq,III)=FREQE(3)
                  ELSEIF (III.GE.300 .AND. III.LT.400) THEN
                    FRQ(ifreq,III)=FREQS(2)
c                  ELSEIF (III.GE.400 .AND. III.LT.500) THEN
c                    FRQ(ifreq,III)=FREQC(3)
                  ELSEIF (III.GE.500 .AND. III.LT.600) THEN
                    FRQ(ifreq,III)=FREQJ(3)
                  ELSE
c                 ADD_GNSS_HERE
c                    WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                    GOTO 70
                  ENDIF
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'6') THEN
                  IF (III.GE.200 .AND. III.LT.300) THEN
                    FRQ(ifreq,III)=FREQE(5)
                  ELSEIF (III.GE.400 .AND. III.LT.500) THEN
                    FRQ(ifreq,III)=FREQC(4)
                  ELSEIF (III.GE.500 .AND. III.LT.600) THEN
                    FRQ(ifreq,III)=FREQJ(4)
                  ELSE
c                 ADD_GNSS_HERE
c                    WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                    GOTO 70
                  ENDIF
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'7') THEN
                  IF (III.GE.200 .AND. III.LT.300) THEN
                    FRQ(ifreq,III)=FREQE(4)
                  ELSEIF (III.GE.400 .AND. III.LT.500) THEN
                    FRQ(ifreq,III)=FREQC(3)
                  ELSE
c                 ADD_GNSS_HERE
c                    WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                    GOTO 70
                  ENDIF
c
                ELSEIF (gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
     1                 (2:2).EQ.'8') THEN
                  IF (III.GE.200 .AND. III.LT.300) THEN
                    FRQ(ifreq,III)=FREQE(2)
                  ELSE
c                 ADD_GNSS_HERE
c                    WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
                    GOTO 70
                  ENDIF
c
                ELSE
c                  WRITE(LFNERR,906) SATNUM(I),
c     1                  gobsdef%sat(indgeos)%obstyp(ifreq+CODPHAS+FIND)
906                 FORMAT(/,'### SR DEFREQ: FREQUENCY NOT DEFINED IN',
     1                ' CONST FILE',/,
     2                16X,'SATELLITE NUMBER : ',I3,/,
     3                16X,'FREQUENCY        : ',A,/)
c                    CALL EXITRC(2)
                    GOTO 70
                ENDIF
              ENDDO
            ENDIF
C
C           WRITE WAVELENGTH IN COMMON ARRAY 'WLGT':
C           ----------------------------------------
            WLGT(1,III)=C/FRQ(1,III)
            WLGT(2,III)=C/FRQ(2,III)
            WLGT(3,III)=WLGT(1,III)*WLGT(2,III)/(WLGT(1,III)+
     1                                                     WLGT(2,III))
            WLGT(4,III)=WLGT(1,III)-WLGT(2,III)
            WLGT(5,III)=WLGT(1,III)*WLGT(2,III)/(WLGT(2,III)-
     1                                                     WLGT(1,III))
C
C           WRITE FACTORS CONCERNING IMPORTANT LINEAR COMBINATIONS
C           IN COMMON ARRAY 'FACLIN':
C           ------------------------------------------------------
            FACLIN(1,1,III)= 1.D0
            FACLIN(1,2,III)= 0.D0
            FACLIN(2,1,III)= 0.D0
            FACLIN(2,2,III)= 1.D0
            FACLIN(3,1,III)= FRQ(1,III)**2/
     1                         (FRQ(1,III)**2-FRQ(2,III)**2)
            FACLIN(3,2,III)=-FRQ(2,III)**2/
     1                         (FRQ(1,III)**2-FRQ(2,III)**2)
            FACLIN(4,1,III)= 1.D0
            FACLIN(4,2,III)=-1.D0
            FACLIN(5,1,III)= FRQ(1,III)/(FRQ(1,III)-FRQ(2,III))
            FACLIN(5,2,III)=-FRQ(2,III)/(FRQ(1,III)-FRQ(2,III))
C
            GOTO 60
          ENDIF
        ENDDO
C
C       ERROR: SATELLITE NOT FOUND IN SATELLITE FILE:
C       ---------------------------------------------
70      IF (PRESENT(NOFRQ)) THEN
          CALL TIMST2(1,2,EPOFRQ,EPOSTR)
          NOFRQ=NOFRQ+1
          IF (PRESENT(SATNOF)) THEN
            IF (NOFRQ.GT.SIZE(SATNOF)) THEN
              WRITE(LFNERR,904) SIZE(SATNOF)
904           FORMAT(/,' *** SR DEFREQ: TOO MANY SATELLITES WITH',
     1                ' UNDEFINED FREQUENCY',/,
     2                16X,'DIMENSION OF ARRAY : ',I6,/)
              CALL EXITRC(2)
            ENDIF
            SATNOF(NOFRQ)=SATNUM(I)
          ENDIF
        ELSE
          CALL TIMST2(1,2,EPOFRQ,EPOSTR)
          WRITE(LFNERR,905) SATNUM(I),FILNAM,EPOSTR
905       FORMAT(/,' *** SR DEFREQ: NOT BOTH FREQUENCIES DEFINED',
     1                ' ',/,
     2                16X,'SATELLITE NUMBER : ',I3,/,
     3                16X,'SATELLITE FILE   : ',A,/,
     4                16X,'TIME WINDOW      : ',A,/)
            CALL EXITRC(2)
        ENDIF
C
60    CONTINUE
C
C     CLOSE FILE WITH SATELLITE INFORMATIONS
C     --------------------------------------
C
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
