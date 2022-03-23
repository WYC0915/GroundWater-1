      MODULE s_RDSUBM
      CONTAINS

C*
      SUBROUTINE RDSUBM(MAXPER,FILSUB,TITLE ,SUBNAM,SUBFAR,NSUB  ,
     1                  SUBPER,SUBMLT,SUBCOE)
CC
CC NAME       :  RDSUBM
CC
CC PURPOSE    :  READ SUBDAILY ERP MODEL FILE
CC
CC PARAMETERS :
CC        IN  :  MAXPER : MAXIMUM NUMBER OF SUBDAILY PERIODS     I*4
CC               FILSUB : FILE NAME OF SUBDAILY ERP MODEL FILE  CH*32
CC        OUT :  TITLE  : TITLE IN SUBDAILY ERP MODEL           CH*80
CC               SUBNAM : SUBDAILY ERP MODEL NAME               CH*16
CC               SUBFAR(I,J),I=1,..,6,J=1,..,6: COEFFICIENTS    R*8
CC                        TO COMPUTE FUNDAMENTAL ARGUMENTS
CC                        I=1,..5: TERMS WITH DT**(I-1) IN
CC                                 ARCSEC PER CENTURY ETC.
CC                        I=6    : NUMBER OF FULL REVOLUTIONS
CC                                 CORRESPONDING TO 1296000"
CC                        J= 1: L = MEAN ANOMALY OF THE MOON
CC                        J= 2: L'= MEAN ANOMALY OF THE SUN
CC                        J= 3: F = MEAN LONGITUDE OF THE MOON - O
CC                        J= 4: D = MEAN LONGITUDE OF THE MOON - MEAN
CC                                  LONGITUDE OF THE SUN
CC                        J= 5: O = MEAN LONGITUDE OF THE NODE OF THE
CC                                  MOON
CC                        J= 6: TP= GREENWICH MEAN SIDEREAL TIME
CC               NSUB   : NUMBER OF SUBDAILY ERP PERIODS         I*4
CC               SUBPER(K),K=1,..,NSUB: APPROX. SUBDAILY PERIODS R*8
CC                        IN DAYS
CC               SUBMLT(J,K),J=1,..,6,K=1,..,NSUB: MULTIPLIERS  I*4
CC                        OF THE FUNDAMENTAL ARGUMENTS FOR EACH
CC                        SUBDAILY TERM (PERIOD)
CC               SUBCOE(L,K),L=1,..,4,K=1,..,NSUB: COEFFICIENTS  R*8
CC                        OF SUBDAILY ERP MODEL IN ARCSEC AND SEC
CC                        L=1: COS PM
CC                        L=2: SIN PM
CC                        L=3: COS UT
CC                        L=4: SIN UT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  26-FEB-98
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               06-MAY-11 : HB: ADDITIONAL FORMAT (X,Y,UT1) FOR
CC                               SUB-FILE SUPPORTED, USE D_MODEL
CC               19-MAY-11 : HB: MODIFICATIONS FOR CALL OF SR SETMODKEY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE d_model, ONLY: setModKey, mod_orb_subFmt, chrValLength
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICOE  , iFmt  , II    , IOSTAT, IPER  , IREC  , JJ    ,
     1          MAXPER, NSUB
C
      CHARACTER(LEN=132)            :: LINE
      CHARACTER(LEN=80)             :: TITLE
      CHARACTER(LEN=fileNameLength) :: FILSUB
      CHARACTER(LEN=16)             :: SUBNAM
      CHARACTER(LEN=chrValLength)   :: setChr
      CHARACTER(LEN=8),PARAMETER    :: srName = 'RDSUBM  '
C
      REAL*8       SUBFAR(6,6),SUBPER(MAXPER),SUBCOE(6,MAXPER)
      REAL*8       SUBCOI(6),numVal
C
      INTEGER*4    SUBMLT(6,MAXPER)
C
C Initialization
C --------------
      iFmt = 0
      SUBFAR = 0.D0
      SUBPER = 0.D0
      SUBCOI = 0.D0
      SUBCOE = 0.D0

      SUBMLT = 0

C OPEN SUBDAILY ERP MODEL FILE
C ----------------------------
      CALL OPNFIL(LFNLOC,FILSUB,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSUB,'RDSUBM')
C
C READ TITLE LINES AND SUBDAILY ERP MODEL NAME
C --------------------------------------------
!!      READ(LFNLOC,1,END=910) TITLE,SUBNAM
!!1     FORMAT(A80,///,25X,A16,////////)

      READ(LFNLOC,1,END=910) TITLE
1     FORMAT(A80,/)
      READ(LFNLOC,2,END=910,ERR=920) line
      IF (line(1:15) == 'FORMAT VERSION:') THEN
        READ(line,'(15X,I2)') iFmt
      ELSE
        iFmt = 1
      ENDIF
      setChr = ' '
      numVal = iFmt*1.D0
      CALL setModKey(mod_orb_subFmt,setChr,srName,numVal)

      READ(LFNLOC,'(25X,A16,////////)',END=910) subNam
C
C READ COEFFICIENTS OF FUNDAMENTAL ARGUMENTS
C ------------------------------------------
      DO IREC=1,6
        READ(LFNLOC,2,END=910,ERR=920) LINE
2       FORMAT(A)
        READ(LINE(3:),*,ERR=920) (SUBFAR(II,IREC),II=1,6)
      ENDDO
      READ(LFNLOC,'(/////)')
C
C READ SUBDAILY TERMS: MULTIPLIERS, PERIOD, AND COEFFICIENTS
C ----------------------------------------------------------
      DO IPER=1,100000
        READ(LFNLOC,2,END=100,ERR=920) LINE
        IF (LINE.EQ.' ') GOTO 100
        IF (IPER.GT.MAXPER) THEN
          WRITE(LFNERR,930) MAXPER,FILSUB
930       FORMAT(/,' *** SR RDSUBM: TOO MANY PERIODS IN ',
     1             'SUBDAILY ERP FILE',
     2           /,16X,'MAXIMUM NUMBER OF PERIODS ALLOWED:',I5,
     3           /,16X,'SUBDAILY ERP FILE NAME           : ',A,/)
          CALL EXITRC(2)
        ENDIF
        IF (iFmt == 1) THEN
          READ(LINE,*,ERR=920) (SUBMLT(II,IPER),II=1,6),SUBPER(IPER),
     1                         (SUBCOI(JJ),JJ=1,4)
          SUBCOE(1,IPER)= -SUBCOI(1)
          SUBCOE(2,IPER)=  SUBCOI(2)
          SUBCOE(3,IPER)=  SUBCOI(2)
          SUBCOE(4,IPER)=  SUBCOI(1)
          SUBCOE(5,IPER)=  SUBCOI(3)
          SUBCOE(6,IPER)=  SUBCOI(4)
          DO ICOE=1,6
            SUBCOE(ICOE,IPER)=SUBCOE(ICOE,IPER)/1.D6
          ENDDO
          SUBPER(IPER)=SUBPER(IPER)/24.D0

        ELSEIF (iFmt == 2) THEN
          READ(LINE,*,ERR=920) (SUBMLT(II,IPER),II=1,6),SUBPER(IPER),
     1                         (SUBCOI(JJ),JJ=1,6)
          DO ICOE=1,6
            SUBCOE(ICOE,IPER)=SUBCOI(ICOE)/1.D6
          ENDDO
        ENDIF
      ENDDO
C
C END OF TERMS REACHED
C --------------------
100   CONTINUE
      NSUB=IPER-1
C
      CLOSE(UNIT=LFNLOC)
      GOTO 999
C
C END OF FILE REACHED
C -------------------
910   WRITE(LFNERR,911) FILSUB
911   FORMAT(/,' *** SR RDSUBM: UNEXPECTED END OF FILE',
     1       /,16X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C ERROR READING FILE
C ------------------
920   WRITE(LFNERR,921) FILSUB
921   FORMAT(/,' *** SR RDSUBM: ERROR READING FILE',
     1       /,16X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C END
C ---
999   RETURN
      END SUBROUTINE

      END MODULE
