C*
      PROGRAM FMTSTD
CC
CC NAME       :  FMTSTD
CC
CC PURPOSE    :  TRANSFORM FORMATTED STAND. ORBIT AND RAD. PRESS.
CC               FILES INTO UNFORMATTED FILES.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.GU
CC
CC CREATED    :  91/01/29 12:37
CC
CC CHANGES    :  22-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               26-AUG-98 : LM: NEW FORMAT
CC               16-AUG-99 : JJ: RM UNUSED VAR RCOEFF, TOTREC, RPRPAR
CC               06-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               06-AUG-03 : HU: NEW STD FORMAT
CC               25-AUG-03 : CU: CHANGE FORMAT STRING
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               13-AUG-04 : CU: USE GTFILE2 INSTEAD OF FMTSTDFL
CC               04-APR-05 : AJ: ALLOW FOR NEW RPR-FORMAT
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL CHANGED
CC               14-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               27-APR-12 : RD: NULLIFY POINTERS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1991     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, fileNameLength,
     1                    lfnPrt, lfnErr, lfnRes, lfnOrb, lfnRpr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: MAXSAT
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_gtfile2
      USE s_opnfil
      USE s_defcon
      USE s_prflna
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_exitrc
      USE s_opnsys

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , IARC    , IDEG    , IFIL    , IFMT    ,
     1          II      , IINT    , IOSTAT  , IQ      , ISAT    ,
     2          IVAR    , K       , KK      , LL      , NARC    ,
     3          NINT    , NLIN    , NRCRPR  , NRCSTD  , NSAT    ,
     4          NVAR    , LFORM   , NSTC    , ISTC    , IFRC    ,
     5          KRCTYP  , NSTCEP  , INTSTC
C
      REAL*8    ELEPRESS, H       , HSEC    , SCALPA  , T0      ,
     1          TA      , TB      , TBHLP   , TBOUND  , TOSC1   ,
     2          ZERO    , ZERON   , TIMSTC  , PARSTC
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
C      PARAMETER (MAXFIL=100)
C
C MAXFIL: MAXIMUM NUMBER OF FILES
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: orbfil
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: rprfil
      CHARACTER*2  LINE1
      CHARACTER*1  SOURCE(10)
      CHARACTER*80 LINE
      character*8  anltyp
      integer*4    locq(6)
      real*8       zcoen(3)
C
      REAL*8       ELESAT(7),COEFF(3,MAXSAT)
C
      INTEGER*4    NAVNUM(MAXSAT)
      INTEGER(i4b) :: norbfil, nrprfil
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(rprfil)
      NULLIFY(orbfil)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C PRINT TITLE
C -----------
      CALL pritit('FMTSTD',
     1            'Convert STD and RPR files (ASCII to binary)')
      CALL prflna
C
C GET INPUT FORMATTED STANDARD ORBIT AND RAD. PRESS. FILE NAMES
C -------------------------------------------------------------
      CALL gtfile2('STDASCII',2,norbfil,orbfil)
      CALL gtfile2('RPRASCII',2,nrprfil,rprfil)
C
C TRANSFORMATION OF STANDARD ORBIT FILES
C --------------------------------------
C WRITE TITLE LINES FOR FILE NAME OUTPUT
      IF (norbfil > 0) WRITE(LFNPRT,1)
1     FORMAT(' File  ASCII standard orbit files',8X,
     1             'Binary standard orbit files       #Rec',
     2       /,' ----  ',32('-'),2X,32('-'),2X,'-----')
C
C LOOP OVER ALL FILES TO BE FORMATTED
C -----------------------------------
      DO IFIL = 1, NORBFIL
C
C OPEN STANDARD ORBIT FILE AND FORMATTED OUTPUT FILE
C --------------------------------------------------
        CALL OPNFIL(LFNRES,ORBFIL(1,IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,ORBFIL(1,IFIL),'FMTSTD')
C
        CALL OPNFIL(LFNORB,ORBFIL(2,IFIL),'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,ORBFIL(2,IFIL),'FMTSTD')
C
C READ NUMBER OF ARCS
        READ(LFNRES,300) NARC
        WRITE(LFNORB) NARC
300     FORMAT(2I4)
C
C READ FORMAT VERSION AND ORBIT DESCRIPTION
        IF (NARC.LT.0) THEN
          READ(LFNRES,300) IFMT,NARC
          WRITE(LFNORB) IFMT,NARC
          READ(LFNRES,300) NLIN
          WRITE(LFNORB) NLIN
          DO I=1,NLIN
            READ(LFNRES,"(A)") LINE
            WRITE(LFNORB) LINE
          ENDDO
        ENDIF
C
        NRCSTD=1
C
C READ OSCULATION EPOCH AND INTERVAL BOUNDARIES
        DO 500 IARC=1,NARC
          READ(LFNRES,301) NSAT,NINT,IQ
          READ(LFNRES,302) (NAVNUM(I),I=1,NSAT)
          READ(LFNRES,303) (SOURCE(I),I=1,10)
          WRITE(LFNORB) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT),
     1                 (SOURCE(I),I=1,10)
301       FORMAT(3I7)
302       FORMAT(24I3)
303       FORMAT(10A1)
C
C CHECK MAXIMUM NUMBER OF SATELLITES
          IF(NSAT.GT.MAXSAT) THEN
            WRITE(LFNERR,1003) NSAT,MAXSAT
1003        FORMAT(/,' *** PG FMTSTD: TOO MANY SATELLITES',/,
     1                           16X,'NUMBER OF SATELLITES:',I3,/,
     2                           16X,'MAXIMUM NUMBER      :',I3,/)
            CALL EXITRC(2)
          END IF
C
C READ OSCULATING EPOCH AND INTERVAL BOUNDARIES
          READ(LFNRES,*) TOSC1,ZERO
          WRITE(LFNORB) TOSC1,ZERO
          DO 51 I=1,NINT+1
            READ(LFNRES,*) TBOUND
            WRITE(LFNORB)  TBOUND
51        CONTINUE
C
C READ SET OF OSCULATING ELEMENTS
          DO 10 ISAT=1,NSAT
            DO 5 K=1,7
              READ(LFNRES, *) ELESAT(K)
5           CONTINUE
            WRITE(LFNORB) (ELESAT(K),K=1,7)
10        CONTINUE
C
C LOOP OVER ALL PARTIAL INTERVALS
C -------------------------------
          DO 200 IINT=1,NINT
C
C READ SET OF POLYNOMIAL COEFFICIENTS
            READ(LFNRES,*) T0,H
            WRITE(LFNORB) T0,H
            DO 30 I=1,IQ+1
              DO 20 ISAT=1,NSAT
                READ(LFNRES,*) (COEFF(K,ISAT),K=1,3)
20            CONTINUE
              WRITE(LFNORB) ((COEFF(K,ISAT),K=1,3),ISAT=1,NSAT)
30          CONTINUE
C
200       CONTINUE
C
C NUMBER OF RECORDS IN ARC
        NRCSTD=NRCSTD +4 +NINT+1 +7*NSAT +NINT*(1+(IQ+1)*NSAT)
C
500     CONTINUE
C
C CLOSE FILES
C -----------
        CLOSE(UNIT=LFNORB)
        CLOSE(UNIT=LFNRES)
C
C WRITE INPUT AND OUTPUT FILE NAMES AND NUMBER OF RECORDS
C -------------------------------------------------------
        WRITE(LFNPRT,2) IFIL,ORBFIL(1,IFIL),ORBFIL(2,IFIL),NRCSTD
2       FORMAT(I5,2X,A32,2X,A32,I7)
C
      ENDDO
C
C TRANSFORMATION OF RAD.PRESS.COEFFICIENT FILES
C ---------------------------------------------
C WRITE TITLE LINES FOR FILE NAME OUTPUT
      IF (nrprfil > 0) WRITE(LFNPRT,3)
3     FORMAT(/,' File  ASCII radiation pressure files',4X,
     1               'Binary radiation pressure files   #Rec',
     2       /,' ----  ',32('-'),2X,32('-'),2X,'-----')
C
C LOOP OVER ALL FILES TO BE FORMATTED
C -----------------------------------
      DO IFIL = 1, NRPRFIL
C
C OPEN RAD.PRESS. INPUT AND OUTPUT FILE
C -------------------------------------
        CALL OPNFIL(LFNRES,RPRFIL(1,IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,RPRFIL(1,IFIL),'FMTSTD')
C
        CALL OPNFIL(LFNRPR,RPRFIL(2,IFIL),'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRPR,IOSTAT,RPRFIL(2,IFIL),'FMTSTD')
C
C READ FORMAT
        READ(LFNRES,299) LINE1
299          FORMAT(1A2)
        IF(LINE1.EQ.'#P') THEN
          WRITE(LFNRPR) LINE1
          LFORM=1
        ELSE
          LFORM=0
          REWIND LFNRES
        END IF
C
C READ NUMBER OF ARCS
        READ(LFNRES,300) NARC
        WRITE(LFNRPR) NARC
C
        NRCRPR=1
C
C READ FIRST AND LAST OBSERVATION TIMES FOR ALL ARCS
C --------------------------------------------------
        DO 1500 IARC=1,NARC
          READ(LFNRES,301) NSAT,NINT,IQ
          READ(LFNRES,302) (NAVNUM(I),I=1,NSAT)
          READ(LFNRES,303) (SOURCE(I),I=1,10)
          WRITE(LFNRPR) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT),
     1                 (SOURCE(I),I=1,10)
          IF(NSAT.GT.MAXSAT) THEN
            WRITE(LFNERR,2003) NSAT,MAXSAT
2003        FORMAT(/,' *** PG FMTSTD: TOO MANY SATELLITES',/,
     1                           16X,'NUMBER OF SATELLITES:',I3,/,
     2                           16X,'MAXIMUM NUMBER      :',I3,/)
            CALL EXITRC(2)
          ENDIF
C
C*******************************************************************
          read(lfnres,'(3(e22.15,1x))') ta, tb, zeron
          write(lfnrpr) ta, tb, zeron

          read(lfnres,'(i22, 1x, a8)') nvar, anltyp
          write(lfnrpr) nvar, anltyp

          do ii = 1, nint
            read(lfnres,'(e22.15)') tbhlp
            write(lfnrpr) tbhlp
          end do

          read(lfnres,'(e22.15)') tbhlp
          write(lfnrpr) tbhlp

          do ii = 1, nsat
            do kk = 1, nvar
              read(lfnres,'(e22.15,1x,e22.15,6(I5,1x))')
     &               elepress, scalpa, (locq(ll),ll=1,6)
              write(lfnrpr) elepress, scalpa, (locq(ll),ll=1,6)
            end do
          end do

C READ & WRITE STOCH. PULSES
          IF(LFORM.EQ.1) THEN
            DO 47 ISAT=1,NSAT
              READ(LFNRES,'(I4)') NSTC
              WRITE(LFNRPR) NSTC
              DO 46 ISTC=1,NSTC
                IFRC=0
                DO 45
                  IFRC=IFRC+1
                  READ(LFNRES,'(I2,1x,I1,1x,I4,1x,e22.15,1x,e22.15)')
     1                        KRCTYP,NSTCEP,INTSTC,TIMSTC,PARSTC
                  WRITE(LFNRPR) KRCTYP,NSTCEP,INTSTC,TIMSTC,PARSTC
                  IF(IFRC.EQ.NSTCEP) GOTO 46
45              CONTINUE
46            CONTINUE
47          CONTINUE
          END IF

          do iint = 1, nint
            read(lfnres,'(e22.15,1x,e22.15)') tbhlp, hsec
            write(lfnrpr) tbhlp, hsec

            do isat = 1, nsat
              do ivar = 1, nvar
                do ideg = 1, iq+1
                  read(lfnres,'(3(e22.15,1x))') (zcoen(kk), kk=1,3)
                  write(lfnrpr) (zcoen(kk), kk=1,3)
                end do
              end do
            end do

          end do
C*******************************************************************
C
          NRCRPR=NRCRPR +4 +NINT+1 +NSAT +NINT*(1+3*(IQ+1)*NSAT)
C
1500    CONTINUE
C
C CLOSE FILES
C -----------
        CLOSE(UNIT=LFNRPR)
        CLOSE(UNIT=LFNRES)
C
C WRITE INPUT AND OUTPUT FILE NAMES AND NUMBER OF RECORDS
C -------------------------------------------------------
        WRITE(LFNPRT,4) IFIL,RPRFIL(1,IFIL),RPRFIL(2,IFIL),NRCRPR
4       FORMAT(I5,2X,A32,2X,A32,I7)
C
      ENDDO
C
      CALL EXITRC(0)
      END
