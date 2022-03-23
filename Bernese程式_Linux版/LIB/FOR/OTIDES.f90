MODULE s_OTIDES
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE OTIDES(NMAX,BETA,CPOT,SPOT,NACT)

! -------------------------------------------------------------------------
! Purpose    :  COMPUTE MODIFIED NORMALIZED COEFFICIENTS CPOT AND
!                SPOT OF THE GRAVITY FIELD DUE TO OCEAN TIDES
!                THE COEFFICIENTS HAVE TO BE SUPPLIED IN A FILE
!                USING THE CSR/UTX FORMAT
!
! Author:     H. Bock
!
! Remark:     F90-Version of old SR OTIDES.f
!
!  AUTHOR     :  G.BEUTLER
!
!  CREATED    :  96/08/24
!
!  CHANGES    :  27-MAY-98 : JJ: MOVE DATA TO AFTER INCLUDE
!                28-JAN-03 : RS: CSPM REAL*4 -> REAL*8
!                06-JUL-04 : RD: FORMAT STATEMENT CORRECTED
!                10-AUG-04 : HU: READ NEW CSR 3.0 FILE
!                08-Sep-05 : HB: CPOT, SPOT starting with index 1 not 4,
!                                love numbers (knmp), starting with index 0
!
! Created:    06-Dec-2004
!
! Changes:    28-Dec-2005 HU: Error message added in case of too many
!                             cpot, spot
!             18-Oct-2006 HU: Allow for more terms than n=50
!             22-Mar-2007 HB: Allow 21000 instead of 3000 terms
!             29-Mar-2007 HB: Compute pre-factors Fnm already at first
!                             call of SR
!             07-Jun-2007 HB: Remove , in READ statement (ubecx-compiler)
!             06-Sep-2007 HB: Improve reading of old CSR-file
!                             Correct HPF-file algorithm
!             05-Aug-2008 DT: Use orbdsc from P_ORBGEN (%otdmin)
!                             -> remove XMIN from parameter list
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             20-Jan-2009 AJ: Correct HPF-file algorithm
!                             (assignment of CHI for different tides)
!             30-Mar-2009 HB: Use terms only if n > 1
!             04-May-2009 UM: write CSPM only if coefficients <= nmax
!                             apply factor 2.0 for order 0 and HPF-file
!             05-May-2009 UM: include EOT-format designed by Adrian
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!
! ---------------------------------------------------------------
! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, &
                      fileNameLength, shortLineLength, lineLength
  USE d_const,  ONLY: PI
  USE p_orbgen, ONLY: orbdsc

  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_exitrc
  USE f_lengt1
  USE s_gtflna
  USE s_opnerr

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b) :: nMax           ! MAXIMUM ORDER FOR DEVELOPMENT
  REAL(r8b),DIMENSION(:) :: beta ! DOODSON'S ARGUMENTS

! input/output:
  REAL(r8b),DIMENSION(:) :: cPot, sPot ! NORMALIZED COEFFICIENTS (UPDATED HERE)

! out:
  INTEGER(i4b)           :: nact  ! Actual number of terms

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'OTIDES'
  INTEGER(i4b), PARAMETER     :: maxTrm = 21000

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)  :: filNam
  CHARACTER(LEN=shortLineLength) :: title
  CHARACTER(LEN=shortLineLength) :: dummy, dummy2, prodTyp, key, modNam, &
       error, doodNum, norm
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=5)               :: proRet

  REAL(r8b),DIMENSION(:),ALLOCATABLE, SAVE :: KNMP
  REAL(r8b),DIMENSION(:),ALLOCATABLE, SAVE :: FNM
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE, SAVE :: FKT
  REAL(r8b),DIMENSION(4,MAXTRM), SAVE :: CSPM
  REAL(r8b), SAVE :: g, ge, rhow
  REAL(r8b) :: dumReal
  REAL(r8b) :: arg, cArg, sArg, rre, pfcn, FNMF
  REAL(r8b) :: ca, sp, xme, xxx, val1, val2, val3, val4
  REAL(r8b) :: chi
  REAL(r8b) :: m0_faktor

  INTEGER(i4b),SAVE :: iFirst = 1
  INTEGER(i4b),DIMENSION(6,MAXTRM), SAVE :: NDOD
  INTEGER(i4b),DIMENSION(2,MAXTRM), SAVE :: NM
  INTEGER(i4b), SAVE :: ntAct, iok, ntop, f_norm
  INTEGER(i4b) :: ind, ii, kk, iDum, iTerm,  iZer, iLine, mm
  INTEGER(i4b) :: iCoef, nWav, nTot, ioStat, inm, n, m, irc, nmx, mmx

  LOGICAL,SAVE :: utopia,hpf,eot


! READ THE INPUT FILE UPON FIRST CALL,
! INITIALIZE PRE-FACTORS
! ------------------------------------
  IF (IFIRST == 1) THEN
    NTOP  =0
    IFIRST=0
    m0_faktor=1.D0
    DO ii=1,MAXTRM
      DO kk=1,4
        CSPM(kk,ii)=0.D0
      END DO
    END DO
    CALL GTFLNA(0,'OTIDES ',FILNAM,IRC)
    IF(IRC /= 0)THEN
      WRITE(LFNERR,'(//,A,//)')' ### SR OTIDES: NO OTIDES-FILE FOUND'
      IOK=0
      RETURN
    ELSE
      IOK=1
    ENDIF
! INITIALIZE GRAVITY CONSTANT G, GRAV ACC ON EARTH SURFACE GE, WATER DENSITY
    G    = 6.67259D-11
    GE   = 9.780327D0
    RHOW = 1025.D0
    f_norm=0
    utopia=.FALSE.
    hpf=.FALSE.
    eot=.FALSE.
!
! OPEN OTIDES FILE
! ----------------
    CALL OPNFIL(LFNLOC,FILNAM,'OLD','FORMATTED','READONLY',' ',IOSTAT)
    CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,srname)
    READ(LFNLOC,'(A80)')TITLE
    READ(TITLE,*)dummy, prodTyp
    IF (dummy == 'product_type' .AND. prodTyp == 'ocean_tides') THEN

! Read header of ocean tides file
! -------------------------------
      DO
        line = nextline(lfnloc,1)
        IF (line(1:11) == 'end_of_head') EXIT
        IF (line       == ''   )         EXIT
        IF (line(1:9) == 'modelname') THEN
          READ(line,*)dummy,dummy2
          modNam = ADJUSTL(dummy2)
        ENDIF
        IF (line(1:22) == 'earth_gravity_constant') THEN
          READ(line,*)dummy2,dumReal
          CYCLE
        ENDIF
        IF (line(1:6) == 'radius') THEN
          READ(line,*)dummy2,RRE
          CYCLE
        ENDIF
        IF (line(1:10) == 'max_degree') THEN
          READ(line,*)dummy2, nmx
          mmx = nmx
          CYCLE
        ENDIF
        IF (line(1:6) == 'errors') THEN
          READ(line,*)dummy2,error
          CYCLE
        ENDIF
        IF (line(1:4) == 'norm') THEN
          READ(line,*)dummy2,norm
          IF (norm=='fully_normalized')f_norm=1
          CYCLE
        ENDIF
        IF (line(1:11) == 'water_density') THEN
          READ(line,*)dummy2,rhow
          CYCLE
        ENDIF
        IF (line(1:3) == 'key') THEN
          eot=.TRUE.
        ENDIF
      ENDDO
      ALLOCATE(KNMP(0:mmx),STAT=irc)
      CALL ALCERR(IRC,'knmp',(/mmx+1/),srname)
      ALLOCATE(FNM(0:mmx),STAT=irc)
      CALL ALCERR(IRC,'fnm',(/mmx+1/),srname)
      IF (f_norm==0) THEN
        ALLOCATE(FKT(0:mmx,0:mmx),STAT=irc)
        CALL ALCERR(IRC,'fkt',(/mmx+1,mmx+1/),srname)
        fkt=0.D0
      ENDIF
      KNMP=0D0
      fnm=0.D0
    ELSE
      READ(LFNLOC,'(4I4)')NWAV,NTOT,NMX,MMX

      ALLOCATE(KNMP(0:mmx),STAT=irc)
      CALL ALCERR(IRC,'knmp',(/mmx+1/),srname)
      ALLOCATE(FNM(0:mmx),STAT=irc)
      CALL ALCERR(IRC,'fnm',(/mmx+1/),srname)
      ALLOCATE(FKT(0:mmx,0:mmx),STAT=irc)
      CALL ALCERR(IRC,'fkt',(/mmx+1,mmx+1/),srname)
      KNMP=0D0
      fnm=0.D0
      fkt=0.D0

      MM=1+(MMX-1)/6

      READ(LFNLOC,'(I4)')IZER
      READ(LFNLOC,'(6F21.14)')RRE,RHOW,XME,PFCN,XXX
      DO II = 1,MM
        READ(LFNLOC,'(6F21.14)')(KNMP(KK),KK=(II-1)*6+1,MIN(II*6,MMX))
      ENDDO

! SKIP NEXT NWAV LINES
      DO II=1,NWAV
        READ(LFNLOC,'(I4)')IDUM
      ENDDO
    ENDIF

! COMPUTE PRE-FACTORS, II->n, KK->m
! ---------------------------------
    IF (f_norm==0) THEN
      DO II=0,mmx
        FKT(II,0)=1.D0
        DO KK=1,II
          FKT(II,KK)=(II+KK)*FKT(II,KK-1)*(II-KK+1)
        ENDDO
        DO KK=1,II
          FKT(II,KK)=DSQRT(FKT(II,KK)/(4*II+2))
        ENDDO
        FKT(II,0)=DSQRT(FKT(II,0)/(2*II+1))
      ENDDO
    ENDIF

! EXTRACT REQUIRED INFORMATION FROM NEXT NTOT LINES
! -------------------------------------------------
! Read coefficients (old and new OTIDES-file)
! -------------------------------------------
    ntAct = 0
    iLine = 0
    IF (prodTyp == 'ocean_tides') THEN
      IF (.NOT. eot) hpf=.TRUE.
    ELSE
      utopia=.TRUE.
    END IF
    DO
      line = nextline(lfnloc,0)
      IF (line == '') EXIT

! New GOCE-HPF file
      IF (hpf) THEN
        line = ADJUSTL(line)
        IF (line(1:5) == 'lovnr') THEN
          READ(line,*)key,n,dumreal
          IF(N > NMAX) CYCLE
          IF(n > ntop) ntop=n
          knmp(n) = dumreal
        ELSE
!alt          READ(line,*)key,n,m,proret,doodNum,val1,val2
          READ(line,'(A3,2I4,2X,A5,2X,A7,1X,E16.10,2X,F11.7)') &
           key,n,m,proret,doodNum,val1,val2
          IF (key  == 'ocs' .OR. key == 'ccs') THEN
            ca = val1
            sp = val2
          ELSEIF (key == 'oap' .OR. key == 'cap') THEN

! Assignment of CHI for different tides
! -------------------------------------
! Diurnal tide
            IF (doodNum(1:7) == '135.655') THEN        ! Q1
              chi = -90.D0
            ELSE IF(doodNum(1:7) ==  '145.555') THEN   ! O1
              chi =  -90.D0
            ELSE IF(doodNum(1:7) ==  '163.555') THEN   ! P1
              chi =  -90.D0
            ELSE IF(doodNum(1:7) ==  '165.555') THEN   ! K1
              chi =  90.D0

! Semi-diurnal tides
            ELSE IF(doodNum(1:7) ==  '235.755') THEN   ! 2N2
               chi =  0.D0
            ELSE IF(doodNum(1:7) ==  '245.655') THEN   ! N2
               chi =  0.D0
            ELSE IF(doodNum(1:7) ==  '255.555') THEN   ! M2
               chi =  0.D0
            ELSE IF(doodNum(1:7) ==  '273.555') THEN   ! S2
               chi =  0.D0
            ELSE IF(doodNum(1:7) ==  '275.555') THEN   ! K2
               chi =  0.D0

            ELSE IF(doodNum(1:7) ==  '455.555') THEN
               chi =  0.D0

! Long period tides
            ELSE IF(doodNum(1:7) ==  ' 55.565') THEN    ! Om1
               chi = 180.D0
            ELSE IF(doodNum(1:7) ==  ' 55.575') THEN    ! Om2
               chi =   0.D0
            ELSE IF(doodNum(1:7) ==  ' 56.554') THEN    ! Sa
               chi =   0.D0
            ELSE IF(doodNum(1:7) ==  ' 57.555') THEN    ! Ssa
               chi =   0.D0
            ELSE IF(doodNum(1:7) ==  ' 65.455') THEN    ! Mm
               chi =   0.D0
            ELSE IF(doodNum(1:7) ==  ' 75.555') THEN    ! Mf
               chi =   0.D0
            ELSE IF(doodNum(1:7) ==  ' 85.455') THEN    ! Mtm
               chi =   0.D0
            ELSE IF(doodNum(1:7) ==  ' 93.555') THEN    ! MSqm
               chi =   0.D0
            ELSE
               WRITE(lfnErr,'(A,/,A,/,A,A,/)')&
                    '*** SR OTIDES: No phase shift specified',&
                                   'for ocean tide WITH',&
                                   'doodson number ',doodNum(1:7)
               CALL exitrc(2)
            END IF

            sp = val1*dsin((val2-chi)*PI/180.D0)
            ca = val1*dcos((val2-chi)*PI/180.D0)

          ELSE
            write(lfnerr,'(A,/,A,A,/,A,A,/)')&
                 '*** SR OTIDES: Unknown or not supported keyword',&
                                'found in ocean tides file: ',trim(filnam),&
                                'Keyword: ',line(1:3)
            CALL exitrc(2)
          ENDIF
          nm(1,ntAct+1) = n
          nm(2,ntAct+1) = m
          IF (lengt1(doodNum) > 6) THEN
            READ(doodNum,'(3I1,1X,3I1)')(nDod(kk,ntAct+1),kk=1,6)
          ELSE
            READ(doodNum,'(2I1,1X,3I1)')(nDod(kk,ntAct+1),kk=2,6)
            nDod(1,ntAct+1) = 0
          ENDIF
          IF (proret == 'retro') THEN
            iCoef = 3
          ELSEIF (TRIM(proret) == 'pro') THEN
            iCoef = 1
          ENDIF
          IF (n<=nmax) then
            CSPM(iCoef,ntAct+1)   = ca
            CSPM(iCoef+1,ntAct+1) = sp
          END IF
        ENDIF

! Old OTIDES-file
      ELSEIF (utopia) THEN
        iLine = iLine + 1
        IF (iLine > nTot) EXIT
        READ(line,'(13X,3I1,1X,3I1,4X,2I2,2X,4D22.14)')     &
             (NDOD(KK,NTACT+1),KK=1,6),(NM(KK,NTACT+1),KK=1,2), &
             (CSPM(KK,NTACT+1),KK=1,4)
! New EOT-file
      ELSEIF (eot) THEN
        line = ADJUSTL(line)
        READ(line,*)doodNum,n,m,val1,val2,val3,val4
        nm(1,ntAct+1) = n
        nm(2,ntAct+1) = m
        IF (lengt1(doodNum) > 6) THEN
          READ(doodNum,'(3I1,1X,3I1)')(nDod(kk,ntAct+1),kk=1,6)
        ELSE
          READ(doodNum,'(2I1,1X,3I1)')(nDod(kk,ntAct+1),kk=2,6)
          nDod(1,ntAct+1) = 0
        ENDIF
        CSPM(1,ntAct+1) = val1
        CSPM(2,ntAct+1) = val2
        CSPM(3,ntAct+1) = val3
        CSPM(4,ntAct+1) = val4
      ENDIF

      IF(NM(1,NTACT+1) <= NMAX.AND.&
           (ABS(CSPM(1,NTACT+1)) > orbdsc%otdmin .OR.&
            ABS(CSPM(2,NTACT+1)) > orbdsc%otdmin .OR.&
            ABS(CSPM(3,NTACT+1)) > orbdsc%otdmin .OR.&
            ABS(CSPM(4,NTACT+1)) > orbdsc%otdmin ))THEN
        IF(nm(1,ntact+1) > ntop) ntop=nm(1,ntact+1)
        NTACT=NTACT+1
        DO KK=2,6
          NDOD(KK,NTACT)=NDOD(KK,NTACT)-5
        ENDDO
        INM=0
        IF(NM(2,NTACT) == 0)INM=1
        N=NM(1,NTACT)
        M=NM(2,NTACT)
! PRE-FACTOR FNM
        IF (FNM(N)==0.D0) FNM(N)=4*PI*G*RHOW/GE/100.D0*(1+KNMP(N))/(2*N+1)
        IF (f_norm==0) THEN
          FNMF=FNM(N)*FKT(N,M)
        ELSE
          FNMF=FNM(N)
        ENDIF
        IF (.NOT.eot) THEN
          DO II=1,4
            CSPM(II,NTACT)=CSPM(II,NTACT)*FNMF
          ENDDO
        ENDIF
      ENDIF
      IF(NTACT == MAXTRM)THEN
        WRITE(LFNERR,'(//,A,A,/,A,I6,//)')&
             ' ### SR OTIDES: NOT ALL TERMS AVAILABLE IN FILE ',trim(filnam),&
             '               COULD BE USED (MAXTRM TOO SMALL)',MAXTRM
        EXIT
      ENDIF
    ENDDO
    CLOSE (UNIT=LFNLOC)

  ENDIF

! COMPUTE OCEAN TIDE CONTRIBUTIONS TO ALL TERMS
! ---------------------------------------------
  IF (IOK == 1)THEN
    DO ITERM=1,NTACT
      N=NM(1,ITERM)
      M=NM(2,ITERM)

! ARGUMENT FOR CURRENT TERM
      ARG=NDOD(1,ITERM)*BETA(1)+NDOD(2,ITERM)*BETA(2)+ &
          NDOD(3,ITERM)*BETA(3)+NDOD(4,ITERM)*BETA(4)+ &
          NDOD(5,ITERM)*BETA(5)+NDOD(6,ITERM)*BETA(6)
      CARG=DCOS(ARG)
      SARG=DSIN(ARG)
      IND=N*(N+1)/2+M+1

      IF (IND.GT.SIZE(CPOT)) THEN
        WRITE(LFNERR,"(/,' *** SR OTIDES: Too many terms', &
                     & /,'                Index :',I6,     &
                     & /,'                Maxpot:',I6)")   &
                     IND,SIZE(CPOT)
        CALL EXITRC(2)
      ENDIF

! CONTRBUTION TO NORMALIZED TERMS
      IF (N > 1) THEN
        IF (m==0 .AND. hpf) THEN
          m0_faktor=2.D0
        ELSE
          m0_faktor=1.D0
        END IF
        IF (utopia .OR. hpf) THEN
          CPOT(IND)=CPOT(IND)+ &
                    m0_faktor*(CSPM(1,ITERM)+CSPM(3,ITERM))*CARG+ &
                    m0_faktor*(CSPM(2,ITERM)+CSPM(4,ITERM))*SARG
          SPOT(IND)=SPOT(IND)+ &
                    m0_faktor*(CSPM(2,ITERM)-CSPM(4,ITERM))*CARG- &
                    m0_faktor*(CSPM(1,ITERM)-CSPM(3,ITERM))*SARG
        ELSEIF (eot) THEN
          CPOT(IND)=CPOT(IND) + CSPM(3,ITERM)*CARG + CSPM(1,ITERM)*SARG
          SPOT(IND)=SPOT(IND) + CSPM(4,ITERM)*CARG + CSPM(2,ITERM)*SARG
        ENDIF
      ENDIF
!! 15.12.2004 sign from IERS1996 corrected, old/wrong:
!!    SPOT(IND)=SPOT(IND)+FNM*&
!!              ((CSPM(2,ITERM)+CSPM(4,ITERM))*CARG- &
!!               (CSPM(1,ITERM)-CSPM(3,ITERM))*SARG)
    ENDDO
  ENDIF
  NACT=NTOP
  RETURN
  END SUBROUTINE OTIDES

END MODULE
