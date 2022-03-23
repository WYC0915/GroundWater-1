C*
      PROGRAM POLXTR
CC
CC NAME       :  POLXTR
CC
CC PURPOSE    :  THIS PROGRAM EXTRACTS THE POLE VALUES OUT OF IERS POLE
CC               RESULT FILE FROM GPSEST. THE RESULTING FILE IS THE
CC               FILE FORMAT TO BE SUBMITTED TO IERS. THIS PROGRAM ALSO
CC               CREATES A PLOT FILE.
CC
CC REMARKS    :  ONLY FOR POLE FILES CONTAINING THREE EPOCHS
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  13-JUL-92
CC
CC CHANGES    :  27-JUL-92 : ??: ADD PARAMETER "NCOL" TO CALL PRFLNA
CC               10-SEP-92 : ??: CALL TO "PRFLNA" CHANGED
CC               17-SEP-92 : ??: USE UT1R FOR COMPUTATION OF SLOPE
CC               19-JUL-93 : ??: ADD LINE NUMBERS TO BE READ FROM GPSEST
CC                               RESULT FILES
CC               04-JAN-94 : ??: PLOTSKELETON NOT NECESSARY
CC               25-APR-94 : ??: CORRECTIONS FOR CPO-MODEL
CC               01-JUL-94 : SF: NEW IERS POLE FORMAT
CC               10-AUG-94 : MR: CALL EXITRC
CC               05-JUN-96 : TS: ADDED SUBDAILY POLE MODEL
CC               18-JUN-96 : TS: ADDED POLE STATISTICS
CC               25-JUN-96 : TS: CHANGED CALL OF STATIS SR.
CC               12-MAR-98 : TS: ADDITIONAL OUTPUT FOR POLE TESTS
CC               16-JUL-98 : DI: IGS/IERS POLE FORMAT 'VERSION 2'
CC               16-AUG-99 : JJ: RM UNUSED VARS LINE, POLC, RMSC
CC                               HLPSTR
CC               15-MAR-00 : TS: INCLUDED RATES FOR PREDICTIONS
CC               09-NOV-00 : CU: SWITCH TO NEW MENY SYSTEM
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               17-OCT-01 : HU: MAXFIL DYNAMIC, NEW OUTPUT STANDARD,
CC                               INDIVIDUAL RECORDS FOR FILES AND RANGES
CC                               OF RECORDS POSSIBLE
CC               19-DEC-01 : HU: ERROR IN DEBUG OUTPUT CORRECTED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               06-OCT-02 : HU: CALL OF RDIEPI MODIFIED
CC               05-FEB-03 : PS: CALL OF WTPOLH AND RDPOLH MODIFIED
CC               10-FEB-03 : PS: CONSISTECY CHECK FOR SUBDAILY POLE
CC                               AND NUTATION MODEL
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               09-OCT-03 : SS/HU: UT1-UTC ANCHOR POINT TAKEN FROM
CC                               LAST POLE FILE IN CASE OF PREDICTION
CC               06-NOV-03 : HU: NEW OPTIONS: FILELIST, OFFSET, ANCHOR
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               04-DEC-03 : RD/HU: ARRAYS DRIFTS,DRFTAP INITIALIZED
CC               15-JAN-04 : HU: GIVE EPOCHS IN ERROR MESSAGE
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-DEC-05 : HU: ERROR WITH LOD AND LEAPSEC FOR PREDICTION
CC               19-JUL-06 : HU: USE A PRIORI FOR PREDICTION, SPECIFY NUMBER
CC                               OF DAYS FOR FIT, STATISTICS ONLY FOR INPUT IEP
CC               20-JUL-06 : HU: INDEX FOR PREDICTION CORRECTED
CC               21-JUL-06 : HU: ROUNDING PROBLEM CORRECTED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               03-SEP-08 : HU/SS: POLOFF COMPUTED AT MIDNIGHT EPOCH
CC               17-SEP-08 : HU: USE RECORD "4" FOR "OFFSET AT LAST EPOCH"
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               11-MAY-11 : HB: SET MODEL KEYS FOR NUTNAM AND SUBNAM
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               12-MAR-12 : RD: USE SPLSTR AS MODULE NOW
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC               30-JUL-12 : RD: SR STATIS WITH OPTIONAL ARGUMENTS
CC               25-OCT-12 : SS: CONSIDER NUTNAM AND SUBNAM OF LASTEST FILE
CC               31-OCT-12 : SS: COMMENTED QUESTIONABLE CHANGES FROM 2008
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, fileNameLength, keyValueLength,
     1                    fileNameLength80, lfnPrt, lfnErr, lfnLoc,
     1                    lfnPlt, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE d_const,  ONLY: ars
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_model,  ONLY: setModKey,chrValLength,
     1                    mod_orb_subMod,mod_orb_nutMod
      USE s_wtskel
      USE s_opnfil
      USE s_pritit
      USE s_splstr
      USE s_readkeys
      USE s_cpodef
      USE s_rdiepi
      USE s_statis
      USE s_wtpolh
      USE s_wtpoli
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_gtfile2
      USE s_alcerr
      USE s_prflna
      USE s_poldef
      USE s_pxinpt
      USE s_readinpf
      USE s_opnerr
      USE s_ut1red
      USE s_rdpolh
      USE s_timstr
      USE s_polyap
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      USE s_wtiepi
      USE s_polinfo
      USE f_djul
      USE f_lincount
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , IEND    , IFIL    , IFILES  , IFOK    ,
     1          IFORM   , IHOUR   , II      , ILEAP   , ILIN    ,
     2          ILST    , IMONTH  , IOSTAT  , IOUTEP  , IPRE    ,
     3          IRC     , IRCIER  , IRCODE  , IRCPLT  , IRCPOL  ,
     4          IRCSKL  , IREAD   , IREC    , IREC1   , IREC2   ,
     5          ISOL    , ISTA    , IUTTYP  , IYEAR   , IFORMA  ,
     6          MAXCOE  , MAXFIL  , MAXFLD  , MIN     , NCOE    ,
     7          NFIL    , NLST    , NOUT    , NUMTST  , NFILP   ,
     8          NFIT
C
      REAL*8    DAY     , DTDRFT  , DUT     , DUT1    ,
     1          DUT1A   , DUT1E   , POLUT1  , POLUTA  , POTIME  ,
     2          RMS     , SEC     , T0PLT   , TDEXT   , TDIFF   ,
     3          TIMTMP  , TJUMP   , TLEAP   , TLEAPA  , TLEAPC  ,
     4          TLEAPG  , TFIRST  , TLAST   , TSTEP   , TPRED
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFLD=  1,MAXCOE= 10)
C
C MAXFLD: MAXIMUM NUMBER OF FIELDS IN PLOT SKELETON FILE
C MAXCOE: MAXIMUM NUMBER OF POLYNOMIAL COEFFICIENTS FOR EXTRAPOLATION
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=80)                                      :: titnew
      CHARACTER(LEN=80)                                      :: titapr
      CHARACTER(LEN=80),DIMENSION(:),ALLOCATABLE             :: title
      CHARACTER(LEN=80),DIMENSION(MAXFLD)                    :: fields
      CHARACTER(LEN=chrValLength)                            :: chrVal
      CHARACTER(LEN=fileNameLength)                          :: filier
      CHARACTER(LEN=fileNameLength)                          :: filpol
      CHARACTER(LEN=fileNameLength)                          :: filplt
      CHARACTER(LEN=fileNameLength)                          :: filskl
      CHARACTER(LEN=fileNameLength)                          :: fillst
      CHARACTER(LEN=fileNameLength80)                        :: filapr
      CHARACTER(LEN=fileNameLength80),DIMENSION(:),ALLOCATABLE :: filnam
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER   :: filinp
      CHARACTER(LEN=1),DIMENSION(:),ALLOCATABLE              :: flg
      CHARACTER*17  TSTRNG
      CHARACTER(LEN=16)                                      :: nutnam
      CHARACTER(LEN=16)                                      :: subnam
      CHARACTER(LEN=16)                                      :: nutnami
      CHARACTER(LEN=16)                                      :: subnami
      CHARACTER(LEN=16)                                      :: nutnam1
      CHARACTER(LEN=16)                                      :: subnam1
      CHARACTER(LEN=16)                                      :: nutnamc
      CHARACTER(LEN=16)                                      :: subnamc
      CHARACTER(LEN=255)                                     :: line
      CHARACTER(LEN=80),DIMENSION(6)                         :: strlst
      CHARACTER(LEN=8),PARAMETER :: pgName = 'POLXTR  '
      CHARACTER*7   CREC(5)
      CHARACTER*3   REMARK
      CHARACTER*1   SNGSTR
C
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: polapr
      REAL(r8b),DIMENSION(:,:),  ALLOCATABLE    :: gpsutc
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: polcoo
      REAL(r8b),DIMENSION(:,:),  ALLOCATABLE    :: poltim
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: rmscoo
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: polrat
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: rmsrat
      REAL(r8b),DIMENSION(:,:),  ALLOCATABLE    :: polsta
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: polcor
      REAL(r8b),DIMENSION(:,:),  ALLOCATABLE    :: drifts
      REAL(r8b),DIMENSION(:,:),  ALLOCATABLE    :: drftap
      REAL(r8b),DIMENSION(:,:),  ALLOCATABLE    :: polpre
      REAL(r8b),DIMENSION(:),    ALLOCATABLE    :: tpre
      REAL(r8b),DIMENSION(:),    ALLOCATABLE    :: vres
      REAL(r8b),DIMENSION(5)                    :: POLOFF
      REAL*8        MEAN(8),MEDIAN(8)
      REAL*8        RMSPOL(8),SIGMA(8)
      REAL*8        COOTMP(5),RATTMP(5),RMS1(5),RMS2(5)
      REAL*8        CORTMP(10)
      REAL*8        POLREF(5)
      REAL*8        CCC(MAXCOE),CRMS(MAXCOE)
      REAL*8        numVal
C
      INTEGER(i4b),DIMENSION(:,:),  ALLOCATABLE :: irecnr
      INTEGER(i4b),DIMENSION(:,:,:),ALLOCATABLE :: nrfpol
      INTEGER*4     IPLFLG(MAXFLD),IRECDF(5)
      INTEGER*4     NRFTMP(3),POLTYP(2),IPOEXT(4),USELST,ICOL(11)
      INTEGER*4     ANCHOR,OFFSET

C     Variables for readKeys
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: uniline
      INTEGER*4                                            :: ios
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(filinp)
      NULLIFY(uniline)
      CALL init_inpkey(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C WRITE TITLE AND FILE NAMES
C --------------------------
      CALL pritit ('POLXTR','Concatenate IERS pole files')
      CALL prflna
C
C READ INPUT OPTIONS
C ------------------
      CALL pxinpt(maxcoe,titnew,ifiles,iuttyp,anchor,
     &            offset,uselst,irecdf,ipoext,ioutep)
C
C READ FILE LIST AND RECORD NUMBERS (uniline) OR COUNT LINES IN INPUT LIST
C ---------------------------------
      IF (ifiles==2) THEN
        NFIL=linCount('EPLIST', 0)
        USELST=3
      ELSE
        CALL gtfile2('EPFIL', 1, nfil, filinp)
      ENDIF
C
C READ UNILINE
C ------------
      IF (ifiles==1.AND.uselst.EQ.1) THEN
        CALL Readkeys('RECLIST',uniline,irc)
        IF (irc /= 0) THEN
          WRITE(lfnerr,"(/,' *** PG POLXTR: Error reading file list')")
          CALL exitrc(2)
        ENDIF
        NLST=SIZE(uniline)
      ELSE
        NLST=NFIL
      ENDIF
C
C ALLOCATE MEMORY
C ---------------
      IF (NLST.GT.NFIL) THEN
        USELST=2
        NFIL  =NLST
      ENDIF
      maxfil=nfil+ipoext(2)

      ALLOCATE (title(maxfil),STAT=ios)
      CALL ALCERR(ios,'title',(/maxfil/),'POLXTR')
      ALLOCATE (filnam(maxfil),STAT=ios)
      CALL ALCERR(ios,'filnam',(/maxfil/),'POLXTR')
      ALLOCATE (flg(maxfil),STAT=ios)
      CALL ALCERR(ios,'flg',(/maxfil/),'POLXTR')

      ALLOCATE (polapr(5,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'polapr',(/5,5,maxfil/),'POLXTR')
      ALLOCATE (gpsutc(5,maxfil),STAT=ios)
      CALL ALCERR(ios,'gpsutc',(/5,maxfil/),'POLXTR')
      ALLOCATE (polcoo(5,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'polcoo',(/5,5,maxfil/),'POLXTR')
      ALLOCATE (poltim(5,maxfil),STAT=ios)
      CALL ALCERR(ios,'poltim',(/5,maxfil/),'POLXTR')
      ALLOCATE (rmscoo(5,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'rmscoo',(/5,5,maxfil/),'POLXTR')
      ALLOCATE (polrat(5,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'polrat',(/5,5,maxfil/),'POLXTR')
      ALLOCATE (rmsrat(5,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'rmsrat',(/5,5,maxfil/),'POLXTR')
      ALLOCATE (polsta(maxfil,8),STAT=ios)
      CALL ALCERR(ios,'polsta',(/maxfil,8/),'POLXTR')
      ALLOCATE (polcor(10,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'polcor',(/10,5,maxfil/),'POLXTR')
      ALLOCATE (drifts(5,maxfil),STAT=ios)
      CALL ALCERR(ios,'drifts',(/5,maxfil/),'POLXTR')
      ALLOCATE (drftap(5,maxfil),STAT=ios)
      CALL ALCERR(ios,'drftap',(/5,maxfil/),'POLXTR')
      ALLOCATE (polpre(maxfil,5),STAT=ios)
      CALL ALCERR(ios,'polpre',(/maxfil,5/),'POLXTR')
      ALLOCATE (tpre(maxfil),STAT=ios)
      CALL ALCERR(ios,'tpre',(/maxfil/),'POLXTR')
      ALLOCATE (vres(maxfil),STAT=ios)
      CALL ALCERR(ios,'vres',(/maxfil/),'POLXTR')

      ALLOCATE (irecnr(5,maxfil),STAT=ios)
      CALL ALCERR(ios,'irecnr',(/5,maxfil/),'POLXTR')
      ALLOCATE (nrfpol(3,5,maxfil),STAT=ios)
      CALL ALCERR(ios,'nrfpol',(/3,5,maxfil/),'POLXTR')
C
      DRIFTS=0D0
      DRFTAP=0D0
C
C USE DEFAULT RECORD NUMBERS
C --------------------------
      IF (uselst.EQ.0) THEN
        DO ifil=1,nfil
          filnam(ifil)=filinp(1,ifil)
          irecnr(1:5,ifil)=irecdf(1:5)
        ENDDO
C
C USE FILE SPECIFIC RECORD NUMBERS FROM INPUT FILE LIST
C -----------------------------------------------------
      ELSEIF (uselst.EQ.3) THEN
        CALL GTFLNA(1,'EPLIST',FILLST,IRC)
        CALL OPNFIL(LFNLOC,FILLST,'OLD','FORMATTED',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILLST,'POLXTR')
        DO ilst=1,nlst
          irc=0
C DECODE LINE
          READ(LFNLOC,"(A)",IOSTAT=ios)line
          irc=irc+ios
          CALL SPLSTR(line,6,' ',nout,strlst,ios)
          irc=irc+ios
          IF (nout.ne.6) irc=irc+1
          filnam(ilst)=strlst(1)
          DO ii=1,nout-1
            READ(strlst(ii+1),*,IOSTAT=ios) irecnr(ii,ilst)
            irc=irc+ios
          ENDDO
          IF (irc.NE.0) THEN
            WRITE(lfnerr,"(/,' *** POLXTR: Error reading file list',
     1                     /,'             File: ',A,
     2                     /,'             Line: ',A,/)")
     3                    TRIM(fillst),TRIM(line)
            CALL EXITRC(2)
          ENDIF
        ENDDO
        CLOSE(LFNLOC)
C
C USE FILE SPECIFIC RECORD NUMBERS FROM UNILINE
C ---------------------------------------------
      ELSE
C COPY FILENAMES TO ARRAY
        DO ifil=1,SIZE(filinp,2)
          filnam(ifil)=filinp(1,ifil)
        ENDDO
        IRECNR=0
        DO ILST=NLST,1,-1
          READ(uniline(ilst),*,IOSTAT=ios)fillst,crec(1:5)
          DO IREC=1,5
            READ(crec(irec),*,IOSTAT=ios)irecdf(irec)
            IF (ios .NE. 0. OR. irecdf(irec) .LE.0) THEN
              WRITE(lfnerr,"(/,' *** PG POLXTR: Invalid number found',//
     1                         ' in uniline, line',I4,
     2                       /,'                ',A)")ilst,uniline(ilst)
              CALL exitrc(2)
            ENDIF
          ENDDO
C
C UNILINE CONTAINS MORE ENTRIES THAN FILE LIST: COPY UNILINE
          IF (USELST.EQ.2) THEN
            filnam(ilst)=fillst
            irecnr(1:5,ilst) = irecdf(1:5)
          ELSE
C UNILINE CONTAINS LESS ENTRIES THAN FILE LIST: SEARCH FOR FILE IN LIST
            DO IFIL=1,NFIL
              IF (fillst.EQ.filnam(ifil)) irecnr(1:5,ifil) = irecdf(1:5)
            ENDDO
          ENDIF
        ENDDO

        DO IFIL=1,NFIL
          IF (irecnr(1,ifil).EQ.0) THEN
            irecnr(1:5,ifil) = irecdf(1:5)
          ELSE
            irecdf(1:5) = irecnr(1:5,ifil)
          ENDIF
        ENDDO
      ENDIF
C
C CHECK NUMBER OF USED FILES (INCLUDING PREDICTIONS)
C --------------------------------------------------
      NUMTST=NFIL+IPOEXT(2)
      IF (NUMTST.GT.MAXFIL) THEN
        WRITE(LFNERR,901) NUMTST,MAXFIL
901     FORMAT(/,' *** PG POLXTR: NUMBER OF FILES GREATER ' //
     1               'THAN "MAXFIL"',
     2         /,16X,'NUMBER OF REQUESTED FILES:',I5,
     3         /,16X,'MAX. NUMBER OF FILES     :',I5,
     4         /,16X,'INCREASE PARAMETER "MAXFIL"',/)
        CALL EXITRC(2)
      ENDIF
C
C DEFINE START AND END RECORD TO BE WRITTEN
C -----------------------------------------
      IF (IOUTEP.EQ.1) THEN
        IF (IUTTYP.EQ.2) THEN
          IREC1=4
          IREC2=4
        ELSE
          IREC1=3
          IREC2=3
        ENDIF
      ELSE
        IREC1=2
        IREC2=4
      ENDIF
C
C OPEN PLOT-FILE
C --------------
      CALL GTFLNA(0,'PLOTRS ',FILPLT,IRCPLT)
      IF (IRCPLT.EQ.0) THEN
        CALL OPNFIL(LFNPLT,FILPLT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILPLT,'POLXTR')
C
C WRITE HEADER FOR PLOT FILE (SKELETON KEY: A)
C --------------------------------------------
        CALL GTFLNA(0,'PLOTSKL',FILSKL,IRCSKL)
        IF (IRCSKL.EQ.0) THEN
          FIELDS(1)=TITNEW(1:61)
          IPLFLG(1)=1
          CALL WTSKEL('A  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        ENDIF
      ENDIF
C
C LOOP OVER ALL POLE FILES IN IERS FORMAT
C ---------------------------------------
      ILEAP=100000
C
      DO 100 IFIL=1,NFIL
C
C OPEN POLE RESULT FILE
C ---------------------
        CALL OPNFIL(LFN001,FILNAM(IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(IFIL),'POLXTR')
C
C READ POLE HEADER
C ----------------
        CALL RDPOLH(LFN001,2,TITLE(IFIL),POLTYP,IFORM ,IEND,
     1              nutnami,subnami)
        IF (nutnami.EQ.'') THEN
              WRITE(LFNERR,905) TRIM(FILNAM(IFIL))
              nutnami='IAU2000'
905           FORMAT(/,' ### PG POLXTR: NO NUTATION MODEL ',
     1           /,16X,'EXTRACTION POLE FILE: ',A,
     2           /,16X,'USING IAU2000 MODEL',/)
        ENDIF

        IF (subnami.EQ.'') THEN
              WRITE(LFNERR,906) TRIM(FILNAM(IFIL))
              subnami='RAY'
906           FORMAT(/,' ### PG POLXTR: NO SUBDAILY POLE MODEL ',
     1           /,16X,'EXTRACTION POLE FILE: ',A,
     2           /,16X,'USING RAY MODEL',/)
        ENDIF

        IF (IFIL.EQ.1) THEN
           nutnamc=nutnami
           subnamc=subnami
           chrVal= ' '
           chrVal(1:16)=nutnami
           CALL setModKey(mod_orb_nutMod,chrVal,pgName,numVal)
           chrVal(1:16)=subnami
           CALL setModKey(mod_orb_subMod,chrVal,pgName,numVal)
        ELSE
           IF (nutnami.NE.nutnamc) THEN
              WRITE(LFNERR,907) TRIM(FILNAM(IFIL)),nutnami,nutnamc
907           FORMAT(/,' ### PG POLXTR: NUTATION MODEL HAS CHANGED ',
     1           /,16X,'EXTRACTION POLE FILE : ',A,
     2           /,16X,'NUTATION MODEL       : ',A,
     3           /,16X,'NUTATION MODEL BEFORE: ',A,/)
              nutnamc=nutnami
           ENDIF
           IF (subnami.NE.subnamc) THEN
              WRITE(LFNERR,908) TRIM(FILNAM(IFIL)),subnami,subnamc
908           FORMAT(/,' ### PG POLXTR: SUBDAILY POLE MODEL',
     1                 ' HAS CHANGED ',
     2           /,16X,'EXTRACTION POLE FILE      : ',A,
     3           /,16X,'SUBDAILY POLE MODEL       : ',A,
     4           /,16X,'SUBDAILY POLE MODEL BEFORE: ',A,/)
              subnamc=subnami
           ENDIF
        ENDIF
        IF (IFIL.EQ.NFIL) THEN
           nutnam1=nutnami
           subnam1=subnami
           chrVal= ' '
           chrVal(1:16)=nutnami
           CALL setModKey(mod_orb_nutMod,chrVal,pgName,numVal)
           chrVal(1:16)=subnami
           CALL setModKey(mod_orb_subMod,chrVal,pgName,numVal)
        ENDIF

        IF (IEND.EQ.1) GOTO 910
        DO I=1,11
          ICOL(I)=12+I
        ENDDO
C
C READ THE THREE POLE COORDINATES GIVEN IN EACH RESULT FILE
C ---------------------------------------------------------
C INDICES: POLCOO(I,J,K)
C          I = 1,...,5:    Component, x, y, ut1-utc, deps, dpsi
C          J = 1,...,5:    Record
C          K = 1,...,NFIL: File number
C
        IREAD=0
        DO 90 ILIN=1,100000
          CALL RDIEPI(LFN001,IFORM,ICOL,TIMTMP,COOTMP,RATTMP,RMS1,
     1                RMS2,CORTMP,NRFTMP,IRCODE)
C
          IF(IRCODE.EQ.1) GOTO 910
C
          DO IREC=1,5
            IF (IRECNR(IREC,ifil).EQ.ILIN) THEN
              POLTIM(IREC,IFIL)=TIMTMP
              DO I=1,5
                POLCOO(I,IREC,IFIL)=COOTMP(I)
                RMSCOO(I,IREC,IFIL)=RMS1(I)
                POLRAT(I,IREC,IFIL)=RATTMP(I)
                RMSRAT(I,IREC,IFIL)=RMS2(I)
              ENDDO
              DO I=1,10
                POLCOR(I,IREC,IFIL)=CORTMP(I)
              ENDDO
              DO I=1,3
                NRFPOL(I,IREC,IFIL)=NRFTMP(I)
              ENDDO
              IREAD=IREAD+1
            ENDIF
          ENDDO
          IF (IREAD.EQ.5) GOTO 70
90      CONTINUE
C
C CLOSE FILE
C ----------
70      CLOSE(UNIT=LFN001)
C
C CHECK ORDER IN TIME
C -------------------
        IF (IFIL.GT.1) THEN
          IF (POLTIM(3,IFIL).LE.POLTIM(3,IFIL-1)) THEN
            WRITE(LFNERR,911)IFIL-1,TRIM(FILNAM(IFIL-1)),
     1                                                 POLTIM(3,IFIL-1),
     1                       IFIL  ,TRIM(FILNAM(IFIL)),POLTIM(3,IFIL)
911         FORMAT(/,' *** PG POLXTR: FILES NOT ORDERED IN TIME ',/,
     1                           16X,'FILE',I6,': ',A,F15.6,/,
     2                           16X,'FILE',I6,': ',A,F15.6,/)
            CALL EXITRC(2)
          ENDIF
        ENDIF
C
C HANDLE LEAP SECOND PROBLEM
C --------------------------
        TLEAP=DNINT(POLCOO(3,5,IFIL)-POLCOO(3,1,IFIL))
        IF (TLEAP.NE.0) POLCOO(3,5,IFIL)=POLCOO(3,5,IFIL)-TLEAP
C
C LOD COMPUTED FROM DIFFERENCE IN UT1-UTC OF RECORD IRECNR(2),IRECNR(4)
C ---------------------------------------------------------------------
        IF (IRECNR(2,ifil).EQ.0 .OR. IRECNR(4,ifil).EQ.0) THEN
          DO I=1,3
            POLRAT(I,3,IFIL)=0
          ENDDO
        ELSEIF (IUTTYP.EQ.1) THEN
          IF (POLTIM(4,IFIL) == POLTIM(2,IFIL)) THEN
            WRITE(lfnerr,"(/,' *** PG POLXTR: Epochs for UT1 ',
     1                        'integration are equal, rec 2 and 4',
     2                     /,'                File:',I6)") IFIL
            CALL exitrc(2)
          ENDIF
          DO I=1,3
            POLRAT(I,3,IFIL)=(POLCOO(I,4,IFIL)-POLCOO(I,2,IFIL))/
     1                         (POLTIM(4,IFIL)-POLTIM(2,IFIL))
          ENDDO
C
C LEAP SECOND
C -----------
          IF (POLCOO(3,4,IFIL)-POLCOO(3,2,IFIL).GE.0.5D0) THEN
            POLRAT(3,3,IFIL)=(POLCOO(3,4,IFIL)-POLCOO(3,2,IFIL)-1.D0)/
     1                                (POLTIM(4,IFIL)-POLTIM(2,IFIL))
          ENDIF
        ENDIF
C
C GET A PRIORI POLE VALUES AND CELESTIAL POLE OFFSETS (AND GPS-UTC)
C -----------------------------------------------------------------
        DO IREC=1,5
          CALL POLDEF(POLTIM(IREC,IFIL),0,POLAPR(1,IREC,IFIL),
     1                POLAPR(2,IREC,IFIL),POLAPR(3,IREC,IFIL),
     2                GPSUTC(IREC,IFIL))
          CALL CPODEF(POLTIM(IREC,IFIL),POLAPR(4,IREC,IFIL),
     1                POLAPR(5,IREC,IFIL))
          DO I=1,5
            IF (I.NE.3) THEN
              POLAPR(I,IREC,IFIL)=POLAPR(I,IREC,IFIL)*ars
            ELSE
              POLAPR(I,IREC,IFIL)=POLAPR(I,IREC,IFIL)*86400.D0
            ENDIF
          ENDDO
          GPSUTC(IREC,IFIL)=GPSUTC(IREC,IFIL)*86400.D0
C
C REDUCTION TO UT1R (RESULTS AND A PRIORI VALUES)
C -----------------------------------------------
          CALL UT1RED(POLTIM(IREC,IFIL),DUT)
          POLCOO(3,IREC,IFIL)=POLCOO(3,IREC,IFIL)-DUT/1000.D0
          POLAPR(3,IREC,IFIL)=POLAPR(3,IREC,IFIL)-DUT/1000.D0
        ENDDO
C
C HANDLE LEAP SECOND PROBLEM
C --------------------------
        TLEAP=DNINT(GPSUTC(5,IFIL)-GPSUTC(1,IFIL))
        IF (TLEAP.NE.0) THEN
          POLAPR(3,5,IFIL)=POLAPR(3,5,IFIL)-TLEAP
        ENDIF
        IF (IFIL.GT.1) THEN
          TLEAP=DNINT(GPSUTC(3,IFIL)-GPSUTC(3,IFIL-1))
          IF (TLEAP.NE.0.D0) THEN
            ILEAP=IFIL
            TJUMP=TLEAP
          ENDIF
        ENDIF
C
C DRIFT IN UT1-UTC , DEPS, AND DPSI
C ---------------------------------
        IF (POLTIM(4,IFIL) == POLTIM(2,IFIL)) THEN
          WRITE(lfnerr,"(/,' *** PG POLXTR: Epochs for integration',
     1                      'of drifts are equal, rec 1 and 5',
     2                  /,'                File:',I6)") IFIL
          CALL exitrc(2)
        ENDIF
        DO I=3,5
          DRIFTS(I,IFIL)=(POLCOO(I,5,IFIL)-POLCOO(I,1,IFIL))/
     1                   (POLTIM(5,IFIL)-POLTIM(1,IFIL))
          DRFTAP(I,IFIL)=(POLAPR(I,5,IFIL)-POLAPR(I,1,IFIL))/
     1                   (POLTIM(5,IFIL)-POLTIM(1,IFIL))
        ENDDO
C
C UT1-UTC TYPE 1: FOR IERS POLE SUBMISSION
C ----------------------------------------
        IF (IUTTYP.EQ.1) THEN
          IF (IFIL.EQ.1) THEN
            DO I=1,5
              DRIFTS(I,IFIL)=0.D0
              DRFTAP(I,IFIL)=0.D0
            ENDDO
          ELSE
            DO I=3,5
C
C TAKE FIRST VALUE OF UT1-UTC OF THE SECOND FILE AS REFERENCE VALUE
C -----------------------------------------------------------------
              IF (IFIL.EQ.2) THEN
                POLCOO(I,3,IFIL-1)=POLCOO(I,1,IFIL)
              ENDIF
C
              POLCOO(I,3,IFIL)=POLCOO(I,3,IFIL-1)+DRIFTS(I,IFIL)
            ENDDO
          ENDIF
          DO IREC=2,4
            IF (IFIL.EQ.ILEAP) POLCOO(3,IREC,IFIL)=
     1                         POLCOO(3,IREC,IFIL)+TJUMP
          ENDDO
C
C UT1-UTC TYPE 2 AND 3:
C HANDLE LEAP SECOND
C --------------------
        ELSEIF (IUTTYP.EQ.2 .OR. IUTTYP.EQ.3) THEN
          DO I=2,3
            TLEAPC=DNINT(POLCOO(3,I,IFIL)-POLCOO(3,I+1,IFIL))
            TLEAPA=DNINT(POLAPR(3,I,IFIL)-POLAPR(3,I+1,IFIL))
            TLEAPG=DNINT(  GPSUTC(I,IFIL)-  GPSUTC(I+1,IFIL))
            IF (TLEAPC.NE.TLEAPA) THEN
              CALL TIMSTR(1,POLTIM(I+1,IFIL),TSTRNG)
              WRITE(LFNERR,902) TRIM(FILNAM(IFIL)),TSTRNG
902           FORMAT(/,' ### PG POLXTR: INCONSISTENT LEAP SECONDS IN ',
     1                 'A PRIORI POLE FILE AND ',
     2           /,16X,'EXTRACTION POLE FILE AND GPS-UTC VALUE',
     3           /,16X,'EXTRACTION POLE FILE: ',A,
     4           /,16X,'EPOCH               : ',A,
     5           /,16X,'EXTRACTION POLE CORRECTED TO A PRIORI POLE',/)
              POLCOO(3,I+1,IFIL)=POLCOO(3,I+1,IFIL)-TLEAPC+TLEAPA
            ENDIF
C
            IF (TLEAPA.NE.TLEAPG) THEN
              CALL TIMSTR(1,POLTIM(I+1,IFIL),TSTRNG)
              WRITE(LFNERR,903) TSTRNG
903           FORMAT(/,' *** PG POLXTR: INCONSISTENT LEAP SECONDS IN ',
     1                 'A PRIORI POLE FILE',
     2           /,16X,'BETWEEN UT1-UTC AND GPS-UTC VALUES',
     3           /,16X,'EPOCH               : ',A,/)
              CALL EXITRC(2)
            ENDIF
          ENDDO
C
C UT1-UTC TYPE 2: EXTRACTION FOR ORBGEN
C TAKE LAST VALUE OF UT1-UTC AS REFERENCE VALUE
C -------------------------------------
          IF (IUTTYP.EQ.2) THEN
            DO I=3,5
              IF (IFIL.EQ.1) THEN
                POLREF(I)=POLAPR(I,2,IFIL)
              ENDIF
              IF (IFIL.EQ.NFIL .AND. I.EQ.3) THEN
                TDEXT=POLCOO(I,2,IFIL)-POLREF(I)
              ENDIF
              TDIFF=POLCOO(I,2,IFIL)-POLREF(I)
              DO IREC=2,4
                POLCOO(I,IREC,IFIL)=POLCOO(I,IREC,IFIL)-TDIFF
              ENDDO
              POLREF(I)=POLCOO(I,4,IFIL)
            ENDDO
C
C UT1-UTC TYPE 3: NO CONTINUITY IN UT1-UTC (ALIGNED TO A PRIORI UT1-UTC)
C ----------------------------------------------------------------------
          ELSE
            DO I=3,5
              TDIFF=POLCOO(I,3,IFIL)-POLAPR(I,3,IFIL)
              DO IREC=2,4
                POLCOO(I,IREC,IFIL)=POLCOO(I,IREC,IFIL)-TDIFF
              ENDDO
            ENDDO
          ENDIF
C
C INVALID UT1-UTC TYPE
C --------------------
        ELSE
          WRITE(LFNERR,904) IUTTYP
904       FORMAT(/,' *** PG POLXTR: INVALID UT1-UTC HANDLING OPTION',
     1           /,16X,'UT1-UTC OPTION:',I3,/)
          CALL EXITRC(2)
        ENDIF
100   CONTINUE
C
C PREDICTION OF POLE AND UT1-UTC VALUES
C -------------------------------------
      IF (IPOEXT(2).NE.0) THEN
C
C GET POLE INFORMATION
C --------------------
        CALL POLINFO(' ',filapr,titapr,iforma,
     1               nutnam,subnam,tfirst,tlast,tstep,tpred)
C
C SET THE TIME OF THE PREDICTED POLE VALUES
C -----------------------------------------
        DO IPRE=1,IPOEXT(2)
          IFIL=NFIL+IPRE
          FILNAM(IFIL)=' --- '
          TITLE(IFIL)='PREDICTION'
C
          DO IREC=2,4
            POLTIM(IREC,IFIL)=POLTIM(IREC,NFIL)+1.D0*IPRE
C
C INITIALIZE RECORDS NOT USED IN CASE OF PREDICTION
C -------------------------------------------------
            DO I=1,5
              POLRAT(I,IREC,IFIL)=0.D0
              RMSCOO(I,IREC,IFIL)=0.D0
              RMSRAT(I,IREC,IFIL)=0.D0
            ENDDO
            DO I=1,3
              NRFPOL(I,IREC,IFIL)=0
            ENDDO
            DO I=1,10
              POLCOR(I,IREC,IFIL)=0.D0
            ENDDO
C
C GET A PRIORI VALUES FOR PREDICTION TIMES
C ----------------------------------------
            IF (POLTIM(IREC,IFIL) .LE. TLAST+1D-5) THEN
              CALL POLDEF(POLTIM(IREC,IFIL),0,POLAPR(1,IREC,IFIL),
     1                    POLAPR(2,IREC,IFIL),POLAPR(3,IREC,IFIL),
     2                    GPSUTC(IREC,IFIL))
              CALL CPODEF(POLTIM(IREC,IFIL),POLAPR(4,IREC,IFIL),
     1                    POLAPR(5,IREC,IFIL))
              DO I=1,5
                IF (I.NE.3) THEN
                  POLAPR(I,IREC,IFIL)=POLAPR(I,IREC,IFIL)*ars
                ELSE
                  POLAPR(I,IREC,IFIL)=POLAPR(I,IREC,IFIL)*86400.D0
                ENDIF
              ENDDO
              GPSUTC(IREC,IFIL)=GPSUTC(IREC,IFIL)*86400.D0
C
C REDUCTION TO UT1R (RESULTS AND A PRIORI VALUES)
C -----------------------------------------------
              CALL UT1RED(POLTIM(IREC,IFIL),DUT)
              POLAPR(3,IREC,IFIL)=POLAPR(3,IREC,IFIL)-DUT/1000.D0
            ELSE
              POLAPR(1:5,IREC,IFIL)=0D0
              GPSUTC(IREC,IFIL)=GPSUTC(IREC,IFIL-1)
            ENDIF
          ENDDO
        ENDDO
C
C COPY POLE INFORMATION FOR PREDICTION
C ------------------------------------
        DO IFIL=1,NFIL
          FLG(IFIL)=CHAR(0)
          TPRE(IFIL)=POLTIM(3,IFIL)
          DO I=1,5
            POLPRE(IFIL,I)=POLCOO(I,3,IFIL)
          ENDDO
          POLPRE(IFIL,3)=POLCOO(3,3,IFIL)-GPSUTC(3,IFIL)
        ENDDO
C
C COPY A PRIORI INFORMATION FOR PREDICTION
C ----------------------------------------
        NFILP=NFIL
        IF (IPOEXT(1).EQ.1) THEN
C
C OFFSET AT LAST EPOCH
C (RECORD SHOULD CORRESPOND TO MIDNIGHT EPOCH OF LAST FILE)
          DO I=1,5
            POLOFF(I)=POLCOO(I,3,NFIL)-POLAPR(I,3,NFIL)
CC            POLOFF(I)=POLCOO(I,4,NFIL)-POLAPR(I,4,NFIL)
          ENDDO
C
C COPY A PRIORI AS INPUT FOR PREDICTION
          DO IPRE=1,IPOEXT(2)
            IFIL=NFIL+IPRE
            IF (POLTIM(3,IFIL) .LE. TLAST) THEN
              NFILP=IFIL
              FLG(IFIL)=CHAR(0)
              TPRE(IFIL)=POLTIM(3,IFIL)
              DO I=1,5
                POLPRE(IFIL,I)=POLAPR(I,3,IFIL)+POLOFF(I)
              ENDDO
              POLPRE(IFIL,3)=POLAPR(3,3,IFIL)-GPSUTC(3,IFIL)
CC              POLPRE(IFIL,3)=POLPRE(IFIL,3)-GPSUTC(3,IFIL)
              TITLE(IFIL)='PREDICTION (A PRIORI)'
            ENDIF
C
C COPY A PRIORI AS PREDICTION
            DO IREC=2,4
              POLCOO(1:5,IREC,IFIL)=POLAPR(1:5,IREC,IFIL)+POLOFF(1:5)
            ENDDO
          ENDDO
        ENDIF
C
C START OF FIT INTERVAL
C ---------------------
        NFIT=1
        IF (IPOEXT(3).GT.0) THEN
          DO IFIL=1,NFILP
            IF (TPRE(IFIL).GE.TPRE(NFILP)-IPOEXT(3)-1D-6) THEN
              NFIT=IFIL
              EXIT
            ENDIF
          ENDDO
        ENDIF
C
C ESTIMATE POLYNOMIAL COEFFICIENTS
C --------------------------------
        NCOE=IPOEXT(4)+1
C
        DO I=1,5
          CALL POLYAP(TPRE(NFIT:NFILP),POLPRE(NFIT:NFILP,I),
     1                FLG(NFIT:NFILP),NFILP-NFIT+1,NCOE,
     2                CCC,CRMS,VRES,RMS,IFOK)
C
C LOOP OVER DAYS TO BE PREDICTED
C ------------------------------
          DO IPRE=1+nfilp-nfil,IPOEXT(2)
            IFIL=NFIL+IPRE
            DO IREC=2,4
              POLCOO(I,IREC,IFIL)=POLPRE(NFILp,I)+
     1                            CCC(2)*(POLTIM(IREC,IFIL)-TPRE(NFILP))
              POLRAT(I,IREC,IFIL)=CCC(2)
              IF (I.EQ.3) THEN
                POLCOO(I,IREC,IFIL)=POLCOO(I,IREC,IFIL)+
     1                              GPSUTC(IREC,IFIL)
                POLRAT(I,IREC,IFIL)=0.0D0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
C END OF PREDICTION PART: INCREASE NFIL FOR SAVING
C ------------------------------------------------
        NFIL=NFIL+IPOEXT(2)
      ENDIF
C
C RETRANSFORM FROM UT1R TO UT1
C ----------------------------
      DO 105 IFIL=1,NFIL
C CHANGE ANCHOR POINT TO A PRIORI UT1-UTC TO LAST
        IF (ANCHOR.EQ.2) THEN
          DO IREC=2,4
            POLCOO(3,IREC,IFIL)=POLCOO(3,IREC,IFIL)+TDEXT
          ENDDO
        ENDIF
C COMPUTE OFFSET AS AVERAGE OF RECORDS 2 AND 4
        IF (OFFSET.EQ.1) THEN
          DO I=1,5
            POLCOO(I,3,IFIL)=(POLCOO(I,2,IFIL)+POLCOO(I,4,IFIL))/2D0
          ENDDO
        ENDIF
C TRANSFORM UT1R TO UT1
        DO IREC=2,4
          CALL UT1RED(POLTIM(IREC,IFIL),DUT1)
          POLCOO(3,IREC,IFIL)=POLCOO(3,IREC,IFIL)+DUT1/1000.D0
          POLAPR(3,IREC,IFIL)=POLAPR(3,IREC,IFIL)+DUT1/1000.D0
        ENDDO
C
C COMPUTE LOD IF UNKNOWN
C ----------------------
        IF (POLRAT(3,3,IFIL).EQ.0.0D0) THEN
          IF (POLTIM(4,IFIL) == POLTIM(2,IFIL)) THEN
            WRITE(lfnerr,"(/,' *** PG POLXTR: Epochs for LOD ',
     1                        'computation are equal, rec 2 and 4',
     2                     /,'                File:',I6)") IFIL
            CALL exitrc(2)
          ENDIF
C
C LEAP SECOND
C -----------
          TLEAP=IDNINT(POLCOO(3,4,IFIL)-POLCOO(3,2,IFIL))
C
C LOD
C ---
          DO IREC=2,4
            POLRAT(3,IREC,IFIL)=
     1                (POLCOO(3,4,IFIL)-POLCOO(3,2,IFIL)-TLEAP)/
     1                          (POLTIM(4,IFIL)-POLTIM(2,IFIL))
          ENDDO
        ENDIF
105   CONTINUE
C
C LIST OF GPSEST RESULT POLE FILE NAMES
C -------------------------------------
      WRITE(LFNPRT,2)
2     FORMAT(' ',131('-'),/,' LIST OF POLE RESULT FILES',/,
     1       ' ',131('-'),//,
     2       ' FILE  POLE RESULT FILENAME',14X,'EXTRACT REC',
     2               5X,'TITLE',/,
     3       ' ----',2X,32('-'),2X,14('-'),2X,75('-'),/)
      DO 110 IFIL=1,NFIL
        IF (FILNAM(IFIL) .EQ. ' --- ') THEN
        WRITE(LFNPRT,31) IFIL,FILNAM(IFIL)(1:32),TRIM(TITLE(IFIL))
31      FORMAT(I5,2X,A32,16X,2X,A)
      ELSE
        WRITE(LFNPRT,32) IFIL,FILNAM(IFIL)(1:32),IRECNR(1:5,IFIL),
     1                   TRIM(TITLE(IFIL))
32      FORMAT(I5,2X,A32,1X,5I3,2X,A)
      ENDIF
110   CONTINUE
C
      WRITE(LFNPRT,4)
4     FORMAT(/,1X,131('-'),/)
C
C WRITE PLOT FILE
C ---------------
      IF (IRCPLT.EQ.0) THEN
C
C REFERENCE TIME FOR PLOT
C -----------------------
        CALL JMT(POLTIM(3,1),IYEAR,IMONTH,DAY)
        T0PLT=DJUL(IYEAR,1,1.D0)
        DO 120 IFIL=1,NFIL
          DAY=POLTIM(3,IFIL)-T0PLT+1
          IF (IFIL.LT.ILEAP) THEN
            POLUT1=POLCOO(3,3,IFIL)
            POLUTA=POLAPR(3,3,IFIL)
          ELSE
            POLUT1=POLCOO(3,3,IFIL)-TJUMP
            POLUTA=POLAPR(3,3,IFIL)-TJUMP
          ENDIF
C
C CORRECT DRIFT FROM UT1R-UTC (LODR) TO UT1-UTC (LOD)
C ---------------------------------------------------
          IF (IFIL.NE.1) THEN
            DTDRFT=1.D0/24.D0
            CALL UT1RED(POLTIM(3,IFIL)-DTDRFT,DUT1A)
            CALL UT1RED(POLTIM(3,IFIL)+DTDRFT,DUT1E)
            DRIFTS(3,IFIL)=DRIFTS(3,IFIL)+(DUT1E-DUT1A)/
     1                               (2*DTDRFT)/1000.D0
            DRFTAP(3,IFIL)=DRFTAP(3,IFIL)+(DUT1E-DUT1A)/
     1                               (2*DTDRFT)/1000.D0
          ENDIF
C
          ISOL=1
          WRITE(LFNPLT,121) DAY,ISOL,POLCOO(1,3,IFIL),POLCOO(2,3,IFIL),
     2                               POLUT1,DRIFTS(3,IFIL)*1000.D0,
     3                               POLCOO(4,3,IFIL),POLCOO(5,3,IFIL),
     4                               DRIFTS(4,IFIL),DRIFTS(5,IFIL)
          ISOL=2
          WRITE(LFNPLT,121) DAY,ISOL,POLAPR(1,3,IFIL),POLAPR(2,3,IFIL),
     2                               POLUTA,DRFTAP(3,IFIL)*1000.D0,
     3                               POLAPR(4,3,IFIL),POLAPR(5,3,IFIL),
     4                               DRFTAP(4,IFIL),DRFTAP(5,IFIL)
121       FORMAT(F10.4,I2,8F15.8)
C
C ASSIGN VALUES TO POLSTA MATRIX FOR STATISTICAL PURPOSE
C ------------------------------------------------------
          POLSTA(IFIL,1)=(POLCOO(1,3,IFIL)-POLAPR(1,3,IFIL))*1.0D3
          POLSTA(IFIL,2)=(POLCOO(2,3,IFIL)-POLAPR(2,3,IFIL))*1.0D3
          POLSTA(IFIL,3)=(POLUT1-POLUTA)*1.0D3
          POLSTA(IFIL,4)=(DRIFTS(3,IFIL)-DRFTAP(3,IFIL))*1000.D0
          POLSTA(IFIL,5)=(POLCOO(4,3,IFIL)-POLAPR(4,3,IFIL))
          POLSTA(IFIL,6)=(POLCOO(5,3,IFIL)-POLAPR(5,3,IFIL))
          POLSTA(IFIL,7)=(DRIFTS(4,IFIL)-DRFTAP(4,IFIL))
          POLSTA(IFIL,8)=(DRIFTS(5,IFIL)-DRFTAP(5,IFIL))
120     CONTINUE
C
C WRITE LAST PART OF PLOT FILE
C ----------------------------
        IF (IRCPLT.EQ.0.AND.IRCSKL.EQ.0) THEN
          CALL WTSKEL('B  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
          CLOSE(UNIT=LFNPLT)
        ENDIF
      ENDIF
C
C CALCULATE STATISTICS WITH POLE DATA
C -----------------------------------
      DO 140 ISTA=1,8
        CALL STATIS(NFIL-IPOEXT(2),POLSTA(1,ISTA),
     1              xMed=MEDIAN(ISTA),xMean =MEAN(ISTA),
     2              xRms=RMSPOL(ISTA),xSigma=SIGMA(ISTA))
140   CONTINUE
C
C WRITE HEADER FOR STATISTICAL DATA
C ---------------------------------
      WRITE(LFNPRT,141) 'DIFF. PARAM.','MEAN','RMS','SIGMA'
141   FORMAT(A14,3A15,/,1X,59('-'))

C
C WRITE STATISTICAL DATA TO OUTPUT FILE
C -------------------------------------
      WRITE(LFNPRT,142) 'X-POLE', MEAN(1),RMSPOL(1),SIGMA(1)
      WRITE(LFNPRT,142) 'Y-POLE', MEAN(2),RMSPOL(2),SIGMA(2)
      WRITE(LFNPRT,142) 'UT1-UTC', MEAN(3),RMSPOL(3),SIGMA(3)
      WRITE(LFNPRT,142) 'DRIFT UT1-UTC', MEAN(4),RMSPOL(4),SIGMA(4)
      WRITE(LFNPRT,142) 'DEPS', MEAN(5),RMSPOL(5),SIGMA(5)
      WRITE(LFNPRT,142) 'DPSI', MEAN(6),RMSPOL(6),SIGMA(6)
      WRITE(LFNPRT,142) 'DRIFT DEPS', MEAN(7),RMSPOL(7),SIGMA(7)
      WRITE(LFNPRT,142) 'DRIFT DPSI', MEAN(8),RMSPOL(8),SIGMA(8)
142   FORMAT(A14,3F15.8)
      WRITE(LFNPRT,143) IDNINT(MEAN(1)*1D3),IDNINT(SIGMA(1)*1D3),
     1                  IDNINT(MEAN(2)*1D3),IDNINT(SIGMA(2)*1D3),
     2                  IDNINT(MEAN(4)*1D3),IDNINT(SIGMA(4)*1D3),NFIL
143   FORMAT(/,1X,56('-'),
     1       /,5X,'       X-POLE        Y-POLE         LOD        NDAY',
     2       /,5X,'     OFF   SIG     OFF   SIG     OFF   SIG         ',
     3       /,5X,'       (uas)         (uas)       (us/day)',
     4       /,1X,56('-'),
     5       /,1X,'SUM:',3(3X,I5,1X,I5),4X,I5,
     6       /,1X,56('-'))
C
C WRITE POLE FILE IN IERS FORMAT
C ------------------------------
      CALL GTFLNA(0,'IERSPOL',FILIER,IRCIER)
      IF (IRCIER.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILIER,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILIER,'POLXTR')
C
C WRITE POLE HEADER
        CALL WTPOLH(LFNLOC,2,TITNEW,POLTYP,nutnam1,subnam1)
        DO IFIL=1,NFIL
          DO IREC=IREC1,IREC2
C
C ROUND TO NEAREST MINUTE
            POTIME=POLTIM(IREC,IFIL)+30.D0/86400.D0
            CALL RADGMS(3,POTIME,SNGSTR,IHOUR,MIN,SEC)
            POTIME=DINT(POTIME)+IHOUR/24.D0+MIN/1440.D0
C
            CALL WTIEPI(LFNLOC,POTIME,POLCOO(1,IREC,IFIL),
     1                  POLRAT(1,IREC,IFIL),RMSCOO(1,IREC,IFIL),
     2                  RMSRAT(1,IREC,IFIL),POLCOR(1,IREC,IFIL),
     3                  NRFPOL(1,IREC,IFIL))
          ENDDO
        ENDDO
C
        CLOSE(UNIT=LFNLOC)
      ENDIF
C
C WRITE POLE FILE IN BERNESE FORMAT
C ---------------------------------
      CALL GTFLNA(0,'POLERS ',FILPOL,IRCPOL)
      IF (IRCPOL.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILPOL,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPOL,'POLXTR')
        CALL WTPOLH(LFNLOC,1,TITNEW,POLTYP,NUTNAM1,SUBNAM1)
        DO IFIL=1,NFIL
          IF (IFIL.GT.NFIL-IPOEXT(2)) THEN
            REMARK='EXT'
          ELSE
            REMARK='GPS'
          ENDIF
          DO IREC=IREC1,IREC2
            CALL WTPOLI(LFNLOC,POLTIM(IREC,IFIL),POLCOO(1,IREC,IFIL),
     1                  GPSUTC(IREC,IFIL),REMARK,RMSCOO(1,IREC,IFIL))
          ENDDO
        ENDDO
C
        CLOSE(UNIT=LFNLOC)
      ENDIF
      GOTO 999
C
C ERROR READING POLE FILE
C -----------------------
910   WRITE(LFNERR,921) TRIM(FILNAM(IFIL))
921   FORMAT(/,' *** PG POLXTR: END OF FILE REACHED BEFORE HAVING',/,
     1                     16X,'READ THREE POLE EPOCHS',/,
     2                     16X,'POLE FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C END
C ---
999   CONTINUE
      CALL EXITRC(0)
      END
