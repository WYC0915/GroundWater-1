C*
      PROGRAM GPSEST
CC
CC NAME       :  GPSEST
CC
CC PURPOSE    :  ESTIMATION OF RELEVANT PARAMETERS FROM GPS
CC               CARRIER PHASE OBSERVATIONS AND/OR CODE OBSERVATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, S.FANKHAUSER, S.SCHAER,
CC               L.MERVART, E.BROCKMANN
CC
CC CREATED    :  87/09/23 10:47
CC
CC CHANGES    :  08-MAY-91 : MAXLOC=2000, MAXAMB=4
CC               09-DEC-91 : ADD PARAMETER "NPAR" TO SUBROUTINE PRIEST
CC               11-FEB-92 : CHANGES DUE TO ERP-PARAMETER ESTIMATION
CC               20-FEB-92 : MAXFIL=250,MAXLOC=3000
CC               07-APR-92 : SUBROUTINE "TMIDLE": MIDDLE OF OBSERVATION
CC                           INTERVALS FOR SUBROUTINE "ORBINF"
CC               30-MAY-92 : ABSOLUTE AND RELATIVE TROPOS. SIGMAS
CC               04-JUN-92 : NEW PARAMETER IN CALL "ORBINF"
CC               25-JUN-92 : SR IERSAV INCLUDED (IERS FORMAT FOR ERP'S)
CC               18-JUL-92 : MAXSNG=40 (PROBLEM WITH SR BLDDBL)
CC               20-JUL-92 : WRITE SESSION NUMBER INTO THE OUTPUT FILE
CC               23-JUL-92 : ADD "NUMSAT" AND "NAVNUM" TO CALL SEQPAR
CC                           TO SET LOCQ FOR MANOUEVRE SATELLITES.
CC                           ADD "TBOUND" TO CALL PRCEPO
CC               28-JUL-92 : SATELLITE SPECIFIC WEIGHTS
CC               31-JUL-92 : IMPLEMENTATION OF "FARA" (RDINPT,INISES,
CC                           PRCEPO,AMBRES,PRIAPR)
CC               01-AUG-92 : ADD PARAMETER "IAPND" TO CALL IERSAV
CC               03-AUG-92 : OPTION TO ESTIMATE POLYNOMIAL FOR ERPS
CC               04-AUG-92 : PROBLEM OF MULTIPLE REFERENCE SATELLITES
CC                           FOR AMBIG. RESOLUTION STRATEGY 2
CC                           LM: NEW OPTION SIGAMB(4)
CC               10-AUG-92 : LM: OPTIMAL SELECTION OF REFERENCE AMB.
CC               14-AUG-92 : NO "CALL AMBREF" FOR AMB. PRE-ELIMINATION
CC               17-AUG-92 : INTRODUCE "NPSAVE(2)" FOR "NPVAL"
CC               10-NOV-92 : ADD PARAMETER "NPARN" TO CALL AMBSTO
CC               18-JAN-93 : CALL AMBRES AND PRIEST CHANGED TO PRINT
CC                           CORRECT REFERENCE AMBIGUITIES
CC               19-FEB-93 : NEW COMMON "MCMLOC". REDNEQ, AMBREF, AND
CC                           AMBSTO WITH ADDITIONAL PARAMETER "NPALCQ"
CC               25-FEB-93 : ADD PARAMETERS "RECTYP","ANTTYP","IANTEN"
CC                           TO CALL PRCEPO
CC               15-MAR-93 : MAXSTA INCREASED FROM 60 TO 70
CC               20-MAR-93 : INTRODUCTION OF STOCHASTIC ORBIT PARAMETERS
CC               20-MAR-93 : CONTINUITY OF EARTH ROTATION PARAMETERS
CC               03-APR-93 : ESTIMATION OF SATELLITE ANTENNA OFFSETS
CC               27-APR-93 : STORE NORMAL EQUATION MATRIX AND PREELIMI.
CC                           OF ANY PARAMETERS ARE POSSIBLE:
CC                           CALLS CHANGED: RDINPT,REDPAR,OBSINF,INISES,
CC                                          PRIEST,CHKOPT,PRIAPR,REDNEQ
CC                           NEW CALLS:     NEQSAV,(REDPAR)
CC               28-APR-93 : REDUCE NUMBER OF CONTINUATION LINES IN
CC                           CALL PRIAPR AND PRCEPO
CC               14-MAY-93 : ESTIMATION OF NEW PARAMETER TYPES
CC                           (POTENTIAL, HILL, ALBEDO, CENTER OF MASS)
CC               11-OCT-93 : NEW CALL OF NEQSAV (SAVE OF POTENTIAL,
CC                           HILL, ALBEDO, CENTER OF MASS)
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               18-DEC-93 : SET MAXSAS TO 9
CC               23-DEC-93 : STORE ANTENNA INFO IN NORMAL EQUATION
CC               27-DEC-93 : MR: SATELLITE OFFSETS WITH TIME WINDOWS
CC               13-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               13-APR-94 : LM: NEW AMBIGUITY RESOLUTION STRATEGY 4
CC               19-APR-94 : RW: CPO-MODEL PARAMETERS
CC               21-APR-94 : MR: NEW PARAMETER "SIGAPR" FOR SR SIGMA1
CC               23-JUN-94 : EB: NEW CALL NEQSAV
CC               01-JUL-94 : SF: NEW IGS ERP FORMAT
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               27-JUL-94 : MR: FLAGS FOR ESTIMATED PARAMETER TYPES
CC               27-JUL-94 : MR: REPLACE SR SYMIN8 BY SR SYMING
CC               29-JUL-94 : MR: OPTION FOR TROPOS. MAPPING FUNCTION
CC               05-AUG-94 : MR: UPDATE "NPARMS" FOR SINGULAR PARAM.
CC               09-AUG-94 : MR: CORRECT ERROR IN SECOND CALL TO REDNEQ
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               16-AUG-94 : SS: CALL CHKOPT
CC               02-SEP-94 : EB: NEW CALL SR NEQPRE
CC               20-SEP-94 : EB: NEW CALL CHKOPT
CC               23-SEP-94 : MR: ADD SR "CLOSES" TO CLOSE FILES
CC               19-OCT-94 : EB: NEW COMMON MXCPAR
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               24-JAN-94 : SF: CHANGES COMMENT FOR MAXFILES (DOS)
CC               28-FEB-95 : EB: NEW CALL REDNEQ
CC               14-MAR-95 : MR: REDUCE NUMBER OF CONTINUATION LINES
CC               07-APR-95 : MR: ADD NUMOBT,NUMMRT TO CALL HEDINF
CC               11-APR-95 : RW: INCLUDE SR STOECL
CC                               NEW SPECIAL STOCHASTIC PULSE HANDLING
CC               20-APR-95 : LM: TURBO VERSION (REDPAR)
CC               21-APR-95 : LM: SUPER TURBO VERSION (REDNEQ)
CC               25-APR-95 : EB: NEW CALL REDNEQ (COORDINATE PREELIMINATION)
CC               04-MAY-95 : GB: TURBO VERSION (SR REDPAR,REDNEQ,REDEPO)
CC               01-JUN-95 : SS: "MAXLOC" REMOVED FROM CALL SEQPAR, SEQEPO
CC               02-JUN-95 : LM: MOVE SR STOECL INTO SR OISTCI
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               13-JUN-95 : MR: MAXSNG=100
CC               14-AUG-95 : LM: MEMORY COMPACT VERSION
CC               17-AUG-95 : LM: MEMORY COMPACT VERSION 2
CC               21-AUG-95 : EB: NEW CALL NEQSAV (ICOADD)
CC               05-SEP-95 : LM: CORRECT INITIALIZATION
CC               25-SEP-95 : TVH: CHANGE CALL TO TROPSV OUTPUT ERRORS
CC               29-SEP-95 : MR: ADD "IEXTRA" TO CALL TROPSV
CC               30-OCT-95 : MR: ADD PARAMETER "MELWUB" TO CALL PRIEST
CC               31-OCT-95 : SS: CALL OF SR REDEPO
CC               01-NOV-95 : MR: ADD COMMA IN COMMON LARGE1
CC               02-NOV-95 : MR: ADD "FILNEQ" TO CALL NEQSAV
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               19-DEC-95 : SS: ADD GIM STUFF TO COMMON "LARGE5"
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7 INSTEAD OF I*4
CC               11-JAN-96 : GB: NEW ORBIT MODEL (MAXVAR, NEW DIMENSIONS
CC                               FOR SEQORB ETC)
CC               25-JAN-96 : SS: INCREASE "MAXFLS" FROM 28 TO 30
CC               19-FEB-96 : SS: INTRODUCE "NSTEFF"
CC               21-FEB-96 : TS: INCREASE "MAXFLS" FROM 30 TO 35
CC                               CONSEQUENTLY ALSO CHANGED SUBROUTINES:
CC                                - PRCEPO
CC                                - GOBSEP
CC                                - ADDNOR
CC               28-FEB-96 : MR: COMMON LARGE5 REORDERED (ALIGNMENT)
CC               26-MAR-96 : MR: ADD "CSESS" TO CALL PRCEPO AND NEQPRE
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS
CC               27-MAR-96 : TS: CLOCK/SLR CHANGES
CC               06-MAY-96 : TS: REMOVED OLD SATELLITE CLOCK STUFF
CC               06-MAY-96 : TS: ADDED NDIFF TO PRIAPR
CC               05-JUN-96 : LM: LINUX DIMENSIONS SEPARATELY
CC               06-JUN-96 : MR: DIMENSIONS FOR PC ADJUSTED
CC               08-JUN-96 : MR: DEC_OSF1 (NOT DEC_UNIX)
CC               17-JUN-96 : MR: MAXSNG=40 FOR VARIOUS PLATFORMS
CC               24-JUN-96 : TS: ALLOW ESTIMATED TROPOSPHERE INPUT
CC               26-JUN-96 : TS: INCREASED MAXPAR FROM 1700 --> 1800
CC               21-JUL-96 : TS: INCREASED MAXFLS FROM 55 --> 60
CC                5-AUG-96 : TS: INCREASED MAXFLS FROM 60 --> 65
CC               27-SEP-96 : TS: MAXFLS IN INCLUDE FILE
CC               23-OCT-96 : MR: CHANGE MAX.DIM MAXAMP,MAXPAR,MAXHIL
CC                               FOR PC AND LINUX
CC               13-NOV-96 : MR: ADD "IFRSES" TO CALL RESOUT TO HANDLE
CC                               CORRELATION STARTEGY 3 FOR RESIDUALS
CC               14-JAN-97 : MR: ADD "OBSFIL" TO CALL PRIAPR
CC               27-JAN-97 : MR: MINIMAL AND MAXIMAL ELEVATION ANGLE
CC               29-JAN-97 : TS: INCREASED MAXPAR FROM 1800 --> 2000
CC               30-JAN-97 : MR: ELEV.-DEP. OBS. WEIGHTING
CC               19-FEB-97 : MP: NAME OF THE SUBROUTINE 'TROPSV' CHANGED
CC                               TO 'TRPSAV', PARAMETER LIST OF 'GETNEQ'
CC                               AND 'NEQSAV'
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               14-APR-97 : SS: PRINTING OF TROPOSPHERE PARAMETERS
CC               15-APR-97 : SS: CHECK ELEVATION CUT-OFF ANGLE
CC               15-APR-97 : SS: WRITING OF SIGMA OF OBSERVATION
CC               17-APR-97 : SS: ADD "ZENMAX" TO CALL ATMSAV
CC               17-APR-97 : MR: ADD "STANUM" TO CALL TRPSAV
CC               18-JUN-97 : TS: INCREASED MAXPAR FROM 2000 --> 2100
CC               08-JUL-97 : LM: NO WEIGHTS IN NEQ-FILE
CC               23-JUL-97 : LM: EXPLICIT LOOPS (VERY UGLY !!!)
CC               03-AUG-97 : LM: NQ0 -- NEQs IN NEW FORMAT
CC               05-AUG-97 : LM: CONSTRAINTS FOR AMBIGUITY FIXING
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               07-AUG-97 : MR: NEW SUBROUTINE REDAMB AND ADD "OBSNUM"
CC                               TO CALL OF PRCEPO
CC               11-AUG-97 : SS: NEW OPTION "STRAMB(2)"
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               14-AUG-97 : SS: "IEPPAR" INTRODUCED
CC               08-SEP-97 : SS: ESTIMATE ZENITH SIP PARAMETERS
CC               22-SEP-97 : SS: 5-DEG BIN OBSERVATION STATISTICS
CC               23-SEP-97 : SS: IONEX VERSION 1.0
CC               26-SEP-97 : DI: USE MAXSAT.inc
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               08-OCT-97 : SS: RESIDUAL NORMALIZATION FLAG
CC               09-OCT-97 : TS: RESIDUAL NORMALIZATION IN RESOUT
CC               22-OCT-97 : SS: CODE BIAS I/O FILES
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               13-NOV-97 : SS: "MAXGIM" FROM 10 TO 300
CC               25-NOV-97 : TS: MAXSAS IN INCLUDED FILE
CC               22-DEC-97 : SS: STATION-SPECIFIC GIMS
CC               05-JAN-98 : TS: ADDED "IZEROD" TO CALL OF RDINPT
CC               21-JAN-98 : SS: "MAXSNG" FROM 85 TO 155
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               26-JAN-98 : SS: RELATIVE SIGMA FOR GIMS
CC               30-MAR-98 : TS: SIMULTANEOUS CODE AND PHASE ZD PROCESSING
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: "INFGIM" ADDED
CC               08-JUN-98 : TS: NORMALIZED RESIDUALS FOR EPOCH RESIDUALS
CC               23-JUN-98 : MR: ADD "MIXED" TO CALL HEDINF
CC               25-JUN-98 : HH: MODIFICATIONS FOR GLONASS
CC               14-OCT-98 : MR: ORBIT ESTIMATION GPS/GLONASS ONLY
CC               21-APR-99 : SS: "MAXSTA" FROM 100 TO 150
CC               04-AUG-99 : PF: ADD "TIMCRD" TO CALL CORSAV
CC               06-AUG-99 : JJ: ADD "C_LAND_F90"
CC               13-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) INTO I:GPSEST
CC               19-AUG-99 : JJ: C_LANG_F90 FOR VARS STOTYP, SVNSTO, STOBND,
CC                               TSTOCH, FILNQ0
CC               27-JAN-00 : TS: CHANGES FOR CLOCK RINEX OUTPUT
CC               17-APR-00 : RD: APRIORI AMBIG. FOR ZERO-DIFF SOLUTIONS
CC               25-JUL-00 : LM: NEW PARAMETERS IN OBSINF AND RDINPT
CC               28-AUG-00 : MR: A PRIORI TROP. OPTIONS (NIELL,DRY_NIELL)
CC               25-OCT-00 : RD: COUNT OBS. FOR CLOCKS
CC               15-MAR-01 : DS: LEO "ZD" AND "DD" PROCESSING
CC               15-MAY-01 : DS: LEO POD
CC               24-AUG-01 : MR: ADD A POSTERIORI RMS TO SR RESEPO
CC               01-DEC-01 : MR: SET NREF=0 IF NO REF. AMBIGUITIES DEFINED
CC               15-MAY-01 : RD: USE F90 SR FOR WRITING THE CLK RINEX FILE
CC               27-JUN-01 : RD: ADD MAXGIM,MAXFIL TO THE PARAMETER OF RDINPT
CC               28-JUN-01 : RD: USE A LIST OF SATELLITES FROM ALL FILES
CC               28-JUN-01 : RD: READ CLK HEADER INFO FROM THE INPUT FILE
CC               09-AUG-01 : HU: INTERFACE OF RDINPT MOVED TO I_ASTLIB
CC               20-DEC-01 : SS: PASS GIM VARIABLES TO SR NEQWRITE
CC               22-DEC-01 : HU: INTERFACE TO NEQWRITE ADDED
CC               22-JAN-02 : RD: CONDITION OF SUM FOR REFERENCE CLOCK
CC               23-JAN-02 : RD: REMOVE ZAMBIG
CC               25-JAN-02 : RD: WRITE CLOCK RINEX FILE IN CLKSAV
CC               09-FEB-02 : RD: WEIGHT REAL*4->REAL*8
CC               08-FEB-02 : MM: REMOVED "SIGTRS" FORM TRPSAV CALL
CC               18-FEB-02 : RD: ADD CLKOBS TO THE CALL OF CLKSUM
CC               07-MAY-02 : SS: DCB UPDATE
CC               27-JUN-02 : RD/DS: MERGING VERSIONS BE AND MUNICH
CC               15-JUL-02 : DS: WRITE KINEMATIC COORDINATS IN KINSAV
CC               07-AUG-02 : HU: REMOVE WRITING OF OLD NEQS
CC               04-SEP-02 : RD: SKIP OBS. IF NO SAT-CLK AVAILABLE
CC               16-SEP-02 : SS/MR: ENABLE NEQ SAVING IN CASE OF AMBRES
CC               18-SEP-02 : DS: KINEMATIC ESTIMATION: LEO,AIRPLANE,
CC                               SHIP, GROUND STATIION, "STATIC" GROUND
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               13-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               29-NOV-02 : RD: CORRECT USAGE OF CLKSUM IN ALL(?) CASES
CC               10-DEC-02 : CU: NEW PARAMETER WGTFILE (A PRIORI CRD SIGMA FILE)
CC               28-Jan-03 : RD: NUMBER OF OBS. FOR KIN. POS.(CLKOBS->NEPOBS)
CC                               NEW CALL OF CHKOPT (RESEPO ONLY IF NECESSARY)
CC                               IMPROVED LOGIC FOR AVAIL. OF EPOCH RESULTS
CC               03-FEB-03 : PS: NUTNAM AND SUBNAM INTRODUCED, SAVE IN NEQ
CC               17-FEB-03 : LM: USE M_MAXDIM, P_GPSEST
CC               05-MAR-03 : CU: USE IPART FOR SR PRIBLH,PRIDIS,GPHELM
CC               07-MAR-03 : MR: MAXAMB MOVED TO M_MAXDIM
CC               14-MAR-03 : HU: ARCINT2 INITIALIZED TO 1
CC               28-MAR-03 : RD: NEW CALL OF SR PRIAPR, ADD INTERFACE
CC               08-APR-03 : RD: NPSAVE->HPSAVE (NOT NUMBER BUT HOURS)
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               30-APR-03 : SS: SATELLITE SYSTEM SELECTION
CC               06-MAY-03 : MM: NEW TRPSAV CALL
CC               16-MAY-03 : HB: INITIALIZE STRUCTURE
CC               19-MAY-03 : RD: NEW CALL OF SR SEQPAR
CC               26-MAY-03 : RD: NEW CALL OF SR AMBRES
CC               28-MAY-03 : RD: ADD STNAME TO SR NEQPRE
CC               10-MAY-03 : RD: MSG: RESIDRS AND RESEPO-KIN ARE BEFORE AMBRES
CC               19-JUN-03 : RD: ADD IRAUX2 TO SR KINSAV
CC               24-JUN-03 : RD: ADD IZEROD TO SR RESEPO
CC               27-JUN-03 : SS: PASS MAXSTA TO SR OBSINF
CC               22-JUL-03 : RD: CONSTRAIN KIN.POS. IN SR WGTKIN
CC               11-AUG-03 : RS: ADD ANTTYP TO CALL OF SR PRIAPR
CC               14-AUG-03 : RS: CHANGE CALLS OF NEQWRITE, CHKOPT, SEQPAR AND
CC                               PHCSAV
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC                               USE PRITIT
CC               10-NOV-03 : RS: ADD RAPZENMAX, CHANGE CALL OF CHKOPT, ADD
CC                               GNROFF, CHANGE CALLS OF RDINPT, SEQPAR, PRIAPR
CC                               AND PHCSAV
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-NOV-03 : RS: ADD NADMAX AND RAPZENMAX TO CALL OF PHCSAV
CC               19-JAN-03 : SS/MM: REVISION OF GPSEST INPUT PANELS
CC               21-Jan-04 : RD: PROCESS ONLY A RANGE OF PARAMETERS IN CLKSUM
CC               27-JAN-04 : HU: EXCLUSION OF ECLIPSING SATELLITE OBSERVATIONS
CC               23-FEB-04 : SS: ADJUST NPARMS PRIOR TO NEQ SAVING
CC               08-MAR-04 : RD: SYNCHRONIZE ISASYS AND MIXED IN HEDINF
CC               29-MAR-04 : CU: ERROR MESSAGE IF TRP ESTIMATION FOR RANGE
CC                               MEASUREMENTS REQUESTED,
CC                               ADD MEATYP TO CALL OF SR NEQWRITE
CC               13-APR-04 : RS: ADD OBSERVATION STATISTICS FOR THE AZIMUTH
CC                               ANGLE AZISOK AT THE SATELLITE
CC               08-JUL-04 : RD: MAKE ARRAYS FOR QBLH AND QDIS ALLOCATABLE
CC                               MAKE WEIGHT ARRAY ALLOCATABLE
CC               16-JUL-04 : RD: UPDATE CLKHED%STACOORD IN CORSAV
CC               20-OCT-04 : RD: DYNAMIC ARRAYS
CC               09-NOV-04 : RD: USER INTERFACE FOR PARAMETER DIMENSIONS
CC               20-MAY-05 : RD: MXCEQN+1, FOR VERY SMALL OBS. FILES
CC               26-MAY-05 : RD: USE DIMENSION MAXSGR FOR SATOFF AND SATSPV
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               12-AUG-05 : RD: MORE CORRECT INIT OF SOME ARRAYS
CC               15-AUG-05 : RD: NEW CALL OF SR GPHELM
CC               05-OCT-05 : RD: ADJUST "MAXSNG" FOR RESEPO
CC               11-JAN-06 : RD: STATISTICS ON PHASE-CONNECTED EPOCHS
CC               03-FEB-06 : RD: ENABLE MODULE FOR SIGMA1
CC               09-FEB-06 : HB: REMOVE IDEL AS PARAMETER FROM SR AMBCST
CC               07-JUN-06 : RD: COMPUTE NO SOLUTION
CC               29-JUN-06 : HB: XXX0 AS NEW ARRAY FOR A PRIORI VALUES
CC                               OF PARAMETERS
CC               24-AUG-06 : AG: TRPLMS FOR PRIEST CALL
CC               18-SEP-06 : HU: POLARIZATION EFFECT START EPOCHS
CC               16-OCT-06 : RD: MANUAL SELECTION OF SAT. FOR ORBIT DETERM.
CC               07-DEC-06 : HB  ICLU AS PARAMETER FOR INISES AND PRCEPO
CC               08-FEB-07 : RD: NEW CALL OF COVSAV AND COVSV1
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               08-MAY-07 : SS: GLONASS AMBIGUITY RESOLUTION ENABLED
CC               12-JUN-07 : AG: COMPILE LIST FOR ANTENNA BUFFER INITIALIZATION
CC               17-JUN-07 : RD: ONLY PHASE FOR RESUBSTITUTION OF EPO-PARAM.
CC               18-JUN-07 : RD: ICOELV INDIV. FOR EACH MEATYP
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL FOR GTSCLK
CC               11-DEC-07 : HB: SPECIAL HANDLING OF SLR STATIONS
CC               23-JAN-08 : RD: RAO+RAP ADDED TO NEQ
CC               29-JAN-08 : SL: MORE PRINTING OPTIONS
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC               29-APR-08 : DT: ADD RANGE OBSERVATIONS; USE SR OBSFILLST
CC               05-MAY-08 : DT: ADD IDIFF TO CHKOPT
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               04-AUG-08 : DT: Use MTypeSLR (D_STACRX) instead of sta_slr
CC               16-OCT-08 : SL: TIMREF added to AMBRES call
CC               28-OCT-08 : DT: Use maxVar from M_MAXDIM
CC               05-NOV-08 : AJ: PASS ZMXLEO TO SR PRIAPR
CC               09-FEB-09 : HB: INDEX FOR COPYING XREF CORRECTED
CC               02-Apr-09 : DT: Add Range Biases (new calls INIMAX,RDINPT,
CC               04-May-09 : RD: CONSIDER CORRECTIONS FROM VIENNA GRID FILES
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               27-MAY-09 : RD: SPECIAL SAMPLING FOR RESUBST. OF EPOCH PARAM.
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               22-Jul-09 : DT: Add timIntF (Call to OBSINF)
CC               09-DEC-09 : RD: ECLIPSING FLAG FOR CLOCK RESULTS ADDED
CC               04-JAN-10 : SL: HOI SCALING PARAMETERS ADDED
CC               15-APR-10 : DT: ADD NOBSPA TO CALL OF NORINI AND CORSAV
CC               27-May-10 : RD: READ ISASYS ALREADY IN OBSINF
CC               13-JUL-10 : DT: Call of STDORB2NEW only if Hill, Albedo or
CC                               Gravity estimation (Call after RDINPT)
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY, REMOVAL OF UNUSED MOD
CC               16-Nov-10 : RD: Distinguish between piece-wise linear param.
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               26-NOV-10 : RD: REDUCE NANRAO/NANCAL TO THE OBSERVED SET
CC               03-DEC-10 : HB: ADD NMXINT,NMXSAP,NMXARC TO SR NORINI AND
CC                               STDSAV/STDSAV2 FOR SMALLER ARRAYS
CC               09-DEC-10 : HB: NMXSAP=MAXSAT IF STOCHASTIC PARAMATERS FOR
CC                               GNSS
CC               16-JAN-11 : RD: STANUM IS NOT INTERNALLY USED ANYMORE
CC               26-JAN-11 : KS: EXIT PROGRAM IF NO OBSERVATIONS IN OBS.FILES
CC               26-JAN-11 : LP: Use sat.-specific obstypes from OBS-HEADER
CC               03-FEB-11 : CR: ADD SWITCH (IREL2) FOR PERIODIC RELATIVISTIC
CC                               J2-CORRECTION
CC               04-Feb-11 : RD: GETSTA MAY BE USED AS MODULE NOW
CC               16-FEB-11 : SS: SIGAMB(5) FOR GLONASS AMBIGUITY RESOLUTION
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               18-FEB-11 : SS: STRAMB(4) TO IGNORE GPS QUARTER-CYCLE BIASES
CC               13-JUL-11 : LP: Write sat-specific obstypes into NEQ
CC               21-JUL-11 : LP: Consistency checks for sat-specific obstypes;
CC                               variables if and ipf removed
CC               25-JUL-11 : HB: ACTIVATE IOREST FOR SAVNEQ
CC               25-AUG-11 : SL: MESSAGE IN CASE OF NO AVAILABLE OBSERVATIONS
CC               09-SEP-11 : RD: CORRECT EPOCHS FOR XREF
CC               14-SEP-11 : LP: GIVE MXCPAR TO SR PRIEST AND PRIGIM
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES FROM INISES
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES FROM PRCEPO
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES FROM OISTCF
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM OISTCI
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM PRIEST
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM PRIAPR
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM CLKINI
CC               26-MAR-12 : RD: USE LISTC1 AS MODULE NOW
CC               20-APR-12 : RD: DON'T HAND OVER UN-ALLOCATED ARRAY TO SRs
CC               27-APR-12 : RD: XXX0 POINTER->ALLOCATABLE
CC               27-APR-12 : RD: REMOVE UNUSED VARIABLES
CC               24-MAY-12 : RD: REMOVE UNUSED VARIABLES FROM AMBRES
CC               05-JUN-12 : LP: CHANGES IN GOBSDEF HANDLING (DIM 4->8),CHECK FRQ NOT OBSTYP
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM RESEPO, RESOUT
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM PRCEPO, AMBRES
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM REDAMB, REDEPO
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM REDNEQ, REDPAR
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC               20-SEP-12 : RD: REMOVE UNUSED VARIABLES FROM TRPSAV
CC               11-OCT-12 : RD: NEW CALL FOR CHKOPT AND AMBRES
CC               06-Jun-13 : RD: NEW CALL FOR CHKMAX
CC               16-Jul-13 : RD: NEW CALL FOR PRIAPR
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnErr, keyValueLength,
     1                    fileNameLength, staNameLength
      USE m_cpu,    ONLY: cpu_start
      USE m_time,   ONLY: t_timint,OPERATOR(.isIn.)
      USE m_global, ONLY: maxsys
      USE m_maxdim, ONLY: MAXSHD
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec, init_clkRec, init_clkHead
      USE d_phaecc, ONLY: init_buf
      USE d_grid,   ONLY: initGridBuffer
      USE d_satfil, ONLY: typeMWTR, typeSLR
      USE d_par,    ONLY: t_par
      USE d_satcrx, ONLY: gtsatm
      USE d_stacrx, ONLY: MTypeSLR
      USE d_rinex3, ONLY: t_gobsdef
C
      USE m_maxdim, ONLY: MAXSAT,MAXFLS,MAXSAS,MAXGIM,MAXVAR
      USE p_gpsest, ONLY: MAXAMB,MAXFRQ,MAXFIL,MAXSTA,MAXARC,MAXPAR,
     1                    MAXAMP,MAXLOC,MAXCMP,MAXTRM,MAXTRP,MAXSNG,
     2                    MAXLCQ,MAXFRS,MAXWGT,MAXPOL,MAXSTC,MAXSTP,
     3                    MAXOFF,MAXOFR,MAXTYP,MAXHIL,MAXPOT,MAXALB,
     4                    MAXCAL,MAXSPV,MAXMEA,
     5                    t_ambTime, init_ambTime, t_optLoad, t_isbTime,
     6                    t_parTyp, t_optGsp
      USE s_ambref
      USE s_corsav
      USE s_gtsensor
      USE s_stdsav
      USE s_kinsav
      USE s_oistcf
      USE s_oistci
      USE s_defses
      USE s_pritit
      USE s_syminvg
      USE s_ambres
      USE s_updpar
      USE s_prcepo
      USE s_trpsav
      USE s_covsav
      USE s_priest
      USE s_covsv1
      USE s_wgtdip
      USE s_polsav
      USE s_qdis
      USE s_resepo
      USE s_redepo
      USE s_readkeys
      USE s_priapr
      USE s_redamb
      USE s_ambchn
      USE s_clkini
      USE s_redneq
      USE s_defcon
      USE s_resout
      USE s_stdsav2
      USE s_opnsys
      USE s_rdinpt
      USE s_qblh
      USE s_gtflna
      USE s_chkopt
      USE s_norini
      USE s_iersav
      USE s_gphelm
      USE s_atmsav
      USE s_alcerr
      USE s_stexin
      USE s_seqpar
      USE s_inises
      USE s_mjdgps
      USE s_prisyn
      USE s_wgtkin
      USE s_primax
      USE s_pcvsav
      USE s_rdimax
      USE s_readinpf
      USE s_clksav
      USE s_obsinf
      USE s_solve
      USE s_pridis
      USE s_smpelv
      USE s_hedinf
      USE s_getsta
      USE s_redpar
      USE s_matcop
      USE s_exitrc
      USE s_ambsto
      USE s_dcbsav
      USE s_closes
      USE s_ckoptc
      USE s_inimax
      USE s_priblh
      USE s_tmidle
      USE s_chkmax
      USE s_clksum
      USE s_savneq
      USE s_obsfillst
      USE s_ambcst
      USE f_ikf
      USE f_lincount
      USE f_sigma1
      USE f_listc1
      USE s_isbsav
      USE s_stdOrb2New
      USE s_chkant
      USE s_gobsdef,ONLY: init_geos
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , IAC     , IARC    , IARCOF  , iant    ,
     1          IDIFF   , IELE0   , IELVNQ  , IEP     , iEpObs  , dummy,
     2          IEPPAR  , IEXTRA  , IFLAG   , IFREQ   , II      ,
     3          ILOC    , IMAN    , IMXAMB  , IMXAMP  , ista    ,
     4          IMXFIL  , IMXFLS  , IMXFRS  , IMXLOC  , IMXPAR  ,
     5          IMXSAS  , IMXSAT  , IMXSNG  , IMXSTA  , INDARC  ,
     6          IOREST  , IORSYS  , IORSYS2 , IP      , numb    ,
     7          IP1     , IPART   , IQXX    , IRAUX2  ,
     8          IRC     , IRCAMB  , IRCAMP  , IRCFIL  , IRCFLS  ,
     9          IRCFRS  , IRCLOC  , IRCNEQ  , IRCODE  , IRCPAR  ,
     1          IRCRES  , IRCSAS  , IRCSAT  , IRCSNG  , iphsep  ,
     2          IRCSTA  , IRESID  , IRETC   , ISATCO  , IACST   ,
     3          ISASYS  , ISAT    , ISES    , ISMAT   , ISYNCR  ,
     4          ITRMAP  , ITROPO  , ITYP    , IWQXX   , IZEROD  ,
     5          JPAR    , JSAT    , KSAT    , LEOARC  , jj      ,
     6          MAXMAN  , MELWUB  , MIXED   , MXCAMB  , MXCAMP  ,
     7          MXCARC  , MXCEQN  , MXCFIL  , MXCFLS  , MXCFRQ  ,
     8          MXCFRS  , MXCLCQ  , MXCLOC  , MXCPAE  , MXCPAR  ,
     9          MXCSAS  , MXCSAT  , MXCSGR  , MXCSHD  , MXCSNG  ,
     1          MXCSTA  , MXCSTC  , MXCSTP  , MXCVAR  , MYDIM   ,
     2          NALB    , NALBGR  , NALLSAT , NALLSTA , NAMB    ,
     3          NAMSES  , NANCAL  , NANOFF  , NANRAO  , NARC    ,
     4          NARC2   , NCAMP   , NCENM   , NCENTR  , NCLKSA  ,
     5          NCLKST  , NCLREQ  , NOBSSAV , NRGB    , NMXINT  ,
     6          NMXINT2 , NFIX    , NFLSES  , NFRSES  , NMXSAP  ,
     7          NFTOT   , NHILL   , NIOREQ  , NKIN    , NLIST   ,
     8          NMAN    , NMXAMB  , NMXAMP  , NMXFIL  , NMXFLS  ,
     9          NMXFRS  , NMXLOC  , NMXPAR  , NMXSAS  , NMXSAT  ,
     1          NMXSNG  , NMXSTA  , NOBEPO  , NOBS    , NORB    ,
     2          NORB2   , NORRES  , NPAEPO  , NPALCQ  , NREC    ,
     3          NPAR    , NPARAR  , NPARMS  , NPARN   , NPASES  ,
     4          NPOL    , NPOT    , NREF    , NRQOFF  , NSAEFF  ,
     5          NSASES  , NSASTC1 , NSASTC2 , NSAT    , NMXARC  ,
     6          NSESS   , NSHD    , NSMPNQ  , NSPEC1  , NSPEC2  ,
     7          NSTAT   , NSTCEP  , NSTCEP2 , NSTEFF  ,
     8          NSTWGT  , NTRREQ  , NTRSTA  , NUMAM1  , NESTSAT ,
     9          NUST    , NWEEK   , NWGT    , NEPSNG  , NOSOL   ,
     .          IREF    , CLKSYS  , NUMISB  , GEOTEC  ,
     1          nflcol  , INDGEOS , grec    , jrec    , obs     ,
     2          samesat , satnumg , satnumj , IREL2
C
      REAL*8    AELL    , BELL    , DTSIM   , EDTLVL  , GPSSEC  ,
     1          RMS     , RMSOLD  , RMSSAV  , SCAALB  , SCACEN  ,
     2          SCAHIL  , SCAPOT  , SCELL   , SECIPL  , SIGAPR  ,
     3          TFRAC   , TIMCRD  , TLAST   , TOBS    , epo     ,
     4          TPRINT  , ZENMAX  , ZMXLEO  , weight_amb
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATIONS
C ------------
C
      CHARACTER(LEN=  6), PARAMETER :: pgName = 'GPSEST'
C
      TYPE(t_clkhead)                                         :: CLKHED
      TYPE(t_clkrec)                                          :: CLKREC
      TYPE(t_timint)                                    :: globalWindow
      TYPE(t_ambTime),              DIMENSION(:,:),ALLOCATABLE:: TIMFIL ! (0:MAXSYS,MAXFIL)
      TYPE(t_par),                  DIMENSION(:),  ALLOCATABLE:: XXREF
      TYPE(t_par),                DIMENSION(:,:),  ALLOCATABLE:: XREF
      TYPE(t_optLoad),              DIMENSION(3)              :: opLoad
      TYPE(t_isbTime),              DIMENSION(:),  POINTER    :: isbTim
      TYPE(t_parTyp),               DIMENSION(:),  ALLOCATABLE:: partyp
      TYPE(t_optGsp)                                          :: optGsp
      TYPE(t_gobsdef),              DIMENSION(:), ALLOCATABLE :: gobsdef ! (MAXFIL)
      TYPE(t_gobsdef)                                        :: gobsdef1
C
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER    ::keyValue
      CHARACTER(LEN=132), DIMENSION(2)                        :: TITLES
      CHARACTER(LEN= 80)                                      :: TITLE
      CHARACTER(LEN= 80)                                      :: WGTFILE
      CHARACTER(LEN= 64)                                      :: STITLE
      CHARACTER(LEN=fileNameLength),DIMENSION(:),  ALLOCATABLE:: HEADER ! (MAXFIL)
      CHARACTER(LEN=fileNameLength),DIMENSION(:),  ALLOCATABLE:: OBSFIL ! (MAXFIL)
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER    :: FilObs
      CHARACTER(LEN=fileNameLength)                           :: FILSTD
      CHARACTER(LEN=fileNameLength)                           :: FILRPR
      CHARACTER(LEN=fileNameLength)                           :: FILORB
      CHARACTER(LEN=fileNameLength)                           :: LEOSTD
      CHARACTER(LEN=fileNameLength)                           :: FILNEQ
      CHARACTER(LEN=fileNameLength)                           :: FILRES
      CHARACTER(LEN= 16)                                      :: DATUM
      CHARACTER(LEN= 20)                                      :: name
      CHARACTER(LEN= 26)                                      :: anten
      CHARACTER(LEN= 26), DIMENSION(:), ALLOCATABLE           :: antlst
      CHARACTER(LEN= 16),           DIMENSION(MAXCMP)         :: CAMPGN ! (MAXCMP)
      CHARACTER(LEN=staNameLength), DIMENSION(:),  ALLOCATABLE:: STASES ! (MAXSTA)
      CHARACTER(LEN= 20),           DIMENSION(:,:),ALLOCATABLE:: RECTYP ! (2,MAXFIL)
      CHARACTER(LEN= 20),           DIMENSION(:,:),ALLOCATABLE:: ANTTYP ! (2,MAXFIL)
      CHARACTER(LEN= 20),           DIMENSION(2,MAXCAL)       :: ANTCAL ! (2,MAXCAL)
      CHARACTER(LEN= 20),           DIMENSION(2,MAXCAL)       :: ANTRAO ! (2,MAXCAL)
      CHARACTER(LEN= 16),           DIMENSION(MAXGIM)         :: NAMGIM ! (MAXGIM)
      CHARACTER(LEN=staNameLength), DIMENSION(:),  POINTER    :: NAMLST ! (maxsta)
      CHARACTER(LEN=staNameLength), DIMENSION(:),ALLOCATABLE::allStaName ! (maxsta)
      CHARACTER(LEN= 16)                                      :: NUTNAM
      CHARACTER(LEN= 16)                                      :: SUBNAM
      CHARACTER(LEN=  6)                                      :: MXNSAT
      CHARACTER(LEN=  6)                                      :: MXNAMB
      CHARACTER(LEN=  6)                                      :: MXNARC
      CHARACTER(LEN=  6)                                      :: MXNFRQ
      CHARACTER(LEN=  6)                                      :: MXNSAS
      CHARACTER(LEN=  6)                                      :: MXNFLS
      CHARACTER(LEN=  6)                                      :: MXNEQN
      CHARACTER(LEN=  6)                                      :: MXNLCQ
      CHARACTER(LEN=  6)                                      :: MXNFIL
      CHARACTER(LEN=  6)                                      :: MXNLOC
      CHARACTER(LEN=  6)                                      :: MXNSTC
      CHARACTER(LEN=  6)                                      :: MXNAMP
      CHARACTER(LEN=  6)                                      :: MXNPAR
      CHARACTER(LEN=  6)                                      :: MXNSTP
      CHARACTER(LEN=  6)                                      :: MXNSHD
      CHARACTER(LEN=  6)                                      :: MXNVAR
      CHARACTER(LEN=  6)                                      :: MXNSGR
      CHARACTER(LEN=  4),       DIMENSION(:,:),   ALLOCATABLE :: CSESS  ! (2,MAXFIL)
      CHARACTER(LEN=  4),       DIMENSION(:),     ALLOCATABLE :: SESSID ! (MAXFIL)
      CHARACTER(LEN=  1),       DIMENSION(10,MAXARC)          :: SOURCE ! (10,MAXARC)
      CHARACTER(LEN=  1),       DIMENSION(:,:,:), ALLOCATABLE :: IAMB1  ! (MAXAMB,MAXFRQ,MAXFLS)
      CHARACTER(LEN=  1),       DIMENSION(10,MAXARC)          :: SOURCE2! (10,MAXARC)
      CHARACTER(LEN=  1),       DIMENSION(:),     ALLOCATABLE :: LSMAT  ! (MAXPAR)
      CHARACTER(LEN=  1),       DIMENSION(:,:,:), ALLOCATABLE :: OBSFLG ! (MAXSAT,MAXFRQ,MAXFLS)
      CHARACTER(LEN=  1),       DIMENSION(:,:),   ALLOCATABLE :: OBSFL1 ! (MAXSAT,MAXFRQ)
      CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER  :: dummy2 ! for OBSFILLST
C
      REAL(r8b), DIMENSION(3)                      :: DXELL
      REAL(r8b), DIMENSION(3)                      :: DRELL
      REAL(r8b), DIMENSION(2,MAXCMP)               :: TAECMP    ! (2,MAXCMP)
      REAL(r8b), DIMENSION(MAXVAR)                 :: PREC      ! (MAXVAR)
      REAL(r8b), DIMENSION(MAXVAR)                 :: PREC2     ! (MAXVAR)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: TIMREF    ! (MAXFIL)
      REAL(r8b), DIMENSION(:,:,:),     ALLOCATABLE :: POSECC    ! (3,2,MAXFIL)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: CLFRTO    ! (2,2*MAXFIL)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: STWGT     ! (3,MAXSTA)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: CLKWGT    ! (2,MAXFIL)
      REAL(r8b), DIMENSION(2,MAXARC)               :: TBOUND    ! (2,MAXARC)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: TIMMID    ! (MAXFIL)
      REAL(r8b), DIMENSION(2,MAXARC)               :: TBOUND2   ! (2,MAXARC)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: BNOR      ! (MAXPAR)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: ASING     ! (MAXSAS*MAXFLS*MAXSNG)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: AAUX      ! (MAXSNG)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: BSING     ! (MAXSAS*MAXFLS)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: AHELP     ! (MAXLOC)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: B1        ! (MAXPAR)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: QB        ! (MAXSTA*(MAXSTA+1)/2)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: QL        ! (MAXSTA*(MAXSTA+1)/2)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: QH        ! (MAXSTA*(MAXSTA+1)/2)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: DISOLD    ! (MAXSTA*(MAXSTA+1)/2)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: DISNEW    ! (MAXSTA*(MAXSTA+1)/2)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: QDD       ! (MAXSTA*(MAXSTA+1)/2)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: SYNC      ! (MAXSAT,MAXFIL)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: WINDOW    ! (2,MAXFIL)
      REAL(r8b), DIMENSION(2,MAXTRM)               :: TRPLIM    ! (2,MAXTRM)
      REAL(r8b), DIMENSION(MAXTRP)                 :: SIGTRP    ! (MAXTRP)
      REAL(r8b), DIMENSION(3,MAXTRM)               :: SIGTRS    ! (3,MAXTRM)
      REAL(r8b), DIMENSION(5)                      :: SIGAMB
      REAL(r8b), DIMENSION(4)                      :: AR2INF
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: RMSSES    ! (MAXFIL)
      REAL(r8b), DIMENSION(:,:,:),     ALLOCATABLE :: AMB0      ! (MAXAMB,MAXFRQ,MAXFLS)
      REAL(r8b), DIMENSION(:,:,:),     ALLOCATABLE :: AMBIGU    ! (MAXAMB,3,MAXFIL)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: RMSAMB    ! (MAXAMP)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: AMBIG1    ! (MAXAMB,3)
      REAL(r8b), DIMENSION(:,:,:),     ALLOCATABLE :: OBSERV    ! (MAXSAT,MAXFRQ,MAXFLS)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: OBSER1    ! (MAXSAT,MAXFRQ)
      REAL(r8b), DIMENSION(:,:,:,:),   ALLOCATABLE :: WGSSES    ! (3,2,MAXFRQ,MAXFLS)
      REAL(r8b), DIMENSION(:,:,:,:),   ALLOCATABLE :: ELLSES    ! (3,2,MAXFRQ,MAXFLS)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: VSIG      ! (MAXSAS*MAXFLS)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: SYNCM     ! (MAXFLS)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: XMAXCL    ! (MAXFIL)
      REAL(r8b), DIMENSION(2,MAXPOL)               :: TPOL      ! (2,MAXPOL)
      REAL(r8b), DIMENSION(5,MAXPOL)               :: SIGPOL    ! (5,MAXPOL)
      REAL(r8b), DIMENSION(2,MAXWGT)               :: TIMWGT    ! (2,MAXWGT)
      REAL(r8b), DIMENSION(MAXWGT)                 :: WGTWGT    ! (MAXWGT)
      REAL(r8b), DIMENSION(:,:,:,:),   ALLOCATABLE :: TIMSTC    ! (3,MAXSTC,MAXSAT,MAXARC)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: SIGSTC    ! (3,MAXSAT)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: SIGSTC1   ! (3,MAXSAT)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: SIGSTC2   ! (3,MAXSAT)
      REAL(r8b), DIMENSION(:,:,:,:),   ALLOCATABLE :: TIMSTC2   ! (3,MAXSTC,MAXSAT,MAXARC)
      REAL(r8b), DIMENSION(2)                      :: scastc
      REAL(r8b), DIMENSION(3,MAXOFR)               :: SIGOFF    ! (3,MAXOFR)
      REAL(r8b), DIMENSION(MAXSTP)                 :: TIMSPC1   ! (MAXSTP)
      REAL(r8b), DIMENSION(MAXSTP)                 :: TIMSPC2   ! (MAXSTP)
      REAL(r8b), DIMENSION(3,MAXSTP)               :: SIGSPC1   ! (3,MAXSTP)
      REAL(r8b), DIMENSION(3,MAXSTP)               :: SIGSPC2   ! (3,MAXSTP)
      REAL(r8b), DIMENSION(2,MAXOFR)               :: TIMOFF    ! (2,MAXOFR)
      REAL(r8b), DIMENSION(:,:,:),     ALLOCATABLE :: POLARS    ! (MAXAMB,MAXFRQ,MAXFLS)
      REAL(r8b), DIMENSION(MAXHIL)                 :: SIGHIL    ! (MAXHIL)
      REAL(r8b), DIMENSION(MAXPOT)                 :: SIGPOT    ! (MAXPOT)
      REAL(r8b), DIMENSION(3)                      :: SIGALB
      REAL(r8b), DIMENSION(3)                      :: SIGCEN
      REAL(r8b)                                    :: SIGRGB
      REAL(r8b), DIMENSION(2)                      :: SIGDIP
      REAL(r8b), DIMENSION(3)                      :: SIGHOI
      REAL(r8b), DIMENSION(MAXCAL)                 :: SIGCAL    ! (MAXCAL)
      REAL(r8b), DIMENSION(2,MAXCAL)               :: SIGRAO    ! (2,MAXCAL)
      REAL(r8b), DIMENSION(2)                      :: SIGDCB
      REAL(r8b), DIMENSION(3,MAXGIM)               :: POLGIM    ! (3,MAXGIM)
      REAL(r8b), DIMENSION(4)                      :: SIGGIM
      REAL(r8b), DIMENSION(2,MAXGIM)               :: EPOGIM    ! (2,MAXGIM)
      REAL(r8b), DIMENSION(3)                      :: SCAGIM
      REAL(r8b), DIMENSION(2,MAXGIM)               :: INFGIM    ! (2,MAXGIM)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: ELEVMM    ! (2,MAXFIL)
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: NADIMM    ! (2,MAXFIL)
      REAL(r8b), DIMENSION(2)                      :: HPSAVE
      REAL(r8b), DIMENSION(MAXSPV)                 :: SIGSPV    ! (MAXSPV)
      REAL(r8b)                                    :: NADMAX
      REAL(r8b)                                    :: NAMAX
      REAL(r8b)                                    :: RAPZENMAX
      REAL(r8b), DIMENSION(2,MAXSHD)               :: TIMSHD    ! (2,MAXSHD)
      REAL(r8b), DIMENSION(:),         POINTER     :: n11,b0
      REAL(r8b), DIMENSION(:),         POINTER     :: n11ar
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: XXX0      ! (MAXPAR)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: WEIGHT    ! (MAXSAS*MAXFLS*(MAXSAS*MAXFLS+1)/2)
      REAL(r8b), DIMENSION(:),         ALLOCATABLE :: TIMMAN    ! MAXMAN
      REAL(r8b), DIMENSION(:,:),       ALLOCATABLE :: timIsb    ! (3,NUMISB)
C
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NUMOBS    ! (MAXFRQ,MAXFIL)
      INTEGER(i4b), DIMENSION(4)                   :: STRAMB
      INTEGER(i4b)                                 :: AR2MOD
      INTEGER(i4b), DIMENSION(20)                  :: PRIOPT
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: AMBDEF    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: AMBSAV    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: LISTUS    ! (2,MAXSTA)
      INTEGER(i4b), DIMENSION(7)                   :: IHELM
      INTEGER(i4b), DIMENSION(MAXVAR)              :: SEQORB    ! (MAXVAR)
      INTEGER(i4b), DIMENSION(MAXVAR)              :: SEQORB2   ! (MAXVAR)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: STFIX     ! (MAXSTA)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: STKIN     ! (MAXSTA)
      INTEGER(i4b), DIMENSION(2,MAXMEA)            :: ICOELV
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ISTCLK    ! (2*MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ISACLK    ! (2*MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NCLK      ! (2*MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IBIAS     ! (2*MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: IRUNIT    ! (2,MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NFREQ     ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NEPOCH    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IRMARK    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ISTWGT    ! (MAXSTA)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: INDP      ! (MAXLOC)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: LOCHLP    ! (MAXLCQ,MAXLOC)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: SATNUM    ! (MAXSAT,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: SVNFIL    ! (MAXSAT,MAXFLS)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: SVNFI1    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: STFIL     ! (2,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: ICARR     ! (MAXFRQ,MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NFRFIL    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ARCINT    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(MAXARC)              :: NUMSAT    ! (MAXARC)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ARCINT2   ! (MAXFIL)
      INTEGER(i4b), DIMENSION(MAXARC)              :: NUMSAT2   ! (MAXARC)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NAVNUM    ! (MAXSAT*MAXARC)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NAVNUM2   ! (MAXSAT*MAXARC)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IDELTT    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IONMOD    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NDIFF     ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: IANTEN    ! (2,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: ISYNC     ! (MAXSAT,MAXFLS)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: IONREQ    ! (3,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: ICLOCK    ! (2,MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: AMB1      ! (MAXPAR)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: AMB2      ! (MAXPAR)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: AMB3      ! (MAXPAR)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: MATCH     ! (MAXPAR)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IDEL      ! (MAXLOC)
      INTEGER(i4b), DIMENSION(MAXTRM)              :: NPARTR    ! (MAXTRM)
      INTEGER(i4b), DIMENSION(MAXTRM)              :: STATRP    ! (MAXTRM)
      INTEGER(i4b), DIMENSION(MAXTRM)              :: ISGTRS    ! (MAXTRM)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NUMSA1    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NUMOB1    ! (MAXSAT,2)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NUMMR1    ! (MAXSAT,2)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: AMBIE1    ! (MAXAMB)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: AMBSA1    ! (MAXAMB,3)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: AMBWL1    ! (MAXAMB,2)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: AMBCL1    ! (MAXAMB,3)
      INTEGER(i4b)                                 :: CORSTR
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NEPFLG    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IFRMAT    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: MEATYP    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NUMAMB    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: AMBIEP    ! (MAXAMB,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: AMBSAT    ! (MAXAMB,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: AMBWLF    ! (MAXAMB,2,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: AMBCLS    ! (MAXAMB,3,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NUMOBT    ! (2,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NUMMRT    ! (2,MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NSATEL    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ICAMPN    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(MAXCMP)              :: NSCAMP    ! (MAXCMP)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: STCAMP    ! (MAXSTA,MAXCMP)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: SATSES    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: FILNUM    ! (MAXFLS)
      INTEGER(i4b), DIMENSION(MAXFRS)              :: FRQSES    ! (MAXFRS)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: ISV12     ! (2,MAXFLS*MAXSAS)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IFIL      ! (MAXFLS*MAXSAS)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IOBNUM    ! (MAXFLS*MAXSAS)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: INDRMS    ! (MAXAMP)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IFRSES    ! (MAXFLS*MAXSAS)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: OBSNUM    ! (MAXFLS)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: OBSEPO    ! (MAXSAT,MAXFRQ,MAXFLS)
      INTEGER(i4b)                                 :: POLMOD
      INTEGER(i4b), DIMENSION(5)                   :: POLPAR
      INTEGER(i4b), DIMENSION(MAXWGT)              :: SATWGT    ! (MAXWGT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: IFDONE    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(MAXTYP)              :: OPTELI    ! (MAXTYP)
      INTEGER(i4b), DIMENSION(MAXTYP)              :: OPTPAR    ! (MAXTYP)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: OBSCLS    ! (MAXAMP)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ICLUST    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: REFAMB    ! (MAXAMP)
      INTEGER(i4b), DIMENSION(3)                   :: FRCTYP
      INTEGER(i4b), DIMENSION(3)                   :: FRCTYP2
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NSTDAY    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NSTDAY1   ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NSTDAY2   ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NUMSTC    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NUMSTC1   ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: NUMSTC2   ! (MAXSAT)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NSTCEF    ! (MAXSAT,MAXARC)
      INTEGER(i4b), DIMENSION(MAXSTP)              :: NUMSPC1   ! (MAXSTP)
      INTEGER(i4b), DIMENSION(MAXSTP)              :: NUMSPC2   ! (MAXSTP)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NSTCEF2   ! (MAXSAT,MAXARC)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: INTSTC    ! (MAXSTC,MAXSAT,MAXARC)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: INTSTC2   ! (MAXSTC,MAXSAT,MAXARC)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: ESTSAT    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(MAXPOL)              :: ISGPOL    ! (MAXPOL)
      INTEGER(i4b), DIMENSION(MAXOFR)              :: GRPOFF    ! (MAXOFR)
      INTEGER(i4b), DIMENSION(MAXOFR)              :: ISGOFF    ! (MAXOFR)
      INTEGER(i4b), DIMENSION(MAXOFF)              :: NSAOFF    ! (MAXOFF)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: SATOFF    ! (MAXSAT,MAXOFF)
      INTEGER(i4b), DIMENSION(3)                   :: PAROFF
      INTEGER(i4b), DIMENSION(MAXOFF)              :: GNROFF    ! (MAXOFF)
      INTEGER(i4b), DIMENSION(3,MAXHIL)            :: HILTYP    ! (3,MAXHIL)
      INTEGER(i4b), DIMENSION(3,MAXPOT)            :: POTTYP    ! (3,MAXPOT)
      INTEGER(i4b), DIMENSION(3)                   :: ALBTYP
      INTEGER(i4b), DIMENSION(MAXALB)              :: NSAALB    ! (MAXALB)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: SATALB    ! (MAXSAT,MAXALB)
      INTEGER(i4b), DIMENSION(3)                   :: CENMAS
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: staRGB    ! (MAXSTA)
      INTEGER(i4b)                                 :: satSpec
      INTEGER(i4b), DIMENSION(3)                   :: OPTDIP
      INTEGER(i4b), DIMENSION(3)                   :: OPTHOI
      INTEGER(i4b), DIMENSION(MAXPOL)              :: ISGNUT    ! (MAXPOL)
      INTEGER(i4b), DIMENSION(2,MAXCAL)            :: NUMCAL    ! (2,MAXCAL)
      INTEGER(i4b), DIMENSION(MAXCAL)              :: PRNCAL    ! (MAXCAL)
      INTEGER(i4b), DIMENSION(MAXCAL)              :: NFRCAL    ! (MAXCAL)
      INTEGER(i4b), DIMENSION(2,MAXCAL)            :: NPTCAL    ! (2,MAXCAL)
      INTEGER(i4b), DIMENSION(2,MAXCAL)            :: NUMRAO    ! (2,MAXCAL)
      INTEGER(i4b), DIMENSION(MAXCAL)              :: PRNRAO    ! (MAXCAL)
      INTEGER(i4b), DIMENSION(MAXCAL)              :: NFRRAO    ! (MAXCAL)
      INTEGER(i4b), DIMENSION(3)                   :: NEURAO
      INTEGER(i4b), DIMENSION(5,MAXTYP)            :: PARLST    ! (5,MAXTYP)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: PARFLG    ! (MAXPAR)
      INTEGER(i4b), DIMENSION(10)                  :: OPTGIM
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: CLKSTA    ! (MAXSTA)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: CLKSAT    ! (MAXSAT)
      INTEGER(i4b), DIMENSION(4)                   :: OPTDCB
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: INDSMP    ! (MAXFIL)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: NOBELV    ! (2,18,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:,:,:),ALLOCATABLE :: NOBAZIS   ! (2,36,MAXSAT,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: NOBNAD    ! (2,30,MAXFIL)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: SATMAN    ! MAXMAN
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: CLKPAR    ! (2,NCLKST+NCLKSA)
      INTEGER(i4b)                                 :: IPOLAR
      INTEGER(i4b), DIMENSION(3)                   :: NSAMPL
      INTEGER(i4b), DIMENSION(3)                   :: NOINCLK
C
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: INDA      ! (MAXSAS*MAXFLS*MAXSNG)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: INDAUX    ! (MAXSNG)
      INTEGER(i4b), DIMENSION(5)                   :: nInpFiles
      INTEGER(i4b), DIMENSION(5)                   :: nFil
      INTEGER(i4b), DIMENSION(:),      POINTER     :: NUMLST    ! (maxsta)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: allStaNum ! (maxsta)
      INTEGER(i4b), DIMENSION(:),      POINTER     :: SATLST    ! (maxSat)
      INTEGER(i4b), DIMENSION(:),      POINTER     :: ISOBSA    ! (maxSat)
      INTEGER(i4b), DIMENSION(:),      POINTER     :: ISOBST    ! (maxSta)
      INTEGER(i4b), DIMENSION(:),      ALLOCATABLE :: allSatNum ! (maxSat)
      INTEGER(i4b), DIMENSION(3)                   :: NEPOBS
      INTEGER(i4b), DIMENSION(2,MAXSPV)            :: NPTSPV    ! (2,MAXSPV)
      INTEGER(i4b)                                 :: NANSPV
      INTEGER(i4b), DIMENSION(MAXSPV)              :: GNRSPV    ! (MAXSPV)
      INTEGER(i4b), DIMENSION(MAXSPV)              :: NSASPV    ! (MAXSPV)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: SATSPV    ! (MAXSAT,MAXSPV)
      INTEGER(i4b)                                 :: NADIGN
      INTEGER(i4b), DIMENSION(MAXSHD)              :: SATSHD    ! (MAXSHD)
      INTEGER(i4b), DIMENSION(MAXSHD)              :: FLGSHD    ! (MAXSHD)
      INTEGER(i4b), DIMENSION(:,:,:),  ALLOCATABLE :: ICLU      ! (0:MAXSYS,MAXAMB,MAXFIL)
      INTEGER(i4b), DIMENSION(:,:),    ALLOCATABLE :: NOBSPA    ! (MAXMEA*MAXSYS,MAXPAR)
      INTEGER(i4b)                                 :: usegeos1
C
      LOGICAL,      DIMENSION(2)                   :: SETSUM
      LOGICAL                                      :: LSTSES
      LOGICAL                                      :: isLeo
C
      INTEGER(i4b),      DIMENSION(:),      ALLOCATABLE :: usegeos  ! (MAXFIL)
      INTEGER(i4b),      DIMENSION(:),      ALLOCATABLE :: iCentr   ! (MAXSTA)
      CHARACTER(LEN=16), DIMENSION(:),      ALLOCATABLE :: stName   ! (MAXSTA)
      INTEGER(i4b),      DIMENSION(:),      ALLOCATABLE :: staNum   ! (MAXSTA)
      CHARACTER(LEN= 1), DIMENSION(:),      ALLOCATABLE :: staFlg   ! (MAXSTA)
      INTEGER(i4b),      DIMENSION(:,:),    ALLOCATABLE :: locq     ! (MAXLCQ,MAXLOC)
      REAL(r8b),         DIMENSION(:,:),    ALLOCATABLE :: xStEll   ! (3,MAXSTA)
      REAL(r8b),         DIMENSION(:,:),    ALLOCATABLE :: xStat    ! (3,MAXSTA)
      REAL(r8b),         DIMENSION(:,:),    ALLOCATABLE :: xStEcc   ! (3,MAXSTA)
      REAL(r8b),         DIMENSION(2,MAXTRM)            :: trpLms   ! (2,MAXTRM)
      REAL(r8b),         DIMENSION(:),      ALLOCATABLE :: xxx      ! (MAXPAR)
      REAL(r8b),         DIMENSION(:),      ALLOCATABLE :: aNor     ! (MAXPAR*(MAXPAR+1)/2)
      INTEGER(i4b),      DIMENSION(2)                   :: iTrGrd
      INTEGER(i4b)                                      :: IFILE

      REAL(r8b),         DIMENSION(:,:),    ALLOCATABLE :: timIntF  ! (2,maxfil)
C
C COMMON BLOCKS
C -------------
c      COMMON/LARGE1/HEADER,OBSFIL,STNAME,STASES,RECTYP,ANTTYP,
c     1              IAMB1,OBSFLG
c      COMMON/LARGE2/XSTAT,XSTELL,XSTECC,POSECC,CLFRTO,STWGT,CLKWGT,
c     1              ANOR,ASING,BSING,N12,N22,SYNC,WINDOW
c      COMMON/LARGE3/TRPLIM,SIGTRP,TRPLMS,SIGTRS,
c     1              AMB0,AMBIGU,AMBIG1,OBSERV,OBSER1,
c     2              WGSSES,ELLSES,VSIG,XMAXCL
c      COMMON/LARGE4/NUMOBS,AMBDEF,AMBSAV,SATNUM,SVNFIL,IWLFAC,
c     1              ISYNC,IONREQ,ICLOCK,ICARR,ISTCLK,NCLK,IRUNIT,
c     2              CSESS,NUMAMB,AMBIEP,STCAMP,ISV12,IFIL,IOBNUM,
c     3              IFRSES,LOCQ,IDEL,INDA
c      COMMON/LARGE5/LOCHLP,OBSCLS,REFAMB,POLGIM,EPOGIM,NAMGIM
C
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMAMP/MXCAMP,MXNAMP
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAS/MXCSAS,MXNSAS
      COMMON/MCMFLS/MXCFLS,MXNFLS
      COMMON/MCMEQN/MXCEQN,MXNEQN
      COMMON/MCMLOC/MXCLOC,MXNLOC
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMSTP/MXCSTP,MXNSTP
      COMMON/MCMSHD/MXCSHD,MXNSHD
      COMMON/MCMVAR/MXCVAR,MXNVAR
      COMMON/MCMSGR/MXCSGR,MXNSGR
C
      DATA SCASTC/1.D6,1.D9/,SCAHIL/1.D+6/,SCAPOT/1.D+9/,
     1     SCAALB/1.D6/,SCACEN/1.D0/,SCAGIM/1.D0,1.D-6,1.D0/

      TITLES(:) = " "
      TITLE     = " "
      STITLE    = " "
      ITRGRD    = 0
      NPOL      = 0
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
      MXCAMP=MAXAMP
      MXNAMP='MAXAMP'
      MXCARC=MAXARC
      MXNARC='MAXARC'
      MXCFRQ=MAXFRQ
      MXNFRQ='MAXFRQ'
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
      MXCSAS=MAXSAS
      MXNSAS='MAXSAS'
      MXCFLS=MAXFLS
      MXNFLS='MAXFLS'
      MXCEQN=MAXSAS*MAXFLS
      MXNEQN='MAXEQN'
      MXCLOC=MAXLOC
      MXNLOC='MAXLOC'
      MXCPAR=MAXPAR
      MXNPAR='MAXPAR'
      MXCLCQ=MAXLCQ
      MXNLCQ='MAXLCQ'
      MXCSTC=MAXSTC
      MXNSTC='MAXSTC'
      MXCSTP=MAXSTP
      MXNSTP='MAXSTP'
      MXCSHD=MAXSHD
      MXNSHD='MAXSHD'
      MXCVAR=MAXVAR
      MXNVAR='MAXVAR'
      MXCSGR=MAXSAT
      MXNSGR='MAXSGR'
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(FilObs)
      NULLIFY(NUMLST)
      NULLIFY(NAMLST)
      NULLIFY(SATLST)
      NULLIFY(ISOBSA)
      NULLIFY(ISOBST)
      NULLIFY(ISBTIM)
      NULLIFY(n11)
      NULLIFY(b0)
      NULLIFY(n11ar)
      NULLIFY(keyValue)
      NULLIFY(dummy2)
      CALL INIT_CLKREC(CLKREC)
      CALL INIT_CLKHEAD(CLKHED)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)

C New Standard Orbits
C -------------------
CCC      CALL stdOrb2NewInit

C
C WRITE TITLE
C -----------
      CALL  pritit('GPSEST','Parameter estimation',131)
C
C READ THE USER SPECIFIED ARRAY DIMENSIONS
C ----------------------------------------
      CALL RDIMAX(IMXLOC,IMXFIL,IMXSTA,IMXSAT,IMXAMB,
     1            IMXPAR,IMXFLS,IMXSAS,IMXAMP,IMXSNG)
C
C GET ALL INPUT OBSERVATION FILES
C -------------------------------
      CALL OBSFILLST('GEOTEC',(/'GNSS','SLR '/),
     1               (/'PZFILES','CZFILES'/),
     2               (/'PSFILES','CSFILES'/),'RZFILES',
     3               0,2,0,nflcol,nFil,FilObs,dummy2)
C
      CALL readKeys('LEOPROC', keyValue, irc)
      IF (keyValue(1) == '1') isLeo = .true.
C
      IRCODE = 0
      CALL readKeys('DIFLVL', keyValue, irc)
      CALL ckoptc(1,'DIFLVL', keyValue, (/'DOUBLE','ZERO  '/),
     1            PGNAME, 'Differencing level', IRC, IRCODE,
     2            maxVal=1,result1=IDIFF)
C
      CALL readKeys('GEOTEC', keyValue, irc)
      CALL ckoptc(1,'GEOTEC', keyValue, (/'GNSS','SLR '/),
     1            PGNAME, 'Space geodetic technique', IRC, IRCODE,
     2            maxVal=1,result1=GEOTEC)
      IF (GEOTEC.EQ.2) IDIFF = 3
C
      IF (IDIFF.EQ.1) THEN
        nFil(1)=0
        nFil(2)=0
        nFil(5)=0
C
        ISATCO=0
      ELSEIF (IDIFF.EQ.2) THEN
        nFil(3)=0
        nFil(4)=0
        nFil(5)=0
C
        ISATCO=1
      ELSE
        nFil(1)=0
        nFil(2)=0
        nFil(3)=0
        nFil(4)=0
C
        ISATCO=0
      ENDIF
C
C Stop program if no files specified -> moved to OBSFILLST
C ----------------------------------
      NMXFIL=0
      DO II=1,5
        NMXFIL=NMXFIL+nFil(II)
      END DO
C
      CALL CHKMAX('MAXFIL','files to be processed',
     1            IMXFIL,NMXFIL,MAXFIL,'P_GPSEST',MXCFIL,IRCFIL)
      IF (IRCFIL.NE.0.OR.IRCODE.NE.0) CALL EXITRC(2)
C
      DEALLOCATE(keyValue,stat=irc)
C
C SORT THE FILENAME INTO THE FILE LIST
C ------------------------------------
      nInpFiles(:)=(/nFil(1),nFil(2),nFil(3),nFil(4),nFil(5)/)
C
      ALLOCATE(HEADER(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'HEADER',(/MXCFIL/),PGNAME)
      ALLOCATE(OBSFIL(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'OBSFIL',(/MXCFIL/),PGNAME)
C
      NFTOT=0
C Phase zero diff.
      IF (nFil(1).GT.0) THEN
        HEADER(NFTOT+1:NFTOT+nFil(1)) = FilObs(1,1:nFil(1))
        OBSFIL(NFTOT+1:NFTOT+nFil(1)) = FilObs(2,1:nFil(1))
        NFTOT=NFTOT+nFil(1)
      ENDIF
C
C Code zero diff.
      IF (nFil(2).GT.0) THEN
        HEADER(NFTOT+1:NFTOT+nFil(2)) =
     1        FilObs(1,nFil(1)+1:nFil(1)+nFil(2))
        OBSFIL(NFTOT+1:NFTOT+nFil(2)) =
     1        FilObs(2,nFil(1)+1:nFil(1)+nFil(2))
        NFTOT=NFTOT+nFil(2)
      ENDIF
C
C Phase single diff.
      IF (nFil(3).GT.0) THEN
        HEADER(NFTOT+1:NFTOT+nFil(3)) = FilObs(1,1:nFil(3))
        OBSFIL(NFTOT+1:NFTOT+nFil(3)) = FilObs(2,1:nFil(3))
        NFTOT=NFTOT+nFil(3)
      ENDIF
C
C Code single diff.
      IF (nFil(4).GT.0) THEN
        HEADER(NFTOT+1:NFTOT+nFil(4)) =
     1        FilObs(1,nFil(3)+1:nFil(3)+nFil(4))
        OBSFIL(NFTOT+1:NFTOT+nFil(4)) =
     1        FilObs(2,nFil(3)+1:nFil(3)+nFil(4))
        NFTOT=NFTOT+nFil(4)
      ENDIF
C
C Range observations
      IF (nFil(5).GT.0) THEN
        HEADER(NFTOT+1:NFTOT+nFil(5)) = FilObs(1,1:nFil(5))
        OBSFIL(NFTOT+1:NFTOT+nFil(5)) = FilObs(2,1:nFil(5))
        NFTOT=NFTOT+nFil(5)
      ENDIF
C
      DEALLOCATE(FilObs)
C
C ALLOCATE SOME MAXFIL-ARRAYS
C ---------------------------
C
      ALLOCATE(NFRFIL(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NFRFIL',(/MXCFIL/),PGNAME)
      NFRFIL=0
C
      ALLOCATE(ICARR(MAXFRQ,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ICARR',(/MAXFRQ,MXCFIL/),PGNAME)
      ICARR=0
C
      ALLOCATE(AMBDEF(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBDEF',(/MXCFIL/),PGNAME)
      AMBDEF=0
C
      ALLOCATE(AMBSAV(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBSAV',(/MXCFIL/),PGNAME)
      AMBSAV=0
C
      ALLOCATE(WINDOW(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'WINDOW',(/2,MXCFIL/),PGNAME)
      WINDOW=0d0
C
      ALLOCATE(NUMOBS(MAXFRQ,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NUMOBS',(/MAXFRQ,MXCFIL/),PGNAME)
      NUMOBS=0
C
      ALLOCATE(IFDONE(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IFDONE',(/MXCFIL/),PGNAME)
      IFDONE=0
C
C
      ALLOCATE(timIntF(2,mxcfil),stat=iac)
      CALL alcerr(iac,'timIntF',(/2,MXCFIL/),PGNAME)
      DO ii = 1,mxcfil
        timIntF(1,ii) = 0.d0
        timIntF(2,ii) = 0.d0
      END DO
C
C READ INFORMATION CONCERNING OBSERVATION FILES
C ---------------------------------------------
      CALL OBSINF(MXCFIL,NFTOT ,HEADER,NFRFIL,ICARR,AMBDEF,AMBSAV,
     1            ISASYS,WINDOW,NUMOBS,IFDONE,MELWUB,globalwindow,
     2            NLIST ,NUMLST,NAMLST,ISOBST,NSAT  ,SATLST,ISOBSA,
     3            NMXSTA,NMXAMB,timIntF)
C
      ALLOCATE(allstanum(nList),STAT=IRC)
      CALL ALCERR(IRC,'allstanum',(/nList/),PGNAME)
C
      ALLOCATE(allstaname(nList),STAT=IRC)
      CALL ALCERR(IRC,'allstaname',(/nList/),PGNAME)
C
      nAllSta=0
      DO iSta=1,nList
        IF (ISOBST(iSta) == 0) THEN
          WRITE(lfnErr,'(/,A,/,16X,A,A,/)')
     1      ' ### PG GPSEST: No observations available for ',
     1      'station ',NAMLST(ista)
          CYCLE
        ENDIF
        nAllSta = nAllSta+1
        allstanum(nallsta)=nAllSta
        allstaname(nallsta)=NAMLST(iSta)
      ENDDO
      DEALLOCATE(NUMLST,STAT=IRC)
      DEALLOCATE(NAMLST,STAT=IRC)
      DEALLOCATE(ISOBST,STAT=IRC)
C
      IF (nAllSta == 0) THEN
        WRITE(LFNERR,'(3(/,A),/)')
     1    ' *** PG GPSEST: There are no observations in the selected',
     2    '                observation files.',
     3    '                All observations might have been rejected.'
        CALL EXITRC(2)
      ENDIF
C
      ALLOCATE(allsatnum(2*nSat),STAT=IRC)
      CALL ALCERR(IRC,'allsatnum',(/2*nSat/),PGNAME)
      nAllSat = 0
      DO iSat = 1,nSat
        IF (ISOBSA(ISAT)==0) CYCLE
        nAllSat=nAllSat+1
        allsatnum(nallsat)=SATLST(iSat)
      ENDDO
      allsatnum(nallsat+1:2*nsat) = 0
C
      ALLOCATE(antlst(nallsat+nallsta),STAT=IRC)
      CALL ALCERR(IRC,'antlst',(/nallsat+nallsta/),PGNAME)
      antlst = ''
C
      NMXSAT=nSat
C
C GET SATELLITE MANOEUVRES
C ------------------------
      MAXMAN=linCount('SATCRUX',6)
C
      ALLOCATE(SATMAN(MAXMAN),STAT=IRC)
      CALL ALCERR(IRC,'SATMAN',(/MAXMAN/),PGNAME)
C
      ALLOCATE(TIMMAN(MAXMAN),STAT=IRC)
      CALL ALCERR(IRC,'TIMMAN',(/MAXMAN/),PGNAME)
C
      CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
      DO IMAN=1,NMAN
        IF (TIMMAN(IMAN).ISIN.GLOBALWINDOW) THEN
C
C WAS THE MANEUVER SATELLITE OBSERVED?
          DO ISAT=1,NALLSAT
            IF (ALLSATNUM(ISAT).EQ.SATMAN(IMAN)) THEN
C
C IS THE MANEUVER SATELLITE ALREADY IN THE LIST?
              KSAT=0
              DO JSAT=NALLSAT+1,2*NALLSAT
                IF (ALLSATNUM(JSAT).EQ.0) THEN
                  IF (KSAT.EQ.0) THEN
                    ALLSATNUM(JSAT)=ALLSATNUM(ISAT)+50
                    NMXSAT=NMXSAT+1
                  ENDIF
                  EXIT
                ENDIF
                IF (ALLSATNUM(JSAT).EQ.ALLSATNUM(ISAT)+50) KSAT=JSAT
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      DEALLOCATE(TIMMAN,STAT=IRC)
      DEALLOCATE(SATMAN,STAT=IRC)
C
      CALL CHKMAX('MAXSTA','stations involved',
     1            IMXSTA,NMXSTA,MAXSTA,'P_GPSEST',MXCSTA,IRCSTA)
C
      CALL CHKMAX('MAXSAT','satellites involved',
     1            IMXSAT,NMXSAT,MAXSAT,'M_MAXDIM',MXCSAT,IRCSAT)
      IF (MXCSAT.GT.MXCSGR) MXCSGR=MXCSAT
C
      CALL CHKMAX('MAXAMB','ambiguities in an observation file',
     1            IMXAMB,NMXAMB,MAXAMB,'P_GPSEST',MXCAMB,IRCAMB)
      IF (IRCSTA+IRCSAT+IRCAMB.NE.0) CALL EXITRC(2)
C
C ALLOCATE ALL ARRAYS CONCERNING MAXFIL, MAXSTA, MAXSAT, MAXAMB
C -------------------------------------------------------------
      ALLOCATE(TIMFIL(0:MAXSYS,MXCFIL),STAT=IAC)
      CALL ALCERR(IAC,'TIMFIL',(/MAXSYS+1,MXCFIL/),PGNAME)
      DO ii = 1,mxcfil
        DO jj = 0,maxsys
          CALL init_ambTime(TIMFIL(jj,ii))
        ENDDO
      ENDDO
C
      ALLOCATE(STASES(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'STASES',(/MXCSTA/),PGNAME)
      STASES=''
C
      ALLOCATE(RECTYP(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'RECTYP',(/2,MXCFIL/),PGNAME)
      RECTYP=''
C
      ALLOCATE(ANTTYP(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ANTTYP',(/2,MXCFIL/),PGNAME)
      ANTTYP=''
C
      ALLOCATE(CSESS(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'CSESS',(/2,MXCFIL/),PGNAME)
      CSESS=''
C
      ALLOCATE(SESSID(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'SESSID',(/MXCFIL/),PGNAME)
      SESSID=''
C
      ALLOCATE(OBSFL1(MXCSAT,MAXFRQ),STAT=IRC)
      CALL ALCERR(IRC,'OBSFL1',(/MXCSAT,MAXFRQ/),PGNAME)
      OBSFL1=''
C
      ALLOCATE(TIMREF(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'TIMREF',(/MXCFIL/),PGNAME)
      TIMREF=0d0
C
      ALLOCATE(POSECC(3,2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'POSECC',(/3,2,MXCFIL/),PGNAME)
      POSECC=0d0
C
      ALLOCATE(CLFRTO(2,2*MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'CLFRTO',(/2,2*MXCFIL/),PGNAME)
      CLFRTO=0d0
C
      ALLOCATE(STWGT(3,MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'STWGT',(/3,MXCSTA/),PGNAME)
      STWGT=0d0
C
      ALLOCATE(CLKWGT(3,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'CLKWGT',(/3,MXCFIL/),PGNAME)
      CLKWGT=0d0
C
      ALLOCATE(TIMMID(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'TIMMID',(/MXCFIL/),PGNAME)
      TIMMID=0d0
C
      ALLOCATE(SYNC(MXCSAT,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'SYNC',(/MXCSAT,MXCFIL/),PGNAME)
      SYNC=0d0
C
      ALLOCATE(RMSSES(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'RMSSES',(/MXCFIL/),PGNAME)
      RMSSES=0d0
C
      ALLOCATE(AMBIGU(MXCAMB,3,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBIGU',(/MXCAMB,3,MXCFIL/),PGNAME)
      AMBIGU=0d0
C
      ALLOCATE(AMBIG1(MXCAMB,3),STAT=IRC)
      CALL ALCERR(IRC,'AMBIG1',(/MXCAMB,3/),PGNAME)
      AMBIG1=0d0
C
      ALLOCATE(OBSER1(MXCSAT,MAXFRQ),STAT=IRC)
      CALL ALCERR(IRC,'OBSER1',(/MXCSAT,MAXFRQ/),PGNAME)
      OBSER1=0d0
C
      ALLOCATE(XMAXCL(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'XMAXCL',(/MXCFIL/),PGNAME)
      XMAXCL=0d0
C
      ALLOCATE(TIMSTC(3,MAXSTC,MXCSAT,MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'TIMSTC',(/3,MAXSTC,MXCSAT,MAXARC/),PGNAME)
      TIMSTC=0d0
C
      ALLOCATE(SIGSTC(3,MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'SIGSTC',(/3,MXCSAT/),PGNAME)
      SIGSTC=0d0
C
      ALLOCATE(SIGSTC1(3,MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'SIGSTC1',(/3,MXCSAT/),PGNAME)
      SIGSTC1=0d0
C
      ALLOCATE(SIGSTC2(3,MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'SIGSTC2',(/3,MXCSAT/),PGNAME)
      SIGSTC2=0d0
C
      ALLOCATE(TIMSTC2(3,MAXSTC,MXCSAT,MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'TIMSTC2',(/3,MAXSTC,MXCSAT,MAXARC/),PGNAME)
      TIMSTC2=0d0
C
      ALLOCATE(NUMSTC2(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NUMSTC2',(/MXCSAT/),PGNAME)
      NUMSTC2=0
C
      ALLOCATE(NSTCEF2(MXCSAT,MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'NSTCEF2',(/MXCSAT,MAXARC/),PGNAME)
      NSTCEF2=0
C
      ALLOCATE(INTSTC2(MAXSTC,MXCSAT,MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'INTSTC2',(/MAXSTC,MXCSAT,MAXARC/),PGNAME)
      INTSTC2=0
C
      ALLOCATE(LISTUS(2,MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'LISTUS',(/2,MXCSTA/),PGNAME)
      LISTUS=0
C
      ALLOCATE(STFIX(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'STFIX',(/MXCSTA/),PGNAME)
      STFIX=0
C
      ALLOCATE(STKIN(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'STKIN',(/MXCSTA/),PGNAME)
      STKIN=0
C
      ALLOCATE(ELEVMM(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ELEVMM',(/2,MXCFIL/),PGNAME)
      ELEVMM=0d0
C
      ALLOCATE(NADIMM(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NADIMM',(/2,MXCFIL/),PGNAME)
      NADIMM=0d0
C
C TIME BIASES FOR SLR:
      IF (IDIFF == 3) THEN
        ALLOCATE(ISTCLK(MXCFIL*MXCSAT),STAT=IRC)
        CALL ALCERR(IRC,'ISTCLK',(/MXCFIL*MXCSAT/),PGNAME)
        ISTCLK=0
C
        ALLOCATE(ISACLK(MXCFIL*MXCSAT),STAT=IRC)
        CALL ALCERR(IRC,'ISACLK',(/MXCFIL*MXCSAT/),PGNAME)
        ISACLK=0
C
C RECEIVER CLOCK OFFSET/ISB/IFB for GNSS
      ELSE
        ALLOCATE(ISTCLK(2*MXCFIL),STAT=IRC)
        CALL ALCERR(IRC,'ISTCLK',(/2*MXCFIL/),PGNAME)
        ISTCLK=0
C
        ALLOCATE(ISACLK(2*MXCFIL),STAT=IRC)
        CALL ALCERR(IRC,'ISACLK',(/2*MXCFIL/),PGNAME)
        ISACLK=0
      ENDIF
C
      ALLOCATE(NCLK(2*MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NCLK',(/2*MXCFIL/),PGNAME)
      NCLK=0
C
      ALLOCATE(IBIAS(2*MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IBIAS',(/2*MXCFIL/),PGNAME)
      IBIAS=0
C
      ALLOCATE(IRUNIT(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IRUNIT',(/2,MXCFIL/),PGNAME)
      IRUNIT=0
C
      ALLOCATE(NFREQ(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NFREQ',(/MXCFIL/),PGNAME)
      NFREQ=0
C
      ALLOCATE(NEPOCH(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NEPOCH',(/MXCFIL/),PGNAME)
      NEPOCH=0
C
      ALLOCATE(IRMARK(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IRMARK',(/MXCFIL/),PGNAME)
      IRMARK=0
C
      ALLOCATE(ISTWGT(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'ISTWGT',(/MXCSTA/),PGNAME)
      ISTWGT=0
C
      ALLOCATE(SATNUM(MXCSAT,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'SATNUM',(/MXCSAT,MXCFIL/),PGNAME)
      SATNUM=0
C
      ALLOCATE(SVNFI1(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'SVNFI1',(/MXCSAT/),PGNAME)
      SVNFI1=0
C
      ALLOCATE(STFIL(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'STFIL',(/2,MXCFIL/),PGNAME)
      STFIL=0
C
      ALLOCATE(ARCINT(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ARCINT',(/MXCFIL/),PGNAME)
      ARCINT=0
C
      ALLOCATE(ARCINT2(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ARCINT2',(/MXCFIL/),PGNAME)
      ARCINT2=0
C
      ALLOCATE(NAVNUM(MXCSAT*MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'NAVNUM',(/MXCSAT*MAXARC/),PGNAME)
      NAVNUM=0
C
      ALLOCATE(NAVNUM2(MXCSAT*MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'NAVNUM2',(/MXCSAT*MAXARC/),PGNAME)
      NAVNUM2=0
C
      ALLOCATE(IDELTT(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IDELTT',(/MXCFIL/),PGNAME)
      IDELTT=0
C
      ALLOCATE(IONMOD(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IONMOD',(/MXCFIL/),PGNAME)
      IONMOD=0
C
      ALLOCATE(NDIFF(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NDIFF',(/MXCFIL/),PGNAME)
      NDIFF=0
C
      ALLOCATE(IANTEN(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IANTEN',(/2,MXCFIL/),PGNAME)
      IANTEN=0
C
      ALLOCATE(IONREQ(3,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IONREQ',(/3,MXCFIL/),PGNAME)
      IONREQ=0
C
      ALLOCATE(ICLOCK(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ICLOCK',(/2,MXCFIL/),PGNAME)
      ICLOCK=0
C
      ALLOCATE(NUMSA1(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NUMSA1',(/MXCSAT/),PGNAME)
      NUMSA1=0
C
      ALLOCATE(NUMOB1(MXCSAT,2),STAT=IRC)
      CALL ALCERR(IRC,'NUMOB1',(/MXCSAT,2/),PGNAME)
      NUMOB1=0
C
      ALLOCATE(NUMMR1(MXCSAT,2),STAT=IRC)
      CALL ALCERR(IRC,'NUMMR1',(/MXCSAT,2/),PGNAME)
      NUMMR1=0
C
      ALLOCATE(AMBIE1(MXCAMB),STAT=IRC)
      CALL ALCERR(IRC,'AMBIE1',(/MXCAMB/),PGNAME)
      AMBIE1=0
C
      ALLOCATE(AMBSA1(MXCAMB,3),STAT=IRC)
      CALL ALCERR(IRC,'AMBSA1',(/MXCAMB,3/),PGNAME)
      AMBSA1=0
C
      ALLOCATE(AMBWL1(MXCAMB,2),STAT=IRC)
      CALL ALCERR(IRC,'AMBWL1',(/MXCAMB,2/),PGNAME)
      AMBWL1=0
C
      ALLOCATE(AMBCL1(MXCAMB,3),STAT=IRC)
      CALL ALCERR(IRC,'AMBCL1',(/MXCAMB,3/),PGNAME)
      AMBCL1=0
C
      ALLOCATE(NEPFLG(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NEPFLG',(/MXCFIL/),PGNAME)
      NEPFLG=0
C
      ALLOCATE(IFRMAT(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'IFRMAT',(/MXCFIL/),PGNAME)
      IFRMAT=0
C
      ALLOCATE(MEATYP(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'MEATYP',(/MXCFIL/),PGNAME)
      MEATYP=0
C
      ALLOCATE(NUMAMB(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NUMAMB',(/MXCFIL/),PGNAME)
      NUMAMB=0
C
      ALLOCATE(AMBIEP(MXCAMB,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBIEP',(/MXCAMB,MXCFIL/),PGNAME)
      AMBIEP=0
C
      ALLOCATE(AMBSAT(MXCAMB,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBSAT',(/MXCAMB,MXCFIL/),PGNAME)
      AMBSAT=0
C
      ALLOCATE(AMBWLF(MXCAMB,2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBWLF',(/MXCAMB,2,MXCFIL/),PGNAME)
      AMBWLF=0
C
      ALLOCATE(AMBCLS(MXCAMB,3,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'AMBCLS',(/MXCAMB,3,MXCFIL/),PGNAME)
      AMBCLS=0
C
      ALLOCATE(ICLU(0:MAXSYS,MXCAMB,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ICLU',(/MAXSYS+1,MXCAMB,MXCFIL/),PGNAME)
      ICLU=0
C
      ALLOCATE(NUMOBT(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NUMOBT',(/2,MXCFIL/),PGNAME)
      NUMOBT=0
C
      ALLOCATE(NUMMRT(2,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NUMMRT',(/2,MXCFIL/),PGNAME)
      NUMMRT=0
C
      ALLOCATE(NSATEL(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NSATEL',(/MXCFIL/),PGNAME)
      NSATEL=0
C
      ALLOCATE(ICAMPN(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ICAMPN',(/MXCFIL/),PGNAME)
      ICAMPN=0
C
      ALLOCATE(STCAMP(MXCSTA,MAXCMP),STAT=IRC)
      CALL ALCERR(IRC,'STCAMP',(/MXCSTA,MAXCMP/),PGNAME)
      STCAMP=0
C
      ALLOCATE(SATSES(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'SATSES',(/MXCSAT/),PGNAME)
      SATSES=0
C
      ALLOCATE(ICLUST(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'ICLUST',(/MXCFIL/),PGNAME)
      ICLUST=0
C
      ALLOCATE(NSTDAY (MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NSTDAY',(/MXCSAT/),PGNAME)
      NSTDAY=0
C
      ALLOCATE(NSTDAY1(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NSTDAY1',(/MXCSAT/),PGNAME)
      NSTDAY1=0
C
      ALLOCATE(NSTDAY2(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NSTDAY2',(/MXCSAT/),PGNAME)
      NSTDAY2=0
C
      ALLOCATE(NUMSTC (MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NUMSTC',(/MXCSAT/),PGNAME)
      NUMSTC=0
C
      ALLOCATE(NUMSTC1(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'NUMSTC1',(/MXCSAT/),PGNAME)
      NUMSTC1=0
C
      ALLOCATE(NSTCEF (MXCSAT,MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'NSTCEF',(/MXCSAT,MAXARC/),PGNAME)
      NSTCEF=0
C
      ALLOCATE(INTSTC (MAXSTC,MXCSAT,MAXARC),STAT=IRC)
      CALL ALCERR(IRC,'INTSTC',(/MAXSTC,MXCSAT,MAXARC/),PGNAME)
      INTSTC=0
C
      ALLOCATE(ESTSAT(MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'ESTSAT',(/MXCSAT/),PGNAME)
      ESTSAT=0
C
      ALLOCATE(SATOFF (MXCSGR,MAXOFF),STAT=IRC)
      CALL ALCERR(IRC,'SATOFF',(/MXCSGR,MAXOFF/),PGNAME)
      SATOFF=0
C
      ALLOCATE(SATALB (MXCSAT,MAXALB),STAT=IRC)
      CALL ALCERR(IRC,'SATALB',(/MXCSAT,MAXALB/),PGNAME)
      SATALB=0
C
      ALLOCATE(CLKSTA (MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'CLKSTA',(/MXCSTA/),PGNAME)
      CLKSTA=0
C
      ALLOCATE(CLKSAT (MXCSAT),STAT=IRC)
      CALL ALCERR(IRC,'CLKSAT',(/MXCSAT/),PGNAME)
      CLKSAT=0
C
      ALLOCATE(INDSMP(MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'INDSMP',(/MXCFIL/),PGNAME)
      INDSMP=0
C
      ALLOCATE(NOBELV(2,18,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NOBELV',(/2,18,MXCFIL/),PGNAME)
      NOBELV=0
C
      ALLOCATE(NOBAZIS(2,36,MAXSAT,MAXFIL),STAT=IRC)
      CALL ALCERR(IRC,'NOBAZIS',(/2,36,MAXSAT,MAXFIL/),PGNAME)
      NOBAZIS=0
C
      ALLOCATE(NOBNAD(2,30,MXCFIL),STAT=IRC)
      CALL ALCERR(IRC,'NOBNAD',(/2,30,MXCFIL/),PGNAME)
      NOBNAD=0
C
      ALLOCATE(SATSPV(MXCSGR,MAXSPV),STAT=IRC)
      CALL ALCERR(IRC,'SATSPV',(/MXCSGR,MAXSPV/),PGNAME)
      SATSPV=0
C
      ALLOCATE(iCentr(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'iCentr',(/MXCSTA/),PGNAME)
      iCentr=0
C
      ALLOCATE(stName(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'stName',(/MXCSTA/),PGNAME)
      stName=''
C
      ALLOCATE(staNum(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'staNum',(/MXCSTA/),PGNAME)
      staNum=0
C
      ALLOCATE(staFlg(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'staFlg',(/MXCSTA/),PGNAME)
      staFlg=''
C
      ALLOCATE(xStEll(3,MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'xStEll',(/3,MXCSTA/),PGNAME)
      xStEll=0d0
C
      ALLOCATE(xStat(3,MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'xStat',(/3,MXCSTA/),PGNAME)
      xStat=0d0
C
      ALLOCATE(xStEcc(3,MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'xStEcc',(/3,MXCSTA/),PGNAME)
      xStEcc=0d0
C
      ALLOCATE(stargb(MXCSTA),STAT=IRC)
      CALL ALCERR(IRC,'stargb',(/MXCSTA/),PGNAME)
      stargb=0
C
      ALLOCATE(usegeos(NFTOT),STAT=IRC)
      CALL ALCERR(IRC,'usegeos',(/NFTOT/),PGNAME)
      usegeos=0
C
      ALLOCATE(gobsdef(NFTOT),STAT=IRC)
      CALL ALCERR(IRC,'gobsdef',(/NFTOT/),PGNAME)
      gobsdef(:)%norec=0
CCC      ALLOCATE(sigrgb(MXCSTA),STAT=IRC)
CCC      CALL ALCERR(IRC,'sigrgb',(/MXCSTA/),PGNAME)
CCC      sigrgb=0d0
C
C READ ALL INPUT INFORMATION FROM INPUT FILE
C ------------------------------------------
      CALL RDINPT(MXCSTA ,MXCFIL ,MAXTRM ,MAXTRP ,MAXPOL ,MAXWGT ,
     1            MXCSAT ,MAXSTP ,MAXOFF ,MAXOFR ,MAXHIL ,MAXPOT ,
     2            MAXALB ,MAXTYP ,MAXCAL ,MAXGIM ,MXCSGR ,ninpfiles,
     3            globalwindow   ,nallsta,allstanum, allstaname  ,
     4            nAllSat,allSatNum      ,STITLE ,PRIOPT ,STRAMB ,
     5            SIGAMB ,AR2MOD ,AR2INF ,CORSTR ,DTSIM  ,SIGAPR ,
     6            ICOELV ,NSAMPL ,NOINCLK,SECIPL ,ZENMAX ,ZMXLEO ,
     7            ITROPO ,IEXTRA ,ISYNCR ,ILOC   ,IHELM  ,NORB   ,
     8            SEQORB ,PREC   ,OPTDCB ,SIGDCB ,NFIX   ,STFIX  ,
     9            NKIN   ,STKIN  ,NSTWGT ,ISTWGT ,STWGT  ,WGTFILE,
     1            NCLREQ ,ISTCLK ,ISACLK ,NCLK   ,IBIAS  ,CLKWGT ,
     2            CLFRTO ,NREC   ,ISBTIM ,NTRSTA ,STATRP ,TRPLMS ,
     3            SIGTRS ,ISGTRS ,NTRREQ ,NPARTR ,TRPLIM ,SIGTRP ,
     4            ITRMAP ,ITRGRD ,NIOREQ ,IONMOD ,IONREQ ,POLMOD ,
     5            POLPAR ,HPSAVE ,NPOL   ,TPOL   ,SIGPOL ,ISGPOL ,
     6            ISGNUT ,NWGT   ,SATWGT ,TIMWGT ,WGTWGT ,NSTCEP ,
     7            FRCTYP ,NSASTC1,NUMSTC1,NSTDAY1,SIGSTC1,NSPEC1 ,
     8            NUMSPC1,TIMSPC1,SIGSPC1,NANOFF ,NSAOFF ,SATOFF ,
     9            PAROFF ,NRQOFF ,GRPOFF ,SIGOFF ,TIMOFF ,ISGOFF ,
     1            NHILL  ,HILTYP ,SIGHIL ,NPOT   ,POTTYP ,SIGPOT ,
     2            NALB   ,ALBTYP ,SIGALB ,NALBGR ,NSAALB ,SATALB ,
     3            NCENM  ,CENMAS ,SIGCEN ,nRGB   ,staRGB ,SatSpec,
     4            sigRGB ,OPTHOI ,SIGHOI ,OPTELI ,OPTPAR ,OPTDIP ,
     5            SIGDIP ,NANCAL ,ANTCAL ,NUMCAL ,PRNCAL ,NFRCAL ,
     6            NPTCAL ,SIGCAL ,OPTGIM ,POLGIM ,SIGGIM ,NAMGIM ,
     7            EPOGIM ,NANRAO ,ANTRAO ,NUMRAO ,PRNRAO ,NFRRAO ,
     8            SIGRAO ,NEURAO ,NCLKST ,NCLKSA ,CLKSTA ,CLKSAT ,
     9            EDTLVL ,IZEROD ,NEPOBS ,CLKSYS ,CLKHED ,NORB2  ,
     1            SEQORB2,NSTCEP2,FRCTYP2,PREC2  ,NSTDAY2,NUMSTC2,
     2            NSASTC2,SIGSTC2,NSPEC2 ,NUMSPC2,TIMSPC2,SIGSPC2,
     3            IEPPAR ,NORRES ,IRAUX2 ,IOREST ,NESTSAT,ESTSAT ,
     4            NOSOL  ,IQXX   ,IPHSEP ,MAXSPV ,NANSPV ,NSASPV ,
     5            SATSPV ,GNRSPV ,NPTSPV ,SIGSPV ,NADMAX ,NUTNAM ,
     6            SUBNAM ,ISASYS ,RAPZENMAX      ,GNROFF ,IPOLAR ,
     7            opLoad ,optGsp ,IREL2)
C
C CHECK CONSISTENCY OF OPTIONS, SET RESIDUAL SAVE FLAG
C ----------------------------------------------------
      CALL CHKOPT(MAXTYP ,STRAMB ,SIGAMB ,NSAMPL(1)      ,OPTELI ,
     1            OPTPAR ,OPTDIP ,SIGDIP ,OPTGIM ,POLGIM ,AR2MOD ,
     2            NIOREQ ,MELWUB ,NFTOT  ,NFRFIL ,ICARR  ,NANOFF ,
     3            NRQOFF ,GRPOFF ,NFIX   ,NKIN   ,NORB   ,NORB2  ,
     4            SEQORB ,SEQORB2,ITRGRD ,ZENMAX ,NANSPV ,CORSTR ,
     5            EPOGIM ,CLKHED ,PRIOPT ,IEPPAR ,IRESID ,IFREQ  ,
     6            NANRAO ,NANCAL ,NOSOL  ,NCLREQ ,ISTCLK ,IBIAS  ,
     7            CLFRTO ,NALLSAT,ALLSATNUM,NALLSTA,ALLSTANUM,
     8            ALLSTANAME ,IDIFF)

C New Standard Orbits if Gravity, Hill or Albedo estimation
C ---------------------------------------------------------
      IF ( npot+nhill+nalb > 0 ) THEN
        CALL stdOrb2NewInit
      END IF
C
C READ HEADER OF OBSFILES, SAVE INFORMATION FOR SUBSEQUENT USE
C ------------------------------------------------------------
      CALL HEDINF(NFTOT ,HEADER,MAXCMP,NCAMP ,CAMPGN,NDIFF ,
     1            NSTAT ,STNAME,STFIL ,NSCAMP,STCAMP,MEATYP,
     2            NSATEL,CSESS ,IDELTT,TIMREF,ICAMPN,NFREQ ,
     3            NEPOCH,IRMARK,IRUNIT,POSECC,NUMOB1,NUMMR1,
     4            NUMOBT,NUMMRT,ICLOCK,RECTYP,ANTTYP,IANTEN,
     5            IFRMAT,NEPFLG,SATNUM,NUMAMB,AMBSAT,AMBIEP,
     6            AMBWLF,AMBIGU,AMBCLS,ISASYS,MIXED,USEGEOS,
     7            GOBSDEF)
C
C ===========================
C !!!Preliminary handling for OVF and MGEX!!!: Apply obstypes only for
C GAL satellites and identically for all stations
C Prepare sat-specific obs info:
C Find index of first OBSFILE with sat-specific obstype info
      CALL init_geos(maxsat,gobsdef1)
      usegeos1 = 0
      gobsdef1%norec = 0

      INDGEOS=1
      DO IFILE=1,NFTOT
        IF (USEGEOS(IFILE).NE.1) CYCLE
        DO grec = 1,GOBSDEF(IFILE)%NOREC
          IF (GOBSDEF(IFILE)%sat(grec)%sysnum.NE.2) CYCLE
          usegeos1 = 1
          INDGEOS  = IFILE
          gobsdef1%norec = gobsdef1%norec + 1
          gobsdef1%sat(gobsdef1%norec) = GOBSDEF(INDGEOS)%sat(grec)
        ENDDO
        IF (usegeos1.EQ.1) EXIT
      ENDDO
C
C !!!Preliminary handling for OVF and MGEX!!!
C Check whether the sat-specific obstypes are the same in all
C input files and add corresponding code information if only phase
C info is available and vice versa (prepare MASTER gobsdef file for whole
C GPSEST run)
C !!! This block must be changed, if the same obstype selection couldn't be
C     assumed for all obsfiles anymore. SR HEDINF provides file-wise gobsdef !!!
      DO IFILE=1,NFTOT
        IF ((USEGEOS(IFILE).NE.1).OR.(IFILE.EQ.INDGEOS)) CYCLE

        DO grec = 1,GOBSDEF(IFILE)%NOREC
          IF (GOBSDEF(IFILE)%sat(grec)%sysnum.NE.2) CYCLE
          satnumg = GOBSDEF(IFILE)%sat(grec)%sysnum*100
     1                 + GOBSDEF(IFILE)%sat(grec)%satnum
          samesat=0
          DO jrec = 1,GOBSDEF1%NOREC
            satnumj = GOBSDEF1%sat(jrec)%sysnum*100
     1                  + GOBSDEF1%sat(jrec)%satnum
            IF (satnumg.NE.satnumj) CYCLE
            samesat=1
            DO obs = 1,8
c           Add missing info for reference geos data set from corresponding
c           geos data set of any code/phase file header
              IF ((gobsdef1%sat(jrec)%obstyp(obs).EQ.'   ')
     1           .AND.(gobsdef(IFILE)%sat(grec)%obstyp(obs).NE.'   '))
     2             gobsdef1%sat(jrec)%obstyp(obs) =
     3             gobsdef(IFILE)%sat(grec)%obstyp(obs)
c             CHECK FOR IDENTICAL OBSERVATION FREQENCIES
              IF ((gobsdef(IFILE)%sat(grec)%obstyp(obs)(2:2).NE.
     1             gobsdef1%sat(jrec)%obstyp(obs)(2:2)).AND.
     2            ((gobsdef(IFILE)%sat(grec)%obstyp(obs).NE.'   ').AND.
     3             (gobsdef1%sat(jrec)%obstyp(obs).NE.'   ')))
     4        THEN

                WRITE(LFNERR,12) IFILE,INDGEOS,satnumg,obs
12              FORMAT(/,' *** PG GPSEST: NOT THE SAME SAT-SPECIFIC',/,
     1                         16X,'FREQUENCY SELECTION IN',/,
     2                         16X,'THE OBS FILE HEADERS',/,
     3                         16X,'FILNUM 1: ',I5,/,
     4                         16X,'FILNUM 2: ',I5,/,
     5                         16X,'SATELLITE SVN : ',I3,/,
     6                         16X,'OBSTYPEINDEX: ',I1,/)
                CALL EXITRC(2)
              ENDIF
            ENDDO
            EXIT
          ENDDO
C       Update list of satellites with specific observation info
          IF (samesat.EQ.1) CYCLE
          GOBSDEF1%NOREC = GOBSDEF1%NOREC+1
          IF (GOBSDEF1%NOREC>maxsat) THEN
            WRITE(LFNERR,13) GOBSDEF1%NOREC,MAXSAT
13          FORMAT(/,' ### PG GPSEST: TOO MANY SATELLITES WITH',/,
     1                         16X,'SPECIFIC OBSTYPE DEFINITION -',/,
     2                         16X,'MAXSAT EXCEEDED',/,
     3                         16X,'# OF RECORDS: ',I5,/,
     4                         16X,'MAXSAT      : ',I5,/)
            CALL EXITRC(2)
          ELSE
            GOBSDEF1%sat(GOBSDEF1%NOREC) =
     1                             GOBSDEF(IFILE)%sat(grec)

            WRITE(LFNERR,14) INDGEOS,IFILE,satnumg
14          FORMAT(/,' ### PG GPSEST: SAT-SPECIFIC',/,
     1                         16X,'OBSERVATION INFO UPDATED',/,
     2                         16X,'FILNUM MISSING: ',I5,/,
     3                         16X,'FILNUM UPDATE : ',I5,/,
     4                         16X,'SATELLITE SVN : ',I3,/,
     5                         16X,'CONTINUING WITH THE NEXT FILE',/)
          ENDIF
        ENDDO
      ENDDO
c
C DEALLOCATE GOBSDEF AND USEGEOS
      IF (ALLOCATED(GOBSDEF)) THEN
          DEALLOCATE(GOBSDEF,STAT=IRC)
      ENDIF
      IF (ALLOCATED(USEGEOS)) THEN
          DEALLOCATE(USEGEOS,STAT=IRC)
      ENDIF
C
      IF (USEGEOS1==1) THEN
        DO jrec = 1,GOBSDEF1%NOREC
          satnumj = GOBSDEF1%sat(jrec)%sysnum*100
     1                  + GOBSDEF1%sat(jrec)%satnum
c CHECK FOR CONSISTENCY OF SAT-SPECIFIC CODE AND PHASE OBSTYPES
          IF ((((gobsdef1%sat(jrec)%obstyp(1).NE.'   ')
     1       .AND.(gobsdef1%sat(jrec)%obstyp(3).NE.'   '))
     2       .AND.(gobsdef1%sat(jrec)%obstyp(1)(2:2).NE.
     3        gobsdef1%sat(jrec)%obstyp(3)(2:2))).OR.
     4        (((gobsdef1%sat(jrec)%obstyp(2).NE.'   ')
     5       .AND.(gobsdef1%sat(jrec)%obstyp(4).NE.'   '))
     6       .AND.(gobsdef1%sat(jrec)%obstyp(2)(2:2).NE.
     7        gobsdef1%sat(jrec)%obstyp(4)(2:2))).OR.
     8        (((gobsdef1%sat(jrec)%obstyp(5).NE.'   ')
     9       .AND.(gobsdef1%sat(jrec)%obstyp(7).NE.'   '))
     1       .AND.(gobsdef1%sat(jrec)%obstyp(5)(2:2).NE.
     2        gobsdef1%sat(jrec)%obstyp(7)(2:2))).OR.
     3        (((gobsdef1%sat(jrec)%obstyp(6).NE.'   ')
     4       .AND.(gobsdef1%sat(jrec)%obstyp(8).NE.'   '))
     5       .AND.(gobsdef1%sat(jrec)%obstyp(6)(2:2).NE.
     6        gobsdef1%sat(jrec)%obstyp(8)(2:2)))) THEN
C
           WRITE(LFNERR,15) satnumj
15         FORMAT(/,' *** PG GPSEST: SAT-SPECIFIC CODE AND PHASE OBS',/,
     1                         16X,'ARE NOT CONSISTENT',/,
     2                         16X,'FOR SATELLITE : ',I3,/)
           CALL  EXITRC(2)
          ENDIF
C TAKE CODE FREQ FROM CORRESPONDING PHASE OR VICE VERSA IF ONE IS MISSING
          DO ii=1,2
            IF ((gobsdef1%sat(jrec)%obstyp(ii).NE.'   ').AND.
     1          (gobsdef1%sat(jrec)%obstyp(ii+2).EQ.'   ')) THEN
              gobsdef1%sat(jrec)%obstyp(ii+2)(1:1)='L'
              gobsdef1%sat(jrec)%obstyp(ii+2)(2:3)=
     1                 gobsdef1%sat(jrec)%obstyp(ii)(2:3)
            ENDIF
c
            IF ((gobsdef1%sat(jrec)%obstyp(ii+4).NE.'   ').AND.
     1          (gobsdef1%sat(jrec)%obstyp(ii+2+4).EQ.'   ')) THEN
              gobsdef1%sat(jrec)%obstyp(ii+2+4)(1:1)='L'
              gobsdef1%sat(jrec)%obstyp(ii+2+4)(2:3)=
     1                 gobsdef1%sat(jrec)%obstyp(ii+4)(2:3)
            ENDIF
C
            IF ((gobsdef1%sat(jrec)%obstyp(ii+2).NE.'   ').AND.
     1          (gobsdef1%sat(jrec)%obstyp(ii).EQ.'   ')) THEN
              gobsdef1%sat(jrec)%obstyp(ii)(1:1)='C'
              gobsdef1%sat(jrec)%obstyp(ii)(2:3)=
     1                              gobsdef1%sat(jrec)%obstyp(ii+2)(2:3)
            ENDIF
c
            IF ((gobsdef1%sat(jrec)%obstyp(ii+2+4).NE.'   ').AND.
     1          (gobsdef1%sat(jrec)%obstyp(ii+4).EQ.'   ')) THEN
              gobsdef1%sat(jrec)%obstyp(ii+4)(1:1)='C'
              gobsdef1%sat(jrec)%obstyp(ii+4)(2:3)=
     1                    gobsdef1%sat(jrec)%obstyp(ii+2+4)(2:3)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C =====================
C
C Compile list of antennas and initialize antenna buffer
C ------------------------------------------------------
      epo = (globalwindow%t(1) + globalwindow%t(2)) / 2
      iant = 0
      DO ii=1,NFTOT
        DO isat=1,NALLSAT
          IF (MEATYP(ii) == 3) THEN
            CALL gtsensor(allSatNum(isat),epo,typeSLR,name,numb)
          ELSE
            CALL gtsensor(allSatNum(isat),epo,typeMWTR,name,numb)
          ENDIF
          WRITE(anten,"(A20,I6)")name,numb
          dummy=listc1(1,26,nallsat+nallsta,antlst,anten,iant)
        ENDDO
      ENDDO
      DEALLOCATE(SATLST,STAT=IRC)
      DEALLOCATE(ISOBSA,STAT=IRC)
      DO ii=1,NFTOT
        DO ista=1,2
          IF (ANTTYP(ista,ii) /= ''.AND. ANTTYP(ista,ii)/=MTypeSLR) THEN
            WRITE(anten,"(A20,I6)")ANTTYP(ista,ii),IANTEN(ista,ii)
            dummy=listc1(1,26,nallsat+nallsta,antlst,anten,iant)
          ENDIF
        ENDDO
      ENDDO
      CALL init_buf(antlist=antlst)
C
C ESTIMATION OF TRP NOT POSSIBLE FOR RANGE MEASUREMENTS
      DO II=1,NFTOT
        IF (MEATYP(II).EQ.3 .AND. OPTPAR(6).NE.0) THEN
          WRITE(LFNERR,'(/,A,/,A)')
     1      ' *** PG GPSEST: The estimation of troposphere parameters',
     2      '                is not available for range measurements.'
          CALL EXITRC(2)
        ENDIF
      ENDDO
C
C GET STATION COORDINATES AND ECCENTRICITY INFORMATION
C ----------------------------------------------------
      CALL GETSTA(NSTAT,STNAME,STANUM,NCENTR,ICENTR,
     1            XSTAT,XSTELL,XSTECC,
     2            DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C TRANSFORM EXTERNAL STATION NUMBERS INTO INTERNAL ONES
C -----------------------------------------------------
      CALL STEXIN(NSTAT ,STNAME,allStaName   ,NCENTR,ICENTR,
     1            NFIX  ,NKIN  ,NCLREQ,NSTWGT,NTRSTA,STFIX ,
     2            STKIN ,ISTCLK,ISTWGT,STATRP,NCLKST,CLKSTA,
     3            NRGB  ,STARGB)
C
C READ SATELLITE ARC INFORMATION AND STOCHASTIC INFO
C --------------------------------------------------
      CALL TMIDLE(NFTOT  ,NEPOCH ,IDELTT ,TIMREF ,WINDOW ,TIMMID)
      CALL OISTCI(NFTOT  ,TIMMID ,NSASTC1,NUMSTC1,NSTDAY1,
     1            NSTCEP ,FRCTYP ,SIGSTC1,NSPEC1 ,NUMSPC1,TIMSPC1,
     2            SIGSPC1,nallsat, allsatnum     ,NMXINT ,NARC   ,
     3            ARCINT ,NUMSAT ,SOURCE ,TBOUND ,NAVNUM ,IORSYS ,
     4            NSTCEF ,TIMSTC ,INTSTC)
      NMXARC=NARC
C
      CALL GTFLNA(0,'LEOSTD ',LEOSTD,IRC)
      IF (IRC.EQ.0.AND.isLeo) THEN
        CALL OISTCF(LEOSTD ,NFTOT  ,TIMMID ,NSASTC2,NUMSTC2,
     1              NSTDAY2,NSTCEP2,FRCTYP2,SIGSTC2,NSPEC2 ,NUMSPC2,
     2              TIMSPC2,SIGSPC2,NARC2  ,ARCINT2,NUMSAT2,SOURCE2,
     3              TBOUND2,NAVNUM2,IORSYS2,NSTCEF2,TIMSTC2,INTSTC2,
     4              NMXINT2)
        NMXINT=NMXINT2
        NMXARC=NARC2
      ELSE
        ARCINT2(1:MXCFIL)=1
        NARC2=0
      END IF
C
C INIT THE BUFFER FOR VIENNA GRID FILES
      CALL initGridBuffer(xstell=XSTELL,TIMINT=GLOBALWINDOW)
C
C DEFINE SATELLITE CLOCK ESTIMATION
C ---------------------------------
C
C DEFINE SATELLITE CLOCK ESTIMATION
C ---------------------------------
      CALL CLKINI(TITLE  ,NSTAT  ,STNAME ,XSTAT  ,DATUM  ,
     1            NFTOT  ,TIMREF ,IDELTT ,NEPOCH ,WINDOW ,NSAMPL ,
     2            DTSIM  ,OPTELI ,nallsat,allsatnum      ,NCLKST ,
     3            CLKSTA ,NCLKSA ,CLKSAT ,CLKHED ,CLKREC)
C
      IF (NCLKST.NE.0 .OR. NCLKSA.NE.0) THEN
        ALLOCATE(CLKPAR(2,CLKHED%NSTA+CLKHED%NSAT),STAT=IRC)
        CALL ALCERR(IRC,'CLKPAR',(/2,CLKHED%NSTA+CLKHED%NSAT/),PGNAME)
        CLKPAR=0
      ELSE
        ALLOCATE(CLKPAR(2,1),STAT=IRC)
        CALL ALCERR(IRC,'CLKPAR',(/2,1/),PGNAME)
        CLKPAR=0
      ENDIF
C
C Deallocate list of stations and satellites
C ------------------------------------------
      DEALLOCATE(allStaNum, stat=irc)
      DEALLOCATE(allStaName,stat=irc)
C
C Check whether the receiver antenna requests are covered by data
C ---------------------------------------------------------------
      CALL chkant(isasys, nftot,  nfreq,  ndiff,  rectyp, anttyp,
     1            ianten, nsatel, satnum, nanrao, antrao, numrao,
     2            sigrao, prnrao, nfrrao, nancal, antcal, numcal,
     3            prncal, nfrcal, nptcal, sigcal)

C
C DEFINE SESSIONS ACCORDING TO PARAMETER CORSTR
C ---------------------------------------------
      CALL DEFSES(NFTOT ,CSESS ,MELWUB,IZEROD,CORSTR,NSATEL,SATNUM,
     1            NFRFIL,NALLSAT,ALLSATNUM,ISASYS,NSESS,SESSID,NMXFLS,
     2            NMXSAS,NMXFRS)
C
      CALL CHKMAX('MAXFLS','files simultaneously processed',
     1            IMXFLS,NMXFLS,MAXFLS,'M_MAXDIM',MXCFLS,IRCFLS)
C
      CALL CHKMAX('MAXSAS','satellites simultaneously processed',
     1            IMXSAS,NMXSAS,MAXSAS,'M_MAXDIM',MXCSAS,IRCSAS)
C
      IMXFRS=0
      CALL CHKMAX('MAXFRS','frequencies simultaneously processed',
     1            IMXFRS,NMXFRS,MAXFRS,'P_GPSEST',MXCFRS,IRCFRS)
      IF (IRCFLS+IRCSAS+IRCFRS.NE.0) CALL EXITRC(2)
C
      MXCEQN=MXCSAS*MXCFLS*MXCFRS+1
C
      NMXSAP=MAXSAT
      IF (isLEO) NMXSAP=NFTOT
C
C DEFINE SEQUENCE OF PARAMETERS:
C -----------------------------
      CALL INIMAX(NFTOT  ,STFIL  ,NSTAT  ,ICENTR ,NFIX   ,STFIX  ,
     1            NKIN   ,STKIN  ,NCLREQ ,ISTCLK ,NCLK   ,IBIAS  ,
     2            CLFRTO ,nRec   ,isbTim ,TIMREF ,NARC   ,NORB   ,
     3            NSATEL ,SATNUM ,NUMSAT ,NAVNUM ,ARCINT ,NFRFIL ,
     4            ICARR  ,MEATYP ,MELWUB ,NIOREQ ,IONREQ ,OPTDCB ,
     5            POLMOD ,POLPAR ,NPOL   ,NTRREQ ,NPARTR ,NTRSTA ,
     6            STATRP ,ITRGRD ,NUMAMB ,AMBCLS ,AMBSAT ,AMBIEP ,
     7            AMBDEF ,NSASTC1,NUMSTC1,NSTCEF ,NSTCEP ,NRQOFF ,
     8            PAROFF ,NHILL  ,NPOT   ,NALBGR ,NALB   ,NCENM  ,
     1            NEPOCH ,WINDOW ,NSAMPL ,IDELTT ,DTSIM  ,NANCAL ,
     2            NFRCAL ,NPTCAL ,STRAMB ,OPTDIP ,OPTHOI ,OPTELI ,
     2            OPTGIM ,POLGIM ,NAMGIM ,EPOGIM ,NANRAO ,NFRRAO ,
     3            NEURAO ,NCLKST ,NCLKSA ,CLKSYS ,NRGB   ,CLKHED ,
     5            CLKREC ,NDIFF  ,STNAME ,IOREST ,NESTSAT,ESTSAT ,
     6            NARC2  ,ARCINT2,NUMSAT2,NAVNUM2,NSASTC2,NSTCEF2,
     7            NUMSTC2,NSTCEP2,NORB2  ,NANSPV ,NPTSPV ,NSASPV ,
     8            CORSTR ,MAXTYP ,MIXED  ,OPLOAD ,MXCSAT ,MXCAMB ,
     9            NMXFLS ,NMXSAS ,NMXFRS ,NMXSNG ,NEPSNG ,NMXLOC ,
     1            NMXPAR ,NMXAMP ,RECTYP ,IRUNIT ,globalWindow   ,
     2            NUMISB ,OPTGSP)
C
      CALL CHKMAX('MAXSNG','non-zero elements in one line '//
     1            'of first design matrix',
     2            IMXSNG,NMXSNG,MAXSNG,'P_GPSEST',MXCSNG,IRCSNG)
C
      CALL CHKMAX('MAXPAR','parameters simultaneously processed',
     1            IMXPAR,NMXPAR,MAXPAR,'P_GPSEST',MXCPAR,IRCPAR)
C
      CALL CHKMAX('MAXAMP','ambiguities simultaneously processed',
     1            IMXAMP,NMXAMP,MAXAMP,'P_GPSEST',MXCAMP,IRCAMP)
C
      CALL CHKMAX('MAXLOC','parameters to be processed',
     1            IMXLOC,NMXLOC,MAXLOC,'P_GPSEST',MXCLOC,IRCLOC)
      IF (IRCSNG+IRCPAR+IRCAMP+IRCLOC.NE.0) CALL EXITRC(2)
C
      MXCPAE=1
C
C ALLOCATE ALL REMAINING ARRAYS
C -----------------------------
      MYDIM=MAX(MXCEQN,MXCPAR)
C
      ALLOCATE(IAMB1(MXCAMB,MAXFRQ,MXCFLS),STAT=IRC)
      CALL ALCERR(IRC,'IAMB1',(/MXCAMB,MAXFRQ,MXCFLS/),PGNAME)
      IAMB1=''
C
      ALLOCATE(LSMAT(MYDIM),STAT=IAC)
      CALL ALCERR(IAC,'LSMAT',(/MYDIM/),PGNAME)
      LSMAT=''
C
      ALLOCATE(OBSFLG(MXCSAT,MAXFRQ,MXCFLS),STAT=IRC)
      CALL ALCERR(IRC,'OBSFLG',(/MXCSAT,MAXFRQ,MXCFLS/),PGNAME)
      OBSFLG=''
C
      ALLOCATE(BNOR(MXCPAR),STAT=IRC)
      CALL ALCERR(IRC,'BNOR',(/MXCPAR/),PGNAME)
      BNOR=0d0
C
      ALLOCATE(ASING(MXCEQN*MXCSNG),STAT=IRC)
      CALL ALCERR(IRC,'ASING',(/MXCEQN*MXCSNG/),PGNAME)
      ASING=0d0
C
      ALLOCATE(AAUX(MXCSNG),STAT=IRC)
      CALL ALCERR(IRC,'AAUX',(/MXCSNG/),PGNAME)
      AAUX=0d0
C
      ALLOCATE(BSING(MXCEQN),STAT=IRC)
      CALL ALCERR(IRC,'BSING',(/MXCEQN/),PGNAME)
      BSING=0d0
C
      ALLOCATE(AHELP(MXCLOC),STAT=IRC)
      CALL ALCERR(IRC,'AHELP',(/MXCLOC/),PGNAME)
      AHELP=0d0
C
      ALLOCATE(B1(MXCPAR),STAT=IRC)
      CALL ALCERR(IRC,'B1',(/MXCPAR/),PGNAME)
      B1=0d0
C
      ALLOCATE(AMB0(MXCAMB,MAXFRQ,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'AMB0',(/MXCAMB,MAXFRQ,MXCFLS/),PGNAME)
      AMB0=0d0
C
      ALLOCATE(RMSAMB(MXCAMP),STAT=IAC)
      CALL ALCERR(IAC,'RMSAMB',(/MXCAMP/),PGNAME)
      RMSAMB=0d0
C
      ALLOCATE(OBSERV(MXCSAT,MAXFRQ,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'OBSERV',(/MXCSAT,MAXFRQ,MXCFLS/),PGNAME)
      OBSERV=0d0
C
      ALLOCATE(WGSSES(3,2,MAXFRQ,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'WGSSES',(/3,2,MAXFRQ,MXCFLS/),PGNAME)
      WGSSES=0d0
C
      ALLOCATE(ELLSES(3,2,MAXFRQ,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'ELLSES',(/3,2,MAXFRQ,MXCFLS/),PGNAME)
      ELLSES=0d0
C
      ALLOCATE(VSIG(MXCEQN),STAT=IAC)
      CALL ALCERR(IAC,'VSIG',(/MXCEQN/),PGNAME)
      VSIG=0d0
C
      ALLOCATE(SYNCM(MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'SYNCM',(/MXCFLS/),PGNAME)
      SYNCM=0d0
C
      ALLOCATE(POLARS(MXCAMB,MAXFRQ,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'POLARS',(/MXCAMB,MAXFRQ,MXCFLS/),PGNAME)
      POLARS=0d0
C
      ALLOCATE(WEIGHT(MXCEQN*(MXCEQN+1)/2),STAT=IAC)
      CALL ALCERR(IAC,'WEIGHT',(/MXCEQN*(MXCEQN+1)/2/),PGNAME)
      WEIGHT=0d0
C
      ALLOCATE(INDP(MXCLOC),STAT=IAC)
      CALL ALCERR(IAC,'INDP',(/MXCLOC/),PGNAME)
      INDP=0
C
      ALLOCATE(LOCHLP(MAXLCQ,MXCLOC),STAT=IAC)
      CALL ALCERR(IAC,'LOCHLP',(/MAXLCQ,MXCLOC/),PGNAME)
      LOCHLP=0
C
      ALLOCATE(SVNFIL(MXCSAT,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'SVNFIL',(/MXCSAT,MXCFLS/),PGNAME)
      SVNFIL=0
C
      ALLOCATE(ISYNC(MXCSAT,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'ISYNC',(/MXCSAT,MXCFLS/),PGNAME)
      ISYNC=0
C
      ALLOCATE(AMB1(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'AMB1',(/MXCPAR/),PGNAME)
      AMB1=0
C
      ALLOCATE(AMB2(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'AMB2',(/MXCPAR/),PGNAME)
      AMB2=0
C
      ALLOCATE(AMB3(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'AMB3',(/MXCPAR/),PGNAME)
      AMB3=0
C
      ALLOCATE(MATCH(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'MATCH',(/MXCPAR/),PGNAME)
      MATCH=0
C
      ALLOCATE(IDEL(MXCLOC),STAT=IAC)
      CALL ALCERR(IAC,'IDEL',(/MXCLOC/),PGNAME)
      IDEL=0
C
      ALLOCATE(FILNUM(MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'FILNUM',(/MXCFLS/),PGNAME)
      FILNUM=0
C
      ALLOCATE(ISV12(2,MXCEQN),STAT=IAC)
      CALL ALCERR(IAC,'ISV12',(/2,MXCEQN/),PGNAME)
      ISV12=0
C
      ALLOCATE(IFIL(MXCEQN),STAT=IAC)
      CALL ALCERR(IAC,'IFIL',(/MXCEQN/),PGNAME)
      IFIL=0
C
      ALLOCATE(INDRMS(MXCAMP),STAT=IAC)
      CALL ALCERR(IAC,'INDRMS',(/MXCAMP/),PGNAME)
      INDRMS=0
C
      ALLOCATE(IOBNUM(MXCEQN),STAT=IAC)
      CALL ALCERR(IAC,'IOBNUM',(/MXCEQN/),PGNAME)
      IOBNUM=0
C
      ALLOCATE(IFRSES(MXCEQN),STAT=IAC)
      CALL ALCERR(IAC,'IFRSES',(/MXCEQN/),PGNAME)
      IFRSES=0
C
      ALLOCATE(OBSNUM(MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'OBSNUM',(/MXCFLS/),PGNAME)
      OBSNUM=0
C
      ALLOCATE(OBSEPO(MXCSAT,MAXFRQ,MXCFLS),STAT=IAC)
      CALL ALCERR(IAC,'OBSEPO',(/MXCSAT,MAXFRQ,MXCFLS/),PGNAME)
      OBSEPO=0
C
      ALLOCATE(OBSCLS(MXCAMP),STAT=IAC)
      CALL ALCERR(IAC,'OBSCLS',(/MXCAMP/),PGNAME)
      OBSCLS=0
C
      ALLOCATE(REFAMB(MXCAMP),STAT=IAC)
      CALL ALCERR(IAC,'REFAMB',(/MXCAMP/),PGNAME)
      REFAMB=0
C
      ALLOCATE(PARFLG(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'PARFLG',(/MXCPAR/),PGNAME)
      PARFLG=0
C
      ALLOCATE(INDA(MXCEQN*MXCSNG),STAT=IAC)
      CALL ALCERR(IAC,'INDA',(/MXCEQN*MXCSNG/),PGNAME)
      INDA=0
C
      ALLOCATE(INDAUX(MXCSNG),STAT=IAC)
      CALL ALCERR(IAC,'INDAUX',(/MXCSNG/),PGNAME)
      INDAUX=0
C
      ALLOCATE(locq(MAXLCQ,MXCLOC),STAT=IAC)
      CALL ALCERR(IAC,'locq',(/MAXLCQ,MXCLOC/),PGNAME)
      locq=0
C
      ALLOCATE(nobspa(MAXMEA*MAXSYS,MXCLOC),STAT=IAC)
      CALL ALCERR(IAC,'nobspa',(/MAXMEA*MAXSYS,MXCLOC/),PGNAME)
      nobspa=0
C
      ALLOCATE(xxx(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'xxx',(/MXCPAR/),PGNAME)
      xxx=0d0
C
      ALLOCATE(XXX0(MXCPAR),STAT=IAC)
      CALL ALCERR(IAC,'XXX0',(/MXCPAR/),PGNAME)
      XXX0=1d20
C
      ALLOCATE(aNor(MXCPAR*(MXCPAR+1)/2),STAT=IAC)
      CALL ALCERR(IAC,'aNor',(/MXCPAR*(MXCPAR+1)/2/),PGNAME)
      aNor=0d0
C
      ALLOCATE(XXREF(clkhed%ref(1)%nRef),STAT=IAC)
      CALL ALCERR(IAC,'XXREF',(/clkhed%ref(1)%nRef/),PGNAME)
C
      ALLOCATE(XREF(clkhed%ref(1)%nRef,clkrec%nEpo),STAT=IAC)
      CALL ALCERR(IAC,'XREF',(/clkhed%ref(1)%nRef,clkrec%nEpo/),PGNAME)
C
      ALLOCATE(TIMISB(3,NUMISB),STAT=IAC)
      CALL ALCERR(IAC,'TIMISB',(/3,NUMISB/),PGNAME)
C
      ALLOCATE(PARTYP(MXCLOC),STAT=IAC)
      CALL ALCERR(IAC,'PARTYP',(/MXCLOC/),PGNAME)
      PARTYP(:)%TYPE  = ''
      PARTYP(:)%OMEGA = 0D0
C
C
C DEFINE SEQUENCE OF PARAMETERS:
C -----------------------------
      CALL SEQPAR(MAXTYP ,NFTOT  ,STFIL  ,NSTAT  ,ICENTR ,NFIX   ,
     1            STFIX  ,NKIN   ,STKIN  ,NCLREQ ,ISTCLK ,ISACLK ,
     2            NCLK   ,IBIAS  ,CLFRTO ,TIMREF ,NARC   ,NORB   ,
     3            NSATEL ,SATNUM ,NUMSAT ,NAVNUM ,SEQORB ,ARCINT ,
     4            NFRFIL ,ICARR  ,MEATYP ,MELWUB ,NIOREQ ,IONREQ ,
     5            IONMOD ,OPTDCB ,POLMOD ,POLPAR ,NPOL   ,NTRREQ ,
     6            NPARTR ,NTRSTA ,STATRP ,ITRGRD ,NUMAMB ,AMBCLS ,
     6            AMBSAT ,AMBIEP ,AMBDEF ,NSASTC1,NUMSTC1,NSTCEF ,
     7            NSTCEP ,FRCTYP ,NRQOFF ,PAROFF ,GRPOFF ,NHILL  ,
     8            HILTYP ,NPOT   ,POTTYP ,NALBGR ,NALB   ,ALBTYP ,
     9            NCENM  ,CENMAS ,NEPOCH ,WINDOW ,NSAMPL ,IDELTT ,
     1            DTSIM  ,NANCAL ,NFRCAL ,NPTCAL ,STRAMB ,OPTDIP ,
     2            OPTHOI ,OPTELI ,OPTGIM ,POLGIM ,NAMGIM ,EPOGIM ,
     3            NANRAO ,NFRRAO ,NEURAO ,NPAR   ,NAMB   ,NPARN  ,
     4            NPARMS ,LOCQ   ,PARLST ,PARTYP ,NCLKST ,CLKSTA ,
     5            NCLKSA ,CLKSAT ,CLKSYS ,CLKHED ,CLKREC ,NDIFF  ,
     6            STNAME ,IOREST ,ISASYS ,NESTSAT,ESTSAT ,NARC2  ,
     7            ARCINT2,NUMSAT2,NAVNUM2,NSASTC2,NSTCEF2,NUMSTC2,
     8            NSTCEP2,FRCTYP2,NORB2  ,SEQORB2,NANSPV ,GNRSPV ,
     9            NPTSPV ,NSASPV ,GNROFF ,OPLOAD ,MXCSAT ,MXCAMB ,
     .            NRGB   ,STARGB ,SATSPEC,nRec   ,ISBTIM ,RECTYP ,
     2            IRUNIT ,globalWindow   ,TIMISB ,OPTGSP ,USEGEOS1,
     3            GOBSDEF1)
C
C INITIALIZE NORMAL EQUATION SYSTEM AND DEFINE A PRIORI WEIGHTS:
C -------------------------------------------------------------
      IFLAG = 0
      CALL NORINI(STRAMB ,NPAR   ,NPARN  ,MXCPAR ,MXCAMP ,SIGAPR ,
     1            LOCQ   ,NSTWGT ,ISTWGT ,STWGT  ,CLKWGT ,PREC   ,
     2            SIGPOL ,NSASTC1,NSASTC2,NUMSTC1,NUMSTC2,SIGSTC1,
     3            SIGSTC2,SCASTC ,INTSTC ,INTSTC2,AELL   ,BELL   ,
     4            XSTELL ,ANOR   ,BNOR   ,OPTDCB ,SIGDCB ,SIGTRP ,
     5            SIGTRS ,ISGTRS ,NOBS   ,RMS    ,INDP   ,TPOL   ,
     6            ISGPOL ,SIGOFF ,ISGOFF ,SIGHIL ,SIGPOT ,SCAHIL ,
     7            SCAPOT ,SIGALB ,SIGCEN ,SCAALB ,SCACEN ,ISGNUT ,
     8            SIGCAL ,SIGGIM ,SCAGIM ,SIGRAO ,IFLAG  ,OPTELI ,
     9            MIXED  ,IZEROD ,PREC2  ,SIGSPV ,SIGRGB ,NOBSPA ,
     .            OPLOAD ,SIGHOI ,TIMISB ,NCLKSA ,NMXINT ,NMXSAP ,
     1            NMXARC ,OPTGSP)
C
C PRINT ALL RELEVANT OR REQUESTED A PRIORI INFORMATION
C ----------------------------------------------------
      CALL PRIAPR(MAXTYP ,NFTOT  ,HEADER ,OBSFIL ,NFRFIL ,ICARR  ,
     1            WINDOW ,NCAMP  ,CAMPGN ,NSTAT  ,STNAME ,STFIL  ,
     2            NSCAMP ,STCAMP ,MEATYP ,NSATEL ,CSESS  ,IDELTT ,
     3            TIMREF ,ICAMPN ,NFREQ  ,NEPOCH ,IRMARK ,NEPFLG ,
     4            ICLOCK ,SATNUM ,NUMAMB ,AMBSAT ,AMBIEP ,AMBWLF ,
     5            AMBIGU ,AMBCLS ,STANUM ,NCENTR ,ICENTR ,XSTAT  ,
     6            XSTELL ,XSTECC ,DATUM  ,AELL   ,BELL   ,DXELL  ,
     7            DRELL  ,SCELL  ,NARC   ,ARCINT ,NUMSAT ,SOURCE ,
     8            TBOUND ,NAVNUM ,NPAR   ,LOCQ   ,NSESS  ,SESSID ,
     9            STITLE ,PRIOPT ,ISYNCR ,ITROPO ,IEXTRA ,AMBDEF ,
     1            STRAMB ,AMBSAV ,SIGAMB ,ZENMAX ,ZMXLEO ,NSAMPL ,
     2            ISASYS ,SIGAPR ,ICOELV ,NORRES ,NESTSAT,ESTSAT ,
     3            NORB   ,SEQORB ,PREC   ,NFIX   ,STFIX  ,NCLREQ ,
     4            ISTCLK ,ISACLK ,NCLK   ,IBIAS  ,CLKWGT ,CLFRTO ,
     5            NSTWGT ,ISTWGT ,STWGT  ,WGTFILE,NIOREQ ,IONMOD ,
     6            IONREQ ,OPTDCB ,SIGDCB ,NTRREQ ,NPARTR ,TRPLIM ,
     7            SIGTRP ,NTRSTA ,STATRP ,TRPLMS ,SIGTRS ,ISGTRS ,
     8            POLPAR ,NPOL   ,TPOL   ,SIGPOL ,ISGPOL ,
     9            CORSTR ,DTSIM  ,NWGT   ,SATWGT ,TIMWGT ,WGTWGT ,
     1            AR2MOD ,AR2INF ,NSTCEP ,FRCTYP ,NSASTC1,NUMSTC1,
     2            NSTCEF ,TIMSTC ,SIGSTC1,NANOFF ,NSAOFF ,SATOFF ,
     3            PAROFF ,NRQOFF ,GRPOFF ,SIGOFF ,TIMOFF ,ISGOFF ,
     4            ITRMAP ,ITRGRD ,OPTDIP ,SIGDIP ,ISGNUT ,NANCAL ,
     5            ANTCAL ,NUMCAL ,PRNCAL ,NFRCAL ,NPTCAL ,SIGCAL ,
     6            OPTGIM ,POLGIM ,SIGGIM ,NAMGIM ,EPOGIM ,NANRAO ,
     7            ANTRAO ,NUMRAO ,PRNRAO ,NFRRAO ,SIGRAO ,NEURAO ,
     8            NDIFF  ,NUMSA1 ,NUMOB1 ,NUMMR1 ,NUMAM1 ,AMBSA1 ,
     9            AMBIE1 ,AMBWL1 ,AMBIG1 ,AMBCL1 ,FILNUM ,TAECMP ,
     1            CLKHED ,NCLKST ,NCLKSA ,NOINCLK,SECIPL ,CLKSYS ,
     2            TITLE  ,TITLES ,NSASTC2,NUMSTC2,SIGSTC2,TIMSTC2,
     3            NARC2  ,ARCINT2,SOURCE2,TBOUND2,NAVNUM2,
     4            NSTCEF2,RECTYP ,IRUNIT ,NORB2  ,NSTCEP2,SEQORB2,
     5            PREC2  ,FRCTYP2,NANSPV ,NSASPV ,SATSPV ,GNRSPV ,
     6            NPTSPV ,SIGSPV ,ANTTYP ,GNROFF ,NADMAX ,RAPZENMAX,
     7            EDTLVL ,IPOLAR ,NSHD   ,SATSHD ,TIMSHD ,FLGSHD ,
     8            IZEROD ,IQXX   ,IPHSEP ,OPLOAD ,TIMISB ,OPTGSP ,
     9            IREL2  ,nAllSat,allSatNum)
C
C INITIALIZE NUMBERS OF CLUSTERS FOR AMBIGUITY PARAMETERS
C -------------------------------------------------------
      DO 600 I=1,MXCAMP
        OBSCLS(I)=-1
600   CONTINUE
      DO 650 I=1,MXCFIL
        ICLUST(I)=0
650   CONTINUE
      DO 660 I=1,MXCPAR
        AMB1(I)=1
660   CONTINUE
C
C LOOP OVER ALL SESSIONS
C ----------------------
      DO 400 ISES=1,NSESS
        LSTSES=(ISES.EQ.NSESS)
C
C INITIALIZE SESSION
C ------------------
        CALL INISES(SESSID(ISES)   ,STRAMB ,CORSTR ,NPAR   ,NPARN  ,
     1              MXCPAR ,MXCAMP ,MAXFRS ,PRIOPT(7)      ,TPRINT ,
     2              IEXTRA ,NFTOT  ,OBSFIL ,CSESS  ,LOCQ   ,ANOR   ,
     3              BNOR   ,AELL   ,BELL   ,DXELL  ,DRELL  ,SCELL  ,
     4              TITLES ,POSECC ,MELWUB ,IZEROD ,MEATYP ,NFREQ  ,
     5              ARCINT ,TBOUND ,NUMSAT ,STFIL  ,STNAME ,XSTAT  ,
     6              XSTELL ,NFRFIL ,ICARR  ,TIMREF ,NEPOCH ,IDELTT ,
     7              NSATEL ,SATNUM ,OPTDIP ,OPTELI ,NSASES ,NFLSES ,
     8              NFRSES ,FRQSES ,STASES ,SATSES ,FILNUM ,IFDONE ,
     9              NPASES ,NAMSES ,IELE0  ,SYNCM  ,ISYNC  ,WGSSES ,
     1              ELLSES ,IAMB1  ,INDARC ,XMAXCL ,TLAST  ,
     2              RMSSES(ISES)   ,POLARS ,NDIFF  ,ELEVMM ,NOBELV ,
     3              ARCINT2,TBOUND2,LEOARC ,NAMAX  ,NADIGN ,NOBNAD ,
     4              NADIMM ,OBSEPO ,NOBAZIS,ICLU,USEGEOS1,GOBSDEF1)
C
C LOOP OVER ALL OBSERVATION EPOCHS
C --------------------------------
        DO 300 IEP=1,1000000
C
C EPOCH PROCESSING
C ----------------
          CALL PRCEPO(SESSID(ISES)   ,STRAMB ,IRESID ,PRIOPT ,TPRINT ,
     1                NPASES ,NSASES ,NFLSES ,NFRSES ,FRQSES ,FILNUM ,
     2                TLAST  ,NSTAT  ,ICENTR ,NSAMPL ,WINDOW ,DTSIM  ,
     3                TIMREF ,IDELTT ,SECIPL ,IFRMAT ,ICARR  ,MEATYP ,
     4                SATSES ,NUMOBS ,STNAME ,WGSSES ,ELLSES ,ISATCO ,
     5                ZENMAX ,ITROPO ,IEXTRA ,CORSTR ,ICOELV ,IAMB1  ,
     6                AMBDEF ,NSATEL ,SATNUM ,NUMAMB ,AMBSAT ,AMBIEP ,
     7                AMBCLS ,AMBWLF ,NFRFIL ,NFREQ  ,AMBIGU ,AMB0   ,
     8                SYNCM  ,ISYNC  ,SYNC   ,ISYNCR ,XMAXCL ,NPAR   ,
     9                NPARN  ,NPAEPO ,IPHSEP ,STFIL  ,XSTAT  ,CLFRTO ,
     1                MELWUB ,IZEROD ,INDARC ,TBOUND(1,INDARC),LOCQ  ,
     2                RECTYP ,ANTTYP ,IANTEN ,CSESS  ,NWGT   ,SATWGT ,
     3                TIMWGT ,WGTWGT ,OBSFLG ,OBSERV ,SVNFIL ,OBSFL1 ,
     4                OBSER1 ,SVNFI1 ,TRPLIM ,TRPLMS ,TPOL   ,IPOLAR ,
     5                ASING  ,AAUX   ,AHELP  ,INDA   ,INDAUX ,BSING  ,
     6                WEIGHT ,TIMSTC ,SCASTC ,SCAHIL ,SCAPOT ,NSAOFF ,
     7                SATOFF ,TIMOFF ,SCAALB ,SCACEN ,NSAALB ,SATALB ,
     8                ITRMAP ,ITRGRD ,POLARS ,ANTCAL ,NUMCAL ,PRNCAL ,
     9                OPTDIP ,OPTGIM ,POLGIM ,EPOGIM ,SCAGIM ,ANTRAO ,
     1                NUMRAO ,PRNRAO ,OPTELI ,NKIN   ,STKIN  ,NPARMS ,
     2                PARLST ,ANOR   ,BNOR   ,XXX0   ,XXREF  ,PARTYP ,
     3                RMS    ,RMSSES(ISES)   ,NOBS   ,NOBSPA ,ELEVMM ,
     4                NOBELV ,OBSCLS ,ICLUST ,NDIFF  ,NCLKST ,CLKSTA ,
     5                NCLKSA ,CLKSAT ,NOINCLK,CLKSYS ,CLKHED ,CLKREC ,
     6                CLKPAR ,NOBEPO ,EDTLVL ,OBSNUM ,OPTDCB ,IEPPAR ,
     7                ISASYS ,TOBS   ,IRETC  ,LEOARC ,TBOUND2(1,LEOARC),
     9                TIMSTC2,ZMXLEO ,POSECC ,AELL   ,BELL   ,DXELL  ,
     1                DRELL  ,SCELL  ,NSASPV ,SATSPV ,NADMAX ,NAMAX  ,
     2                NOBNAD ,NADIMM ,RAPZENMAX,NSHD ,SATSHD ,TIMSHD ,
     3                FLGSHD ,OBSEPO ,TIMFIL ,ICLU   ,MXCSNG ,MXCFLS ,
     4                MXCSAS ,MXCFRS ,MXCSAT ,LSTSES ,NOBAZIS,XSTELL ,
     5                NRGB   ,OPLOAD ,TIMISB ,IREL2)
C
          IF(IRETC.EQ.1) GOTO 310
C
C Copy reference values
C ---------------------
          IF (nobs > 0) THEN
            iEpObs=IDNINT(((TOBS-CLKHED%TFIRST)*86400D0)/
     1                             CLKREC%EPOCH(1)*CLKREC%NEPO)+1
            DO iRef=1,clkhed%ref(1)%nRef
              xRef(iRef,iEpObs)=xxRef(iRef)
            ENDDO
          ENDIF

C SET WEIGHTS FOR EPOCH-SPECIFIC PARAMETERS AND PRE-ELIMINATION
C -------------------------------------------------------------
C
          IF (IEPPAR.EQ.1.AND.NPAEPO.GT.0) THEN
            IF (MXCPAE.LT.NPAEPO) MXCPAE=NPAEPO
C
C CONDITION OF SUM FOR EPOCH CLOCKS TO BE PREELIMINATED NOW
            SETSUM(1)=(OPTELI(23).EQ.3)
            SETSUM(2)=(OPTELI(24).EQ.3)
            CALL CLKSUM(MAXLCQ,CLKHED,STNAME,STFIL,NEPOBS,
     1                  NPAR-NPAEPO+1,NPAR,LOCQ,SIGAPR,ANOR,SETSUM)
C
C CONSTRAINTS FOR KINEMATIC POSITIONS
            CALL WGTKIN(NPAR-NPAEPO+1,NPAR  ,ANOR  ,LOCQ  ,STNAME,
     1                  CLKHED,CLKREC,AELL  ,BELL  ,DXELL ,DRELL ,
     2                  SCELL ,XSTAT ,SIGAPR,NSTWGT,ISTWGT,STWGT )
C
            CALL REDEPO(SIGAPR,SIGDIP(1)    ,OPTDIP,NFLSES,FILNUM,
     1                  NPAR  ,NPAEPO,NPARMS,PARLST,ANOR  ,BNOR  ,
     2                  LOCQ  ,PARTYP,RMS   ,RMSSES(ISES) ,PARFLG)
          ENDIF
C
C PRE-ELIMINATE AMBIGUITIES IF POSSIBLE
C -------------------------------------
          IF (STRAMB(1).EQ.-1 .AND.
     1        STRAMB(2).GE. 0 .AND.
     2        IEPPAR.EQ.0) THEN
            IF (STRAMB(2).GT.0) THEN
              CALL MJDGPS(TOBS,GPSSEC,NWEEK)
              TFRAC=GPSSEC-DNINT(GPSSEC/STRAMB(2))*STRAMB(2)
              IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 300
            ENDIF
            CALL REDAMB(NFLSES,FILNUM,OBSNUM,NPAR  ,NAMB  ,NPARN ,
     1                  NPASES,NAMSES,NPARMS,PARLST,ANOR  ,BNOR  ,
     2                  XXX0  ,LOCQ  ,PARTYP,RMS   ,NOBSPA,PARFLG)
          ENDIF
C
C GOTO NEXT EPOCH
C ---------------
300     CONTINUE
C
C CLOSE SESSION: OBSERVATION AND METEO FILES
C ------------------------------------------
310     CONTINUE
C
        CALL CLOSES(NFLSES,IEXTRA)
! ------------------------------------------
        weight_amb=1.D20
        iacst=0
        IF (NDIFF(1)==0.AND.icarr(1,1)/=4) THEN
          IF(STRAMB(1)/=-1.OR.(STRAMB(1)==-1.AND.IEPPAR==1)) THEN
            nobssav=nobs
            IF (clksys == 0) THEN
              CALL ambcst(npar,nobs,nfTot,timFil(0,:),locq,
     1                    anor,weight_amb)
            ELSE
              DO ii = 1,maxsys
                CALL ambcst(npar,nobs,nfTot,timFil(ii,:),locq,
     1                      anor,weight_amb)
              ENDDO
            ENDIF
            iacst=nobs-nobssav
          ENDIF
        ENDIF
! ------------------------------------------
C
C PRE-ELIMINATE AMBIGUITIES AND OTHER PARAMETERS
C ----------------------------------------------
        IF (STRAMB(1).EQ.-1 .AND.
     1      IEPPAR.EQ.0) NAMB=NPAR-NPARN
C
        CALL REDPAR(OPTELI,NFLSES,FILNUM,IFDONE,NFTOT ,STFIL ,
     1              TRPLMS,NEPOCH,TIMREF,IDELTT,NPAR  ,NAMB  ,
     2              NPARN ,NPASES,NAMSES,NPARMS,PARLST,ANOR  ,
     3              BNOR  ,XXX0  ,LOCQ  ,PARTYP,RMS   ,NOBSPA,
     4              PARFLG,CLFRTO,ARCINT)
400   CONTINUE
C
C CLOSE PROCESSING
C ----------------
      CALL PRISYN(PRIOPT,CORSTR,NFTOT ,CSESS ,STFIL ,STNAME,
     1            XMAXCL,NDIFF ,ELEVMM,NOBELV,ZENMAX,NOBNAD,
     2            NADIMM,NSATEL,SATNUM,NOBAZIS,TIMFIL)
C
C GET THE SAMPLING RATE AND THE MINIMAL ELEVATION ANGLE
C -----------------------------------------------------
      CALL SMPELV(NSAMPL(1),NFTOT ,IDELTT,ZENMAX,ELEVMM,NSMPNQ,IELVNQ,
     1            INDSMP)
C
C ELIMINATE PARAMETERS WITH NO OBSERVATIONS; PRE-ELIMINATION
C ----------------------------------------------------------
C
C CONDITION OF SUM FOR EPOCH CLOCKS TO BE PREELIMINATED NOW
      SETSUM(1)=( OPTELI(23).EQ.1 .OR.
     1           (OPTELI(23).EQ.3.AND.NSAMPL(3).NE.0))
      SETSUM(2)=( OPTELI(24).EQ.1 .OR.
     1           (OPTELI(24).EQ.3.AND.NSAMPL(3).NE.0))
      CALL CLKSUM(MAXLCQ,CLKHED,STNAME,STFIL,NEPOBS,
     1            1     ,NPARN,LOCQ,SIGAPR,ANOR,SETSUM)
C
C CONSTRAINTS FOR KINEMATIC POSITIONS
      IF (OPTELI(21).EQ.1) THEN
        CALL WGTKIN(1     ,NPARN ,ANOR  ,LOCQ  ,STNAME,
     1              CLKHED,CLKREC,AELL  ,BELL  ,DXELL ,DRELL ,
     2              SCELL ,XSTAT ,SIGAPR,NSTWGT,ISTWGT,STWGT )
      ENDIF
C
      CALL REDNEQ(0,MAXTYP,1,OPTELI,NPAR,
     1            NAMB,NPARN,NPARMS,PARLST,RMS,NPALCQ,ANOR,BNOR,XXX0,
     2            LOCQ,PARTYP,INDP,OBSCLS,NOBSPA,PARFLG)
C
C SELECT AND ELIMINATE REFERENCE AMBIGUITIES AND REF. ION. PARAMETERS
C -------------------------------------------------------------------
      IF (STRAMB(1).NE.-1 .AND. NCLKST+NCLKSA.EQ.0 ) THEN
        CALL AMBREF(MXCLOC,MXCAMP,NFTOT ,NFRFIL,ICARR ,OBSCLS,
     1              ICLUST,NUMAMB,AMBSAT,AMBIEP,AMBCLS,MIXED ,
     2              OPTDIP,NEPOCH,NPAR  ,NAMB  ,NPARN ,NPARMS,
     3              NPALCQ,PARLST,ANOR  ,BNOR  ,XXX0  ,LOCQ  ,
     4              PARTYP,INDP  ,IDEL  ,NOBSPA,NREF  ,REFAMB)
      ELSE
        NREF=0
      ENDIF
C
      CALL PRIMAX(PRIOPT,
     1            IMXLOC,NMXLOC,MAXLOC,IMXFIL,NMXFIL,MAXFIL,
     2            IMXSTA,NMXSTA,MAXSTA,IMXSAT,NMXSAT,MAXSAT,
     3            IMXAMB,NMXAMB,MAXAMB,IMXPAR,NMXPAR,MAXPAR,
     4            IMXFLS,NMXFLS,MAXFLS,IMXSAS,NMXSAS,MAXSAS,
     5            IMXAMP,NMXAMP,MAXAMP,IMXSNG,NMXSNG,MAXSNG)
C
C COPY FREE NORMAL EQUATION MATRICES FOR NEQ SAVING
C -------------------------------------------------
      CALL GTFLNA(0,'NEQUARS',FILNEQ,IRCNEQ)
      IF (IRCNEQ.EQ.0) THEN
C
C AMBIGUITY RESOLUTION
        IF (STRAMB(1).GT.0) THEN
          CALL MATCOP(1,NPARN*(NPARN+1)/2,ANOR,n11ar)
C
C SAVE THE NEQ FILE WITHOUT INVERSION
        ELSE IF (NOSOL.EQ.1) THEN
          CALL SAVNEQ(FILNEQ ,TITLES ,NPAR   ,NSTAT  ,RMS    ,LOCQ   ,
     1                ANOR   ,BNOR   ,PARTYP ,XSTAT  ,STNAME ,XSTELL ,
     2                RECTYP ,ANTTYP ,IANTEN ,CSESS  ,NFREQ  ,STFIL  ,
     3                POSECC ,TRPLMS ,ITROPO ,IEXTRA ,XXX    ,NCAMP  ,
     4                TAECMP ,NSTCEP ,NSTCEP2,NSTCEF ,NSTCEF2,NSASTC1,
     5                NSASTC2,TIMSTC ,TIMSTC2,NUMSTC1,NUMSTC2,SCASTC ,
     6                TBOUND ,TPOL   ,SCACEN ,NOBS   ,NPARMS ,NFTOT  ,
     7                NSMPNQ ,IELVNQ ,NANOFF ,NSAOFF ,SATOFF ,ITRMAP ,
     8                ITRGRD ,NAMGIM ,EPOGIM ,SCAGIM ,NUTNAM ,SUBNAM ,
     9                DATUM  ,NADMAX ,IFREQ  ,NANSPV ,NSASPV ,SATSPV ,
     1                MEATYP(1),NOINCLK(2)   ,SECIPL ,CLKHED ,CLKREC ,
     2                NSAMPL(3),ANTCAL,NUMCAL,PRNCAL ,ANTRAO ,NUMRAO ,
     3                PRNRAO ,IOREST ,NOBSPA ,OPLOAD ,TIMISB ,timIntF,
     4                USEGEOS1,GOBSDEF1)
C
C PRINT ONLY THE PARAMETER/OBSERVATION STATISTIC
          CALL PRIEST(TITLES ,1      ,NPARMS ,NPAR   ,NPARN  ,NOBS   ,
     1                NFTOT  ,MEATYP ,NUMOBS ,PRIOPT ,RMS    ,TRPLMS ,
     2                SIGAPR ,SECIPL ,ICOELV(1,1),XXX,NSTAT  ,STNAME ,
     3                STANUM ,ICENTR ,XSTAT  ,XSTELL ,XSTECC ,LOCQ   ,
     4                ANOR   ,XXX0   ,AELL   ,BELL   ,DXELL  ,DRELL  ,
     5                SCELL  ,STRAMB ,NFRFIL ,ICARR  ,MAXFRS ,NOSOL  ,
     6                NUMAMB ,AMBSAT ,AMBIEP ,AMBWLF ,AMBIGU ,AMBCLS ,
     7                NREF   ,SCASTC ,SCAPOT ,SCAHIL ,SCAALB ,SCACEN ,
     8                TIMSTC ,TIMOFF ,ANTCAL ,NUMCAL ,PRNCAL ,OPTGIM ,
     9                POLGIM ,NAMGIM ,EPOGIM ,SCAGIM ,ANTRAO ,NUMRAO ,
     1                PRNRAO ,MAXTYP ,OPTELI ,PARLST ,MELWUB ,IEXTRA ,
     2                ITROPO ,ITRGRD ,ZENMAX ,RECTYP ,STFIL  ,NDIFF  ,
     3                INFGIM ,NEPOBS ,NCLKST ,NCLKSA ,NOINCLK(2)     ,
     4                CLKHED ,CLKREC ,NSAMPL ,IRAUX2 ,TIMSTC2, PARFLG,
     5                NADMAX ,NAMAX  ,NADIGN ,RAPZENMAX,iacst,SATSPEC,
     6                OPLOAD ,NALLSAT,ALLSATNUM      ,CLFRTO ,TIMISB ,
     7                MXCPAR ,USEGEOS1,GOBSDEF1,TIMREF ,IDELTT)
C
          CALL EXITRC(0)
C
C "NORMAL" CASE
        ELSE
          CALL MATCOP(1,NPAR*(NPAR+1)/2,ANOR,n11)
          CALL MATCOP(1,NPAR,BNOR,b0)
        ENDIF
      ENDIF
C
C DEFINE A PRIORI WEIGHTS:
C ------------------------
      IFLAG = 1
      CALL NORINI(STRAMB ,NPAR   ,NPARN  ,MXCPAR ,MXCAMP ,SIGAPR ,
     1            LOCQ   ,NSTWGT ,ISTWGT ,STWGT  ,CLKWGT ,PREC   ,
     2            SIGPOL ,NSASTC1,NSASTC2,NUMSTC1,NUMSTC2,SIGSTC1,
     3            SIGSTC2,SCASTC ,INTSTC ,INTSTC2,AELL   ,BELL   ,
     4            XSTELL ,ANOR   ,BNOR   ,OPTDCB ,SIGDCB ,SIGTRP ,
     5            SIGTRS ,ISGTRS ,NOBS   ,RMS    ,INDP   ,TPOL   ,
     6            ISGPOL ,SIGOFF ,ISGOFF ,SIGHIL ,SIGPOT ,SCAHIL ,
     7            SCAPOT ,SIGALB ,SIGCEN ,SCAALB ,SCACEN ,ISGNUT ,
     8            SIGCAL ,SIGGIM ,SCAGIM ,SIGRAO ,IFLAG  ,OPTELI ,
     9            MIXED  ,IZEROD ,PREC2  ,SIGSPV ,SIGRGB ,NOBSPA ,
     .            OPLOAD ,SIGHOI ,TIMISB ,NCLKSA ,NMXINT ,NMXSAP ,
     1            NMXARC ,OPTGSP)
C
C COPY NORMAL EQUATION MATRICES FOR AMBIGUITY RESOLUTION
C ------------------------------------------------------
      IF (STRAMB(1).GT.0) THEN
        CALL MATCOP(1,NPAR*(NPAR+1)/2,ANOR,n11)
        CALL MATCOP(1,NPAR,BNOR,b0)
      ENDIF
C
C DEFINE A PRIORI WEIGHTS FOR DIFFERENTIAL IONOSPHERE PARAMETERS
C --------------------------------------------------------------
      CALL WGTDIP(OPTDIP,SIGDIP,NPARN ,LOCQ  ,SIGAPR,IDELTT,
     1            ANOR  )
C
C CONDITION OF SUM FOR EPOCH CLOCKS
C ---------------------------------
      SETSUM(1)=( OPTELI(23).NE.1.AND.
     1           (OPTELI(23).NE.3.OR.NSAMPL(3).NE.0))
      SETSUM(2)=( OPTELI(24).NE.1.AND.
     1           (OPTELI(24).NE.3.OR.NSAMPL(3).NE.0))
      CALL CLKSUM(MAXLCQ,CLKHED,STNAME,STFIL,NEPOBS,
     1            1     ,NPARN,LOCQ,SIGAPR,ANOR,SETSUM)
C
C CONSTRAINTS FOR KINEMATIC POSITIONS
      CALL WGTKIN(1     ,NPARN ,ANOR  ,LOCQ  ,STNAME,
     1            CLKHED,CLKREC,AELL  ,BELL  ,DXELL ,DRELL ,
     2            SCELL ,XSTAT ,SIGAPR,NSTWGT,ISTWGT,STWGT )
C
C INVERSION OF NORMAL EQUATION SYSTEM (TRIANGULATED)
C --------------------------------------------------
      CALL SYMINVG(NPAR,ANOR,1,ISMAT,PARFLG)
C
C UPDATE NUMBER OF SINGULAR PARAMETERS
C ------------------------------------
      CALL UPDPAR(0,NPAR,LOCQ,IDEL,PARFLG,NPARMS,PARLST)
C
C COMPUTE RESULT
C --------------
      CALL SOLVE(NPAR,ANOR,BNOR,XXX)
C
C COMPUTE RMS - ERROR
C -------------------
      RMSOLD=RMS
      RMSSAV=RMS
      IF (RMSOLD.LE.0.D0) RMSOLD=1.D0
      RMS=SIGMA1(0,NPAR,NPARMS,NOBS,XXX,INDP,ASING,INDA,BSING,WEIGHT,
     1           BNOR,RMSOLD,VSIG,SIGAPR)
C
C PRINT RESIDUALS (IF WISHED)
C ---------------------------
      IF (IRESID.NE.0) THEN
        IF (IZEROD.EQ.0) THEN
          CALL RESOUT(TITLE ,NFTOT ,STFIL ,STNAME,TIMREF,CSESS ,
     1                NFRFIL,ICARR ,MEATYP,NPAR  ,ANOR  ,XXX   ,
     2                INDP  ,INDA  ,ASING ,BSING ,WEIGHT,ISV12 ,
     3                IFIL  ,IOBNUM,IFRSES,IDELTT,NSATEL,SATNUM,
     4                NDIFF ,NORRES)
        ELSE
          CALL RESEPO(TITLE ,NFTOT ,STFIL ,STNAME,TIMREF,CSESS ,
     1                NFRFIL,ICARR ,MEATYP,NEPOBS,NPAR  ,XXX   ,
     2                INDP  ,INDA  ,ASING ,BSING ,WEIGHT,ISV12 ,
     3                IFIL  ,IOBNUM,IFRSES,IDELTT,NSATEL,SATNUM,
     4                NDIFF ,NORRES,SECIPL,CLKHED,CLKREC,NSTAT ,
     5                XSTAT ,IRAUX2,AELL  ,BELL  ,DXELL ,DRELL ,
     6                SCELL ,NSTWGT,ISTWGT,STWGT ,SIGAPR,IQXX  ,
     7                IPHSEP,ANOR  ,IZEROD,NOINCLK(2),   MXCFLS,
     8                MXCSAS,MXCFRS,MXCPAR,MXCLOC,MXCSNG,MXCPAE,
     9                NEPSNG)
        ENDIF
      ENDIF
C
C OUTPUT PROGRAM RESULTS, PART 1 AND 2 (BEFORE AND AFTER AMBRES)
C --------------------------------------------------------------
      DO IPART=1,2
C
C AMBIGUITY RESOLUTION
C --------------------
        IF (IPART.EQ.2) THEN
          IF (STRAMB(1).LE.0 .OR. NPAR-NPARN.EQ.0) EXIT
C
          IF (IRESID.NE.0) THEN
            CALL GTFLNA(0,'RESIDRS',FILRES,IRCRES)
            IF (IRCRES.EQ.0) THEN
              WRITE(LFNERR,'(/,A,/,16X,A,/)')
     1              ' ### PG GPSEST: THE VALUES IN THE RESIDUAL' //
     2              ' FILE ARE FROM THE SOLUTION ',
     3              'BEFORE RESOLVING THE AMBIGUITIES.'
            ENDIF
            IF (NKIN.GT.0.AND.OPTELI(21).EQ.3) THEN
              WRITE(LFNERR,'(/,A,/,16X,A,/)')
     1              ' ### PG GPSEST: THE ESTIMATES FOR THE EPOC' //
     2              'H PARAMETERS COME FROM THE',
     3              'SOLUTION BEFORE RESOLVING THE AMBIGUITIES.'
            ENDIF
          ENDIF
C
          CALL AMBRES(TITLES,PRIOPT(10)   ,STNAME,NFTOT ,NUMOBS,
     1                STFIL ,ICENTR,STRAMB,SIGAMB,AR2MOD,AR2INF,
     2                NPAR  ,NPARAR,NPARN ,NPARMS,PARLST,NREF  ,
     3                LOCQ  ,LOCHLP,XXX   ,n11   ,b0    ,B1    ,
     4                NOBS  ,RMS   ,RMSOLD,RMSSES,BNOR  ,ANOR  ,
     5                PARFLG,AMB1  ,AMB2  ,AMB3  ,MATCH ,NUMAMB,
     6                AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,OBSCLS,
     7                SIGAPR,ICARR ,MELWUB,NDIFF ,RECTYP,TIMREF)
          RMSSAV=RMSOLD
C
C PREPARE AND SAVE RESOLVED AMBIGUITIES
C -------------------------------------
          CALL AMBCHN(NPARAR,NPARN ,NREF  ,NUMAMB,LOCQ  ,AMBWLF,
     1                AMB1  ,XXX   ,AMBIGU,AMBCLS)
          CALL AMBSTO(NFTOT ,HEADER,AMBSAV,NPARAR,NPARN ,NREF  ,
     1                NPALCQ,LOCQ  ,XXX   ,NUMAMB,AMBSAT,AMBWLF,
     2                AMBIEP,AMBIGU,AMBCLS,NUMSA1,NUMOB1,NUMMR1,
     3                NUMAM1,AMBSA1,AMBWLF,AMBIE1,AMBIG1,AMBCL1)
C
C UPDATE SOLUTION VECTOR AND PARAMETER DESCRIPTOR
C -----------------------------------------------
          IP1=NPARN
          DO IP=NPARN+1,NPARAR
            IF(AMB1(IP).EQ.0) CYCLE
            IP1=IP1+1
            XXX(IP1)=XXX(IP)
            LOCQ(:,IP1)=LOCQ(:,IP)
          ENDDO
C
C RECONSTRUCT FREE NORMAL EQUATION MATRICES FOR NEQ SAVING
C --------------------------------------------------------
          IF (IRCNEQ.EQ.0) THEN
            CALL MATCOP(0,NPARN*(NPARN+1)/2,n11ar,n11)
            CALL MATCOP(-1,0,n11ar,n11ar)
          ELSE
            CALL MATCOP(-1,0,n11,n11)
            CALL MATCOP(-1,0,b0,b0)
          ENDIF
        ENDIF
C
        CALL PRIEST(TITLES ,IPART  ,NPARMS ,NPAR   ,NPARN  ,NOBS   ,
     1              NFTOT  ,MEATYP ,NUMOBS ,PRIOPT ,RMS    ,TRPLMS ,
     2              SIGAPR ,SECIPL ,ICOELV(1,1)    ,XXX    ,NSTAT  ,
     3              STNAME ,STANUM ,ICENTR ,XSTAT  ,XSTELL ,XSTECC ,
     4              LOCQ   ,ANOR   ,XXX0   ,AELL   ,BELL   ,DXELL  ,
     5              DRELL  ,SCELL  ,STRAMB ,NFRFIL ,ICARR  ,MAXFRS ,
     6              NOSOL  ,NUMAMB ,AMBSAT ,AMBIEP ,AMBWLF ,AMBIGU ,
     7              AMBCLS ,NREF   ,SCASTC ,SCAPOT ,SCAHIL ,SCAALB ,
     8              SCACEN ,TIMSTC ,TIMOFF ,ANTCAL ,NUMCAL ,PRNCAL ,
     9              OPTGIM ,POLGIM ,NAMGIM ,EPOGIM ,SCAGIM ,ANTRAO ,
     1              NUMRAO ,PRNRAO ,MAXTYP ,OPTELI ,PARLST ,MELWUB ,
     2              IEXTRA ,ITROPO ,ITRGRD ,ZENMAX ,RECTYP ,STFIL  ,
     3              NDIFF  ,INFGIM ,NEPOBS ,NCLKST ,NCLKSA ,NOINCLK(2),
     4              CLKHED ,CLKREC ,NSAMPL ,IRAUX2 ,TIMSTC2,PARFLG ,
     5              NADMAX ,NAMAX  ,NADIGN ,RAPZENMAX,iacst,SATSPEC,
     6              OPLOAD ,NALLSAT,ALLSATNUM      ,CLFRTO ,TIMISB ,
     7              MXCPAR ,USEGEOS1,GOBSDEF1,TIMREF ,IDELTT)
C
C COMPUTE AND PRINT RMS OF COORDINATES+COORDINATE DIFFERENCES
C -----------------------------------------------------------
        ALLOCATE(QB(IKF(NSTAT,NSTAT)),STAT=IAC)
        CALL ALCERR(IAC,'QB',(/IKF(NSTAT,NSTAT)/),'GPSEST')
        ALLOCATE(QL(IKF(NSTAT,NSTAT)),STAT=IAC)
        CALL ALCERR(IAC,'QL',(/IKF(NSTAT,NSTAT)/),'GPSEST')
        ALLOCATE(QH(IKF(NSTAT,NSTAT)),STAT=IAC)
        CALL ALCERR(IAC,'QH',(/IKF(NSTAT,NSTAT)/),'GPSEST')
C
        CALL QBLH(NSTAT,LOCQ,NPAR,XSTAT,ANOR,QB,QL,QH,NUST,LISTUS)
        IF (PRIOPT(17) == 1 .OR. PRIOPT(18) == 1) THEN
          IF (PRIOPT(17) == 1)
     1      CALL PRIBLH(TITLES,IPART,RMS,QB,QL,QH,NUST,LISTUS,STANUM)
C
C COMPUTE AND PRINT SLOPE DISTANCES AND THEIR RMS ERRORS
C ------------------------------------------------------
          IF (PRIOPT(18) == 1) THEN
            ALLOCATE(DISOLD(IKF(NSTAT,NSTAT)),STAT=IAC)
            CALL ALCERR(IAC,'DISOLD',(/IKF(NSTAT,NSTAT)/),'GPSEST')
            ALLOCATE(DISNEW(IKF(NSTAT,NSTAT)),STAT=IAC)
            CALL ALCERR(IAC,'DISNEW',(/IKF(NSTAT,NSTAT)/),'GPSEST')
            ALLOCATE(QDD(IKF(NSTAT,NSTAT)),STAT=IAC)
            CALL ALCERR(IAC,'QDD',(/IKF(NSTAT,NSTAT)/),'GPSEST')
C
            CALL QDIS(XSTAT,STFIX,NFIX,XXX,ANOR,NUST,LISTUS,
     1                     DISOLD,DISNEW,QDD)
            CALL PRIDIS(NSTAT,TITLES,IPART,RMS,DISOLD,DISNEW,QDD,
     1                     NUST,LISTUS,STANUM,NFIX)
C
            DEALLOCATE(DISOLD,STAT=IAC)
            DEALLOCATE(DISNEW,STAT=IAC)
            DEALLOCATE(QDD,STAT=IAC)
          ENDIF
        ENDIF
C
        DEALLOCATE(QB,STAT=IAC)
        DEALLOCATE(QL,STAT=IAC)
        DEALLOCATE(QH,STAT=IAC)
C
C COMPUTE HELMERT TRANSFORM BETWEEN A PRIORI AND ESTIMATED COORDINATES
C --------------------------------------------------------------------
        CALL GPHELM(NSTAT,TITLES,IPART,ILOC,STNAME,XSTAT,NFIX,XXX,NUST,
     1              LISTUS,STANUM,IHELM,AELL,BELL,DXELL,DRELL,SCELL)

      ENDDO
C
      NSTEFF=NSTAT-NCENTR
      NSAEFF=0
      DO IARC=1,NARC
        IF (NUMSAT(IARC).GT.NSAEFF) NSAEFF=NUMSAT(IARC)
      ENDDO
C
C SAVE RESULTS
C ------------
      TIMCRD=(TAECMP(1,1)+TAECMP(2,1))/2.D0
      CALL CORSAV(TITLE ,NSTAT ,STNAME,ICENTR,STAFLG,XSTAT ,
     1            XSTECC,NFIX  ,STFIX ,NPAR  ,LOCQ  ,XXX   ,
     2            TIMCRD,CLKHED,NOBSPA)
C
      CALL CLKSAV(TITLE,NPAR,LOCQ,RMS,XXX,ANOR,XXX0,XREF,SECIPL,
     1            NOINCLK(2),CLKHED,CLKREC,OPTELI,NEPOBS,NSHD  ,
     2            SATSHD,TIMSHD,IRAUX2,CLKSYS,CLKPAR)
C
      IWQXX=0
      CALL KINSAV(MXCLCQ,TITLE,NPAR,XSTAT,XSTECC,LOCQ,RMS,XXX,ANOR,
     1            XXX0,PARFLG,OPTELI,STNAME,DATUM,IWQXX,CLKHED,
     2            CLKREC,NEPOBS(3),IRAUX2)
C
      FILSTD=' '
      IARCOF=0
      IF (IOREST.NE.3) THEN
        CALL STDSAV(TITLE,NPAR,RMS,LOCQ,XXX,ANOR,NSTCEP,SCASTC,
     1              INTSTC,TIMSTC,FILSTD,FILRPR,FILORB,IARCOF,
     2              NMXINT,NMXSAP,NMXARC)
      END IF
      IF (IOREST.EQ.3 .OR. IOREST.EQ.0) THEN
        JPAR=1
        FILSTD=' '
        FILRPR=' '
        FILORB=' '
        CALL STDSAV2(TITLE,NPAR,RMS,LOCQ,XXX,ANOR,NSTCEP2,SCASTC,
     1              INTSTC2,TIMSTC2,FILSTD,FILRPR,FILORB,JPAR,IARCOF,
     2              NMXINT,NMXSAP,NMXARC)
      END IF
C
      CALL TRPSAV(TITLE ,ITROPO,IEXTRA,ICENTR,STNAME,STAFLG,
     1            NPAR  ,LOCQ  ,XSTELL,TRPLMS,XXX   ,ANOR  ,RMS   ,
     2            NSMPNQ,IELVNQ,NSTAT ,NCENTR,XSTAT ,DATUM ,ITRMAP,
     3            ITRGRD)
C
      CALL DCBSAV(TITLE ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1            STNAME,CLFRTO,NALLSAT,ALLSATNUM)
C
      CALL ISBSAV(TITLE ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1            TIMISB,STNAME,ICARR)
C
      CALL ATMSAV(TITLE ,NPAR  ,RMS   ,LOCQ  ,XXX   ,ANOR  ,
     1            OPTGIM,POLGIM,NAMGIM,EPOGIM,SCAGIM,NSTEFF,
     2            NSAEFF,ZENMAX,NSAMPL(1),INFGIM,IORSYS)
C
      CALL POLSAV(TITLE,NPOL,TPOL,NPAR,HPSAVE(1),DTSIM,LOCQ,
     1            RMS,XXX,ANOR)
C
      CALL IERSAV(TITLE,NPOL,TPOL,NPAR,HPSAVE(2),DTSIM,LOCQ,
     1            RMS,XXX,ANOR,NSTEFF,NFIX,NSTWGT,STWGT,
     2            NFTOT,SATNUM,NSATEL)
C
      CALL PCVSAV(TITLE ,NPAR  ,LOCQ  ,XXX   ,NANRAO,ANTRAO,
     1            NUMRAO,PRNRAO,NFRRAO,NANCAL,ANTCAL,NUMCAL,
     2            PRNCAL,NFRCAL,NANSPV,NSASPV,SATSPV,GNRSPV,
     3            globalwindow ,NADMAX,RAPZENMAX)
C
      FILSTD=' '
      CALL COVSAV(FILSTD,TITLE,RMS,DBLE(NOBS),DBLE(NPARMS),
     1            NPAR,ANOR,LOCQ,STNAME)
C
      CALL COVSV1(TITLE,RMS,DBLE(NOBS),DBLE(NPARMS),
     1            NPAR,ANOR,LOCQ,STNAME,TIMISB)
C
C PREPARE AND SAVE NORMAL EQUATION SYSTEM AND RELATED INFORMATION
C ---------------------------------------------------------------
      IF (IRCNEQ == 0) THEN
C
C ELIMINATE PARAMETERS WITH NO OBSERVATIONS; PRE-ELIMINATION
C ----------------------------------------------------------
        CALL REDNEQ(0,MAXTYP,2,OPTELI,NPAR,
     1              NAMB,NPARN,NPARMS,PARLST,RMSSAV,NPALCQ,n11,b0,XXX0,
     2              LOCQ,PARTYP,INDP,OBSCLS,NOBSPA,PARFLG)
C
C ADJUST NPARMS IN CASE OF NON-PRE-ELIMINATED SINGULAR PARAMETERS
C ---------------------------------------------------------------
        DO iTyp=1,maxtyp
          IF (opteli(iTyp) == 0) nParms=nParms+parlst(5,iTyp)
        ENDDO
C
C SAVE THE NEQ FILE
C -----------------
        CALL SAVNEQ(FILNEQ ,TITLES ,NPAR   ,NSTAT  ,RMSSAV ,LOCQ   ,
     1              N11    ,B0     ,PARTYP ,XSTAT  ,STNAME ,XSTELL ,
     2              RECTYP ,ANTTYP ,IANTEN ,CSESS  ,NFREQ  ,STFIL  ,
     3              POSECC ,TRPLMS ,ITROPO ,IEXTRA ,XXX    ,NCAMP  ,
     4              TAECMP ,NSTCEP ,NSTCEP2,NSTCEF ,NSTCEF2,NSASTC1,
     5              NSASTC2,TIMSTC ,TIMSTC2,NUMSTC1,NUMSTC2,SCASTC ,
     6              TBOUND ,TPOL   ,SCACEN ,NOBS   ,NPARMS ,NFTOT  ,
     7              NSMPNQ ,IELVNQ ,NANOFF ,NSAOFF ,SATOFF ,ITRMAP ,
     8              ITRGRD ,NAMGIM ,EPOGIM ,SCAGIM ,NUTNAM ,SUBNAM ,
     9              DATUM  ,NADMAX ,IFREQ  ,NANSPV ,NSASPV ,SATSPV ,
     1              MEATYP(1),NOINCLK(2)   ,SECIPL ,CLKHED ,CLKREC ,
     2              NSAMPL(3),ANTCAL,NUMCAL,PRNCAL ,ANTRAO ,NUMRAO ,
     3              PRNRAO ,IOREST ,NOBSPA ,OPLOAD ,TIMISB ,timIntF,
     4              USEGEOS1,GOBSDEF1)
C
C DEALLOCATE COPY OF NORMAL EQUATION MATRICES
C -------------------------------------------
        CALL MATCOP(-1,0,n11,n11)
        CALL MATCOP(-1,0,b0,b0)
      END IF
C
      CALL EXITRC(0)
      END


