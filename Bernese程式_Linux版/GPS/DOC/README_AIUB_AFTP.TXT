======================================================================

AIUB's Anonymous FTP Server                     Last mod.: 06-Nov-2012

======================================================================

The anonymous ftp server of the AIUB contains daily updated files
resulting from the GNSS data analysis performed for the IGS, in both
international and Bernese formats. Additionally, product files from
the reprocessing activities of CODE are available. The server may be
accessed through

  ftp://ftp.unibe.ch/aiub   or   http://www.aiub.unibe.ch/download


Contents of this file
*********************
- General remarks

- Files in Bernese format
  . Data content in the directories in BSWUSER50
  . Data content in the directories in BSWUSER52

- CODE product files

- Files from the CODE reprocessing


----------------------------------------------------------------------
General remarks
----------------------------------------------------------------------
- All filenames are in uppercase.

- Most of the files are UNIX compressed (indicated by the extension
  .Z).

- We use the following abbreviations for time in the filenames below
  (menu time variables refer to the respective variables used in the
  Bernese GNSS Software, since Version 5.0):

    Used time        Menu time
    abbreviation     variable         Description
    ---------------  ---------------  --------------------------------
    mm               $M               Month (two digits)
    yy               $Y               Year (two digits)
    ddd              $+0              Day of Year (DOY) (three digits)
    dddh             $S+0             Hourly session
                                      (DOY+character indicating
                                      the hourly session A..X)
    yyyy             $Y+0             Year (four digits)
    wwww             $W+0             GPS Week
    yymm             $M+0             Year, Month
    wwwwd            $WD+0            GPS Week and Day of week
    yyddd            $YD+0            Year and DOY


----------------------------------------------------------------------
Files in Bernese format
----------------------------------------------------------------------
The main directory tree for the Bernese users area distinguishes
between the versions of the Bernese GPS Software. Currently we have
BSWUSER50 and BSWUSER52.

With the introduction of new formats during future developments at
AIUB new top-directories like BSWUSER53 may appear. If there are sub-
directories from different versions in place they contain the full set
of files, but in the respective format. If a file is identical in
two versions, the same file will appear in both subdirectories.

    Directory           Short comment on content
    ------------------  ------------------------------------------
    BSWUSER50           Files in format for BSW Version 5.0
      ATM               Bernese troposphere and ionosphere files
      GEN               General BSW files
      ORB               Bernese clock, ERP, and DCB files
      STA               Bernese station coordinate files
      TXT               Diverse text files

    BSWUSER52           Files in format for BSW Version 5.2
      ATM               Bernese troposphere and ionosphere files
      GEN               General BSW files
      ORB               Bernese clock, ERP, and DCB files
      STA               Bernese station coordinate files


Some of the products are stored in yearly subdirectories, e.g.,

  http://www.aiub.unibe.ch/download/BSWUSER52/ATM/yyyy/


Data content in the directories in BSWUSER50
********************************************
The following list of files refers to the content of the directory
tree BSWUSER50.

Please note that all products are based on a rigorous GNSS (GPS and
GLONASS) combined analysis.

ATM contains troposphere and ionosphere files in Bernese format in
    yearly subdirectories:

  yyyy/

    CORyyddd.TRP.Z      Troposphere information in Bernese format
                        from rapid solution where final information
                        is not yet available
    CORwwwwd.ION.Z      Ionosphere files of rapid solution where
                        final information is not yet available
    CODyyddd.TRP.Z      Troposphere path delays of final solution
    COEyyddd.TRP.Z      Troposphere path delays of EUREF solution
    COEyyddd.INX.Z      Ionosphere information in IONEX format from
                        EUREF solution
    COEyyddd.ION.Z      Ionosphere information in Bernese format
                        from EUREF solution

GEN contains general files such as DATUM., RECEIVER., SAT_yyyy.CRX,
    gravity fields, nutation models, and subdaily pole models.
    Furthermore, several sets of satellite information files (e.g.,
    SATELLIT.I05), antenna phase center files (e.g., PHAS_COD.I05),
    and ANTEX files (I05.ATX) are located here. We refer to
    BSWUSER50/TXT/ANTEX_README.pdf for more information.

ORB contains daily updated files containing the P1-P2 and P1-C1 DCB
    values, respectively, and some ERP files in Bernese Format

  P1C1.DCB          CODE moving 30-day P1-C1 DCB solution, Bernese
                    format, containing only the GPS satellites
  P1P2.DCB          CODE moving 30-day P1-P2 DCB solution, Bernese
                    format, containing all GPS and GLONASS satellites
  P1P2_ALL.DCB      CODE moving 30-day P1-P2 DCB solution, Bernese
                    format, containing all GPS and GLONASS satellites
                    and all stations used
  BULLET_A.ERP      Accumulated Bulletin A Earth rotation parameters
                    as of 1992
  C04_yyyy.ERP      Yearly files containing the final IERS C04 Earth
                    rotation information


  Yearly subdirectories contain the following files in Bernese format:

  yyyy/

    BRDyyddd.CLK.Z      Broadcast clock information
    CORwwwwd.ERP.Z      Daily CODE rapid ERP files where final
                        information is not yet available
    CODwwww7.ERP.Z      Weekly CODE final ERP files as from week 0978
    CODwwww7.GCC.Z      Weekly CODE final GCC files as from week 1400
    CODyyddd.ERP.Z      Daily CODE final ERP files as from week 1706
    CODyyddd.GCC.Z      Daily CODE final GCC files as from week 1706
    CODyyddd.CLK.Z      High rate (30 sec) satellite clock corrections
                        from the CODE final IGS solution
    CODyyddd.CLK_05S.Z  High rate ( 5 sec) satellite clock corrections
                        from the CODE final IGS solution
    CORyyddd.DCB.Z      Daily P1-P2 DCB estimates of rapid where
                        final information is not yet available
    CODyyddd.DCB.Z      Daily P1-P2 DCB estimates of final solution

  The daily ERP files CODyyddd.ERP.Z are fully consistent with the
  CODE final orbits available at URL

    http://www.aiub.unibe.ch/download/CODE/yyyy/CODwwwwd.EPH.Z

  whereas the files CORwwwwd.ERP.Z are consistent with the CODE rapid
  orbits available at URL

    http://www.aiub.unibe.ch/download/CODE/CODwwwwd.EPH_R


STA contains the following station related files

  CODE.STA        Bernese station information file for all stations
                  included in the CODE processing
  FES2004.BLQ     FES2004 ocean tide loading corrections for all
                  stations included in the CODE processing
  IGS.STA         Translation of the igs.SNX station information
                  (http://www.igs.org/igscb/station/general/igs.snx)
  IGS_FULL.STA    As IGS.STA but keeping individual antenna calibration
                  number
  EUREF.STA       Translation of the euref.SNX station information
                  (ftp://ftp.epncb.oma.be/pub/station/general/euref.snx)
  EUREF_FULL.STA  As EUREF.STA but keeping individual antenna calibration
                  number

  For each ITRF and IGS realizations, we provide three files:
  "REFNAM"_R.CRD: fiducial sites coordinate extraction in Bernese
                  format from reference frame "REFNAM"
  "REFNAM"_R.VEL: fiducial sites velocity extraction in Bernese
                  format from reference frame "REFNAM"
  "REFNAM".FIX:   list of fiducial sites of reference frame "REFNAM"

  Yearly subdirectories contain the following files in Bernese
  format:

  yyyy/

    CODwwww7.CRD.Z      Weekly coordinate files of the CODE final
                        global solution as from week 0978
    COEwwww7.CRD.Z      Weekly coordinate files of the CODE EUREF
                        solution as from week 0886


Data content in the directories in BSWUSER52
********************************************
The following list of files refers to the content of the directory
tree BSWUSER52. They can only be used with version 5.2 of the Bernese
GNSS Software.

Please note that this tree is still under development and some files
are still missing.

ATM contains troposphere and ionosphere files in Bernese format in
    yearly subdirectories:

  yyyy/

    CODyyddd.TRP.Z      Troposphere path delays of final solution
    COEyyddd.TRP.Z      Troposphere path delays of EUREF solution
    COEyyddd.INX.Z      Ionosphere information in IONEX format from
                        EUREF solution
    COEyyddd.ION.Z      Ionosphere information in Bernese format from
                        EUREF solution

GEN contains general files such as DATUM., RECEIVER., SAT_yyyy.CRX .
    The input files containing gravity field, nutation model, and
    subdaily pole model are placed here for the recommended (default)
    models. Input files for alternative models are located in the
    subdirectory GEN/ALTERNATIVE_MODELS.
    Furthermore, several sets of satellite information files (e.g.,
    SATELLIT.I08), antenna phase center files (e.g., PCV_COD.I08),
    and ANTEX files (I08.ATX) are located here.

ORB contains daily updated files containing the P1-P2 and P1-C1 DCB
    values, respectively, and some ERP files in Bernese format

  Yearly subdirectories contain the following files in Bernese format:

  yyyy/

    CODwwww7.ERP.Z      Weekly CODE final ERP files as from week 0978
    CODwwww7.GCC.Z      Weekly CODE final GCC files as from week 1400
    CODyyddd.ERP.Z      Daily CODE final ERP files as from week 1706
    CODyyddd.GCC.Z      Daily CODE final GCC files as from week 1706
    CODyyddd.CLK.Z      High rate (30 sec) satellite clock corrections
                        from the CODE final IGS solution
    CODyyddd.CLK_05S.Z  High rate ( 5 sec) satellite clock corrections
                        from the CODE final IGS solution
    CODyyddd.DCB.Z      Daily P1-P2 DCB estimates of final solution
    BRDyyddd.CLK.Z      Broadcast clock information

  The daily ERP files CODyyddd.ERP.Z are fully consistent with the
  CODE final orbits available at URL

    http://www.aiub.unibe.ch/download/CODE/yyyy/CODwwwwd.EPH.Z

STA contains the following station related files

  CODE.STA        Bernese station information file for all stations
                  included in the CODE processing
  FES2004.BLQ     FES2004 ocean tide loading corrections for all
                  stations included in the CODE processing
  IGS.STA         Translation of the igs.SNX station information
                  (http://www.igs.org/igscb/station/general/igs.snx)
  IGS_FULL.STA    As IGS.STA but keeping individual antenna calibration
                  number
  EUREF.STA       Translation of the euref.SNX station information
                  (ftp://ftp.epncb.oma.be/pub/station/general/euref.snx)
  EUREF_FULL.STA  As EUREF.STA but keeping individual antenna calibration
                  number

  For each ITRF and IGS realizations, we provide three files:
  "REFNAM"_R.CRD: fiducial sites coordinate extraction in Bernese
                  format from reference frame "REFNAM"
  "REFNAM"_R.VEL: fiducial sites velocity extraction in Bernese
                  format from reference frame "REFNAM"
  "REFNAM".FIX:   list of fiducial sites of reference frame "REFNAM"

  Yearly subdirectories contain the following files in Bernese format:

  yyyy/

    CODyyddd.CRD.Z    Daily coordinate files of the CODE  final
                      global solution as from week 1706
    CODwwww7.CRD.Z    Weekly coordinate files of the CODE
                      final global solution as from week 0978
    COEwwww7.CRD.Z    Weekly coordinate files of the CODE EUREF
                      solution as from week 0886


----------------------------------------------------------------------
CODE product files
----------------------------------------------------------------------
CODE product files in international format, i.e., precise orbit files
in SP3c, clock information in Clock RINEX format, solution files in
SINEX format, Earth rotation files in IERS format, ionosphere infor-
mation in IONEX and Navigation RINEX format, and troposphere informa-
tion in Troposphere SINEX format, are available in the directory

  http://www.aiub.unibe.ch/download/CODE

This directory contains the following CODE rapid, ultra-rapid, and
predicted GNSS based products:

  COD.EPH_U         CODE ultra-rapid orbits, updated every 6 hours
  COD.ERP_U         CODE ultra-rapid ERPs belonging to the
                    ultra-rapid orbit product
  COD.TRO_U         CODE ultra-rapid troposphere product in SINEX
                    format
  COD.SUM_U         Summary of stations used for the latest
                    ultra-rapid orbit
  COD.ION_U         Last update of CODE rapid ionosphere product
                    (1 day) complemented with ionosphere
                    predictions (2 days)
  COD.EPH_5D        Last update of CODE 5-day orbit predictions, from
                    rapid analysis, including all active GLONASS
                    satellites

  CODwwwwd.EPH_R    CODE rapid orbits
  CODwwwwd.EPH_P    CODE 24-hour orbit predictions
  CODwwwwd.EPH_P2   CODE 48-hour orbit predictions
  CODwwwwd.EPH_5D   CODE 5-day orbit predictions
  CODwwwwd.ERP_R    CODE rapid ERPs belonging to the rapid orbits
  CODwwwwd.ERP_P    CODE predicted ERPs belonging to the predicted
                    24-hour orbits
  CODwwwwd.ERP_P2   CODE predicted ERPs belonging to the predicted
                    48-hour orbits
  CODwwwwd.ERP_5D   CODE predicted ERPs belonging to the predicted
                    5-day orbits
  CODwwwwd.CLK_R    CODE rapid clock product, Clock RINEX format
  CODwwwwd.TRO_R    CODE rapid troposphere product, SINEX format
  CODwwwwd.SNX_R.Z  CODE rapid solution, SINEX format
  CORGddd0.yyI      CODE rapid ionosphere product, IONEX format
  COPGddd0.yyI      CODE 1-day or 2-day ionosphere predictions,
                    in IONEX format
  CODwwwwd.ION_R    CODE rapid ionosphere product, Bernese format
  CODwwwwd.ION_P    CODE 1-day ionosphere predictions, Bernese format
  CODwwwwd.ION_P2   CODE 2-day ionosphere predictions, Bernese format
  CGIMddd0.yyN_R    Improved Klobuchar-style coefficients, RINEX
                    format
  CGIMddd0.yyN_P    1-day predictions of improved Klobuchar-style
                    coefficients
  CGIMddd0.yyN_P2   2-day predictions of improved Klobuchar-style
                    coefficients
  P1C1.DCB          CODE sliding 30-day P1-C1 DCB solution, Bernese
                    format, containing only the GPS satellites
  P1P2.DCB          CODE sliding 30-day P1-P2 DCB solution, Bernese
                    format, containing all GPS and GLONASS satellites
  P1P2_ALL.DCB      CODE sliding 30-day P1-P2 DCB solution, Bernese
                    format, containing all GPS and GLONASS satellites
                    and all stations used
  P1P2_GPS.DCB      CODE sliding 30-day P1-P2 DCB solution, Bernese
                    format, containing only the GPS satellites

Yearly subdirectories contain the CODE final products:

  yyyy/

    CODwwwwd.EPH.Z  CODE final GNSS orbits, our official IGS orbit
                    product
    CODwwwwd.ERP.Z  CODE final ERPs belonging to the final orbits
    CODwwwwd.CLK.Z  CODE final clock product, Clock RINEX format,
                    with a sampling of 30 sec for the satellite and
                    reference (station) clock corrections, and
                    5 minutes for all remaining station clock
                    corrections
    CODwwwwd.CLK_05S.Z
                    CODE final clock product, Clock RINEX format,
                    with a sampling of  5 sec for the satellite and
                    reference (station) clock corrections, and
                    5 minutes for all remaining station clock
                    corrections
    CODwwwwd.SNX.Z  CODE daily SINEX product
    CODwwwwd.TRO.Z  CODE final troposphere product, SINEX format
    CODGddd0.yyI.Z  CODE final ionosphere product, IONEX format
    CODwwwwd.ION.Z  CODE final ionosphere product, Bernese format
    CODwwww7.SNX.Z  CODE weekly SINEX product
    CODwwww7.SUM.Z  CODE weekly summary files
    CODwwww7.ERP.Z  CODE ERPs from a weekly solution
    COXwwwwd.EPH.Z  CODE precise GLONASS orbits (for GPS weeks
                    0990 to 1066)
    COXwwww7.SUM.Z  CODE weekly summary files of GLONASS analysis
    CGIMddd0.yyN.Z  Navigation messages containing improved
                    Klobuchar-style ionosphere coefficients
    P1C1yymm.DCB.Z  CODE monthly P1-C1 DCB solutions, Bernese format,
                    containing only the GPS satellites
    P1P2yymm.DCB.Z  CODE monthly P1-P2 DCB solutions, Bernese format,
                    containing all GPS and GLONASS satellites
    P1P2yymm_ALL.DCB.Z
                    CODE monthly P1-P2 DCB solutions, Bernese format,
                    containing all GPS and GLONASS satellites and all
                    stations used

As soon as a final product is available the corresponding rapid,
ultra-rapid, or predicted product is removed from the aftp server.


----------------------------------------------------------------------
Files from the CODE reprocessing
----------------------------------------------------------------------
CODE product files from the reprocessing activity 2011 are available
in the directory

  http://www.aiub.unibe.ch/download/REPRO_2011


GEN52               General files such as CONST., DATUM., GPSUTC.,
                    gravity fields, nutation and subdaily pole models,
                    as well as Earth and ocean tide models.

  PCV_COD.I08       Antenna phase center file
  SAT_1452.I08      Satellite information file
  SAT_yyyy.CRX      Yearly satellite problem files

REF52
  CODE.STA          Station information file (Format version 1.01)
  CODE.ABB          Station abbreviation table
  CODE.CLU          Cluster definition file
  ATM_VAPL.ATL      S1 and S2 atmospheric tidal coefficients
  EOP08C04.ERP      Earth rotation parameters
  FES2004.BLQ       Ocean loading displacements (as of 26-Apr-2011)
  IGS08COD.SNX      SINEX file containing coordinates and velocities
                    of the IGS08 fiducial sites

BSWUSER52
  yyyy/

    CODyyddd.TRP.Z   Troposphere information files
    CODwwww7.CRD.Z   Weekly coordinate files
    CODwwww7.ERP.Z   Weekly final Earth rotation parameter files,
                     Bernese format
    CODwwww7.GCC.Z   Weekly geocenter coordinate files

CODE
  CODE_REPRO_2011.ACN  Analysis strategy summary

  yyyy/

    CODwwwwd.EPH.Z   Final GNSS orbits
    CODwwww7.ERP.Z   Weekly final Earth rotation parameter files,
                     IERS format
    CODwwww7.SNX.Z   Weekly SINEX product

======================================================================

