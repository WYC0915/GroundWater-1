======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 31-Oct-2012
----------------------------------------------------------------------
Conversion from version 5.0 to 5.2
======================================================================


----------------------------------------------------------------------
Changed files
----------------------------------------------------------------------

Changed formats
---------------
- Station information/problem file
  (extensions STA and CRX in the STA directory of the campaign)
  Use program STA2STA to convert these files from version 5.0 (format
  version "0.00") to the new format ("1.01" or higher).

  Menu >Service >Station information files >Convert station
                                            information files

- Antenna phase center correction file
  Use the new program ATX2PCV to read the corrections from ANTEX for
  all the stations contained in the station information file.

  Menu >Conversion >ANTEX to Bernese format

- Satellite information file
  The new formatted file is included in the distribution, but you can
  also download it from

  ftp://ftp.unibe.ch/aiub/BSWUSER52/GEN

- Constant and datum information files
  Files extended with the additional records are included in the
  distribution, but you can also download them from

  ftp://ftp.unibe.ch/aiub/BSWUSER52/GEN

- Program input files (extension INP in ${U}/PAN or %U%\PAN directory)
  Use the new input files distributed with the software. You may use

  Menu >Config >Update program input files

  to update your old input files to the new version.

  Attention:
    The new input files cannot be displayed with your menu from
    version 5.0 anymore.


New formats without converters
------------------------------
There are several more file formats that have changed from version 5.0
to version 5.2. The old formats can still be read by the programs but
files stored by the program from version 5.2 cannot be handled by the
version 5.0 programs.

These file types are:
- Coordinate and velocity files (extensions CRD and VEL in the STA
  directory of the campaign)
- Normal equation files (extension NQ0 in the SOL directory of the
  campaign)
- Standard orbit files (extension STD in the ORB directory of the
  campaign)
- Observation (header) files (extensions CZH, PZH, RZH, CSH, and PSH
  in the OBS directory of the campaign)
- DCB files may contain new record types (extension DCB in the ORB
  directory of the campaign)


New file types
--------------
- Grid files (Extension GRD in the GRD directory of the campaign)
  VMF1 coefficients and geophysical models

- Atmospheric tidal loading (Extension ATL in the STA directory of the
  campaign)
  Extraction (done by program GRDS1S2) from a global grid for the
  stations to be processed.

  Menu >Service >Coordinate tools >Extract atmospheric tidal loading
                                   coefficients

- Gravitational effect of the solid Earth tides on orbits
  (Extension TPO in ${X}/GEN - or %X%\GEN% - directory)
  To be used by ORBGEN analogue to the corrections for the ocean tides

- Satellite-/receiver-specific observation type selection
  (in ${X}/GEN - or %X%\GEN% - directory)
  To select the measurement types from a RINEX 2 or a RINEX 3
  observation file to be transferred for the first and second
  frequency into the Bernese formatted observation file

- Bias exchange file (extension BIA in the ORB directory of the
  campaign)
  International exchange format for receiver/satellite code and phase
  biases

- Event list file and Earthquake file
  (extensions EVL and ERQ in STA directory of the campaign)
  To be used to define a priori events for FODITS and to report
  significant events from a FODITS analysis (EVL only)

  Menu >Coordinate tools >Analysis of time series

- SLR corrections (extension SLR in the STA directory of the campaign)
  To provide biases and center of mass corrections for SLR processing


File types not supported anymore
--------------------------------
- All file types prior version 5.0 are not supported anymore, in
  particular old normal equation files (extension NEQ), station
  renaming tables (extension STN), receiver/antenna renaming tables
  (extension TRN), antenna height translation tables (HTR), old
  abbreviation tables, etc.

- Orbital element files (extention ELE) generated with version 5.0
  cannot be integrated by ORBGEN of version 5.2.


----------------------------------------------------------------------
Campaign directories
----------------------------------------------------------------------
To process an existing campaign created with Bernese GPS Software
Version 5.0, the whole campaign directory has to be moved (or copied)
to the new campaign area.

Example (UNIX command line syntax):

  cp -pr ${P}(V5.0)/MY_CAMP ${P}(V5.2)/MY_CAMP

with

  ${P}(V5.0): campaign area of version 5.0 (e.g., ~/GPSDATA)
  ${P}(V5.2): campaign area of version 5.2 (~/GPSDATA/CAMPAIGN52)

Add the campaign to the new list of campaigns (version 5.2).

  Menu >Campaign >Edit list of campaigns : + ${P}/MY_CAMP
  Menu >Campaign >Select active campaign : ${P}/MY_CAMP


After applying the conversions mentioned above, the campaign files
can already be processed with the new software in interactive mode or
by using the example BPEs from version 5.2.


----------------------------------------------------------------------
Transfer BPEs from V5.0 to V5.2
----------------------------------------------------------------------
A general recommendation is to start developping BPEs for version 5.2
using the distributed examples as templates. This way you can be sure
that the latest features are used.

Nevertheles, there might be reasons why you want to transfer your
existing BPEs from V5.0 to V5.2. In this case, the user files from the
BSW 5.0 user environment have to be moved (or copied) to the new user
environment of BSW 5.2.

Example (UNIX command line syntax):

 cp -pi  ${U}(V5.0)/PCF/*    ${U}(V5.2)/PCF    # Don't overwrite files
 cp -pi  ${U}(V5.0)/SCRIPT/* ${U}(V5.2)/SCRIPT # Don't overwrite files
 cp -pri ${U}(V5.0)/OPT/*    ${U}(V5.2)/OPT    # Don't overwrite files

with

  ${U}(V5.0): GPSUSER area of version 5.0 (e.g., ~/GPSUSER)
  ${U}(V5.2): GPSUSER area of version 5.2 (~/GPSUSER52)

Remarks:    - PCF files with the same name have to be renamed.

            - The user scripts (in $U/SCRIPT) which have the same name
              in version 5.0 and 5.2 used for the example campaign
              can be skipped from copying. I.e. the scripts from
              version 5.2 should be used.
              The only exeptions might be the ???_COP and the
              ???_SUM scripts.

            - If you overwrite existing program input files in option
              subdirectories, the behaviour of existing BPEs may
              change (even after updating program input files, see
              next section).
              If you want to avoid this, you may rename option
              subdirectories and adapt your PCF accordingly.

----------------------------------------------------------------------
Update of the program input files
----------------------------------------------------------------------
The program input files can be updated with the tool UPDPAN.

  Menu >Configure >Update program input files

It is recommended to select all INP files from ${X}/PAN (using "*")
and updating the existing files in OPT/* to be able to run the old
BPEs with the new software version.

The output file from UPDPAN as well as the updated program input
panels which changed should be studied carefully.


----------------------------------------------------------------------
Example: RNX2SNX_V50.PCF with new software
----------------------------------------------------------------------
The next step is to update a few input options that have changed or
new ones which are required in the new version. For this purpose use
the program CHNGEN.

  Menu >Configure >Change general options

Select the panel directories ${U}/OPT/* and set a new value for the
two names

    Satellite info             -> SATELLIT.I01
    Phase center ecc.          -> PCV.I01

Then, some other keywords have to be changed on the next panel.

Keyword        Old value       New value
*******        *********       *********
JPLEPH         DE200           DE405
ABBREV         ABBREV          EXAMPLE
LENINTER       1.0             01 00 00
VARLNINT       6.0             06 00 00
PRINT          NO              0
PRINT          YES             1
BLKRET         0               NO
BLKRET         1               YES

This is already enough for the RNX2SNX.PCF example of the old BSW.
For other BPEs there might be additional keywords to be changed.

One option has been added to the program input file of RESRMS ("Create
residual statistics") that has to be set manually by opening the
correspondig panels.

  Menu >BPE >Edit PCF program input files
  Select RNX2SNX_V50.PCF
  Select all RESRMS panels (e.g. in GPSEDT and RESCHK steps)
  Set sampling rate of residual files to "180"


Note: in your own BPEs other keywords may be affected.

----------------------------------------------------------------------
Update the models
----------------------------------------------------------------------
After verification of the results, you can now update to the latest
models, e.g., reference frame IGb08 and antenna model I08.
For further instructions in this matter, see Sections 20.5.3 ("Change
of Reference Frame") and 20.5.4 ("Change of the Antenna Models") in
the Software Documentation.

======================================================================
