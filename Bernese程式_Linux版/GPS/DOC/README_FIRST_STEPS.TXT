======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 23-Oct-2012
----------------------------------------------------------------------
First steps
======================================================================


----------------------------------------------------------------------
Start and configure the Bernese user interface
----------------------------------------------------------------------

WINDOWS-platforms:
  Double-click on the icon on your desktop to start the menu of the
  Bernese GNSS Software.

UNIX-platforms:
  Make sure that the environment variables are loaded. Depending on
  your shell use one of the following commands:
      1. ". /home/BERN52/GPS/EXE/LOADGPS.setvar"  or
      2. "source /home/BERN52/GPS/EXE/LOADGPS.setvar"
  (Adapt the path to LOADGPS.setvar if necessary).
  You may add this step to your login script.
  Type "G" on the command line to start the menu of the Bernese GNSS
  Software.

You can freely change the size of the window, background and button
colors, and fonts to your preferences using the items in the submenu
"Configure > Menu layout". The settings will be recorded.
Using "Configure > Menu layout > Reset menu layout" you can always
reset the settings to the factory defaults (use the key sequence:
"<ALT>+f l r y" in case the font becomes illegible).


----------------------------------------------------------------------
General overview
----------------------------------------------------------------------
Read the general help file (menu item "Help > General") first to get
an overview on the Bernese GNSS Software, paying particular attention
to the following items:

  - how to use the menu,
  - the configuration of the menu and the Bernese environment,
  - the selection of the current session and the active campaign,
  - the usage of the session table, and
  - the different types of variables and their usage.

Inspect the list of programs in the general help (alphabetical
order) or in the menu item "Configure > Program names" (order
corresponding to the menubar) to get an impression on all availalble
programs of the software package and how to access them from the menu.

The structure of the subdirectories in the campaign may have changed
since the last version. We suggest to browse (not change!) through the
panels in the menu items "Configure > Paths and Extensions" to get an
overview on the different file types of the Bernese GNSS Software and
the new directory structure in the campaign.


----------------------------------------------------------------------
Getting started
----------------------------------------------------------------------
In the following, this file provides information on how to start
working with the new version of the Bernese GNSS Software:

  Section A) General dataflow
  Section B) Run the examples
  Section C) Setup a new campaign, prepare the processing
  Section D) Setup an automated processing
  Section E) How to proceed in case of an error in the BPE?


A. GENERAL DATAFLOW
===================

Within several years, a reasonnable amount of GNSS data accumulates
which needs to be handled for efficient analysis. The general
organization of the dataflow should be organized according to your
needs, projects, and infrastructure. From our own experience and
contact with numerous users, we recommend to organize the general
dataflow as follows:

1) DATAPOOL:
   This is an area of your harddisk where you can collect external
   data (e.g., orbits, RINEX files from IGS stations, etc.) as well as
   the observation files from/for your project(s). This DATAPOOL can
   either be filled manually or automatically with a set of FTP
   scripts. The file ${D}/README_DATAPOOL.TXT
   (or %D%\README_DATAPOOL.TXT) details the motivations for a DATAPOOL
   area and proposes a structure for it.

2) CAMPAIGN52:
   In this area are located the processing campaigns. When starting to
   process data, the necessary files are first copied from the
   DATAPOOL into the active campaign. Now you run the programs of the
   Bernese GNSS Software on the data in this area to generate the
   final results for you project (or a certain session).

3) SAVEDISK:
   The result files from the processing in the campaign area are
   archived in the SAVEDISK area at the end of the processing.
   Here you only need to store the important (final) results from your
   project before you can clean up the campaign directory you have
   used to generate these results.
   The copies from the main result files from the processing examples
   are provided as reference solution in a proposed structure of the
   SAVEDISK area. A description can be found in the file
   ${S}/README_SAVEDISK.TXT (or %S%\README_SAVEDISK.TXT).


B. RUN THE EXAMPLES
===================

The examples distributed with version 5.2 of the Bernese GNSS
Software were completely redesigned and are more instructive than in
the previous versions. Each example consists of a ready-to-use BPE
PCF:

  PPP_BAS.PCF   Computes a priori coordinates for a set of stations on
                the basis of a Precise Point Positioning (PPP) ana-
                lysis.
  PPP_DEMO.PCF  PPP containing several extended processing examples,
                like pseudo-kinematic, high-rate troposphere, or
                ionosphere solutions.
  RNX2SNX.PCF   Computes station coordinates and troposphere para-
                meters based on RINEX data from a regional GNSS
                network.
  BASTST.PCF    Baseline by baseline processing for trouble shooting.
  CLKDET.PCF    Computes station and satellite clock corrections
                based on RINEX data for a regional GNSS network
                starting from broadcast clock.
  SLRVAL.PCF    Validation of satellite orbits (GNSS or LEO) with
                Satellite Laser Ranging (SLR) data
  LEOPOD.PCF    Reduced-dynamic and kinematic orbit determinations for
                a Low Earth Orbiting (LEO) satellite (in this case
                GRACE A or B)

The corresponding <BPENAME>.README explain the basic structure, the
functionalities, and the input and result files.
All example BPEs contain a section 'Input files' in the official
international standard formats (RINEX for observation files; SP3c for
precise orbit files, IERS format for Earth rotation parameters), that
are copied from the DATAPOOL into your CAMPAIGN.

The examples may also be used to test the installation by checking the
correct behaviour of the BPE and programs: compare your results with
the results generated with our own compiler and available in the
corresponding directories of the SAVEDISK ($S or %S%).

To run a BPE example, start the menu and follow the instructions:

1. For NON-Windows users only: extract the examples using the
   "${X}/EXE/configure.pm" tool if this was not done before during the
   installation.

2. Add the name of the campaign "${P}/EXAMPLE" to the campaign list in
   the menu item "Campaign > Edit list of campaigns".
   Pay attention to the brackets: ${P} is an environment variable, see
   the general help for more details on the usage of variables.

3. Select the campaign "${P}/EXAMPLE" as the active campaign using the
   menu item "Campaign > Select active campaign".

4. The example contains data for four sessions: day of year 207 and
   208 of year 2010 resp. 205 and 206 of year 2011. Select one of
   these days for the current session (e.g., day 207 of year 2010).
   Use the menu item "Configure > Set session/compute date" to set the
   date. The session character needs to be "0" and the provided
   session table has the default name "SESSIONS.SES" and is located in
   the STA directory.

5. The CPU control file is located in the "${U}/PAN" directory and has
   the default extension "CPU". An example CPU control file is
   distributed for WINDOWS and UNIX platforms.
   Users with a WINDOWS-platform can skip this item because the file
   does not need any change.
   Users on an UNIX-platform are invited to inspect the CPU control
   file (menu item "BPE > Edit CPU file"). It contains an entry for
   the localhost that works for all shells because it uses a bourne
   shell. You can use this entry for a first run of the BPE. To invoke
   the full power of the BPE, adapt the example entries to your system
   (bourne-shell or c-shell syntax, or rsh/ssh access to remote
   hosts). Please refer to the help panel for more details.

6. Now you are ready to run the BPE examples.

   Run the PPP_DEMO.PCF (example of a precise point positioning) at
   first for session 2070 of year 2010 to get a priori coordinates for
   all stations and to complete the list of abbreviations.
   Use the menu item "BPE > Start BPE process" to start the BPE.
   To run the BPE on WINDOWS-platforms make sure that the input field
   "Client's environment file" in panel "BPE 1: Client
   Environment/Session Selection" is empty (see online help).
   Using the default settings you will process the current session in
   the active campaign. Select the "PPP_DEMO.PCF" as the "Process
   control file" in the panel "BPE 2: Process Control Options". Select
   the "CPU control file" (default name "USER.CPU"). In the panel
   "BPE 3: Output Filenames" you should select reasonable filenames
   (e.g., "Task ID" = "PP", "Status file" = "PPP_DEMO.RUN", "Program
   output" = "PPP_DEMO.OUT"). Please check in the last panel ("BPE 4:
   Server Variables Sent to Client") that
   - the variable V_UPD is set to "Y" (update files in the DATAPOOL
     with the PPP results) and
   - the variable V_SAV is set to "Y" (copy result files from PPP to
     the SAVEDISK area)
   Press the "^Run"-button to start the BPE.

   If the BPE does not run or makes no progress please inspect 
   section "E. HOW TO PROCEED IN CASE OF AN ERROR IN THE BPE?"
   in this README-file.

7. Go into the directory ${D}/REF52 (%D%\REF52) and compare the
   EXAMPLE.CRD, EXAMPLE.VEL, and EXAMPLE.ABB file just generated by
   the BPE with the corresponding _REF files delivered as reference
   together with the software. In the SAVEDISK area there are more
   reference files to verify your installation (subdirectories of
   ${S}/PPP/2010/ or %S%\PPP\2010, see ${S}/README_SAVEDISK.TXT or
   %S%\README_SAVEDISK.TXT for a list of files).
   We expect no (significant) differences between your solution and
   the reference files.

8. In an analogue way you may process the remaining days for the
   PPP_DEMO.PCF BPE or the other examples like RNX2SNX.PCF, 
   CLKDET.PCF, LEOPOD.PCF, and SLRVAL.PCF.
   Data and orbit information for the days 207 and 208 of year 2010 as
   well as 205 and 206 of year 2011 are included in the datapool as
   distributed with the software (note: for the LEOPOD.PCF the data
   are only distributed for the two days in 2007).
   Compare your result files in the savedisk-directories with the
   reference files flagged with "_REF". If they are identical (within
   the "compiler noise") you can assume that your installation of the
   Bernese GNSS Software works correctly. The a priori coordinates
   must stem from the PPP_DEMO.PCF of session 2070 of year 2010 to get
   comparable results.

After running the example BPEs you are invited to inspect the
subdirectories of the campaign ${P}/EXAMPLE (or %P%\EXAMPLE) to become
familiar with the directory structure and the location of the files.

We also propose to inspect the input panels of the BPE examples (menu
item "BPE > Edit PCF program input files") to become familiar with the
layout of the new program panels and to get an overview on the new
options in the version 5.2 of the Bernese GNSS Software. You can use
the online help (menu item "Help > Help on context") to get more
information on options in the cursor's focus.


C. SET UP A NEW CAMPAIGN, PREPARE THE PROCESSING
===============================================

This section is a cookbook to set up a new campaign and prepare the
processing. Depending on your application you may have to adapt the
following procedures.

1. Add the name of your new campaign (e.g., "${P}/MYFIRST") to the
   campaign list in the menu item "Campaign > Edit list of campaigns".

   Note to WINDOWS-users: you have to use the same UNIX-style syntax
   here because the variables are handled by the C++ menu and the
   Perl-environment.

   If the campaign is located on a disk referred by another variable
   (e.g., ${Q}, instead of ${P}) the UNIX-users must add the
   definition of the environment variable to the file
   "${X}/EXE/LOADGPS.setvar". WINDOWS-users must also define the cor-
   responding environment variable in their registry. They also have
   to add the variable to the "BERNESE_VARIABLES" entry of the
   registry. All users must also add this new environment variable to
   the list in the "${U}/PAN/MENU_VAR.INP" file (menu item
   "Configure > Menu variables").

2. Select the campaign "${P}/MYFIRST" as the active campaign using the
   menu item "Campaign > Select active campaign".
   You will get a message that no session table is available in your
   actual campaign. Do not worry about it at this stage.

3. Create the directory structure using the menu item
   "Campaign > Create new campaign". On the right hand side of your
   panel is a field with the label "Copy files into the campaign"
   with the default entry:
       from:  ${X}/SKL/SESSIONS.SES    to:  STA
   SESSIONS.SES is a session table (see step 4. below) that is pre-
   pared for a daily processing scheme. If you plan to set  up an
   hourly processing scheme, you should copy ${X}/SKL/HOURLY.SES
   instead.

   Note: you need write permissions especially in the top-level
   directory where the campaign directory will be created.

4. If you have decided to use the alternative session table
   ("STA/HOURLY.SES" instead of "STA/SESSIONS.SES") you have to enable
   it in the Date Selection Dialog ("Configure > Set
   session/compute date"), input field for "Session Table". The
   "Session Char" should also be changed from "0" to a letter between
   "A" and "X" in that case. Confirm the selection with the "OK"-
   button.

   Independently from which of the two default session tables you
   copied in step 3 into your campaign area, we recommend to inspect
   the session table using the menu item "Campaign > Edit session
   table".

   The online help will provide more information on the philosophy of
   the session definition in the Bernese GNSS Software. We propose to
   use the "open session definition" whenever possible.

Your campaign is now ready to process your data.
In the next steps we fill up the DATAPOOL with the necessary infor-
mation:

5. Copy your RINEX observation files into a specific subdirectory of
   the DATAPOOL (e.g., "myRINEX"). They may be UNIX-compressed files
   in the Hatanaka compact format. All BPEs contain a script "RNX_COP"
   copying the observation files from the DATAPOOL into the campaign.
   There is a BPE server variable "V_RNXDIR" that must contain this
   directory name. You can either specify the name (e.g., "myRINEX")
   either directly in the PCF file or in the last input panel when you
   start the BPE using the menu ("BPE > Start BPE processing").
   The mechanism how the RINEX files are selected for the processing
   when you are using the RNX_COP script is described in the
   README files of the BPEs.

6. Transfer all other input data files to your local host and put them
   into the campaign directories:

    ${D}/COD/CODwwwwd.PRE.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy
    ${D}/COD/CODwwwwd.CLK.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy
    ${D}/COD/CODwwwwd.ERP.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy or
    ${D}/COD/CODwwww7.ERP.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy
                                  (before week 1706)
   To use IGS products instead:
    ${D}/IGS/IGSwwwwd.PRE.Z  from ftp://ftp.igs.org/igscb/product/wwww
    ${D}/IGS/IGLwwwwd.PRE.Z  from
                          ftp://ftp.igs.org/igscb/glonass/product/wwww
    ${D}/IGS/IGSwwwwd.CLK.Z  from ftp://ftp.igs.org/igscb/product/wwww
    ${D}/IGS/IGSwwww7.ERP.Z  from ftp://ftp.igs.org/igscb/product/wwww

   Other files in Bernese formats:
    ${D}/BSW52/CODwwwwd.ION.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy
    (for higher order ionosphere corrections and/or QIF ambiguity
    resolution)

    ${D}/BSW52/P1P2yymm.DCB.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy
    (for ionosphere determination)

    ${D}/BSW52/P1C1yymm.DCB.Z  from ftp://ftp.unibe.ch/aiub/CODE/yyyy
    (for clock estimation)

   Reference frame definition:
    ${D}/REF52/IGB08_R.CRD  from ftp://ftp.unibe.ch/aiub/BSWUSER52/STA
    ${D}/REF52/IGB08_R.VEL  from ftp://ftp.unibe.ch/aiub/BSWUSER52/STA

   VMF1-coefficients (if needed):
    ${D}/VMF1/VMFG_yyyymmdd.Hhh.gz
             from  http://ggosatm.hg.tuwien.ac.at/DELAY/GRID/VMFG/yyyy

   The time stamps in the file names need to refer to the day to be
   processed - you may open the dialogue "Configure > Set session/
   compute date" to get the value for the actual session:
        wwww ... GPS week
        d    ... day of GPS week
        yyyy ... four-digit year
        yy   ... two-digit year
        mm   ... month of year
        dd   ... day of month
        hh   ... hour of the day

In the following steps the project specific files in ${D}/REF52 are
prepared.
We recommend that all files belonging to one project have the same
basename.
They are copied into the campaign by the BPE-specific copy script
(XXX_COP, where XXX is the identifier of the BPE, e.g., PPP). The
basename of these files is given into the BPE by the BPE server
variable V_CRDINF.

7. Create the "Station information file"
   You have three different ways to create your station information
   file:
   - You may use the entries from the RINEX headers as a starting
     point for the generation of the station information file. For
     this purpose you need to extract all RINEX observation files into
     the RAW-directory of your campaign (no compress or Hatanaka
     compact format is permitted).
     You may use the menu item "Service > Station information files >
     Extract information from RINEX" to prepare a draft version of the
     station information file.
   - If the equipment information is available in SINEX format you can
     import these records into a station information file using the
     menu item  "Service > Station information files > Extract
     information from SINEX". Of course you have to complete the list
     of stations if necessary.
   - You can simply create this file from scratch by hand with the
     support of the menu item "Campaign > Edit station files > Station
     information file".

   Extractions from the RINEX header or SINEX files should only serve
   as a draft version of the station information file. It is necessary
   to inspect and complete the obtained station information file using
   the menu ("Campaign > Station files > Station information") or with
   your favorite text editor. Check all entries with your observation
   protocols for the receiver and antenna information records. Make
   sure that the station name corresponds to the name you want to use
   for your analysis.
   For each station a record on the used equipment is expected
   ("Receiver type", "Antenna type", and "Antenna eccentricity"). If
   you write SINEX files also the "Description" field is recommended.
   The columns "Ant #" and "Antenna serial nbr" need only be filled
   for thoses stations where you want to introduce individual antenna
   calibration corrections (instead of type mean values).

   If you want to change the names of the stations in your processing
   with respect to the observation RINEX file, you have to introduce
   the station infornmation file in the PPP_BAS.PCF (step 10).
   Otherwise you can prepare the file also after the PPP_BAS.PCF step.

8. When importing the observations from the RINEX observation files
   into the Bernese format, the header information is extensively
   checked with respect to the equipment given in the station infor-
   mation file.
   If there are known inconsistencies (e.g., a missspelled receiver
   type) they can be listed in "Accepted station information
   inconsistencies" in panel "RXOBV3 5.2: Check Content of RINEX
   Header 2".

9. Create a PLD-file (tectonic plate assignment for the NUVEL-model)
   for all your sites. You can use the menu item "Campaign > Station
   files > Tectonic plate assignment" to do this. If you have a
   Bernese coordinate file containing the station names it can be
   selected to support the creation of the PLD-file. Otherwise cancel
   the dialogue "Select Skeleton Coordinate File" when creating a new
   PLD-file.

10.If you have a priori coordinates from a phase solution for all your
   sites you may skip this step.

   Run the PPP_BAS.PCF for the current session in your individual
   campaign to update the coordinate, velocity, and abbreviation
   files.
   Remark: PPP_DEMO.PCF that has been used above adds some examples
   for further opportunities and applications.

   If your actual campaign is located on a disk referred by another
   variable than the default location ${P} (e.g., ${Q}) you have to
   make sure that this environment variable is listed in the
   "${U}/OPT/PPP_???/MENU_VAR.INP" file in each option directory used
   by PPP_BAS.PCF (use menu item "BPE > Edit single menu/program input
   file" to edit these files).

   The names of the input files of the processing are defined in the
   variable section of the PPP_BAS.PCF. They can be adapted in the
   panel "BPE 4: Server Variables Sent to Client" which is displayed
   when you start the BPE:
     V_RNXDIR:  Directory with the RINEX files to be processed
     V_CRDINF:  Project-related basename of the basic files to be
                used for the tectonic plate assignment (default
                extension PLD -- generated in step 9), coordinate
                (default extension CRD), velocity (default
                extension VEL), and abbreviation table (default
                extension ABB). The CRD, VEL, and ABB files may exist
                in ${D}/REF52 but they can also be created.
     V_STAINF:  Station information file (the project-related filename
                is recommended -- from step 7); blank if no station
                information file is used (equipment records from the
                RINEX files are used directly)
     V_CRXINF:  Accepted station information inconsistencies
                (project-related filename -- from step 8) or blank if
                no such file is needed
     V_BLQINF:  Ocean tidal loading corrections if available
                (project-related filename -- from step 12)
     V_ATLINF:  Atmospheric tidal loading corrections if available
                (project-related filename -- from step 13)

   It is important that the variable V_UPD is set to "Y" to allow an
   update of the coordinate, velocity, and abbraviation files in the
   reference file directory of the DATAPOOL.

   After running the BPE a processing protocol file OUT/PPPyyssss.PRC
   is available. Information on its expected content is given in the
   $U/PCF/PPP_BAS.README (or %U%\PCF\PPP_BAS.README).

   In the STA-directory of your campaign the following files result
   from the PPP_BAS.PCF:
     STA/PPPyyssss.CRD:  coordinate results for the epoch of the
                         observations.
     STA/REFyyssss.CRD:  coordinate results for the epoch given in the
                         BPE server variable V_REFEPO (for example
                         2005-01-01) the NUVEL-1A velocities if a
                         PLD-file was given.
     STA/APRyyssss.VEL:  NUVEL-1A velocities for all stations included
                         in the PPP.
     STA/(V_CRDINF).CRD: Content of STA/IGS08_R.CRD plus the coordi-
                         nate results for all new stations.
     STA/(V_CRDINF).VEL: Content of STA/IGS08_R.VEL plus the NUVEL-1A
                         velocities for all new stations.
     STA/(V_CRDINF).ABB: New or updated abbreviation table.
   If you have allowed the BPE to update the files in the reference
   directory (V_UPD = "Y") the three files STA/(V_CRDINF).CRD,
   STA/(V_CRDINF).VEL, and STA/(V_CRDINF).ABB are copied to
   ${D}/REF52 directory and are ready for further use.

11.The station name abbreviation table is created or updated
   automatically in the PPP_BAS.PCF.
   The four- and two-character abbreviations for all stations are
   unique.
   You may inspect this file using the menu item "Campaign > Station
   files > Abbreviation table" or with your favorite text editor. If
   you change any value, please make sure that the abbreviations are
   still unambiguous for all stations.

12.Use the web-service at http://www.oso.chalmers.se/~loading/ to get
   the table of the ocean loading coefficients for your sites. The
   coordinates from the "${D}/REF52/(prorect-related filename).CRD"
   may be copied into the input field of the web-page (for efficien-
   cy, select only those sites where you have no coefficients yet).
   For the GPS analysis you need the vertical and horizontal displa-
   cement. No corrections for the center of mass motion have to be
   applied. The ocean loading file is sent by e-mail in the BLQ-format
   to you. You have to save these information in a file with a
   project-related name and the extension "BLQ" in ${D}/REF52
   directory. (See "${X}/DOC/README_BLQ.TXT" for further information)

   You may introduce the BLQ-file into the PPP_BAS (step 10) if you
   already have the corrections for your sites. In this case the
   corrections will be applied. In the protocol file are listed sta-
   tions where no ocean loading corrections were available. The
   BLQ-file is not mandatory for the PPP_BAS. You can create this file
   later. It is recommended to apply the corrections for the ocean
   loading displacement at least for the final processing step of
   GPSEST.

13.Generate a atmospheric tidal pressure loading table using the menu
   item "Service > Coordinate tools > Extract atmospheric tidal
   loading coefficients". The corrections need to be consistent if you
   plan to apply atmospheric non-tidal pressure loading corrections.
   Otherwise you should extract the information from the grid file
   "${X}/GEN/s1_s2_def_ce.dat" (CoF: Center of Figure related cor-
   rections).
   The input coordinate file is the project-related coordinate file
   resulted from PPP_BAS.PCF; the result file should have the same
   name but with the extension ATL. S1/S2 center of mass corrections
   should be added consistently from the RAY_PONTE model. (See also
   "${X}/DOC/README_ATL.TXT" for further information)

Your campaign is now ready for any further analysis of your data.

You can follow the tutorial from the Bernese Introductory Course
available on the installtion CD-ROM or online from the Bernese GNSS
Software home page. It gives cookbook-like instructions for a manual
processing of a regional network.
Further information are available from the online help. It is inten-
ded as a guideline for the processing, especially for the new features
of the Bernese GNSS Software, version 5.2, and their usage.


D. SETUP AN AUTOMATED PROCESSING
================================

The BPE is the core tool for the automated processing of GNSS data
using the Bernese GNSS Software. After going through step C. you are
ready to run the example BPEs on your own data. As long as you are
still using the same structure of the DATAPOOL and SAVEDISK areas you
can manage most the adaptations by the BPE server variables.

If you start to change the structure (e.g., putting orbits in yearly
subdirectories) you need to adapt the copy scripts at the beginning of
each BPE accordingly (XXX_COP where XXX is an identifier for each BPE,
e.g., PPP or CLK). All result files are only copied by the save
scripts of the BPE into the SAVEDISK area (XXX_SAV with XXX again as
the identifier for each BPE). Of course you are invited to adapt the
example BPEs to your own needs.
Do not forget to include new files that are created in your adapted
BPE in the deletion script (XXX_DEL again stands XXX for the BPE
identifier).

We recommend to go through the panels (at least of the main programs)
for the provided example BPEs (menu: "BPE > Edit PCF program input
files") to become familiar with the options and features. This is in
particular important for users of previous versions of the Software
because the version 5.2 contains a lot of new features that might be
useful also for your applications. We recommend to inspect the online
help for a detailed description of all input fields.

Starting BPE without the menu
-----------------------------
As in previous versions, it is also possible to start the BPE without
going through the menu. We have prepared a Perl module, which is
necessary for this. Also, you can find the Perl script rnx2snx_pcs.pl
in the directory %U%/SCRIPT, which provides an example for the actual
script used to run the BPE for the RNX2SNX example. This script can be
called from the command line, or from any job scheduler.
Note: we highly recommend to use Perl for such tasks, although you
could also use shell/batch scripts.


E. HOW TO PROCEED IN CASE OF AN ERROR IN THE BPE?
=================================================

- Check that the campaign is set correctly.

- Check that the session is set correctly.

- Check the BPE output file ("^OUTPUT"-button).

- Check protocol and log files from the BPE. These files are written
  into the BPE subdirectory of the campaign. Their filenames include
  task-ID, session year and day of year, PID, and sub-PID.
  Protocol files (default extension PRT) provide information on the
  process started as well as warnings and error messages from programs
  executed in the scripts.
  Log files (default extension LOG) give the output from scripts and
  programs that is written to the standard output and to the error
  output. Debug output written in the scripts appears in the log file.

- Check whether you are permitted to establish a TCP/IP-connection 
  from the BPE client to the BPE server using the hostname given in
  $BPE_SERVER_HOST (in particular for UNIX/Linux since for Windows
  platforms the variable is typically set to "localhost" which should
  work in any case).

- Check whether the CPU keywords you used in your PCF are available
  (MAXJOB > 0).

- Make sure that your PCF is not in the list of "Suspended processes"
  (second panel in the CPU file "BPE > Edit CPU file"). The online
  help gives more insight on the algorithm behind.

- Check that the corresponding jobsubmit commands are correct:

  WINDOWS-platforms: the line must be the default line reported in the
  online help as well as in the distributed CPU file (USER.CPU in
  ${X}/PAN).

  UNIX-platforms: take your command line from the CPU-file, replace
  <COMMAND> by "ls", <ARGS> by "-l", and <LOG> by "ls.out". Execute
  the command. It has to run in the background, without any further
  input from keyboard (e.g., entering of a password to get access to a
  remote host). The answer of the "ls -l" command is expected to be
  written into the "ls.out"-file. Check the command also with a "ls"
  command for a file which does not exist to make sure that the error
  output is directed into the "ls.out"-file.

- Reset the CPU-file (menu item "BPE > Reset CPU file"). This action
  sets the entries for the number of running jobs to zero. This is
  always required after a BPE job has been killed. The reset of the
  CPU file does no harm, even if executed while a BPE is running.

======================================================================
