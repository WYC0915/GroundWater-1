======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 26-Oct-2012
----------------------------------------------------------------------
DATAPOOL area
======================================================================


----------------------------------------------------------------------
Motivation for the DATAPOOL area
----------------------------------------------------------------------
The idea of the DATAPOOL area is to place local copies of external
files somewhere on your filesystem. It has several advantages compared
to downloading the data each time when starting the processing:

- The files are downloaded only once, even if they are used for
  several campaigns.
- The data download can be organized with a set of scripts running
  independently from the Bernese GNSS Software environment, scheduled
  by the expected availability of the external files to download.
- The processing itself becomes independent from the availability of
  external data sources.

Data flow philosophy:

  --download--> DATAPOOL --copy--> CAMPAIGN --move--> SAVEDISK
     (data)               (data)            (results)

----------------------------------------------------------------------
Structure and content of the DATAPOOL area
----------------------------------------------------------------------
The DATAPOOL area contains several subdirectories taking into account
the different potential sources of files and their formats:

- RINEX: The data of GNSS stations is provided in RINEX files. The
         directory contains observation (Hatanaka-compressed) and
         navigation (GPS and GLONASS) files. These RINEX files are
         "originary" files that are not changed during the processing.

         The RINEX files can be downloaded from international data
         centers. Project-specific files are copied into this
         area. If you mix the station lists from different projects,
         take care on the uniqueness of the four-character IDs of all
         stations in the RINEX file names.

- HOURLY: The same as the RINEX directory but dedicated to hourly
         RINEX data as used for near real-time applications.
         Note: not all stations in this example provide hourly RINEX
         files.

- SLR_NP: The SLR data is provided in the quicklook normal point
         format. The directory contains the normal point files
         downloaded from the ILRS data centers (either CDDIS or EDC).
         For the example BPEs we provide reduced allsat files contain-
         ing only the SLR normal points to the GNSS satellites of the
         respective date.

- STAT_LOG: This directory contains the station information files
         (e.g., from ftp://igscb.jpl.nasa.gov/pub/station/general).
         This information may be completed by the originary informa-
         tion on the reference frame (e.g., the IGb08.snx from
         ftp://igscb.jpl.nasa.gov/pub/station/coord).
         It also contains (apart from the coordinates and velocities
         of selected IGS sites) the history of the used equipment as
         it has been assumed for the reference frame generation. A
         comparison with the igs.snx file constructed at the IGSCB
         (IGS Central Bureau) from the site information files may be
         useful for a verification of the history records.

- COD/ESA/IGS: Orbits, EOP, and satellite clock corrections are basic
         external information for a GNSS analysis. The source of the
         files may be the FTP server from CODE
         (ftp://ftp.unibe.ch/aiub/CODE), or the CDDIS FTP server
         (e.g., for downloading GPS-related IGS products
         ftp://cddis.gsfc.nasa.gov/gps/products and in
         ftp://cddis.gsfc.nasa.gov/glonass/products for GLONASS
         related IGS products). The files are named with the GPS-week
         and the day of the week (apart from files containing infor-
         mation for the entire week, e.g., ERP, or the processing
         summaries). The IGS provides GPS and GLONASS orbits only in
         separate files (IGS/IGL-series from the final product line)
         stemming from independent combination procedures with dif-
         ferent contributing analysis centers. Nevertheless, they are
         consistent enough to merge both files together as the first
         step of the processing. CODE as well as other analysis
         centers of the IGS (e.g., ESA or GFZ) contribute fully com-
         bined multi-GNSS solutions to the IGS final (and ultra-rapid)
         product line. The contributions of these groups are already
         combined GPS/GLONASS orbit files.

- LEO:   This directory is intended to host files which are necessary
         for LEO data processing. RINEX files are stored in the
         subdirectory RINEX (of the LEO directory). The corresponding
         attitude files are placed in the subdirectory ATTIT.
         Some LEO RINEX files (e.g., for CHAMP) are available at
         anonymous ftp-servers like the CDDIS server. To access data
         from other missions, one often has to be a registered user,
         as for example for the ISDC at GFZ. GRACE data is only
         available in a specific binary data format which has to be
         converted to ASCII by a program available on the data server.
         The GRACE data for the example BPE has been converted by
         ourselves.

- BSW52: In this directory we have placed files containing external
         input information in Bernese specific formats. The files are
         neutral with respect to the data you are going to process.
         Typical examples are ionosphere maps or differencial code
         bias (DCB) files. These files can be downloaded from
         ftp://ftp.unibe.ch/aiub/CODE or
         ftp://ftp.unibe.ch/aiub/BSWUSER52 areas.

- REF52: Here we propose to collect files in Bernese format which are
         useful for several campaigns. Typical examples are station
         coordinate, velocity, and information files. All stations of
         a project are contained in one file but the processing of the
         project's data may be performed in different campaigns.

- MSC:   This directory contains two files that are intended to be
         located and maintained in the campaign area. These files are
         only used by BPE user scripts and are therefore not distri-
         buted in the ${X}/DOC (or %X%\DOC) directory together with
         other example files. These are
         - EXAMPLE.OBSSEL: An example for the observation file
               selection (containing the four-character abbreviations
               from the RINEX files). It may be used by the
               PPP_BAS.PCF, PPP_DEMO.PCF, RNX2SNX.PCF, and CLKDET.PCF
               example BPEs.
         - SATSEL.FIX: a selection list of satellites to be included
               in the SLR processing using the BPE SLRVAL.PCF.

- VMF1:  The grids for the Vienna Mapping Function (VMF1) are located
         in a separate directory. They are not used for the examples
         but it shall indicate that for other types of files other
         directories may be created.


You don't have to follow the the structure of the DATAPOOL we are
using to provide the example data. It is derived from the structure
as we have established it at our institute. You are free to adopt or
adapt it. Needless to say that corresponding paths to the data in the
scripts copying the data into your campaign area for processing need
to be in adquation with your DATAPOOL structure.
It may be useful, for instance, to add yearly subdirectories if you
expect a large amount of data (in order to limit the number of files
in a directory).


----------------------------------------------------------------------
Location of the DATAPOOL area
----------------------------------------------------------------------
For practical reasons the DATAPOOL area is located in the GPSDATA
environment when the software is distributed to the users. The user is
free to move it to another place in his system. It is used in conjunc-
tion with a specific DATAPOOL path variable ${D} that is defined in
the ${X}/EXE/LOADGPS.setvar environment file (UNIX) or in the registry
of the current user (Windows).

To change the location of the DATAPOOL area follow these steps:

UNIX-Users:
1) Move the DATAPOOL tree to the new location.
2) Change the value for the DATAPOOL path variable ${D} in the file
   ${X}/EXE/LOADGPS.setvar.
3) Reload the Bernese environment variables by running
   ". ${X}/EXE/LOADGPS.setvar"   or   "source ${X}/EXE/LOADGPS.setvar"

WINDOWS-Users:
1) Move the DATAPOOL tree to the new location.
2) Open the dialog box to define the Windows user variables
3) Change the value of the variable "D" to the new location of the
   DATAPOOL area.
4) Relogin (possibly a reboot may be necessary)

The new location of the DATAPOOL area is now available on the system.


----------------------------------------------------------------------
Putting data from the DATAPOOL area into the campaign
----------------------------------------------------------------------
Each BPE starts with a few copy scripts. We recommend to call them
RNX_COP (generic, used in all example BPEs) and XXX_COP (where XXX 
stands for an identifier of the BPE, e.g., PPP). The first of the 
two scripts copies the RINEX files for all selected stations of the 
session being processed into the campaign. The second script calls 
the BPE-utility (bpe_util) "copyRef" to copy all necessary files 
from the $D/REF52 directory into the campaign. Additional 
information (like orbits, ERP, or ionosphere maps for example) 
must also copied according to the needs of the BPE.

======================================================================
