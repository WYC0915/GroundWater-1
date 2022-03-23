======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 31-Oct-2012
----------------------------------------------------------------------
SAVEDISK area
======================================================================


----------------------------------------------------------------------
Motivation for the SAVEDISK area
----------------------------------------------------------------------
When processing GNSS data, a lot of files from various processing
steps will populate your campaign directories. The main result files
from the data analysis are collected in the SAVEDISK area. This area
is intended as long-term archive for your result files.
Because the result files are stored in the SAVEDISK area, you can
easily clean up your campaign area without loosing important files.
Please keep in mind that the computing performance decreases if you
have several thousands of files in a directory.


----------------------------------------------------------------------
Structure and content of the SAVEDISK area
----------------------------------------------------------------------
We propose to build subdirectories in the SAVEDISK area for each of
your projects. If these projects collect data over several years
yearly subdirectories are recommended. It is also practical to use
further subdirectories like ATM, ORB, OUT, SOL, STA to distribute the
files and to get shorter listings if you are looking for a file.

Of course you are free to organize your own structure in the SAVEDISK
area. You only have to adapt the corresponding XXX_SAV scripts in your
BPEs (with XXX as an identifier of the BPE, e.g., PPP).


----------------------------------------------------------------------
Location of the SAVEDISK area
----------------------------------------------------------------------
For practical reasons the SAVEDISK area is located in the GPSDATA
environment when the software is distributed to the users. The user
is free to move it to another place in his system. It is used in
conjunction with a specific SAVEDISK path variable ${S} that is
defined in the ${X}/EXE/LOADGPS.setvar environment file (on UNIX) or
in the registry of the current user (on Windows).

To change the location of the SAVEDISK area follow these steps:

UNIX-Users:
1) Move the SAVEDISK tree to the new location.
2) Change the value for the SAVEDISK path variable ${S} in the file
   ${X}/EXE/LOADGPS.setvar.
3) Reload the Bernese environment variables by running
   ". ${X}/EXE/LOADGPS.setvar"   or   "source ${X}/EXE/LOADGPS.setvar"

WINDOWS-Users:
1) Move the SAVEDISK tree to the new location.
2) Open the dialog box to define the Windows user variables
3) Change the value of the variable "S" to the new location of the
   SAVEDISK area.
4) Relogin (possibly a reboot may be necessary)

The new location of the SAVEDISK area is now available on the system.


----------------------------------------------------------------------
Putting data from the campaign into the SAVEDISK area
----------------------------------------------------------------------
Each BPE ends with a sequence of scripts to generate the processing
protocol, to put the result files from the processing CAMPAIGN into
the SAVEDISK area, and to clean up the campaign directory structure.
We recommend to name them XXX_SUM, XXX_SAV, and XXX_DEL (where XXX
stands for an identifier of the BPE, e.g., PPP).

The script to save the result files into the SAVEDISK area (provided
for each processing examples) is organized as follows:
- A V_RESULT variable gives the name of the directory where the result
  files will be stored in the SAVEDISK area.
- The recommended directory structure is applied (see above). If you
  want to use another structure in the SAVEDISK area you have to
  organize it in the XXX_SAV script.
- In the XXX_SAV script first an array is filled with strings cont-
  aining the source file, the target directory and a flag indica-
  ting whether the file shall be compressed or be stored as a plain
  text file. This array is an argument of the routine "copy2archive"
  provided by the BPE-utility (${BPE}/bpe_util.pm or
  %BPE%\bpe_util.pm).
Please have a look into the corresponding example scripts provided
with the distribution for the processing examples (see script
${U}/SCRIPT/???_SAV or %U%\SCRIPT\???_SAV).

There is a BPE server variable V_SAV in the provided processing
example BPEs. It needs to be set to "YES" that the result files
are copied into the SAVEDISK area. Running the BPEs without storing
result files in the SAVEDISK area is mainly intended for experiment
or development phases.


----------------------------------------------------------------------
Content of the SAVEDISK in the distribution
----------------------------------------------------------------------
The SAVEDISK area contains after its installation a directory struc-
ture according to the description above. Each example BPE is assumed
as a project. Therefore, you will find on the top level of the
SAVEDISK the directories PPP, RNS2SNX, CLKDET, LEOPOD, and SLRVAL.

In each of these directories you will find several files ending with
_REF. They are generated by running the example BPEs on the system at
AIUB.
If you have run the example BPEs after the installation with the given
settings you can compare your results with these reference files. You
should obtain the same results (apart from effects introduced by
different compilers and/or operating systems which have been used).


The following reference files are provided:

PPP_DEMO.PCF
------------
PPP/2010/OUT/PPP102070.PRC_REF          PPP/2010/OUT/PPP102080.PRC_REF
PPP/2011/OUT/PPP112050.PRC_REF          PPP/2011/OUT/PPP112060.PRC_REF

PPP/2010/SOL/RED102070.SNX.gz_REF       PPP/2010/SOL/RED102080.SNX.gz_REF
PPP/2011/SOL/RED112050.SNX.gz_REF       PPP/2011/SOL/RED112060.SNX.gz_REF
PPP/2010/OUT/PPP102070.CLK.gz_REF       PPP/2010/OUT/PPP102080.CLK.gz_REF
PPP/2011/OUT/PPP112050.CLK.gz_REF       PPP/2011/OUT/PPP112060.CLK.gz_REF
PPP/2010/STA/PPP102070.CRD_REF          PPP/2010/STA/PPP102080.CRD_REF
PPP/2011/STA/PPP112050.CRD_REF          PPP/2011/STA/PPP112060.CRD_REF
PPP/2010/ATM/PPP102070.TRP_REF          PPP/2010/ATM/PPP102080.TRP_REF
PPP/2011/ATM/PPP112050.TRP_REF          PPP/2011/ATM/PPP112060.TRP_REF
PPP/2010/ATM/PPP102070.TRO_REF          PPP/2010/ATM/PPP102080.TRO_REF
PPP/2011/ATM/PPP112050.TRO_REF          PPP/2011/ATM/PPP112060.TRO_REF


PPP_DEMO.PCF extensions
-----------------------
Geocenter estimation:
PPP/2010/STA/GCC102070.CRD_REF          PPP/2010/STA/GCC102080.CRD_REF
PPP/2011/STA/GCC112050.CRD_REF          PPP/2011/STA/GCC112060.CRD_REF
PPP/2010/STA/GCC102070.GCC_REF          PPP/2010/STA/GCC102080.GCC_REF
PPP/2011/STA/GCC112050.GCC_REF          PPP/2011/STA/GCC112060.GCC_REF

Kinematic solutions:
PPP/2010/OUT/KIN2070GANP.SUM_REF        PPP/2010/OUT/KIN2080GANP.SUM_REF
PPP/2010/OUT/KIN2070HERT.SUM_REF        PPP/2010/OUT/KIN2080HERT.SUM_REF
    ....                                    ....
PPP/2011/OUT/KIN2050GANP.SUM_REF        PPP/2011/OUT/KIN2060GANP.SUM_REF
PPP/2011/OUT/KIN2050HERT.SUM_REF        PPP/2011/OUT/KIN2060HERT.SUM_REF
    ....                                    ....
PPP/2010/STA/KIN2070GANP.KIN_REF        PPP/2010/STA/KIN2080GANP.KIN_REF
PPP/2010/STA/KIN2070HERT.KIN_REF        PPP/2010/STA/KIN2080HERT.KIN_REF
    ....                                    ....
PPP/2011/STA/KIN2050GANP.KIN_REF        PPP/2011/STA/KIN2060GANP.KIN_REF
PPP/2011/STA/KIN2050HERT.KIN_REF        PPP/2011/STA/KIN2060HERT.KIN_REF
    ....                                    ....

High-rate troposphere solutions:
PPP/2010/ATM/TRP102070.TRP_REF          PPP/2010/ATM/TRP102080.TRP_REF
PPP/2011/ATM/TRP112050.TRP_REF          PPP/2011/ATM/TRP112060.TRP_REF
PPP/2010/ATM/TRP102070.TRO_REF          PPP/2010/ATM/TRP102080.TRO_REF
PPP/2011/ATM/TRP112050.TRO_REF          PPP/2011/ATM/TRP112060.TRO_REF
PPP/2010/STA/TRP102070.CRD_REF          PPP/2010/STA/TRP102080.CRD_REF
PPP/2011/STA/TRP112050.CRD_REF          PPP/2011/STA/TRP112060.CRD_REF

Ionosphsere solutions:
PPP/2010/ATM/RIM102070.ION_REF          PPP/2010/ATM/RIM102080.ION_REF
PPP/2011/ATM/RIM112050.ION_REF          PPP/2011/ATM/RIM112060.ION_REF
PPP/2010/ATM/RIM102070.INX_REF          PPP/2010/ATM/RIM102080.INX_REF
PPP/2011/ATM/RIM112050.INX_REF          PPP/2011/ATM/RIM112060.INX_REF
PPP/2010/ORB/RIM102070.DCB_REF          PPP/2010/ORB/RIM102080.DCB_REF
PPP/2011/ORB/RIM112050.DCB_REF          PPP/2011/ORB/RIM112060.DCB_REF

PPP/2010/ATM/ION102070.ION_REF          PPP/2010/ATM/ION102080.ION_REF
PPP/2011/ATM/ION112050.ION_REF          PPP/2011/ATM/ION112060.ION_REF
PPP/2010/ORB/ION102070.DCB_REF          PPP/2010/ORB/ION102080.DCB_REF
PPP/2011/ORB/ION112050.DCB_REF          PPP/2011/ORB/ION112060.DCB_REF


RNX2SNX.PCF
-----------
RNX2SNX/2010/OUT/R2S102070.PRC_REF      RNX2SNX/2010/OUT/R2S102080.PRC_REF
RNX2SNX/2011/OUT/R2S112050.PRC_REF      RNX2SNX/2011/OUT/R2S112060.PRC_REF

RNX2SNX/2010/SOL/F1_102070.SNX.gz_REF   RNX2SNX/2010/SOL/F1_102080.SNX.gz_REF
RNX2SNX/2011/SOL/F1_112050.SNX.gz_REF   RNX2SNX/2011/SOL/F1_112060.SNX.gz_REF

RNX2SNX/2010/STA/F1_102070.CRD_REF      RNX2SNX/2010/STA/F1_102080.CRD_REF
RNX2SNX/2011/STA/F1_112050.CRD_REF      RNX2SNX/2011/STA/F1_112060.CRD_REF
RNX2SNX/2010/ATM/F1_102070.TRP_REF      RNX2SNX/2010/ATM/F1_102080.TRP_REF
RNX2SNX/2011/ATM/F1_112050.TRP_REF      RNX2SNX/2011/ATM/F1_112060.TRP_REF
RNX2SNX/2010/ATM/F1_102070.TRO_REF      RNX2SNX/2010/ATM/F1_102080.TRO_REF
RNX2SNX/2011/ATM/F1_112050.TRO_REF      RNX2SNX/2011/ATM/F1_112060.TRO_REF


CLKDET.PCF
----------
CLKDET/2010/OUT/CLK102070.PRC_REF       CLKDET/2010/OUT/CLK102080.PRC_REF
CLKDET/2011/OUT/CLK112050.PRC_REF       CLKDET/2011/OUT/CLK112060.PRC_REF

CLKDET/2010/ORB/CB_102070.DCB_REF       CLKDET/2010/ORB/CB_102080.DCB_REF
CLKDET/2011/ORB/CB_112050.DCB_REF       CLKDET/2011/ORB/CB_112060.DCB_REF

CLKDET/2010/ATM/TP_102070.TRP_REF       CLKDET/2010/ATM/TP_102080.TRP_REF
CLKDET/2011/ATM/TP_112050.TRP_REF       CLKDET/2011/ATM/TP_112060.TRP_REF
CLKDET/2010/ORB/TP_102070.CLK_REF       CLKDET/2010/ORB/TP_102080.CLK_REF
CLKDET/2011/ORB/TP_112050.CLK_REF       CLKDET/2011/ORB/TP_112060.CLK_REF
CLKDET/2010/OUT/TP_102070.CLK.gz_REF    CLKDET/2010/OUT/TP_102080.CLK.gz_REF
CLKDET/2011/OUT/TP_112050.CLK.gz_REF    CLKDET/2011/OUT/TP_112060.CLK.gz_REF
CLKDET/2010/STA/TP_102070.CRD_REF       CLKDET/2010/STA/TP_102080.CRD_REF
CLKDET/2011/STA/TP_112050.CRD_REF       CLKDET/2011/STA/TP_112060.CRD_REF

CLKDET/2010/STA/CW_102070.SOS_REF       CLKDET/2010/STA/CW_102080.SOS_REF
CLKDET/2011/STA/CW_112050.SOS_REF       CLKDET/2011/STA/CW_112060.SOS_REF


LEOPOD.PCF
----------
LEOPOD/2010/OUT/GRCAAF102070.PRC_REF    LEOPOD/2010/OUT/GRCAAF102080.PRC_REF

LEOPOD/2010/ORB/RDAF102080.PRE_REF      LEOPOD/2010/ORB/RDAF102080.STD_REF
LEOPOD/2010/ORB/RDAF102070.STD_REF      LEOPOD/2010/ORB/RDAF102080.ELE_REF
LEOPOD/2010/ORB/RDAF102070.ELE_REF      LEOPOD/2010/ORB/RDAF102070.PRE_REF
LEOPOD/2010/OUT/RDAF102070.RES_REF      LEOPOD/2010/OUT/RDAF102080.RES_REF

LEOPOD/2010/STA/KNAF102070.KIN_REF      LEOPOD/2010/STA/KNAF102080.KIN_REF
LEOPOD/2010/ORB/KNAF102070.PRE_REF      LEOPOD/2010/ORB/KNAF102080.PRE_REF
LEOPOD/2010/OUT/KNAF102070.CLK.gz_REF   LEOPOD/2010/OUT/KNAF102080.CLK.gz_REF
LEOPOD/2010/OUT/KNAF102070.RES_REF      LEOPOD/2010/OUT/KNAF102080.RES_REF


SLRVAL.PCF
----------
SLRVAL/2010/SLRVAL10207.PRC_REF         SLRVAL/2010/SLRVAL10208.PRC_REF
SLRVAL/2011/SLRVAL11205.PRC_REF         SLRVAL/2011/SLRVAL11206.PRC_REF


The reference files have been computed:
----------------------------------------------------------------------
Bernese GNSS Software Version 5.2
Release:  2013-07-17

between   17-Jul-2013 10:40:58   and   17-Jul-2013 13:27:40
----------------------------------------------------------------------
Fortran compiler:
G95 (GCC 4.0.3 (g95 0.92!) Jun 24 2009)
Copyright (C) 2002-2008 Free Software Foundation, Inc.

Perl interpreter:
This is perl, v5.10.1 (*) built for x86_64-linux-thread-multi

C compiler:
gcc (GCC) 4.4.7 20120313 (Red Hat 4.4.7-3)
Copyright (C) 2010 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

QT-library:
QMake version 2.01a
Using Qt version 4.7.4 in /gpfs/homefs/aiub/l_bernese/general/tools/qt_4.7.4_linux-g++-64/lib
----------------------------------------------------------------------
Operating system:
Linux sedna.ubelix.unibe.ch 2.6.32-358.11.1.el6.x86_64  x86_64 x86_64 x86_64 GNU/Linux
----------------------------------------------------------------------


