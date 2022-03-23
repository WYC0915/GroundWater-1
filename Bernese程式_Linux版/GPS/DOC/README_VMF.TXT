======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 24-Oct-2012
----------------------------------------------------------------------
Installation of the VMF1 coefficient grid files
======================================================================


----------------------------------------------------------------------
Introduction
----------------------------------------------------------------------
In the last years, troposphere mapping functions have been developed
on the basis of data from numerical weather models, e.g., from the
European Centre for Medium-Range Weather Forecasts (ECMWF)
[http://www.ecmwf.int]. The Vienna Mapping Functions (VMF1) are rely-
ing on empirical equations for the "b" and "c" coefficients of the
continued fraction form, whereas the "a" coefficients are determined
from rigorously ray-traced mapping functions at 3 degrees elevation.
The VMF1 coefficients are provided on a global grid (2.5 x 2.0 de-
grees) as well as for selected regions with a resolution of 0.25
degrees for recent data.

  Source: http://ggosatm.hg.tuwien.ac.at/DELAY/readme.txt


----------------------------------------------------------------------
Source of VMF1 grid files
----------------------------------------------------------------------
VMF1 grid files with a time resolution of 6 hours can be downloaded
from

  http://ggosatm.hg.tuwien.ac.at/DELAY/GRID/VMFG/yyyy

where the file names are VMFG_yyyymmdd.Hhh
   yyyy: year
     mm: month
     dd: day of month
     hh: hour of the day, either 00, 06, 12, or 18.


----------------------------------------------------------------------
Integrate VMF1 grid files into a Bernese campaign
----------------------------------------------------------------------
By default, grid files are stored in the GRD directory within a
Bernese campaign and have the file extension ".GRD".
It is recommended to merge the original 6 hourly files to 24-hour
files and to rename the resulting VMF1 grid file in the campaign
directory to, e.g.:

  VMF$YD+n.GRD

For a specific day you will need the five(!) VMFG files: four from the
day itself (00, 06, 12, and 18) plus the 00 file from the following
day. You can concatenate the consecutive files without changing/remo-
ving the (intermediate) file headers.

Make sure you have all necessary grid files copied to the campaign
directory for the session(s) you are about to analyze.


----------------------------------------------------------------------
Remarks
----------------------------------------------------------------------
- Only the grid format of the VMF1 grid files are supported in the
  Bernese GNSS Software.

- Other information in the same grid format may be used as well.
  Example: Atmospheric pressure loading coefficients from
           http://ggosatm.hg.tuwien.ac.at/LOADING/VERSION2/GRID

======================================================================
