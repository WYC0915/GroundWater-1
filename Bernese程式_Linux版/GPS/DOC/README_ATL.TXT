======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 03-Oct-2012
----------------------------------------------------------------------
Installation of the S1/S2 atmospheric tidal loading grid files
======================================================================


----------------------------------------------------------------------
Source of the S1/S2 displacement grids
----------------------------------------------------------------------
On the web site of the Global Geophysical Fluid Center (GGFC)

  http://geophy.uni.lu/ggfc-atmosphere/tide-loading-calculator.html

the atmospheric tidal loading displacement grids refering either to
- CE: center of the solid Earth
  (can be used in this context also as "CF: center of figure")
- CM: center of the mass
are provided.
The corrections are given for the Ray and Ponte model [2003] according
to the IERS 2010 conventions.

The file "s1_s2_def_ce.dat" is distributed with the Bernese GNSS
Software in the "${X}/GEN" directory.


IMPORTANT NOTE:
If you plan to correct also for atmospheric non-tidal loading, it is
essential to use a consistent tidal loading contribution. The format
of the expected grid file is described in the last section of this
file.


----------------------------------------------------------------------
Implementation of the displacement grids
----------------------------------------------------------------------
S1/S2 loading coefficients are stored in the STA directory of a
Bernese campaign.

Use the program GRDS1S2 from

  Menu >Service >Coordinate tools >Extract atmospheric tidal loading
                                   coefficients

to generate the ".ATL" file for a set of station coordinates given in
a Bernese formatted coordinate file.

The program GRDS1S2.f is a derivative of the program grdintrp.f from
the above mentioned web-site. It has been adapted for the environment
of Bernese GNSS Software to read and write files in the expected for-
mats (courtesy to Tonie van Dam, University of Luxembourg).


----------------------------------------------------------------------
Using alternative atmospheric tidal loading grids
----------------------------------------------------------------------
At beginning of the file a short description of the data may be given
that is copied into the resulting Bernese formatted ".ATL" file. These
comments are indicated by "$$ " at the beginning of the line.

The data part consists of 14 columns with real numbers (no column
width is predefined but they must be seperated by at least one blank
character):
-  1/ 2:  longitude, latitude
-  3/ 4:  cosine and sine terms for S1 in the radial direction
-  5/ 6:  cosine and sine terms for S2 in the radial direction
-  7/ 8:  cosine and sine terms for S1 in the north direction
-  9/10:  cosine and sine terms for S2 in the north direction
- 11/12:  cosine and sine terms for S1 in the east direction
- 13/14:  cosine and sine terms for S2 in the east direction

If you introduce a grid with a higher resolution than 1-degree you
have to increase the arrays defined by MXLON and MXLAT in
$FG/GRDS1S2.f (or %FG:\GRDS1S2.f).

======================================================================
