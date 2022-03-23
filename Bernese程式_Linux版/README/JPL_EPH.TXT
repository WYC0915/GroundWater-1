======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 09-May-2013
----------------------------------------------------------------------
Installation of the JPL planetary and lunar ephemerides
======================================================================


----------------------------------------------------------------------
Source of ephemerides
----------------------------------------------------------------------
The JPL ephemerides are prepared at JPL by M.E. Standish et al.
Information may be found at

  http://ssd.jpl.nasa.gov?planet_eph_export

The ephemerides may be downloaded from this site (see the section
'Download the ephemerides' below).
There is also a CD available containing binary and ASCII versions of
the JPL ephemerides covering the time interval from 1599 to 2200. The
CD may be purchased for $24.95 from William-Bell, Inc. See

  http://www.willbell.com/software/jpl.htm


----------------------------------------------------------------------
Download the ephemerides
----------------------------------------------------------------------
The ephemeris package is available from the anonymous ftp server

  ssd.jpl.nasa.gov

When connected change to the directory "pub/eph/planets/ascii/de405".

Get the following files:

  header.405
  testpo.405
  ascp1980.405    : covers the time interval between 1980 and 2000
  ascp2000.405    : covers the time interval between 2000 and 2020


----------------------------------------------------------------------
Installation procedure
----------------------------------------------------------------------
The Fortran programs needed for the installation of the JPL epheme-
rides on your computer are included in the distribution of the Bernese
GNSS Software. The programs are the original JPL routines, slightly
adapted to simplify their usage.

The ephemeris files have to be downloaded as ASCII files and converted
into binary format on the target machine using the program "ASC2EPH".
The ASCII files available at JPL cover a time interval of 20 years
each. The required files are listed above in Section 'Download the
ephemerides'.

Note: please make sure that the Bernese environment variables (e.g.
      ${XG}) are available when you run the programs.

      - UNIX platforms:
          Make sure that the environment variables are loaded.
          Depending on your shell use
          ". BERN52/GPS/LOADGPS.setvar" or
          "source BERN52/GPS/LOADGPS.setvar"
          (adapt the path to LOADGPS.setvar if necessary).
          You may add this step into your login script.

      - WINDOWS platforms:
          You need to reboot (win9x systems) or to re-login after
          running the software installation scripts.

1) Concatenate the ASCII files:

   - Concatenate the header file and the ASCII ephemeris files to one
     temporary file:

     - UNIX:    cat header.405    > temp.405
                cat ascp1980.405 >> temp.405
                cat ascp2000.405 >> temp.405

     - Windows: copy header.405+ascp1980.405+ascp2000.405 temp.405

2) Convert the file from ASCII to binary

   Prepare an input file ASC2EPH.INP containing:
     - First line:  file name of the concatenated ASCII file
     - Second line: file name of the resulting binary file (for
       example JPLEPH)

     Example:
       echo temp.405 >ASC2EPH.INP
       echo JPLEPH  >>ASC2EPH.INP

   Run the program in the directory of the file "temp.405":

     - UNIX:    Use the command ${XG}/ASC2EPH

     - Windows: Use the command %XG%\ASC2EPH

   This will create the file JPLEPH containing the binary DE405
   ephemeris.

3) Check the readability of the file and test the values against
   "testpo.405".

   - Prepare an input file TESTEPH.INP containing:
     - First line:  name of the binary file which you want to test
                    (e.g., JPLEPH)
     - Second line: name of the test-data file (testpo.405)
     - Third line:  DE number of the ephemeris (DE405)

     Example for TESTEPH.INP:
       JPLEPH
       testpo.405
       DE405

   - Run the program:
     - UNIX:    use the command ${XG}/TESTEPH

     - Windows: use the command %XG%\TESTEPH

     There should be no warnings or errors.
     The output of the program "TESTEPH" will report the time span of
     the newly generated ephemeris file JPLEPH given in Julian Epheme-
     ris Days (2444208.50 to 2458864.50).
     The last column at the bottom gives the differences to the test
     file.

     Example output:

 line - jed -  ...  --- jpl value ---  --- user value --- --  diff. --

 100 2447221.5 ...   -0.4321082450831    -0.4321082450831  0.36138E-13
 200 2450265.5 ... 2269.9483911098296  2269.9483911098296 -0.00000E+00
 300 2453310.5 ...   -0.0104722325301    -0.0104722325301  0.15380E-13
 400 2456352.5 ...    0.1312929459907     0.1312929459907  0.30143E-13


----------------------------------------------------------------------
Implementation of the JPL ephemerides
----------------------------------------------------------------------
Within the Bernese GNSS Software the JPL ephemerides
- are located in the directory of general files:
     ${X}/GEN (UNIX notation) resp. %X%\GEN (Windows notation)
- have the extension .EPH
- have the default name DE405.EPH.

To implement the newly generated ephemeris file into the Bernese GNSS
Software simply copy the file "JPLEPH" to the name DE405.EPH in the
${X}/GEN (UNIX) resp. %X%\GEN (Windows) directory.

- UNIX:    cp JPLEPH $X/GEN/DE405.EPH

- Windows: copy JPLEPH %X%\GEN\DE405.EPH

======================================================================
