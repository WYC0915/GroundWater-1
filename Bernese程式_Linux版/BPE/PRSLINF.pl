#!/usr/bin/perl -w

# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.0
# -------------------------------------------------------------------------
#
# Script:     PRSLINF.pl
#
# Purpose:    Get a file name without leading path
#
# Author:     L. Mervart
#
# Created:    26-FEB-2002
#
# Changes:
#
# Copyright:  Astronomical Institute
#              University of Berne
#                  Switzerland
# -------------------------------------------------------------------------

use strict;
use File::Basename;

my($path);

while ($path = <STDIN>) {
  print basename($path), "\n";
}

