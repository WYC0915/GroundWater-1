#!/usr/bin/perl -w

# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.0
# -------------------------------------------------------------------------
#
# Script:     PRSLIN.pl
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

my($fullname) = <STDIN>;
my($opt)      = <STDIN>;

my($name,$path,$suffix);

($name, $path, $suffix) = fileparse($fullname, '\..*');

if (defined($opt) && $opt eq "-F") {
  print $name . $suffix, "\n";
}
else {
  print $name, "\n";
}

