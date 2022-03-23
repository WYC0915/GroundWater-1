#!/usr/bin/perl -w

# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.0
# -------------------------------------------------------------------------
#
# Script:     MATCH.pl
#
# Purpose:    Match a pattern within a given string
#
# Example:    MATCH.pl GPSEST.L07 '.*\.L(\d\d)\s*$'
#
# Author:     L. Mervart
#
# Created:    13-JUN-2002
#
# Changes:
#
# Copyright:  Astronomical Institute
#              University of Berne
#                  Switzerland
# -------------------------------------------------------------------------

use strict;

my($string, $pattern) = @ARGV;

if (!defined($pattern)) {
  die "Usage: MATCH.pl string pattern\n";
}

if  ( $string =~ /$pattern/ ) {
  print "$1\n";
}

